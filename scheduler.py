#!/usr/bin/env python3
"""
ljwx-stock模型自动调度训练脚本
支持定时训练和监控
"""

import os
import sys
import time
import logging
import subprocess
import schedule
import json
from datetime import datetime, timedelta
from typing import Dict, List

class TrainingScheduler:
    """训练调度器"""
    
    def __init__(self):
        self.logger = logging.getLogger(__name__)
        self.config = {
            'log_dir': 'logs',
            'results_dir': 'data/models',
            'training_script': 'continuous_training.py',
            'max_models_to_keep': 5,
            'min_success_rate': 0.8
        }
        
        # 确保目录存在
        os.makedirs(self.config['log_dir'], exist_ok=True)
        os.makedirs(self.config['results_dir'], exist_ok=True)
        
        # 配置日志
        self.setup_logging()
    
    def setup_logging(self):
        """设置日志"""
        log_file = os.path.join(self.config['log_dir'], f"scheduler_{datetime.now().strftime('%Y%m%d')}.log")
        
        logging.basicConfig(
            level=logging.INFO,
            format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
            handlers=[
                logging.StreamHandler(),
                logging.FileHandler(log_file, encoding='utf-8')
            ]
        )
    
    def run_training(self) -> Dict:
        """运行训练任务"""
        self.logger.info("🚀 开始定时训练任务")
        
        try:
            # 运行持续训练脚本
            result = subprocess.run(
                [sys.executable, self.config['training_script']],
                capture_output=True,
                text=True,
                timeout=3600  # 1小时超时
            )
            
            if result.returncode == 0:
                self.logger.info("✅ 训练任务完成")
                return {"status": "success", "output": result.stdout}
            else:
                self.logger.error(f"❌ 训练任务失败: {result.stderr}")
                return {"status": "error", "error": result.stderr}
                
        except subprocess.TimeoutExpired:
            self.logger.error("❌ 训练任务超时")
            return {"status": "timeout", "error": "Training timeout"}
        except Exception as e:
            self.logger.error(f"❌ 训练任务异常: {e}")
            return {"status": "exception", "error": str(e)}
    
    def get_model_performance_history(self) -> List[Dict]:
        """获取模型性能历史"""
        performance_history = []
        results_dir = self.config['results_dir']
        
        for filename in os.listdir(results_dir):
            if filename.startswith('continuous_training_results_') and filename.endswith('.json'):
                file_path = os.path.join(results_dir, filename)
                
                try:
                    with open(file_path, 'r', encoding='utf-8') as f:
                        data = json.load(f)
                        
                    if 'performance' in data and 'new_model' in data:
                        performance_history.append({
                            'timestamp': data.get('start_time'),
                            'model_name': data.get('new_model'),
                            'success_rate': data['performance']['performance']['success_rate'],
                            'data_count': data.get('total_data_count', 0),
                            'status': data.get('status')
                        })
                        
                except Exception as e:
                    self.logger.warning(f"读取性能文件失败 {filename}: {e}")
        
        # 按时间戳排序
        performance_history.sort(key=lambda x: x['timestamp'])
        return performance_history
    
    def cleanup_old_models(self):
        """清理旧模型"""
        try:
            # 获取所有ljwx-stock模型
            result = subprocess.run(
                ['ollama', 'list'],
                capture_output=True,
                text=True
            )
            
            if result.returncode != 0:
                return
            
            lines = result.stdout.strip().split('\n')
            ljwx_models = []
            
            for line in lines[1:]:  # 跳过标题行
                if 'ljwx-stock-continuous' in line:
                    model_info = line.split()
                    if len(model_info) >= 4:
                        model_name = model_info[0]
                        modified_time = ' '.join(model_info[3:])
                        ljwx_models.append({
                            'name': model_name,
                            'modified': modified_time
                        })
            
            # 保留最新的几个模型
            if len(ljwx_models) > self.config['max_models_to_keep']:
                # 按名称排序（包含时间戳）
                ljwx_models.sort(key=lambda x: x['name'])
                
                # 删除旧模型
                models_to_delete = ljwx_models[:-self.config['max_models_to_keep']]
                
                for model in models_to_delete:
                    try:
                        subprocess.run(
                            ['ollama', 'rm', model['name']],
                            capture_output=True,
                            text=True
                        )
                        self.logger.info(f"🗑️  删除旧模型: {model['name']}")
                    except Exception as e:
                        self.logger.warning(f"删除模型失败 {model['name']}: {e}")
                        
        except Exception as e:
            self.logger.error(f"清理旧模型失败: {e}")
    
    def monitor_model_performance(self):
        """监控模型性能"""
        performance_history = self.get_model_performance_history()
        
        if len(performance_history) < 2:
            return
        
        latest = performance_history[-1]
        previous = performance_history[-2]
        
        # 检查性能是否下降
        if latest['success_rate'] < self.config['min_success_rate']:
            self.logger.warning(f"⚠️  模型性能下降: {latest['success_rate']:.2%}")
        
        # 比较最新模型和之前模型的性能
        performance_change = latest['success_rate'] - previous['success_rate']
        
        if performance_change > 0.1:
            self.logger.info(f"📈 模型性能提升: +{performance_change:.2%}")
        elif performance_change < -0.1:
            self.logger.warning(f"📉 模型性能下降: {performance_change:.2%}")
    
    def generate_training_report(self) -> str:
        """生成训练报告"""
        performance_history = self.get_model_performance_history()
        
        if not performance_history:
            return "暂无训练历史"
        
        latest = performance_history[-1]
        
        report = f"""
📊 ljwx-stock模型训练报告
{'='*50}

🕒 最新训练时间: {latest['timestamp']}
🤖 最新模型: {latest['model_name']}
📈 成功率: {latest['success_rate']:.2%}
📦 训练数据量: {latest['data_count']}条

📋 历史性能趋势:
"""
        
        for i, perf in enumerate(performance_history[-5:]):  # 显示最近5次
            report += f"  {i+1}. {perf['timestamp'][:10]} - {perf['success_rate']:.2%} ({perf['data_count']}条)\n"
        
        # 计算平均性能
        if len(performance_history) > 1:
            avg_success_rate = sum(p['success_rate'] for p in performance_history) / len(performance_history)
            report += f"\n📊 平均成功率: {avg_success_rate:.2%}"
        
        return report
    
    def scheduled_training_job(self):
        """定时训练任务"""
        self.logger.info("⏰ 执行定时训练任务")
        
        # 1. 运行训练
        result = self.run_training()
        
        # 2. 监控性能
        self.monitor_model_performance()
        
        # 3. 清理旧模型
        self.cleanup_old_models()
        
        # 4. 生成报告
        report = self.generate_training_report()
        self.logger.info(f"训练报告:\n{report}")
        
        return result
    
    def start_scheduler(self):
        """启动调度器"""
        self.logger.info("🚀 启动ljwx-stock模型训练调度器")
        
        # 设置调度任务
        schedule.every().day.at("02:00").do(self.scheduled_training_job)  # 每天凌晨2点
        schedule.every().monday.at("10:00").do(self.scheduled_training_job)  # 每周一上午10点
        schedule.every().friday.at("16:00").do(self.scheduled_training_job)  # 每周五下午4点
        
        self.logger.info("📅 调度任务已设置:")
        self.logger.info("   - 每天凌晨 02:00")
        self.logger.info("   - 每周一上午 10:00")
        self.logger.info("   - 每周五下午 16:00")
        
        # 显示初始报告
        initial_report = self.generate_training_report()
        self.logger.info(f"初始状态报告:\n{initial_report}")
        
        # 主循环
        try:
            while True:
                schedule.run_pending()
                time.sleep(60)  # 每分钟检查一次
                
        except KeyboardInterrupt:
            self.logger.info("⚠️  调度器被用户中断")
        except Exception as e:
            self.logger.error(f"❌ 调度器错误: {e}")

def main():
    """主函数"""
    print("⏰ ljwx-stock模型训练调度器")
    print("=" * 60)
    
    scheduler = TrainingScheduler()
    
    # 检查命令行参数
    if len(sys.argv) > 1:
        command = sys.argv[1]
        
        if command == "run":
            # 立即运行一次训练
            print("🚀 立即执行训练任务")
            result = scheduler.scheduled_training_job()
            print(f"训练结果: {result['status']}")
            
        elif command == "report":
            # 生成报告
            report = scheduler.generate_training_report()
            print(report)
            
        elif command == "cleanup":
            # 清理旧模型
            print("🗑️  清理旧模型")
            scheduler.cleanup_old_models()
            
        elif command == "monitor":
            # 监控性能
            scheduler.monitor_model_performance()
            
        else:
            print(f"未知命令: {command}")
            print("可用命令: run, report, cleanup, monitor")
    else:
        # 启动调度器
        scheduler.start_scheduler()

if __name__ == "__main__":
    main()