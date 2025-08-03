#!/usr/bin/env python3
"""
ljwx-stock模型持续训练脚本
支持增量训练数据生成和模型更新
"""

import os
import sys
import json
import logging
import tempfile
import subprocess
from datetime import datetime, timedelta
from typing import List, Dict, Optional
import pandas as pd

# 添加项目路径
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

from llm.tushare_data_extractor import TuShareDataExtractor
from llm.llm_training_data_generator import LLMTrainingDataGenerator

class ContinuousTrainer:
    """持续训练系统"""
    
    def __init__(self, tushare_token: str = None):
        self.logger = logging.getLogger(__name__)
        
        # 初始化组件
        self.data_extractor = TuShareDataExtractor(tushare_token)
        self.data_generator = LLMTrainingDataGenerator()
        
        # 配置
        self.config = {
            'base_model': 'ljwx-stock',
            'training_data_dir': 'data/llm_training',
            'models_dir': 'data/models',
            'max_training_examples_per_batch': 200,
            'stock_batch_size': 30,
            'days_back': 90,
            'incremental_days': 30
        }
        
        # 确保目录存在
        os.makedirs(self.config['training_data_dir'], exist_ok=True)
        os.makedirs(self.config['models_dir'], exist_ok=True)
        
    def get_latest_model(self) -> str:
        """获取最新的ljwx-stock模型"""
        try:
            result = subprocess.run(
                ['ollama', 'list'],
                capture_output=True,
                text=True
            )
            
            if result.returncode == 0:
                lines = result.stdout.strip().split('\n')
                ljwx_models = []
                
                for line in lines[1:]:  # 跳过标题行
                    if 'ljwx-stock' in line:
                        model_name = line.split()[0]
                        ljwx_models.append(model_name)
                
                if ljwx_models:
                    # 返回最新的训练模型，优先trained版本
                    trained_models = [m for m in ljwx_models if 'trained' in m]
                    if trained_models:
                        return sorted(trained_models)[-1]
                    else:
                        return 'ljwx-stock:latest'
                else:
                    return 'ljwx-stock:latest'
            
        except Exception as e:
            self.logger.error(f"获取模型列表失败: {e}")
        
        return 'ljwx-stock:latest'
    
    def get_existing_training_data(self) -> List[Dict]:
        """获取现有训练数据"""
        all_training_data = []
        training_dir = self.config['training_data_dir']
        
        # 查找所有JSONL文件
        for filename in os.listdir(training_dir):
            if filename.endswith('.jsonl') and 'stock_training_data' in filename:
                file_path = os.path.join(training_dir, filename)
                
                try:
                    with open(file_path, 'r', encoding='utf-8') as f:
                        for line in f:
                            if line.strip():
                                data = json.loads(line)
                                all_training_data.append(data)
                except Exception as e:
                    self.logger.warning(f"读取训练文件失败 {filename}: {e}")
        
        self.logger.info(f"现有训练数据: {len(all_training_data)}条")
        return all_training_data
    
    def get_training_stocks(self, existing_data: List[Dict]) -> List[str]:
        """获取已训练的股票代码"""
        trained_stocks = set()
        
        for data in existing_data:
            input_text = data.get('input', '')
            # 从输入文本中提取股票代码
            if '股票代码:' in input_text:
                lines = input_text.split('\n')
                for line in lines:
                    if '股票代码:' in line:
                        stock_code = line.split(':')[1].strip()
                        trained_stocks.add(stock_code)
                        break
        
        return list(trained_stocks)
    
    def generate_new_training_data(self, exclude_stocks: List[str] = None) -> List[Dict]:
        """生成新的训练数据"""
        self.logger.info("🔄 生成新的训练数据")
        
        exclude_stocks = exclude_stocks or []
        
        # 获取股票列表
        stocks = self.data_extractor.get_stock_list(limit=100)
        if stocks.empty:
            self.logger.error("无法获取股票列表")
            return []
        
        # 提取股票代码
        if 'ts_code' in stocks.columns:
            all_stock_codes = stocks['ts_code'].tolist()
        else:
            stocks.reset_index(inplace=True)
            all_stock_codes = [f"{code}.SZ" if code.startswith('0') or code.startswith('3') 
                              else f"{code}.SH" for code in stocks['code'].tolist()]
        
        # 过滤已训练的股票，选择新股票
        new_stock_codes = [code for code in all_stock_codes if code not in exclude_stocks]
        
        if not new_stock_codes:
            # 如果没有新股票，则使用不同时间段的数据
            self.logger.info("没有新股票，使用不同时间段的数据")
            new_stock_codes = all_stock_codes[:self.config['stock_batch_size']]
            days_back = self.config['days_back'] + self.config['incremental_days']
        else:
            new_stock_codes = new_stock_codes[:self.config['stock_batch_size']]
            days_back = self.config['days_back']
        
        self.logger.info(f"选择股票: {len(new_stock_codes)}只")
        
        # 设置日期范围
        end_date = datetime.now().strftime('%Y%m%d')
        start_date = (datetime.now() - timedelta(days=days_back)).strftime('%Y%m%d')
        
        # 提取数据
        try:
            dataset = self.data_extractor.extract_comprehensive_dataset(
                stock_codes=new_stock_codes,
                start_date=start_date,
                end_date=end_date,
                include_financial=False,
                include_news=False
            )
            
            if 'daily_data' not in dataset or dataset['daily_data'].empty:
                self.logger.error("未获取到有效的股票数据")
                return []
            
            # 生成训练样本
            training_examples = self.data_generator.create_training_examples(
                dataset['daily_data'], 
                max_examples=self.config['max_training_examples_per_batch']
            )
            
            self.logger.info(f"生成新训练样本: {len(training_examples)}条")
            return training_examples
            
        except Exception as e:
            self.logger.error(f"生成训练数据失败: {e}")
            return []
    
    def merge_training_data(self, existing_data: List[Dict], new_data: List[Dict]) -> List[Dict]:
        """合并训练数据"""
        merged_data = existing_data.copy()
        
        # 去重：基于input内容
        existing_inputs = set()
        for data in existing_data:
            existing_inputs.add(data.get('input', ''))
        
        new_count = 0
        for data in new_data:
            if data.get('input', '') not in existing_inputs:
                merged_data.append(data)
                new_count += 1
        
        self.logger.info(f"合并训练数据: 新增{new_count}条，总计{len(merged_data)}条")
        return merged_data
    
    def save_training_data(self, training_data: List[Dict], filename: str) -> str:
        """保存训练数据"""
        file_path = os.path.join(self.config['training_data_dir'], filename)
        
        with open(file_path, 'w', encoding='utf-8') as f:
            for data in training_data:
                f.write(json.dumps(data, ensure_ascii=False) + '\n')
        
        self.logger.info(f"训练数据已保存: {file_path}")
        return file_path
    
    def create_incremental_model(self, base_model: str, training_data: List[Dict], model_name: str) -> bool:
        """创建增量训练模型"""
        try:
            self.logger.info(f"🚀 创建增量训练模型: {model_name}")
            
            system_prompt = """你是ljwx-stock，一个专业的股票投资分析助手。你具备以下核心能力：

🎯 **专业定位**
- 专注于A股市场分析和投资建议
- 基于技术分析和基本面分析提供专业意见
- 为投资者提供客观、理性的决策支持

📊 **分析能力**
1. **技术指标分析**：熟练解读K线、均线、RSI、MACD、布林带等技术指标
2. **价格走势预测**：基于历史数据和技术模式判断价格趋势
3. **风险评估**：客观评估投资风险等级和风险因素
4. **市场情绪分析**：理解市场心理和资金流向

💡 **服务原则**
- 始终提供客观、专业的分析意见
- 明确标注风险等级和注意事项
- 不做绝对化的投资承诺
- 提醒投资者理性投资、风险自担

📋 **输出格式**
- 分析结论简洁明了，逻辑清晰
- 提供具体的操作建议（买入/持有/卖出）
- 标明风险等级（低/中/高）
- 给出关键支撑位和阻力位

请记住：股市有风险，投资需谨慎。所有分析仅供参考，不构成投资建议。"""

            # 构建Modelfile
            modelfile_content = f"""FROM {base_model}
SYSTEM "{system_prompt}"
PARAMETER temperature 0.7
PARAMETER top_p 0.9
PARAMETER top_k 40
PARAMETER repeat_penalty 1.1
PARAMETER num_predict 1024

"""
            
            # 添加训练样本（限制数量以避免模型过大）
            sample_count = min(len(training_data), 100)
            for i, example in enumerate(training_data[:sample_count]):
                instruction = example.get('instruction', '')
                input_text = example.get('input', '')
                output_text = example.get('output', '')
                
                user_message = f"{instruction}\n\n{input_text}" if input_text else instruction
                
                modelfile_content += f'MESSAGE user "{user_message}"\n'
                modelfile_content += f'MESSAGE assistant "{output_text}"\n\n'
            
            # 创建临时Modelfile
            with tempfile.NamedTemporaryFile(mode='w', suffix='.modelfile', delete=False) as f:
                f.write(modelfile_content)
                modelfile_path = f.name
            
            try:
                self.logger.info(f"   基础模型: {base_model}")
                self.logger.info(f"   训练样本: {sample_count}条")
                
                result = subprocess.run(
                    ['ollama', 'create', model_name, '-f', modelfile_path],
                    capture_output=True,
                    text=True,
                    timeout=600
                )
                
                if result.returncode == 0:
                    self.logger.info(f"✅ 增量模型创建完成: {model_name}")
                    return True
                else:
                    self.logger.error(f"❌ 增量模型创建失败: {result.stderr}")
                    return False
                    
            finally:
                os.unlink(modelfile_path)
                
        except Exception as e:
            self.logger.error(f"❌ 创建增量模型时出错: {e}")
            return False
    
    def test_model_performance(self, model_name: str) -> Dict:
        """测试模型性能"""
        test_cases = [
            {
                "input": "分析以下股票的技术指标：股票代码: 000001.SZ，当前价格: 11.20元，RSI: 58，MACD: 0.03，5日均线: 11.10元",
                "expected_keywords": ["技术指标", "RSI", "MACD", "投资建议"]
            },
            {
                "input": "评估风险等级：股票代码: 600000.SH，波动率: 0.35，日涨跌幅: +5.2%",
                "expected_keywords": ["风险", "波动率", "建议"]
            }
        ]
        
        results = {
            "model_name": model_name,
            "test_time": datetime.now().isoformat(),
            "performance": {}
        }
        
        successful_tests = 0
        total_tests = len(test_cases)
        
        for i, test_case in enumerate(test_cases):
            try:
                result = subprocess.run(
                    ['ollama', 'run', model_name],
                    input=test_case['input'],
                    capture_output=True,
                    text=True,
                    timeout=30
                )
                
                if result.returncode == 0 and result.stdout.strip():
                    response = result.stdout.strip()
                    
                    # 检查关键词
                    keyword_matches = sum(1 for keyword in test_case['expected_keywords'] 
                                        if keyword in response)
                    
                    if keyword_matches >= len(test_case['expected_keywords']) // 2:
                        successful_tests += 1
                    
                    self.logger.info(f"   测试用例 {i+1}: 通过")
                else:
                    self.logger.warning(f"   测试用例 {i+1}: 失败")
                    
            except Exception as e:
                self.logger.error(f"   测试用例 {i+1} 出错: {e}")
        
        success_rate = successful_tests / total_tests
        results["performance"] = {
            "success_rate": success_rate,
            "successful_tests": successful_tests,
            "total_tests": total_tests
        }
        
        self.logger.info(f"模型性能测试: {successful_tests}/{total_tests} ({success_rate:.2%})")
        return results
    
    def run_continuous_training(self) -> Dict:
        """运行持续训练"""
        self.logger.info("🚀 开始持续训练流程")
        
        results = {
            "start_time": datetime.now().isoformat(),
            "status": "running",
            "steps": {}
        }
        
        try:
            # 1. 获取当前最新模型
            latest_model = self.get_latest_model()
            self.logger.info(f"当前最新模型: {latest_model}")
            results["base_model"] = latest_model
            
            # 2. 获取现有训练数据
            existing_data = self.get_existing_training_data()
            results["existing_data_count"] = len(existing_data)
            
            # 3. 获取已训练的股票
            trained_stocks = self.get_training_stocks(existing_data)
            self.logger.info(f"已训练股票: {len(trained_stocks)}只")
            
            # 4. 生成新训练数据
            new_data = self.generate_new_training_data(exclude_stocks=trained_stocks)
            results["new_data_count"] = len(new_data)
            
            if not new_data:
                results["status"] = "no_new_data"
                results["message"] = "没有生成新的训练数据"
                return results
            
            # 5. 合并训练数据
            merged_data = self.merge_training_data(existing_data, new_data)
            results["total_data_count"] = len(merged_data)
            
            # 6. 保存合并后的训练数据
            timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
            training_file = f"stock_training_data_continuous_{timestamp}.jsonl"
            training_path = self.save_training_data(merged_data, training_file)
            results["training_file"] = training_path
            
            # 7. 创建新的增量训练模型
            new_model_name = f"ljwx-stock-continuous-{timestamp}"
            success = self.create_incremental_model(latest_model, merged_data, new_model_name)
            
            if success:
                results["new_model"] = new_model_name
                
                # 8. 测试新模型性能
                performance = self.test_model_performance(new_model_name)
                results["performance"] = performance
                
                results["status"] = "success"
                self.logger.info("✅ 持续训练完成")
            else:
                results["status"] = "model_creation_failed"
                self.logger.error("❌ 模型创建失败")
            
        except Exception as e:
            results["status"] = "error"
            results["error"] = str(e)
            self.logger.error(f"❌ 持续训练失败: {e}")
        
        results["end_time"] = datetime.now().isoformat()
        return results
    
    def save_training_results(self, results: Dict):
        """保存训练结果"""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        results_file = os.path.join(self.config['models_dir'], f"continuous_training_results_{timestamp}.json")
        
        with open(results_file, 'w', encoding='utf-8') as f:
            json.dump(results, f, ensure_ascii=False, indent=2)
        
        self.logger.info(f"训练结果已保存: {results_file}")

def main():
    """主函数"""
    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s - %(levelname)s - %(message)s'
    )
    
    logger = logging.getLogger(__name__)
    
    print("🔄 ljwx-stock 持续训练系统")
    print("=" * 60)
    
    try:
        # 获取TuShare token
        tushare_token = os.getenv('TUSHARE_TOKEN')
        if not tushare_token:
            print("⚠️  未设置TUSHARE_TOKEN环境变量，将使用免费API")
        
        # 初始化持续训练器
        trainer = ContinuousTrainer(tushare_token)
        
        print("📋 持续训练配置:")
        print(f"   基础模型: {trainer.config['base_model']}")
        print(f"   训练批次大小: {trainer.config['max_training_examples_per_batch']}条")
        print(f"   股票批次大小: {trainer.config['stock_batch_size']}只")
        print(f"   历史数据天数: {trainer.config['days_back']}天")
        print()
        
        # 运行持续训练
        results = trainer.run_continuous_training()
        
        # 保存结果
        trainer.save_training_results(results)
        
        # 显示结果
        print("\n📊 持续训练结果:")
        print("=" * 60)
        print(f"状态: {results['status']}")
        
        if results['status'] == 'success':
            print(f"✅ 新模型: {results['new_model']}")
            print(f"📊 现有数据: {results['existing_data_count']}条")
            print(f"🆕 新增数据: {results['new_data_count']}条")
            print(f"📈 总计数据: {results['total_data_count']}条")
            
            if 'performance' in results:
                perf = results['performance']['performance']
                print(f"🧪 性能测试: {perf['successful_tests']}/{perf['total_tests']} ({perf['success_rate']:.2%})")
            
            print(f"\n🎯 使用新模型:")
            print(f"   ollama run {results['new_model']}")
        
        elif results['status'] == 'no_new_data':
            print("ℹ️  没有新的训练数据生成")
        
        else:
            print(f"❌ 训练失败: {results.get('error', '未知错误')}")
        
    except KeyboardInterrupt:
        print("\n⚠️ 训练被用户中断")
    except Exception as e:
        logger.error(f"❌ 持续训练失败: {e}")
        print(f"\n❌ 持续训练失败: {e}")

if __name__ == "__main__":
    main()