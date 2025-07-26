#!/usr/bin/env python3
"""
A股数据调度管理系统
支持定时任务、增量更新、错误重试、监控告警
"""
import schedule
import time
import logging
from datetime import datetime, timedelta
from typing import Dict, List, Callable, Optional
import threading
import json
import os
import sys
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import config
from scripts.fetch_comprehensive_data import ComprehensiveDataFetcher
from utils.db_helper import db
from sqlalchemy import text

logger = logging.getLogger(__name__)

class DataScheduler:
    """数据调度管理器"""
    
    def __init__(self):
        self.fetcher = ComprehensiveDataFetcher()
        self.status_file = "logs/scheduler_status.json"
        self.running = False
        self.tasks_status = {}
        
        # 任务配置
        self.task_config = {
            # 基本信息 - 每周更新
            'stock_basic': {
                'frequency': 'weekly',
                'day': 'monday',
                'time': '09:00',
                'function': self.fetcher.fetch_basic_info_data,
                'retry_count': 3,
                'timeout': 3600,  # 1小时
                'priority': 1  # 优先级最高
            },
            
            # 财务数据 - 季度更新（报告期后）
            'financial_data': {
                'frequency': 'monthly',
                'day': 15,  # 每月15日
                'time': '10:00',
                'function': self.fetcher.fetch_financial_data,
                'retry_count': 2,
                'timeout': 7200,  # 2小时
                'priority': 2
            },
            
            # 资金流向 - 每日更新
            'money_flow': {
                'frequency': 'daily',
                'time': '18:30',
                'function': self.fetcher.fetch_money_flow_data,
                'retry_count': 3,
                'timeout': 1800,  # 30分钟
                'priority': 3
            },
            
            # 股东数据 - 季度更新
            'shareholder_data': {
                'frequency': 'monthly',
                'day': 20,  # 每月20日
                'time': '11:00',
                'function': self.fetcher.fetch_shareholder_data,
                'retry_count': 2,
                'timeout': 3600,
                'priority': 4
            },
            
            # 公告数据 - 每日更新
            'announcement_data': {
                'frequency': 'daily',
                'time': '19:00',
                'function': self.fetcher.fetch_announcement_data,
                'retry_count': 3,
                'timeout': 900,  # 15分钟
                'priority': 5
            },
            
            # 行情扩展 - 每日更新
            'market_extension': {
                'frequency': 'daily',
                'time': '19:30',
                'function': self.fetcher.fetch_market_extension_data,
                'retry_count': 3,
                'timeout': 900,
                'priority': 6
            },
            
            # 宏观数据 - 每月更新
            'macro_data': {
                'frequency': 'monthly',
                'day': 5,  # 每月5日
                'time': '12:00',
                'function': self.fetcher.fetch_macro_data,
                'retry_count': 2,
                'timeout': 1800,
                'priority': 7
            }
        }
        
        self.load_status()
        
    def load_status(self):
        """加载调度状态"""
        try:
            if os.path.exists(self.status_file):
                with open(self.status_file, 'r', encoding='utf-8') as f:
                    self.tasks_status = json.load(f)
        except Exception as e:
            logger.warning(f"加载调度状态失败: {e}")
            self.tasks_status = {}
            
    def save_status(self):
        """保存调度状态"""
        try:
            os.makedirs(os.path.dirname(self.status_file), exist_ok=True)
            with open(self.status_file, 'w', encoding='utf-8') as f:
                json.dump(self.tasks_status, f, ensure_ascii=False, indent=2, default=str)
        except Exception as e:
            logger.error(f"保存调度状态失败: {e}")
            
    def setup_schedules(self):
        """设置定时任务"""
        logger.info("🕒 设置定时任务调度...")
        
        for task_name, config in self.task_config.items():
            frequency = config['frequency']
            time_str = config['time']
            
            if frequency == 'daily':
                schedule.every().day.at(time_str).do(
                    self.run_task_safe, task_name
                ).tag(task_name)
                
            elif frequency == 'weekly':
                day = config.get('day', 'monday')
                getattr(schedule.every(), day).at(time_str).do(
                    self.run_task_safe, task_name
                ).tag(task_name)
                
            elif frequency == 'monthly':
                # 月度任务通过每日检查实现
                schedule.every().day.at(time_str).do(
                    self.check_monthly_task, task_name
                ).tag(f"{task_name}_monthly")
                
            logger.info(f"✅ 任务 {task_name} 已调度: {frequency} at {time_str}")
            
    def check_monthly_task(self, task_name: str):
        """检查月度任务是否需要执行"""
        config = self.task_config[task_name]
        target_day = config.get('day', 1)
        
        today = datetime.now()
        if today.day == target_day:
            logger.info(f"📅 月度任务 {task_name} 触发执行")
            return self.run_task_safe(task_name)
        else:
            logger.debug(f"📅 月度任务 {task_name} 非执行日: {today.day} != {target_day}")
            
    def run_task_safe(self, task_name: str):
        """安全执行任务（带错误处理和重试）"""
        config = self.task_config.get(task_name)
        if not config:
            logger.error(f"❌ 未找到任务配置: {task_name}")
            return
            
        max_retries = config.get('retry_count', 1)
        timeout = config.get('timeout', 3600)
        func = config['function']
        
        for attempt in range(max_retries):
            try:
                logger.info(f"🚀 开始执行任务: {task_name} (尝试 {attempt + 1}/{max_retries})")
                
                # 更新任务状态
                self.update_task_status(task_name, 'running', f"执行中 (尝试 {attempt + 1})")
                
                # 使用线程执行任务以支持超时
                task_thread = threading.Thread(target=func, args=('incremental',))
                task_thread.daemon = True
                task_thread.start()
                task_thread.join(timeout)
                
                if task_thread.is_alive():
                    logger.error(f"❌ 任务 {task_name} 执行超时 ({timeout}秒)")
                    self.update_task_status(task_name, 'timeout', f"执行超时 (尝试 {attempt + 1})")
                    continue
                    
                # 任务成功完成
                logger.info(f"✅ 任务 {task_name} 执行成功")
                self.update_task_status(task_name, 'success', "执行成功")
                self.log_task_completion(task_name)
                return
                
            except Exception as e:
                logger.error(f"❌ 任务 {task_name} 执行失败 (尝试 {attempt + 1}): {e}")
                self.update_task_status(task_name, 'error', f"执行失败: {str(e)}")
                
                if attempt < max_retries - 1:
                    wait_time = (attempt + 1) * 60  # 递增等待时间
                    logger.info(f"⏳ {wait_time}秒后重试...")
                    time.sleep(wait_time)
                else:
                    self.log_task_failure(task_name, e)
                    
    def update_task_status(self, task_name: str, status: str, message: str = ""):
        """更新任务状态"""
        self.tasks_status[task_name] = {
            'status': status,
            'message': message,
            'last_update': datetime.now().isoformat(),
            'last_run': datetime.now().isoformat() if status in ['running', 'success'] else 
                       self.tasks_status.get(task_name, {}).get('last_run')
        }
        self.save_status()
        
    def log_task_completion(self, task_name: str):
        """记录任务完成日志"""
        try:
            with db.engine.connect() as conn:
                conn.execute(text("""
                    INSERT INTO system_log (operation_type, operation_detail, success_count, error_count)
                    VALUES (:operation_type, :operation_detail, :success_count, :error_count)
                """), {
                    'operation_type': f'scheduler_{task_name}',
                    'operation_detail': f'定时任务 {task_name} 执行成功',
                    'success_count': 1,
                    'error_count': 0
                })
                conn.commit()
        except Exception as e:
            logger.warning(f"记录任务完成日志失败: {e}")
            
    def log_task_failure(self, task_name: str, error: Exception):
        """记录任务失败日志"""
        try:
            with db.engine.connect() as conn:
                conn.execute(text("""
                    INSERT INTO system_log (operation_type, operation_detail, success_count, error_count)
                    VALUES (:operation_type, :operation_detail, :success_count, :error_count)
                """), {
                    'operation_type': f'scheduler_{task_name}',
                    'operation_detail': f'定时任务 {task_name} 执行失败: {str(error)}',
                    'success_count': 0,
                    'error_count': 1
                })
                conn.commit()
        except Exception as e:
            logger.warning(f"记录任务失败日志失败: {e}")
            
    def run_task_manually(self, task_name: str, mode: str = 'incremental'):
        """手动执行任务"""
        logger.info(f"🔧 手动执行任务: {task_name} (模式: {mode})")
        
        config = self.task_config.get(task_name)
        if not config:
            logger.error(f"❌ 未找到任务配置: {task_name}")
            return False
            
        try:
            func = config['function']
            result = func(mode)
            logger.info(f"✅ 手动任务 {task_name} 执行成功: {result}")
            self.update_task_status(task_name, 'manual_success', f"手动执行成功: {result}")
            return True
        except Exception as e:
            logger.error(f"❌ 手动任务 {task_name} 执行失败: {e}")
            self.update_task_status(task_name, 'manual_error', f"手动执行失败: {str(e)}")
            return False
            
    def get_status_report(self) -> Dict:
        """获取调度状态报告"""
        report = {
            'scheduler_running': self.running,
            'total_tasks': len(self.task_config),
            'tasks_status': self.tasks_status,
            'next_runs': {},
            'health_check': self.health_check()
        }
        
        # 获取下次执行时间
        for job in schedule.jobs:
            task_name = list(job.tags)[0] if job.tags else 'unknown'
            report['next_runs'][task_name] = job.next_run.isoformat() if job.next_run else None
            
        return report
        
    def health_check(self) -> Dict:
        """健康检查"""
        health = {
            'database_connected': False,
            'tushare_connected': False,
            'disk_space_sufficient': False,
            'last_successful_tasks': {}
        }
        
        try:
            # 数据库连接检查
            with db.engine.connect() as conn:
                conn.execute(text("SELECT 1"))
            health['database_connected'] = True
        except:
            health['database_connected'] = False
            
        try:
            # TuShare连接检查
            self.fetcher.pro.trade_cal(start_date='20240101', end_date='20240101')
            health['tushare_connected'] = True
        except:
            health['tushare_connected'] = False
            
        try:
            # 磁盘空间检查
            import shutil
            _, _, free = shutil.disk_usage('.')
            health['disk_space_sufficient'] = free > 1024 * 1024 * 1024  # 1GB
        except:
            health['disk_space_sufficient'] = False
            
        # 最近成功任务
        for task_name, status in self.tasks_status.items():
            if status.get('status') == 'success':
                health['last_successful_tasks'][task_name] = status.get('last_run')
                
        return health
        
    def start_scheduler(self):
        """启动调度器"""
        logger.info("🚀 启动数据调度器...")
        
        self.setup_schedules()
        self.running = True
        
        logger.info(f"📋 已注册 {len(self.task_config)} 个定时任务")
        logger.info("⏰ 调度器运行中，按 Ctrl+C 停止...")
        
        try:
            while self.running:
                schedule.run_pending()
                time.sleep(60)  # 每分钟检查一次
                
        except KeyboardInterrupt:
            logger.info("🛑 接收到停止信号...")
        finally:
            self.stop_scheduler()
            
    def stop_scheduler(self):
        """停止调度器"""
        logger.info("🛑 停止数据调度器...")
        self.running = False
        schedule.clear()
        self.save_status()
        logger.info("✅ 调度器已停止")
        
    def print_schedule_info(self):
        """打印调度信息"""
        logger.info("📅 定时任务调度信息:")
        logger.info("-" * 80)
        
        for task_name, config in self.task_config.items():
            frequency = config['frequency']
            time_str = config['time']
            priority = config['priority']
            
            status_info = self.tasks_status.get(task_name, {})
            last_run = status_info.get('last_run', '从未执行')
            last_status = status_info.get('status', 'unknown')
            
            logger.info(f"任务: {task_name}")
            logger.info(f"  频率: {frequency} at {time_str}")
            logger.info(f"  优先级: {priority}")
            logger.info(f"  上次执行: {last_run}")
            logger.info(f"  最新状态: {last_status}")
            logger.info("")

# 全局调度器实例
scheduler = DataScheduler()

def main():
    """主函数"""
    import argparse
    
    parser = argparse.ArgumentParser(description='A股数据调度管理系统')
    parser.add_argument('command', choices=['start', 'status', 'run', 'health'], 
                       help='操作命令')
    parser.add_argument('--task', help='任务名称（用于run命令）')
    parser.add_argument('--mode', choices=['full', 'incremental'], default='incremental',
                       help='执行模式（用于run命令）')
    
    args = parser.parse_args()
    
    # 配置日志
    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s - %(levelname)s - %(message)s',
        handlers=[
            logging.StreamHandler(),
            logging.FileHandler('logs/scheduler.log', encoding='utf-8')
        ]
    )
    
    try:
        if args.command == 'start':
            scheduler.start_scheduler()
            
        elif args.command == 'status':
            report = scheduler.get_status_report()
            print(json.dumps(report, indent=2, ensure_ascii=False, default=str))
            
        elif args.command == 'run':
            if not args.task:
                logger.error("❌ 请指定任务名称 --task")
                return
                
            if args.task not in scheduler.task_config:
                logger.error(f"❌ 未知任务: {args.task}")
                logger.info(f"可用任务: {list(scheduler.task_config.keys())}")
                return
                
            success = scheduler.run_task_manually(args.task, args.mode)
            if success:
                logger.info("✅ 手动任务执行成功")
            else:
                logger.error("❌ 手动任务执行失败")
                
        elif args.command == 'health':
            health = scheduler.health_check()
            print("🏥 系统健康检查:")
            for key, value in health.items():
                status = "✅" if value else "❌"
                print(f"  {key}: {status} {value}")
                
    except Exception as e:
        logger.error(f"❌ 程序执行失败: {e}")

if __name__ == "__main__":
    main() 