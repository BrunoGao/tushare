#!/usr/bin/env python3
"""
A股数据调度管理系统 - 增强版
支持定时任务、增量更新、智能错误重试、监控告警、故障恢复
"""
import schedule
import time
import logging
from datetime import datetime, timedelta
from typing import Dict, List, Callable, Optional, Any
import threading
import json
import os
import sys
import traceback
import signal
import psutil
from enum import Enum
from dataclasses import dataclass, asdict
from concurrent.futures import ThreadPoolExecutor, TimeoutError
import requests
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import config
from scripts.fetch_comprehensive_data import ComprehensiveDataFetcher
from utils.db_helper import db
from utils.redis_cache_manager import cache_manager
from sqlalchemy import text

logger = logging.getLogger(__name__)

class TaskStatus(Enum):
    """任务状态枚举"""
    PENDING = "pending"
    RUNNING = "running"
    SUCCESS = "success"
    FAILED = "failed"
    TIMEOUT = "timeout"
    RETRYING = "retrying"
    CANCELLED = "cancelled"
    SKIPPED = "skipped"

class ErrorType(Enum):
    """错误类型枚举"""
    NETWORK_ERROR = "network_error"
    API_LIMIT_ERROR = "api_limit_error"
    DATABASE_ERROR = "database_error"
    TIMEOUT_ERROR = "timeout_error"
    DATA_ERROR = "data_error"
    SYSTEM_ERROR = "system_error"
    UNKNOWN_ERROR = "unknown_error"

@dataclass
class TaskExecution:
    """任务执行记录"""
    task_name: str
    start_time: datetime
    end_time: Optional[datetime] = None
    status: TaskStatus = TaskStatus.PENDING
    error_type: Optional[ErrorType] = None
    error_message: str = ""
    retry_count: int = 0
    execution_time: float = 0.0
    records_processed: int = 0
    memory_usage: float = 0.0
    cpu_usage: float = 0.0

@dataclass
class RetryStrategy:
    """重试策略"""
    max_retries: int = 3
    base_delay: int = 60  # 基础延迟(秒)
    max_delay: int = 3600  # 最大延迟(秒)
    exponential_backoff: bool = True
    jitter: bool = True  # 添加随机抖动

class DataScheduler:
    """数据调度管理器"""
    
    def __init__(self):
        self.fetcher = ComprehensiveDataFetcher()
        self.status_file = "logs/scheduler_status.json"
        self.error_log_file = "logs/scheduler_errors.json"
        self.running = False
        self.tasks_status = {}
        self.error_history = []
        self.executor = ThreadPoolExecutor(max_workers=4)
        self.active_tasks = {}  # 当前运行的任务
        self.task_locks = {}  # 任务锁，防止重复执行
        
        # 错误分类器
        self.error_classifier = {
            'connection': ErrorType.NETWORK_ERROR,
            'timeout': ErrorType.TIMEOUT_ERROR,
            'limit': ErrorType.API_LIMIT_ERROR,
            'database': ErrorType.DATABASE_ERROR,
            'data': ErrorType.DATA_ERROR,
            'memory': ErrorType.SYSTEM_ERROR,
            'permission': ErrorType.SYSTEM_ERROR
        }
        
        # 智能重试策略
        self.retry_strategies = {
            ErrorType.NETWORK_ERROR: RetryStrategy(max_retries=5, base_delay=30),
            ErrorType.API_LIMIT_ERROR: RetryStrategy(max_retries=3, base_delay=300),
            ErrorType.DATABASE_ERROR: RetryStrategy(max_retries=3, base_delay=60),
            ErrorType.TIMEOUT_ERROR: RetryStrategy(max_retries=2, base_delay=120),
            ErrorType.DATA_ERROR: RetryStrategy(max_retries=2, base_delay=60),
            ErrorType.SYSTEM_ERROR: RetryStrategy(max_retries=1, base_delay=300),
            ErrorType.UNKNOWN_ERROR: RetryStrategy(max_retries=2, base_delay=120)
        }
        
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
        self.load_error_history()
    
    def classify_error(self, error: Exception) -> ErrorType:
        """智能错误分类"""
        error_str = str(error).lower()
        
        # 网络相关错误
        if any(keyword in error_str for keyword in ['connection', 'network', 'timeout', 'unreachable']):
            return ErrorType.NETWORK_ERROR
        
        # API限制错误
        if any(keyword in error_str for keyword in ['limit', 'quota', 'rate', 'throttle']):
            return ErrorType.API_LIMIT_ERROR
        
        # 数据库错误
        if any(keyword in error_str for keyword in ['database', 'mysql', 'sql', 'connection']):
            return ErrorType.DATABASE_ERROR
        
        # 超时错误
        if any(keyword in error_str for keyword in ['timeout', 'timed out']):
            return ErrorType.TIMEOUT_ERROR
        
        # 数据错误
        if any(keyword in error_str for keyword in ['data', 'format', 'parse', 'invalid']):
            return ErrorType.DATA_ERROR
        
        # 系统错误
        if any(keyword in error_str for keyword in ['memory', 'disk', 'permission', 'system']):
            return ErrorType.SYSTEM_ERROR
        
        return ErrorType.UNKNOWN_ERROR
    
    def calculate_retry_delay(self, error_type: ErrorType, attempt: int) -> int:
        """计算重试延迟时间"""
        strategy = self.retry_strategies.get(error_type, RetryStrategy())
        
        if strategy.exponential_backoff:
            delay = min(strategy.base_delay * (2 ** attempt), strategy.max_delay)
        else:
            delay = strategy.base_delay
        
        # 添加随机抖动
        if strategy.jitter:
            import random
            jitter = random.uniform(0.8, 1.2)
            delay = int(delay * jitter)
        
        return delay
    
    def should_retry(self, error_type: ErrorType, attempt: int) -> bool:
        """判断是否应该重试"""
        strategy = self.retry_strategies.get(error_type, RetryStrategy())
        return attempt < strategy.max_retries
    
    def log_error(self, task_name: str, error: Exception, error_type: ErrorType, attempt: int):
        """记录错误信息"""
        error_record = {
            'timestamp': datetime.now().isoformat(),
            'task_name': task_name,
            'error_type': error_type.value,
            'error_message': str(error),
            'attempt': attempt,
            'traceback': traceback.format_exc()
        }
        
        self.error_history.append(error_record)
        
        # 保持错误历史记录在合理范围内
        if len(self.error_history) > 1000:
            self.error_history = self.error_history[-500:]
        
        self.save_error_history()
    
    def save_error_history(self):
        """保存错误历史"""
        try:
            os.makedirs(os.path.dirname(self.error_log_file), exist_ok=True)
            with open(self.error_log_file, 'w', encoding='utf-8') as f:
                json.dump(self.error_history, f, ensure_ascii=False, indent=2, default=str)
        except Exception as e:
            logger.error(f"保存错误历史失败: {e}")
    
    def load_error_history(self):
        """加载错误历史"""
        try:
            if os.path.exists(self.error_log_file):
                with open(self.error_log_file, 'r', encoding='utf-8') as f:
                    self.error_history = json.load(f)
        except Exception as e:
            logger.warning(f"加载错误历史失败: {e}")
            self.error_history = []
    
    def get_task_lock(self, task_name: str) -> bool:
        """获取任务锁，防止重复执行"""
        if task_name in self.task_locks:
            return False
        
        self.task_locks[task_name] = threading.Lock()
        return self.task_locks[task_name].acquire(blocking=False)
    
    def release_task_lock(self, task_name: str):
        """释放任务锁"""
        if task_name in self.task_locks:
            try:
                self.task_locks[task_name].release()
            except:
                pass
            finally:
                del self.task_locks[task_name]
    
    def monitor_task_resources(self, task_name: str) -> Dict[str, float]:
        """监控任务资源使用"""
        try:
            process = psutil.Process()
            return {
                'cpu_percent': process.cpu_percent(),
                'memory_mb': process.memory_info().rss / 1024 / 1024,
                'memory_percent': process.memory_percent()
            }
        except:
            return {'cpu_percent': 0, 'memory_mb': 0, 'memory_percent': 0}
        
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
        """安全执行任务（带智能错误处理和重试）"""
        config = self.task_config.get(task_name)
        if not config:
            logger.error(f"❌ 未找到任务配置: {task_name}")
            return
        
        # 检查任务锁，防止重复执行
        if not self.get_task_lock(task_name):
            logger.warning(f"⚠️ 任务 {task_name} 正在执行中，跳过本次调度")
            return
        
        try:
            self._execute_task_with_retry(task_name, config)
        finally:
            self.release_task_lock(task_name)
    
    def _execute_task_with_retry(self, task_name: str, config: Dict):
        """带智能重试的任务执行"""
        func = config['function']
        timeout = config.get('timeout', 3600)
        
        execution = TaskExecution(
            task_name=task_name,
            start_time=datetime.now()
        )
        
        attempt = 0
        while True:
            try:
                logger.info(f"🚀 开始执行任务: {task_name} (尝试 {attempt + 1})")
                
                # 更新任务状态
                execution.status = TaskStatus.RUNNING
                execution.retry_count = attempt
                self.active_tasks[task_name] = execution
                self.update_task_status(task_name, TaskStatus.RUNNING.value,
                                      f"执行中 (尝试 {attempt + 1})")
                
                # 监控资源使用
                start_resources = self.monitor_task_resources(task_name)
                start_time = time.time()
                
                # 使用线程池执行任务以支持超时和取消
                future = self.executor.submit(func, 'incremental')
                
                try:
                    result = future.result(timeout=timeout)
                    execution_time = time.time() - start_time
                    
                    # 更新执行记录
                    execution.end_time = datetime.now()
                    execution.status = TaskStatus.SUCCESS
                    execution.execution_time = execution_time
                    
                    # 计算资源使用
                    end_resources = self.monitor_task_resources(task_name)
                    execution.memory_usage = end_resources['memory_mb']
                    execution.cpu_usage = end_resources['cpu_percent']
                    
                    logger.info(f"✅ 任务 {task_name} 执行成功 (耗时: {execution_time:.2f}秒)")
                    self.update_task_status(task_name, TaskStatus.SUCCESS.value,
                                          f"执行成功 (耗时: {execution_time:.2f}秒)")
                    self.log_task_completion(task_name, execution)
                    
                    # 缓存成功状态
                    cache_manager.set('scheduler', f'last_success_{task_name}',
                                    execution.end_time.isoformat(), 3600)
                    
                    return
                    
                except TimeoutError:
                    future.cancel()
                    error = TimeoutError(f"任务执行超时 ({timeout}秒)")
                    error_type = ErrorType.TIMEOUT_ERROR
                    
                except Exception as e:
                    error = e
                    error_type = self.classify_error(e)
                
            except Exception as e:
                error = e
                error_type = self.classify_error(e)
            
            # 处理错误
            execution.error_type = error_type
            execution.error_message = str(error)
            execution.end_time = datetime.now()
            execution.status = TaskStatus.FAILED
            
            # 记录错误
            self.log_error(task_name, error, error_type, attempt)
            logger.error(f"❌ 任务 {task_name} 执行失败 (尝试 {attempt + 1}): {error}")
            
            # 判断是否重试
            if self.should_retry(error_type, attempt):
                attempt += 1
                execution.status = TaskStatus.RETRYING
                
                # 计算重试延迟
                delay = self.calculate_retry_delay(error_type, attempt)
                
                logger.info(f"⏳ {delay}秒后重试 (错误类型: {error_type.value})")
                self.update_task_status(task_name, TaskStatus.RETRYING.value,
                                      f"等待重试 (延迟: {delay}秒)")
                
                time.sleep(delay)
            else:
                # 重试次数用尽，任务失败
                logger.error(f"❌ 任务 {task_name} 最终失败，已达到最大重试次数")
                self.update_task_status(task_name, TaskStatus.FAILED.value,
                                      f"最终失败: {error}")
                self.log_task_failure(task_name, error, execution)
                break
        
        # 清理活跃任务记录
        if task_name in self.active_tasks:
            del self.active_tasks[task_name]
                    
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
        
    def log_task_completion(self, task_name: str, execution: TaskExecution):
        """记录任务完成日志"""
        try:
            with db.engine.connect() as conn:
                # 创建任务执行记录表（如果不存在）
                conn.execute(text("""
                    CREATE TABLE IF NOT EXISTS task_execution_log (
                        id BIGINT AUTO_INCREMENT PRIMARY KEY,
                        task_name VARCHAR(100),
                        start_time TIMESTAMP,
                        end_time TIMESTAMP,
                        status VARCHAR(20),
                        execution_time DECIMAL(10,3),
                        retry_count INT,
                        memory_usage DECIMAL(10,2),
                        cpu_usage DECIMAL(5,2),
                        records_processed INT,
                        error_type VARCHAR(50),
                        error_message TEXT,
                        INDEX idx_task_name (task_name),
                        INDEX idx_start_time (start_time),
                        INDEX idx_status (status)
                    ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='任务执行记录'
                """))
                
                # 插入执行记录
                conn.execute(text("""
                    INSERT INTO task_execution_log
                    (task_name, start_time, end_time, status, execution_time, retry_count,
                     memory_usage, cpu_usage, records_processed, error_type, error_message)
                    VALUES (:task_name, :start_time, :end_time, :status, :execution_time, :retry_count,
                            :memory_usage, :cpu_usage, :records_processed, :error_type, :error_message)
                """), {
                    'task_name': execution.task_name,
                    'start_time': execution.start_time,
                    'end_time': execution.end_time,
                    'status': execution.status.value,
                    'execution_time': execution.execution_time,
                    'retry_count': execution.retry_count,
                    'memory_usage': execution.memory_usage,
                    'cpu_usage': execution.cpu_usage,
                    'records_processed': execution.records_processed,
                    'error_type': execution.error_type.value if execution.error_type else None,
                    'error_message': execution.error_message
                })
                
                # 兼容旧的系统日志
                conn.execute(text("""
                    INSERT INTO system_log (operation_type, operation_detail, success_count, error_count)
                    VALUES (:operation_type, :operation_detail, :success_count, :error_count)
                """), {
                    'operation_type': f'scheduler_{task_name}',
                    'operation_detail': f'定时任务 {task_name} 执行成功 (耗时: {execution.execution_time:.2f}秒)',
                    'success_count': 1,
                    'error_count': 0
                })
                
                conn.commit()
        except Exception as e:
            logger.warning(f"记录任务完成日志失败: {e}")
            
    def log_task_failure(self, task_name: str, error: Exception, execution: TaskExecution):
        """记录任务失败日志"""
        try:
            with db.engine.connect() as conn:
                # 插入执行记录
                conn.execute(text("""
                    INSERT INTO task_execution_log
                    (task_name, start_time, end_time, status, execution_time, retry_count,
                     memory_usage, cpu_usage, records_processed, error_type, error_message)
                    VALUES (:task_name, :start_time, :end_time, :status, :execution_time, :retry_count,
                            :memory_usage, :cpu_usage, :records_processed, :error_type, :error_message)
                """), {
                    'task_name': execution.task_name,
                    'start_time': execution.start_time,
                    'end_time': execution.end_time,
                    'status': execution.status.value,
                    'execution_time': execution.execution_time,
                    'retry_count': execution.retry_count,
                    'memory_usage': execution.memory_usage,
                    'cpu_usage': execution.cpu_usage,
                    'records_processed': execution.records_processed,
                    'error_type': execution.error_type.value if execution.error_type else None,
                    'error_message': execution.error_message
                })
                
                # 兼容旧的系统日志
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