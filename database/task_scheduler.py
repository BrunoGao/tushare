#!/usr/bin/env python3
"""
数据同步任务调度器
基于APScheduler实现定时数据获取和技术指标计算
"""

import os
import time
import logging
import json
from datetime import datetime, timedelta, time as dt_time
from typing import Dict, List, Optional, Any, Callable
from dataclasses import dataclass, asdict
from enum import Enum
import threading
from pathlib import Path

from apscheduler.schedulers.background import BackgroundScheduler
from apscheduler.jobstores.sqlalchemy import SQLAlchemyJobStore
from apscheduler.executors.pool import ThreadPoolExecutor
from apscheduler.events import EVENT_JOB_EXECUTED, EVENT_JOB_ERROR, EVENT_JOB_MISSED
import pytz

from .tushare_data_manager import TuShareDataManager
from .technical_indicator_calculator import TechnicalIndicatorCalculator
from .data_validator import DataValidator, DataCleaner

# 设置日志
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

class TaskStatus(Enum):
    """任务状态"""
    PENDING = "pending"
    RUNNING = "running"
    SUCCESS = "success"
    FAILED = "failed"
    SKIPPED = "skipped"

class TaskType(Enum):
    """任务类型"""
    SYNC_STOCK_BASIC = "sync_stock_basic"
    SYNC_DAILY_DATA = "sync_daily_data"
    SYNC_FINANCIAL_DATA = "sync_financial_data"
    SYNC_MARKET_DATA = "sync_market_data"
    CALCULATE_INDICATORS = "calculate_indicators"
    DATA_VALIDATION = "data_validation"
    DATA_CLEANUP = "data_cleanup"
    SYSTEM_MAINTENANCE = "system_maintenance"

@dataclass
class TaskExecutionLog:
    """任务执行日志"""
    task_id: str
    task_type: str
    status: TaskStatus
    start_time: str
    end_time: Optional[str] = None
    duration: Optional[float] = None
    result: Optional[Dict[str, Any]] = None
    error_message: Optional[str] = None
    
class DataSyncTaskScheduler:
    """数据同步任务调度器"""
    
    def __init__(self, config: Dict[str, Any]):
        """
        初始化任务调度器
        
        Args:
            config: 配置参数，包含数据库配置、TuShare token等
        """
        self.config = config
        self.db_config = config['database']
        self.tushare_token = config['tushare_token']
        self.timezone = pytz.timezone(config.get('timezone', 'Asia/Shanghai'))
        
        # 初始化组件
        self.data_manager = TuShareDataManager(self.tushare_token, self.db_config)
        self.indicator_calculator = TechnicalIndicatorCalculator(self.db_config)
        self.data_validator = DataValidator()
        self.data_cleaner = DataCleaner()
        
        # 任务执行日志
        self.execution_logs: List[TaskExecutionLog] = []
        self.max_log_entries = 1000
        
        # 初始化调度器
        self._init_scheduler()
        
        # 任务配置
        self.task_configs = self._load_task_configs()
        
    def _init_scheduler(self):
        """初始化APScheduler调度器"""
        # 创建作业存储
        jobstores = {
            'default': SQLAlchemyJobStore(url=f"sqlite:///{self.config.get('scheduler_db', 'data/scheduler.db')}")
        }
        
        # 创建执行器
        executors = {
            'default': ThreadPoolExecutor(20),
            'sync_executor': ThreadPoolExecutor(5),  # 数据同步专用执行器
            'calc_executor': ThreadPoolExecutor(3)   # 计算任务专用执行器
        }
        
        # 调度器配置
        job_defaults = {
            'coalesce': True,  # 合并错过的作业
            'max_instances': 1,  # 同一时间只能有一个实例运行
            'misfire_grace_time': 300  # 错过作业的宽限时间（秒）
        }
        
        # 创建调度器
        self.scheduler = BackgroundScheduler(
            jobstores=jobstores,
            executors=executors,
            job_defaults=job_defaults,
            timezone=self.timezone
        )
        
        # 添加事件监听器
        self.scheduler.add_listener(self._job_executed, EVENT_JOB_EXECUTED)
        self.scheduler.add_listener(self._job_error, EVENT_JOB_ERROR)
        self.scheduler.add_listener(self._job_missed, EVENT_JOB_MISSED)
    
    def _load_task_configs(self) -> Dict[str, Dict]:
        """加载任务配置"""
        return {
            # 股票基础信息同步 - 每日早上8:30
            'sync_stock_basic': {
                'task_type': TaskType.SYNC_STOCK_BASIC,
                'schedule': {'trigger': 'cron', 'hour': 8, 'minute': 30},
                'executor': 'sync_executor',
                'enabled': True,
                'description': '同步股票基础信息'
            },
            
            # 日线数据同步 - 交易日收盘后17:30
            'sync_daily_data': {
                'task_type': TaskType.SYNC_DAILY_DATA,
                'schedule': {'trigger': 'cron', 'hour': 17, 'minute': 30, 'day_of_week': '0-4'},
                'executor': 'sync_executor',
                'enabled': True,
                'description': '同步日线行情数据'
            },
            
            # 市场数据同步 - 交易日收盘后18:00
            'sync_market_data': {
                'task_type': TaskType.SYNC_MARKET_DATA,
                'schedule': {'trigger': 'cron', 'hour': 18, 'minute': 0, 'day_of_week': '0-4'},
                'executor': 'sync_executor',
                'enabled': True,
                'description': '同步市场基础数据（PE、PB等）'
            },
            
            # 技术指标计算 - 交易日数据同步后19:00
            'calculate_indicators': {
                'task_type': TaskType.CALCULATE_INDICATORS,
                'schedule': {'trigger': 'cron', 'hour': 19, 'minute': 0, 'day_of_week': '0-4'},
                'executor': 'calc_executor',
                'enabled': True,
                'description': '计算技术指标'
            },
            
            # 财务数据同步 - 每月1日和15日的9:00
            'sync_financial_data': {
                'task_type': TaskType.SYNC_FINANCIAL_DATA,
                'schedule': {'trigger': 'cron', 'day': '1,15', 'hour': 9, 'minute': 0},
                'executor': 'sync_executor',
                'enabled': True,
                'description': '同步财务数据'
            },
            
            # 数据验证 - 每日20:00
            'data_validation': {
                'task_type': TaskType.DATA_VALIDATION,
                'schedule': {'trigger': 'cron', 'hour': 20, 'minute': 0},
                'executor': 'default',
                'enabled': True,
                'description': '数据质量验证'
            },
            
            # 系统维护 - 每周日凌晨2:00
            'system_maintenance': {
                'task_type': TaskType.SYSTEM_MAINTENANCE,
                'schedule': {'trigger': 'cron', 'day_of_week': 6, 'hour': 2, 'minute': 0},
                'executor': 'default',
                'enabled': True,
                'description': '系统维护和清理'
            }
        }
    
    def start(self):
        """启动调度器"""
        try:
            # 添加所有任务
            self._add_scheduled_tasks()
            
            # 启动调度器
            self.scheduler.start()
            
            logger.info("任务调度器启动成功")
            logger.info(f"已添加{len(self.scheduler.get_jobs())}个定时任务")
            
            # 显示任务列表
            self._log_scheduled_jobs()
            
        except Exception as e:
            logger.error(f"任务调度器启动失败: {e}")
            raise
    
    def stop(self):
        """停止调度器"""
        if self.scheduler.running:
            self.scheduler.shutdown()
            logger.info("任务调度器已停止")
    
    def _add_scheduled_tasks(self):
        """添加定时任务"""
        for task_id, config in self.task_configs.items():
            if not config.get('enabled', True):
                continue
            
            try:
                # 根据任务类型选择执行函数
                func = self._get_task_function(config['task_type'])
                
                # 添加定时任务
                self.scheduler.add_job(
                    func=func,
                    args=[task_id],
                    id=task_id,
                    name=config['description'],
                    executor=config.get('executor', 'default'),
                    replace_existing=True,
                    **config['schedule']
                )
                
                logger.info(f"已添加任务: {task_id} - {config['description']}")
                
            except Exception as e:
                logger.error(f"添加任务{task_id}失败: {e}")
    
    def _get_task_function(self, task_type: TaskType) -> Callable:
        """根据任务类型获取执行函数"""
        task_functions = {
            TaskType.SYNC_STOCK_BASIC: self._execute_sync_stock_basic,
            TaskType.SYNC_DAILY_DATA: self._execute_sync_daily_data,
            TaskType.SYNC_FINANCIAL_DATA: self._execute_sync_financial_data,
            TaskType.SYNC_MARKET_DATA: self._execute_sync_market_data,
            TaskType.CALCULATE_INDICATORS: self._execute_calculate_indicators,
            TaskType.DATA_VALIDATION: self._execute_data_validation,
            TaskType.SYSTEM_MAINTENANCE: self._execute_system_maintenance
        }
        
        return task_functions.get(task_type, self._execute_default_task)
    
    def _execute_sync_stock_basic(self, task_id: str):
        """执行股票基础信息同步"""
        log = self._start_task_log(task_id, TaskType.SYNC_STOCK_BASIC.value)
        
        try:
            result = self.data_manager.sync_stock_basic_info()
            
            self._complete_task_log(log, TaskStatus.SUCCESS, {
                'records_processed': result.records_processed,
                'records_success': result.records_success,
                'records_failed': result.records_failed,
                'duration': result.duration
            })
            
            logger.info(f"股票基础信息同步完成: 成功{result.records_success}条，失败{result.records_failed}条")
            
        except Exception as e:
            self._complete_task_log(log, TaskStatus.FAILED, error_message=str(e))
            logger.error(f"股票基础信息同步失败: {e}")
            raise
    
    def _execute_sync_daily_data(self, task_id: str):
        """执行日线数据同步"""
        log = self._start_task_log(task_id, TaskType.SYNC_DAILY_DATA.value)
        
        try:
            # 检查是否是交易日
            if not self._is_trading_day():
                self._complete_task_log(log, TaskStatus.SKIPPED, {'reason': '非交易日'})
                logger.info("今日非交易日，跳过日线数据同步")
                return
            
            result = self.data_manager.sync_daily_data(incremental=True)
            
            self._complete_task_log(log, TaskStatus.SUCCESS, {
                'records_processed': result.records_processed,
                'records_success': result.records_success,
                'records_failed': result.records_failed,
                'duration': result.duration
            })
            
            logger.info(f"日线数据同步完成: 成功{result.records_success}条，失败{result.records_failed}条")
            
        except Exception as e:
            self._complete_task_log(log, TaskStatus.FAILED, error_message=str(e))
            logger.error(f"日线数据同步失败: {e}")
            raise
    
    def _execute_sync_financial_data(self, task_id: str):
        """执行财务数据同步"""
        log = self._start_task_log(task_id, TaskType.SYNC_FINANCIAL_DATA.value)
        
        try:
            result = self.data_manager.sync_financial_data()
            
            self._complete_task_log(log, TaskStatus.SUCCESS, {
                'records_processed': result.records_processed,
                'records_success': result.records_success,
                'records_failed': result.records_failed,
                'duration': result.duration
            })
            
            logger.info(f"财务数据同步完成: 成功{result.records_success}条，失败{result.records_failed}条")
            
        except Exception as e:
            self._complete_task_log(log, TaskStatus.FAILED, error_message=str(e))
            logger.error(f"财务数据同步失败: {e}")
            raise
    
    def _execute_sync_market_data(self, task_id: str):
        """执行市场数据同步"""
        log = self._start_task_log(task_id, TaskType.SYNC_MARKET_DATA.value)
        
        try:
            if not self._is_trading_day():
                self._complete_task_log(log, TaskStatus.SKIPPED, {'reason': '非交易日'})
                logger.info("今日非交易日，跳过市场数据同步")
                return
            
            result = self.data_manager.sync_market_data()
            
            self._complete_task_log(log, TaskStatus.SUCCESS, {
                'records_processed': result.records_processed,
                'records_success': result.records_success,
                'records_failed': result.records_failed,
                'duration': result.duration
            })
            
            logger.info(f"市场数据同步完成: 成功{result.records_success}条，失败{result.records_failed}条")
            
        except Exception as e:
            self._complete_task_log(log, TaskStatus.FAILED, error_message=str(e))
            logger.error(f"市场数据同步失败: {e}")
            raise
    
    def _execute_calculate_indicators(self, task_id: str):
        """执行技术指标计算"""
        log = self._start_task_log(task_id, TaskType.CALCULATE_INDICATORS.value)
        
        try:
            if not self._is_trading_day():
                self._complete_task_log(log, TaskStatus.SKIPPED, {'reason': '非交易日'})
                logger.info("今日非交易日，跳过技术指标计算")
                return
            
            # 获取需要计算的股票列表
            stock_codes = self.indicator_calculator._get_active_stock_codes()
            
            # 选择关键指标进行计算
            key_indicators = ['ma', 'rsi', 'macd', 'kdj', 'bollinger', 'atr']
            
            results = self.indicator_calculator.calculate_all_indicators_batch(
                stock_codes=stock_codes[:500],  # 限制前500只股票以控制计算时间
                indicators=key_indicators,
                parallel=True
            )
            
            success_count = sum(1 for r in results if r.success)
            total_indicators = sum(len(r.calculated_indicators) for r in results if r.success)
            
            self._complete_task_log(log, TaskStatus.SUCCESS, {
                'stocks_processed': len(results),
                'stocks_success': success_count,
                'total_indicators': total_indicators,
                'indicators': key_indicators
            })
            
            logger.info(f"技术指标计算完成: 成功处理{success_count}只股票，计算{total_indicators}个指标")
            
        except Exception as e:
            self._complete_task_log(log, TaskStatus.FAILED, error_message=str(e))
            logger.error(f"技术指标计算失败: {e}")
            raise
    
    def _execute_data_validation(self, task_id: str):
        """执行数据验证"""
        log = self._start_task_log(task_id, TaskType.DATA_VALIDATION.value)
        
        try:
            # 这里实现数据质量检查逻辑
            validation_results = {}
            
            # 可以添加具体的数据验证逻辑
            # 例如：检查最近数据的完整性、异常值等
            
            self._complete_task_log(log, TaskStatus.SUCCESS, {
                'validation_results': validation_results,
                'message': '数据验证完成'
            })
            
            logger.info("数据验证任务完成")
            
        except Exception as e:
            self._complete_task_log(log, TaskStatus.FAILED, error_message=str(e))
            logger.error(f"数据验证失败: {e}")
            raise
    
    def _execute_system_maintenance(self, task_id: str):
        """执行系统维护"""
        log = self._start_task_log(task_id, TaskType.SYSTEM_MAINTENANCE.value)
        
        try:
            maintenance_tasks = []
            
            # 1. 清理过期日志
            self._cleanup_execution_logs()
            maintenance_tasks.append("清理执行日志")
            
            # 2. 数据库优化（如果是MySQL）
            if self.db_config.get('type') == 'mysql':
                self._optimize_database()
                maintenance_tasks.append("数据库优化")
            
            # 3. 清理临时文件
            self._cleanup_temp_files()
            maintenance_tasks.append("清理临时文件")
            
            self._complete_task_log(log, TaskStatus.SUCCESS, {
                'maintenance_tasks': maintenance_tasks,
                'message': '系统维护完成'
            })
            
            logger.info(f"系统维护完成: {', '.join(maintenance_tasks)}")
            
        except Exception as e:
            self._complete_task_log(log, TaskStatus.FAILED, error_message=str(e))
            logger.error(f"系统维护失败: {e}")
            raise
    
    def _execute_default_task(self, task_id: str):
        """默认任务执行器"""
        log = self._start_task_log(task_id, "default")
        self._complete_task_log(log, TaskStatus.SUCCESS, {'message': '默认任务完成'})
        logger.info(f"默认任务执行完成: {task_id}")
    
    def _start_task_log(self, task_id: str, task_type: str) -> TaskExecutionLog:
        """开始任务日志记录"""
        log = TaskExecutionLog(
            task_id=task_id,
            task_type=task_type,
            status=TaskStatus.RUNNING,
            start_time=datetime.now().isoformat()
        )
        
        self.execution_logs.append(log)
        logger.info(f"开始执行任务: {task_id} ({task_type})")
        
        return log
    
    def _complete_task_log(self, log: TaskExecutionLog, status: TaskStatus, 
                          result: Dict[str, Any] = None, error_message: str = None):
        """完成任务日志记录"""
        log.status = status
        log.end_time = datetime.now().isoformat()
        
        start_dt = datetime.fromisoformat(log.start_time)
        end_dt = datetime.fromisoformat(log.end_time)
        log.duration = (end_dt - start_dt).total_seconds()
        
        if result:
            log.result = result
        
        if error_message:
            log.error_message = error_message
        
        # 保持日志数量在限制内
        if len(self.execution_logs) > self.max_log_entries:
            self.execution_logs = self.execution_logs[-self.max_log_entries:]
    
    def _job_executed(self, event):
        """作业执行完成事件处理"""
        logger.debug(f"任务执行完成: {event.job_id}")
    
    def _job_error(self, event):
        """作业执行错误事件处理"""
        logger.error(f"任务执行错误: {event.job_id}, 异常: {event.exception}")
    
    def _job_missed(self, event):
        """作业错过事件处理"""
        logger.warning(f"任务被跳过: {event.job_id}")
    
    def _is_trading_day(self, date: datetime = None) -> bool:
        """检查是否是交易日"""
        if date is None:
            date = datetime.now()
        
        # 简单的交易日检查：周一到周五
        # 实际应用中应该考虑节假日
        return date.weekday() < 5
    
    def _cleanup_execution_logs(self):
        """清理执行日志"""
        # 只保留最近7天的日志
        cutoff_date = datetime.now() - timedelta(days=7)
        cutoff_str = cutoff_date.isoformat()
        
        original_count = len(self.execution_logs)
        self.execution_logs = [
            log for log in self.execution_logs 
            if log.start_time >= cutoff_str
        ]
        
        cleaned_count = original_count - len(self.execution_logs)
        logger.info(f"清理了{cleaned_count}条执行日志")
    
    def _optimize_database(self):
        """数据库优化"""
        try:
            connection = self.data_manager.get_connection()
            cursor = connection.cursor()
            
            # 执行表优化
            tables = ['stock_daily', 'technical_indicators', 'financial_data']
            for table in tables:
                try:
                    cursor.execute(f"OPTIMIZE TABLE {table}")
                    logger.debug(f"优化表: {table}")
                except Exception as e:
                    logger.warning(f"优化表{table}失败: {e}")
            
            connection.close()
            logger.info("数据库优化完成")
            
        except Exception as e:
            logger.error(f"数据库优化失败: {e}")
    
    def _cleanup_temp_files(self):
        """清理临时文件"""
        temp_dirs = ['temp', 'cache', 'logs']
        cleaned_files = 0
        
        for temp_dir in temp_dirs:
            temp_path = Path(temp_dir)
            if temp_path.exists():
                try:
                    for file_path in temp_path.glob('*'):
                        if file_path.is_file():
                            # 删除7天前的文件
                            if file_path.stat().st_mtime < time.time() - 7 * 24 * 3600:
                                file_path.unlink()
                                cleaned_files += 1
                except Exception as e:
                    logger.warning(f"清理临时目录{temp_dir}失败: {e}")
        
        logger.info(f"清理了{cleaned_files}个临时文件")
    
    def _log_scheduled_jobs(self):
        """记录已调度的任务"""
        jobs = self.scheduler.get_jobs()
        logger.info("当前调度任务:")
        for job in jobs:
            next_run = job.next_run_time.strftime('%Y-%m-%d %H:%M:%S') if job.next_run_time else "暂停"
            logger.info(f"  - {job.id}: {job.name} (下次运行: {next_run})")
    
    def get_job_status(self) -> Dict[str, Any]:
        """获取任务状态"""
        jobs = self.scheduler.get_jobs()
        
        job_status = []
        for job in jobs:
            job_status.append({
                'id': job.id,
                'name': job.name,
                'next_run_time': job.next_run_time.isoformat() if job.next_run_time else None,
                'trigger': str(job.trigger),
                'executor': job.executor
            })
        
        # 最近执行日志
        recent_logs = sorted(
            self.execution_logs,
            key=lambda x: x.start_time,
            reverse=True
        )[:10]
        
        return {
            'scheduler_running': self.scheduler.running,
            'total_jobs': len(jobs),
            'jobs': job_status,
            'recent_executions': [asdict(log) for log in recent_logs],
            'last_updated': datetime.now().isoformat()
        }
    
    def run_task_manually(self, task_id: str) -> bool:
        """手动运行任务"""
        try:
            if task_id not in self.task_configs:
                logger.error(f"未知任务ID: {task_id}")
                return False
            
            config = self.task_configs[task_id]
            func = self._get_task_function(config['task_type'])
            
            # 在新线程中执行任务
            def run_task():
                try:
                    func(task_id)
                    logger.info(f"手动任务执行成功: {task_id}")
                except Exception as e:
                    logger.error(f"手动任务执行失败: {task_id}, 错误: {e}")
            
            thread = threading.Thread(target=run_task)
            thread.daemon = True
            thread.start()
            
            return True
            
        except Exception as e:
            logger.error(f"启动手动任务失败: {e}")
            return False
    
    def pause_job(self, job_id: str) -> bool:
        """暂停任务"""
        try:
            self.scheduler.pause_job(job_id)
            logger.info(f"任务已暂停: {job_id}")
            return True
        except Exception as e:
            logger.error(f"暂停任务失败: {e}")
            return False
    
    def resume_job(self, job_id: str) -> bool:
        """恢复任务"""
        try:
            self.scheduler.resume_job(job_id)
            logger.info(f"任务已恢复: {job_id}")
            return True
        except Exception as e:
            logger.error(f"恢复任务失败: {e}")
            return False

def create_default_config() -> Dict[str, Any]:
    """创建默认配置"""
    return {
        'database': {
            'type': 'sqlite',
            'database': 'data/stock_analysis.db'
        },
        'tushare_token': os.getenv('TUSHARE_TOKEN', ''),
        'timezone': 'Asia/Shanghai',
        'scheduler_db': 'data/scheduler.db'
    }

def main():
    """主函数 - 任务调度器测试"""
    # 创建配置
    config = create_default_config()
    
    if not config['tushare_token']:
        logger.error("请设置TUSHARE_TOKEN环境变量")
        return
    
    # 创建调度器
    scheduler = DataSyncTaskScheduler(config)
    
    try:
        print("=== 数据同步任务调度器测试 ===")
        
        # 启动调度器
        scheduler.start()
        
        # 获取状态
        status = scheduler.get_job_status()
        print(f"\n调度器状态:")
        print(f"  运行状态: {status['scheduler_running']}")
        print(f"  任务总数: {status['total_jobs']}")
        
        print(f"\n已调度任务:")
        for job in status['jobs']:
            next_run = job['next_run_time']
            if next_run:
                next_run = datetime.fromisoformat(next_run).strftime('%Y-%m-%d %H:%M:%S')
            else:
                next_run = "暂停"
            print(f"  - {job['id']}: {job['name']} (下次: {next_run})")
        
        # 手动执行一个测试任务
        print(f"\n手动执行测试任务...")
        success = scheduler.run_task_manually('sync_stock_basic')
        print(f"任务启动{'成功' if success else '失败'}")
        
        # 等待一段时间查看执行结果
        time.sleep(3)
        
        # 获取最新状态
        status = scheduler.get_job_status()
        if status['recent_executions']:
            print(f"\n最近执行记录:")
            for log in status['recent_executions'][:3]:
                duration = log['duration'] or 0
                print(f"  - {log['task_id']}: {log['status']} (耗时: {duration:.2f}秒)")
        
        print(f"\n调度器将持续运行，按Ctrl+C停止...")
        
        # 保持运行
        while True:
            time.sleep(10)
    
    except KeyboardInterrupt:
        print(f"\n正在停止调度器...")
        scheduler.stop()
        print("调度器已停止")
    
    except Exception as e:
        logger.error(f"调度器运行错误: {e}")
        scheduler.stop()

if __name__ == "__main__":
    main()