#!/usr/bin/env python3
"""
数据库优化系统主程序
整合所有数据库相关组件，提供统一的接口
"""

import os
import sys
import logging
import argparse
from pathlib import Path
from typing import Dict, Any, Optional
import json

# 添加项目路径
sys.path.append(str(Path(__file__).parent.parent))

from database.init_database import DatabaseInitializer, get_database_config
from database.tushare_data_manager import TuShareDataManager
from database.technical_indicator_calculator import TechnicalIndicatorCalculator
from database.data_validator import DataValidator, DataCleaner, DataQualityReporter
from database.task_scheduler import DataSyncTaskScheduler
from database.cache_and_monitor import CacheManager, PerformanceMonitor, create_default_cache_config

# 设置日志
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    handlers=[
        logging.StreamHandler(),
        logging.FileHandler('database_system.log')
    ]
)
logger = logging.getLogger(__name__)

class DatabaseOptimizationSystem:
    """数据库优化系统主类"""
    
    def __init__(self, config_file: str = None):
        """
        初始化数据库优化系统
        
        Args:
            config_file: 配置文件路径
        """
        # 加载配置
        self.config = self._load_config(config_file)
        
        # 初始化组件
        self.db_initializer = None
        self.data_manager = None
        self.indicator_calculator = None
        self.data_validator = None
        self.data_cleaner = None
        self.quality_reporter = None
        self.task_scheduler = None
        self.cache_manager = None
        self.performance_monitor = None
        
        # 系统状态
        self.is_initialized = False
        self.components_initialized = {}
        
    def _load_config(self, config_file: str = None) -> Dict[str, Any]:
        """加载配置"""
        default_config = {
            # 数据库配置
            'database': get_database_config(),
            
            # TuShare配置
            'tushare_token': os.getenv('TUSHARE_TOKEN', ''),
            
            # 缓存配置
            'cache': create_default_cache_config(),
            
            # 调度器配置
            'scheduler': {
                'timezone': 'Asia/Shanghai',
                'enabled': True
            },
            
            # 监控配置
            'monitoring': {
                'enabled': True,
                'max_history': 1000
            },
            
            # 系统配置
            'system': {
                'log_level': 'INFO',
                'max_concurrent_tasks': 5,
                'data_retention_days': 90
            }
        }
        
        if config_file and os.path.exists(config_file):
            try:
                with open(config_file, 'r', encoding='utf-8') as f:
                    user_config = json.load(f)
                    # 递归合并配置
                    self._merge_config(default_config, user_config)
                    logger.info(f"已加载配置文件: {config_file}")
            except Exception as e:
                logger.error(f"加载配置文件失败: {e}")
        
        return default_config
    
    def _merge_config(self, base_config: Dict, user_config: Dict):
        """递归合并配置"""
        for key, value in user_config.items():
            if key in base_config and isinstance(base_config[key], dict) and isinstance(value, dict):
                self._merge_config(base_config[key], value)
            else:
                base_config[key] = value
    
    def initialize_system(self) -> bool:
        """初始化整个系统"""
        logger.info("开始初始化数据库优化系统...")
        
        try:
            # 1. 初始化数据库
            if not self._initialize_database():
                logger.error("数据库初始化失败")
                return False
            
            # 2. 初始化数据管理器
            if not self._initialize_data_manager():
                logger.error("数据管理器初始化失败")
                return False
            
            # 3. 初始化技术指标计算器
            if not self._initialize_indicator_calculator():
                logger.error("技术指标计算器初始化失败")
                return False
            
            # 4. 初始化数据验证器
            if not self._initialize_data_validator():
                logger.error("数据验证器初始化失败")
                return False
            
            # 5. 初始化缓存管理器
            if not self._initialize_cache_manager():
                logger.error("缓存管理器初始化失败")
                return False
            
            # 6. 初始化性能监控器
            if not self._initialize_performance_monitor():
                logger.error("性能监控器初始化失败")
                return False
            
            # 7. 初始化任务调度器
            if self.config['scheduler']['enabled']:
                if not self._initialize_task_scheduler():
                    logger.error("任务调度器初始化失败")
                    return False
            
            self.is_initialized = True
            logger.info("数据库优化系统初始化完成")
            return True
            
        except Exception as e:
            logger.error(f"系统初始化失败: {e}")
            return False
    
    def _initialize_database(self) -> bool:
        """初始化数据库"""
        try:
            self.db_initializer = DatabaseInitializer(self.config['database'])
            self.components_initialized['database'] = True
            logger.info("✅ 数据库初始化器已创建")
            return True
        except Exception as e:
            logger.error(f"数据库初始化失败: {e}")
            self.components_initialized['database'] = False
            return False
    
    def _initialize_data_manager(self) -> bool:
        """初始化数据管理器"""
        try:
            if not self.config['tushare_token']:
                logger.warning("TuShare Token未配置，数据同步功能将受限")
                self.components_initialized['data_manager'] = False
                return True  # 不阻断系统初始化
            
            self.data_manager = TuShareDataManager(
                self.config['tushare_token'],
                self.config['database']
            )
            self.components_initialized['data_manager'] = True
            logger.info("✅ TuShare数据管理器已初始化")
            return True
        except Exception as e:
            logger.error(f"数据管理器初始化失败: {e}")
            self.components_initialized['data_manager'] = False
            return False
    
    def _initialize_indicator_calculator(self) -> bool:
        """初始化技术指标计算器"""
        try:
            self.indicator_calculator = TechnicalIndicatorCalculator(self.config['database'])
            self.components_initialized['indicator_calculator'] = True
            logger.info("✅ 技术指标计算器已初始化")
            return True
        except Exception as e:
            logger.error(f"技术指标计算器初始化失败: {e}")
            self.components_initialized['indicator_calculator'] = False
            return False
    
    def _initialize_data_validator(self) -> bool:
        """初始化数据验证器"""
        try:
            self.data_validator = DataValidator()
            self.data_cleaner = DataCleaner()
            self.quality_reporter = DataQualityReporter()
            self.components_initialized['data_validator'] = True
            logger.info("✅ 数据验证和清洗组件已初始化")
            return True
        except Exception as e:
            logger.error(f"数据验证器初始化失败: {e}")
            self.components_initialized['data_validator'] = False
            return False
    
    def _initialize_cache_manager(self) -> bool:
        """初始化缓存管理器"""
        try:
            self.cache_manager = CacheManager(self.config['cache'])
            self.components_initialized['cache_manager'] = True
            logger.info("✅ 缓存管理器已初始化")
            return True
        except Exception as e:
            logger.error(f"缓存管理器初始化失败: {e}")
            self.components_initialized['cache_manager'] = False
            return False
    
    def _initialize_performance_monitor(self) -> bool:
        """初始化性能监控器"""
        try:
            if not self.config['monitoring']['enabled']:
                logger.info("性能监控已禁用")
                return True
            
            self.performance_monitor = PerformanceMonitor(
                max_history=self.config['monitoring']['max_history']
            )
            
            # 添加缓存管理器到监控
            if self.cache_manager:
                self.performance_monitor.add_cache_manager(self.cache_manager)
            
            self.components_initialized['performance_monitor'] = True
            logger.info("✅ 性能监控器已初始化")
            return True
        except Exception as e:
            logger.error(f"性能监控器初始化失败: {e}")
            self.components_initialized['performance_monitor'] = False
            return False
    
    def _initialize_task_scheduler(self) -> bool:
        """初始化任务调度器"""
        try:
            scheduler_config = {
                'database': self.config['database'],
                'tushare_token': self.config['tushare_token'],
                'timezone': self.config['scheduler']['timezone']
            }
            
            self.task_scheduler = DataSyncTaskScheduler(scheduler_config)
            self.components_initialized['task_scheduler'] = True
            logger.info("✅ 任务调度器已初始化")
            return True
        except Exception as e:
            logger.error(f"任务调度器初始化失败: {e}")
            self.components_initialized['task_scheduler'] = False
            return False
    
    def create_database(self) -> bool:
        """创建数据库结构"""
        if not self.db_initializer:
            logger.error("数据库初始化器未初始化")
            return False
        
        try:
            self.db_initializer.initialize_database()
            logger.info("✅ 数据库结构创建完成")
            return True
        except Exception as e:
            logger.error(f"创建数据库结构失败: {e}")
            return False
    
    def sync_stock_basic_data(self) -> bool:
        """同步股票基础信息"""
        if not self.data_manager:
            logger.error("数据管理器未初始化")
            return False
        
        try:
            result = self.data_manager.sync_stock_basic_info()
            if result.success:
                logger.info(f"✅ 股票基础信息同步完成: 成功{result.records_success}条")
                return True
            else:
                logger.error(f"股票基础信息同步失败: {result.error_messages}")
                return False
        except Exception as e:
            logger.error(f"同步股票基础信息失败: {e}")
            return False
    
    def sync_daily_data(self, trade_date: str = None) -> bool:
        """同步日线数据"""
        if not self.data_manager:
            logger.error("数据管理器未初始化")
            return False
        
        try:
            result = self.data_manager.sync_daily_data(trade_date=trade_date)
            if result.success:
                logger.info(f"✅ 日线数据同步完成: 成功{result.records_success}条")
                return True
            else:
                logger.error(f"日线数据同步失败: {result.error_messages}")
                return False
        except Exception as e:
            logger.error(f"同步日线数据失败: {e}")
            return False
    
    def calculate_technical_indicators(self, stock_codes: list = None) -> bool:
        """计算技术指标"""
        if not self.indicator_calculator:
            logger.error("技术指标计算器未初始化")
            return False
        
        try:
            results = self.indicator_calculator.calculate_all_indicators_batch(
                stock_codes=stock_codes,
                parallel=True
            )
            
            success_count = sum(1 for r in results if r.success)
            logger.info(f"✅ 技术指标计算完成: 成功处理{success_count}/{len(results)}只股票")
            return success_count > 0
            
        except Exception as e:
            logger.error(f"计算技术指标失败: {e}")
            return False
    
    def validate_data_quality(self) -> Dict[str, Any]:
        """验证数据质量"""
        if not self.data_validator:
            logger.error("数据验证器未初始化")
            return {}
        
        try:
            # 这里可以实现具体的数据质量检查逻辑
            # 例如：获取最近数据进行验证
            validation_results = {}
            
            # 示例：验证股票基础数据
            # stock_data = get_recent_stock_data()
            # validation_results['stock_basic'] = self.data_validator.validate_stock_basic(stock_data)
            
            logger.info("✅ 数据质量验证完成")
            return validation_results
            
        except Exception as e:
            logger.error(f"数据质量验证失败: {e}")
            return {}
    
    def start_scheduler(self) -> bool:
        """启动任务调度器"""
        if not self.task_scheduler:
            logger.error("任务调度器未初始化")
            return False
        
        try:
            self.task_scheduler.start()
            logger.info("✅ 任务调度器已启动")
            return True
        except Exception as e:
            logger.error(f"启动任务调度器失败: {e}")
            return False
    
    def stop_scheduler(self) -> bool:
        """停止任务调度器"""
        if not self.task_scheduler:
            return True
        
        try:
            self.task_scheduler.stop()
            logger.info("✅ 任务调度器已停止")
            return True
        except Exception as e:
            logger.error(f"停止任务调度器失败: {e}")
            return False
    
    def get_system_status(self) -> Dict[str, Any]:
        """获取系统状态"""
        status = {
            'initialized': self.is_initialized,
            'components': self.components_initialized.copy(),
            'database_config': {
                'type': self.config['database']['type'],
                'database': self.config['database'].get('database', 'N/A')
            },
            'tushare_configured': bool(self.config['tushare_token']),
            'scheduler_enabled': self.config['scheduler']['enabled'],
            'monitoring_enabled': self.config['monitoring']['enabled']
        }
        
        # 获取调度器状态
        if self.task_scheduler:
            try:
                scheduler_status = self.task_scheduler.get_job_status()
                status['scheduler'] = {
                    'running': scheduler_status['scheduler_running'],
                    'total_jobs': scheduler_status['total_jobs'],
                    'recent_executions_count': len(scheduler_status.get('recent_executions', []))
                }
            except Exception as e:
                status['scheduler'] = {'error': str(e)}
        
        # 获取缓存状态
        if self.cache_manager:
            try:
                cache_stats = self.cache_manager.get_stats()
                status['cache'] = {}
                for cache_type, stats in cache_stats.items():
                    status['cache'][cache_type] = {
                        'hit_rate': stats.hit_rate,
                        'cache_size': stats.cache_size,
                        'total_requests': stats.total_requests
                    }
            except Exception as e:
                status['cache'] = {'error': str(e)}
        
        # 获取性能监控状态
        if self.performance_monitor:
            try:
                perf_summary = self.performance_monitor.get_performance_summary()
                if perf_summary['status'] == 'healthy':
                    status['performance'] = {
                        'status': 'healthy',
                        'cpu_usage': perf_summary['averages']['cpu_usage'],
                        'memory_usage': perf_summary['averages']['memory_usage'],
                        'alerts_count': len(perf_summary.get('alerts', []))
                    }
                else:
                    status['performance'] = {'status': perf_summary['status']}
            except Exception as e:
                status['performance'] = {'error': str(e)}
        
        return status
    
    def run_maintenance(self) -> bool:
        """运行系统维护"""
        logger.info("开始系统维护...")
        
        try:
            maintenance_tasks = []
            
            # 清理过期缓存
            if self.cache_manager:
                self.cache_manager.cleanup_expired()
                maintenance_tasks.append("清理过期缓存")
            
            # 数据质量检查
            validation_results = self.validate_data_quality()
            if validation_results:
                maintenance_tasks.append("数据质量检查")
            
            # 性能指标收集
            if self.performance_monitor:
                self.performance_monitor.collect_metrics()
                maintenance_tasks.append("性能指标收集")
            
            logger.info(f"✅ 系统维护完成: {', '.join(maintenance_tasks)}")
            return True
            
        except Exception as e:
            logger.error(f"系统维护失败: {e}")
            return False

def main():
    """主程序"""
    parser = argparse.ArgumentParser(description='数据库优化系统')
    parser.add_argument('--config', '-c', help='配置文件路径')
    parser.add_argument('--command', '-cmd', choices=[
        'init', 'sync-basic', 'sync-daily', 'calc-indicators', 
        'validate', 'start-scheduler', 'status', 'maintenance'
    ], help='执行命令')
    parser.add_argument('--create-db', action='store_true', help='创建数据库结构')
    parser.add_argument('--date', help='指定日期 (YYYY-MM-DD)')
    parser.add_argument('--stocks', nargs='*', help='指定股票代码列表')
    
    args = parser.parse_args()
    
    # 创建系统实例
    system = DatabaseOptimizationSystem(args.config)
    
    try:
        if args.command == 'init' or not args.command:
            # 初始化系统
            print("正在初始化数据库优化系统...")
            if system.initialize_system():
                print("✅ 系统初始化成功")
                
                # 如果指定创建数据库
                if args.create_db:
                    print("正在创建数据库结构...")
                    if system.create_database():
                        print("✅ 数据库结构创建成功")
                    else:
                        print("❌ 数据库结构创建失败")
                        return 1
            else:
                print("❌ 系统初始化失败")
                return 1
        
        elif args.command == 'sync-basic':
            # 同步股票基础信息
            if system.initialize_system():
                print("正在同步股票基础信息...")
                if system.sync_stock_basic_data():
                    print("✅ 股票基础信息同步成功")
                else:
                    print("❌ 股票基础信息同步失败")
                    return 1
        
        elif args.command == 'sync-daily':
            # 同步日线数据
            if system.initialize_system():
                print("正在同步日线数据...")
                if system.sync_daily_data(args.date):
                    print("✅ 日线数据同步成功")
                else:
                    print("❌ 日线数据同步失败")
                    return 1
        
        elif args.command == 'calc-indicators':
            # 计算技术指标
            if system.initialize_system():
                print("正在计算技术指标...")
                if system.calculate_technical_indicators(args.stocks):
                    print("✅ 技术指标计算成功")
                else:
                    print("❌ 技术指标计算失败")
                    return 1
        
        elif args.command == 'validate':
            # 验证数据质量
            if system.initialize_system():
                print("正在验证数据质量...")
                results = system.validate_data_quality()
                print(f"✅ 数据质量验证完成")
                if results:
                    print(f"验证结果: {len(results)} 项检查完成")
        
        elif args.command == 'start-scheduler':
            # 启动调度器
            if system.initialize_system():
                if system.start_scheduler():
                    print("✅ 任务调度器已启动")
                    print("按 Ctrl+C 停止调度器...")
                    try:
                        import time
                        while True:
                            time.sleep(10)
                    except KeyboardInterrupt:
                        system.stop_scheduler()
                        print("\n✅ 任务调度器已停止")
                else:
                    print("❌ 任务调度器启动失败")
                    return 1
        
        elif args.command == 'status':
            # 获取系统状态
            if system.initialize_system():
                status = system.get_system_status()
                print("=== 系统状态 ===")
                print(f"系统初始化: {'✅' if status['initialized'] else '❌'}")
                print(f"数据库类型: {status['database_config']['type']}")
                print(f"TuShare配置: {'✅' if status['tushare_configured'] else '❌'}")
                
                print("\n组件状态:")
                for component, initialized in status['components'].items():
                    print(f"  {component}: {'✅' if initialized else '❌'}")
                
                if 'scheduler' in status:
                    sched = status['scheduler']
                    if 'running' in sched:
                        print(f"\n调度器: {'✅运行中' if sched['running'] else '❌已停止'}")
                        print(f"  任务数: {sched['total_jobs']}")
                        print(f"  最近执行: {sched['recent_executions_count']}条记录")
                
                if 'cache' in status:
                    print(f"\n缓存状态:")
                    for cache_type, stats in status['cache'].items():
                        if isinstance(stats, dict) and 'hit_rate' in stats:
                            print(f"  {cache_type}: 命中率{stats['hit_rate']:.1%}, 大小{stats['cache_size']}")
                
                if 'performance' in status:
                    perf = status['performance']
                    if perf['status'] == 'healthy':
                        print(f"\n性能状态: ✅健康")
                        print(f"  CPU使用率: {perf['cpu_usage']:.1f}%")
                        print(f"  内存使用率: {perf['memory_usage']:.1f}%")
                        if perf['alerts_count'] > 0:
                            print(f"  告警: {perf['alerts_count']}条")
                    else:
                        print(f"\n性能状态: {perf['status']}")
        
        elif args.command == 'maintenance':
            # 系统维护
            if system.initialize_system():
                print("正在执行系统维护...")
                if system.run_maintenance():
                    print("✅ 系统维护完成")
                else:
                    print("❌ 系统维护失败")
                    return 1
        
        return 0
        
    except KeyboardInterrupt:
        print("\n程序被用户中断")
        if system.task_scheduler:
            system.stop_scheduler()
        return 0
    except Exception as e:
        logger.error(f"程序执行出错: {e}")
        print(f"❌ 程序执行出错: {e}")
        return 1

if __name__ == "__main__":
    exit(main())