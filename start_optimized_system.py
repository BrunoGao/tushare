#!/usr/bin/env python3
"""
优化系统启动脚本
一键启动所有优化后的系统组件
"""
import sys
import os
import time
import threading
import subprocess
import logging
from datetime import datetime
from typing import Dict, List
import signal
import atexit

# 添加项目路径
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

# 配置日志
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    handlers=[
        logging.StreamHandler(),
        logging.FileHandler('logs/system_startup.log', encoding='utf-8')
    ]
)
logger = logging.getLogger(__name__)

class OptimizedSystemManager:
    """优化系统管理器"""
    
    def __init__(self):
        self.processes = {}
        self.threads = {}
        self.running = False
        self.startup_time = datetime.now()
        
        # 注册退出处理
        atexit.register(self.cleanup)
        signal.signal(signal.SIGINT, self.signal_handler)
        signal.signal(signal.SIGTERM, self.signal_handler)
        
    def signal_handler(self, signum, frame):
        """信号处理器"""
        logger.info(f"🛑 收到信号 {signum}，开始优雅关闭...")
        self.stop_all_services()
        sys.exit(0)
    
    def start_system(self):
        """启动优化系统"""
        logger.info("🚀 启动灵境万象优化系统")
        logger.info("=" * 80)
        
        try:
            # 1. 系统预检查
            self.pre_startup_check()
            
            # 2. 启动核心服务
            self.start_core_services()
            
            # 3. 启动监控服务
            self.start_monitoring_services()
            
            # 4. 启动API服务
            self.start_api_service()
            
            # 5. 启动调度服务
            self.start_scheduler_service()
            
            # 6. 系统就绪检查
            self.post_startup_check()
            
            self.running = True
            logger.info("✅ 系统启动完成！")
            self.print_system_status()
            
            # 保持运行
            self.keep_running()
            
        except Exception as e:
            logger.error(f"❌ 系统启动失败: {e}")
            self.cleanup()
            sys.exit(1)
    
    def pre_startup_check(self):
        """启动前检查"""
        logger.info("🔍 执行启动前检查...")
        
        # 检查Python版本
        if sys.version_info < (3, 8):
            raise RuntimeError("需要Python 3.8或更高版本")
        
        # 检查必要目录
        required_dirs = ['logs', 'data', 'cache']
        for dir_name in required_dirs:
            os.makedirs(dir_name, exist_ok=True)
        
        # 检查数据库连接
        try:
            from utils.db_helper import db
            from sqlalchemy import text
            with db.engine.connect() as conn:
                conn.execute(text("SELECT 1"))
            logger.info("✅ 数据库连接正常")
        except Exception as e:
            logger.warning(f"⚠️ 数据库连接异常: {e}")
        
        # 检查Redis连接
        try:
            from utils.redis_cache_manager import cache_manager
            health = cache_manager.health_check()
            if health['status'] == 'healthy':
                logger.info("✅ Redis缓存连接正常")
            else:
                logger.warning(f"⚠️ Redis缓存状态: {health['status']}")
        except Exception as e:
            logger.warning(f"⚠️ Redis连接异常: {e}")
        
        logger.info("✅ 启动前检查完成")
    
    def start_core_services(self):
        """启动核心服务"""
        logger.info("🔧 启动核心服务...")
        
        # 启动数据健康检查器
        try:
            from utils.data_health_checker import data_health_checker
            
            def health_checker_worker():
                logger.info("🏥 数据健康检查器已启动")
                # 执行一次初始检查
                health_report = data_health_checker.check_system_health()
                logger.info(f"📊 系统健康评分: {health_report.get('overall_score', 0)}")
                
                # 如果有严重问题，尝试自动修复
                if health_report.get('overall_status') == 'CRITICAL':
                    logger.warning("🚨 检测到严重问题，启动自动修复...")
                    repair_result = data_health_checker.auto_repair(health_report)
                    logger.info(f"🔧 自动修复完成: {repair_result}")
            
            health_thread = threading.Thread(target=health_checker_worker, daemon=True)
            health_thread.start()
            self.threads['health_checker'] = health_thread
            
        except Exception as e:
            logger.error(f"❌ 数据健康检查器启动失败: {e}")
        
        # 预热缓存
        try:
            from utils.redis_cache_manager import cache_manager
            
            def cache_preloader():
                logger.info("🔥 开始缓存预热...")
                
                # 预热基础数据
                preload_config = {
                    'system_status': lambda: {'status': 'ready', 'timestamp': datetime.now().isoformat()}
                }
                
                results = cache_manager.preload_cache(preload_config)
                logger.info(f"🔥 缓存预热完成: {results}")
            
            cache_thread = threading.Thread(target=cache_preloader, daemon=True)
            cache_thread.start()
            self.threads['cache_preloader'] = cache_thread
            
        except Exception as e:
            logger.error(f"❌ 缓存预热失败: {e}")
        
        logger.info("✅ 核心服务启动完成")
    
    def start_monitoring_services(self):
        """启动监控服务"""
        logger.info("📊 启动监控服务...")
        
        try:
            from utils.system_monitor import system_monitor
            
            def monitoring_worker():
                logger.info("📈 系统监控器已启动")
                system_monitor.start_monitoring()
            
            monitor_thread = threading.Thread(target=monitoring_worker, daemon=True)
            monitor_thread.start()
            self.threads['system_monitor'] = monitor_thread
            
            # 等待监控器初始化
            time.sleep(2)
            
            logger.info("✅ 监控服务启动完成")
            
        except Exception as e:
            logger.error(f"❌ 监控服务启动失败: {e}")
    
    def start_api_service(self):
        """启动API服务"""
        logger.info("🌐 启动API服务...")
        
        try:
            import config
            
            def api_worker():
                logger.info(f"🚀 API服务器启动中... (http://{config.API_HOST}:{config.API_PORT})")
                
                # 导入并启动Flask应用
                from api.app import app
                app.run(
                    host=config.API_HOST,
                    port=config.API_PORT,
                    debug=False,  # 生产环境关闭调试
                    use_reloader=False,  # 避免重复启动
                    threaded=True
                )
            
            api_thread = threading.Thread(target=api_worker, daemon=True)
            api_thread.start()
            self.threads['api_service'] = api_thread
            
            # 等待API服务启动
            time.sleep(3)
            
            # 测试API服务
            try:
                import requests
                response = requests.get(f'http://{config.API_HOST}:{config.API_PORT}/api/health', timeout=5)
                if response.status_code == 200:
                    logger.info("✅ API服务启动成功")
                else:
                    logger.warning(f"⚠️ API服务响应异常: {response.status_code}")
            except Exception as e:
                logger.warning(f"⚠️ API服务测试失败: {e}")
            
        except Exception as e:
            logger.error(f"❌ API服务启动失败: {e}")
    
    def start_scheduler_service(self):
        """启动调度服务"""
        logger.info("⏰ 启动调度服务...")
        
        try:
            from utils.data_scheduler import scheduler
            
            def scheduler_worker():
                logger.info("📅 数据调度器已启动")
                # 不启动完整调度器，只进行状态检查
                status_report = scheduler.get_status_report()
                logger.info(f"📋 调度器状态: {status_report['scheduler_running']}")
                logger.info(f"📊 配置任务数: {status_report['total_tasks']}")
                
                # 可以在这里添加定期状态检查
                while self.running:
                    time.sleep(300)  # 每5分钟检查一次
                    if not self.running:
                        break
                    
                    try:
                        health = scheduler.health_check()
                        if not health['database_connected']:
                            logger.warning("⚠️ 调度器数据库连接异常")
                    except:
                        pass
            
            scheduler_thread = threading.Thread(target=scheduler_worker, daemon=True)
            scheduler_thread.start()
            self.threads['scheduler'] = scheduler_thread
            
            logger.info("✅ 调度服务启动完成")
            
        except Exception as e:
            logger.error(f"❌ 调度服务启动失败: {e}")
    
    def post_startup_check(self):
        """启动后检查"""
        logger.info("🔍 执行启动后检查...")
        
        # 检查所有线程状态
        active_threads = sum(1 for thread in self.threads.values() if thread.is_alive())
        logger.info(f"📊 活跃线程数: {active_threads}/{len(self.threads)}")
        
        # 执行系统集成测试（简化版）
        try:
            logger.info("🧪 执行快速系统检查...")
            
            # 测试多策略推荐引擎
            from utils.multi_strategy_recommender import multi_strategy_recommender
            test_result = multi_strategy_recommender.analyze_single_stock('000001.SZ')
            if test_result.get('total_score', 0) > 0:
                logger.info("✅ 多策略推荐引擎正常")
            else:
                logger.warning("⚠️ 多策略推荐引擎异常")
            
            # 测试缓存管理器
            from utils.redis_cache_manager import cache_manager
            cache_stats = cache_manager.get_stats()
            logger.info(f"✅ 缓存管理器正常 (命中率: {cache_stats.get('hit_rate', 0)}%)")
            
        except Exception as e:
            logger.warning(f"⚠️ 系统检查异常: {e}")
        
        logger.info("✅ 启动后检查完成")
    
    def print_system_status(self):
        """打印系统状态"""
        startup_duration = (datetime.now() - self.startup_time).total_seconds()
        
        logger.info("=" * 80)
        logger.info("🎉 灵境万象优化系统已就绪")
        logger.info("=" * 80)
        logger.info(f"🕐 启动时间: {startup_duration:.2f}秒")
        logger.info(f"🧵 运行线程: {len([t for t in self.threads.values() if t.is_alive()])}")
        logger.info(f"🌐 API服务: http://localhost:5000")
        logger.info(f"📊 推荐界面: http://localhost:5000/frontend/recommendation_dashboard.html")
        logger.info(f"📈 实时界面: http://localhost:5000/frontend/realtime_dashboard.html")
        logger.info("=" * 80)
        logger.info("🔧 可用功能:")
        logger.info("  • 多策略智能推荐")
        logger.info("  • 实时数据监控")
        logger.info("  • 系统健康检查")
        logger.info("  • Redis缓存加速")
        logger.info("  • 数据自动调度")
        logger.info("  • 高级技术指标")
        logger.info("=" * 80)
        logger.info("💡 使用提示:")
        logger.info("  • 按 Ctrl+C 优雅关闭系统")
        logger.info("  • 查看 logs/ 目录获取详细日志")
        logger.info("  • 运行 python test_system_integration.py 进行完整测试")
        logger.info("=" * 80)
    
    def keep_running(self):
        """保持系统运行"""
        try:
            while self.running:
                time.sleep(1)
                
                # 检查关键线程状态
                dead_threads = [name for name, thread in self.threads.items() if not thread.is_alive()]
                if dead_threads:
                    logger.warning(f"⚠️ 检测到线程异常退出: {dead_threads}")
                
        except KeyboardInterrupt:
            logger.info("🛑 收到中断信号，开始关闭系统...")
            self.stop_all_services()
    
    def stop_all_services(self):
        """停止所有服务"""
        logger.info("🛑 停止所有服务...")
        self.running = False
        
        # 停止系统监控
        try:
            from utils.system_monitor import system_monitor
            system_monitor.stop_monitoring()
            logger.info("✅ 系统监控已停止")
        except Exception as e:
            logger.error(f"❌ 停止系统监控失败: {e}")
        
        # 等待线程结束
        for name, thread in self.threads.items():
            if thread.is_alive():
                logger.info(f"⏳ 等待线程结束: {name}")
                thread.join(timeout=5)
        
        # 停止进程
        for name, process in self.processes.items():
            try:
                process.terminate()
                process.wait(timeout=5)
                logger.info(f"✅ 进程已停止: {name}")
            except Exception as e:
                logger.error(f"❌ 停止进程失败 {name}: {e}")
        
        logger.info("✅ 所有服务已停止")
    
    def cleanup(self):
        """清理资源"""
        if self.running:
            self.stop_all_services()

def main():
    """主函数"""
    try:
        # 确保日志目录存在
        os.makedirs('logs', exist_ok=True)
        
        # 创建系统管理器并启动
        manager = OptimizedSystemManager()
        manager.start_system()
        
    except KeyboardInterrupt:
        logger.info("🛑 系统被用户中断")
        sys.exit(0)
    except Exception as e:
        logger.error(f"❌ 系统启动异常: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main()