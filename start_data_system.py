#!/usr/bin/env python3
"""
启动完整的数据同步和调度系统
"""

import subprocess
import sys
import os
import time
import signal
import logging
from pathlib import Path

# 添加项目路径
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

class DataSystemManager:
    """数据系统管理器"""
    
    def __init__(self):
        self.processes = {}
        self.is_running = True
        
    def start_full_system(self):
        """启动完整系统"""
        logger.info("🚀 启动股票数据系统")
        
        try:
            # 1. 首先进行快速数据同步
            logger.info("📊 执行初始数据同步...")
            self._run_quick_sync()
            
            # 2. 启动定时调度器
            logger.info("⏰ 启动定时调度器...")
            self._start_scheduler()
            
            # 3. 启动API服务器
            logger.info("🌐 启动API服务器...")
            self._start_api_server()
            
            # 4. 保持运行并监控
            self._monitor_system()
            
        except KeyboardInterrupt:
            logger.info("收到停止信号")
            self._stop_all_services()
        except Exception as e:
            logger.error(f"系统启动失败: {e}")
            self._stop_all_services()
    
    def _run_quick_sync(self):
        """运行快速数据同步"""
        try:
            project_root = Path(__file__).parent.parent
            sync_script = project_root / "scheduler" / "quick_data_sync.py"
            
            # 运行数据同步（同步最近7天数据以快速启动）
            result = subprocess.run([
                sys.executable, str(sync_script), '--days', '7'
            ], capture_output=True, text=True, timeout=600)
            
            if result.returncode == 0:
                logger.info("✅ 初始数据同步完成")
            else:
                logger.error(f"❌ 数据同步失败: {result.stderr}")
                
        except subprocess.TimeoutExpired:
            logger.warning("⚠️ 数据同步超时，继续启动系统")
        except Exception as e:
            logger.error(f"❌ 数据同步异常: {e}")
    
    def _start_scheduler(self):
        """启动定时调度器"""
        try:
            project_root = Path(__file__).parent.parent
            scheduler_script = project_root / "scheduler" / "stock_data_scheduler.py"
            
            # 在后台启动调度器
            process = subprocess.Popen([
                sys.executable, str(scheduler_script)
            ], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            
            self.processes['scheduler'] = process
            logger.info(f"✅ 调度器已启动 (PID: {process.pid})")
            
        except Exception as e:
            logger.error(f"❌ 启动调度器失败: {e}")
    
    def _start_api_server(self):
        """启动API服务器"""
        try:
            project_root = Path(__file__).parent.parent
            api_script = project_root / "api" / "app.py"
            
            # 在后台启动API服务器
            process = subprocess.Popen([
                sys.executable, str(api_script)
            ], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            
            self.processes['api'] = process
            logger.info(f"✅ API服务器已启动 (PID: {process.pid})")
            
        except Exception as e:
            logger.error(f"❌ 启动API服务器失败: {e}")
    
    def _monitor_system(self):
        """监控系统状态"""
        logger.info("📱 系统监控启动，按 Ctrl+C 停止")
        
        try:
            while self.is_running:
                # 检查各进程状态
                for name, process in self.processes.items():
                    if process.poll() is not None:
                        logger.error(f"❌ {name} 进程异常退出 (返回码: {process.returncode})")
                        # 可以在这里实现自动重启逻辑
                
                time.sleep(10)  # 每10秒检查一次
                
        except KeyboardInterrupt:
            self.is_running = False
    
    def _stop_all_services(self):
        """停止所有服务"""
        logger.info("🛑 正在停止所有服务...")
        
        for name, process in self.processes.items():
            try:
                logger.info(f"停止 {name}...")
                process.terminate()
                
                # 等待进程正常退出
                try:
                    process.wait(timeout=5)
                    logger.info(f"✅ {name} 已停止")
                except subprocess.TimeoutExpired:
                    # 强制杀死进程
                    process.kill()
                    logger.warning(f"⚠️ {name} 被强制停止")
                    
            except Exception as e:
                logger.error(f"❌ 停止 {name} 失败: {e}")
        
        logger.info("🔚 所有服务已停止")

def quick_start():
    """快速启动（仅同步数据）"""
    logger.info("🚀 快速启动模式 - 仅同步最新数据")
    
    try:
        project_root = Path(__file__).parent.parent
        sync_script = project_root / "scheduler" / "quick_data_sync.py"
        
        # 运行快速同步
        subprocess.run([sys.executable, str(sync_script), '--days', '7'], check=True)
        logger.info("✅ 数据同步完成")
        
        # 显示数据概览
        subprocess.run([sys.executable, str(sync_script), '--summary'], check=True)
        
    except subprocess.CalledProcessError as e:
        logger.error(f"❌ 快速启动失败: {e}")
    except Exception as e:
        logger.error(f"❌ 快速启动异常: {e}")

def main():
    """主函数"""
    import argparse
    
    parser = argparse.ArgumentParser(description='股票数据系统管理器')
    parser.add_argument('--mode', choices=['full', 'quick'], default='full', 
                       help='启动模式: full=完整系统, quick=仅同步数据')
    parser.add_argument('--sync-only', action='store_true', 
                       help='仅执行数据同步，不启动服务')
    
    args = parser.parse_args()
    
    if args.sync_only or args.mode == 'quick':
        quick_start()
    else:
        manager = DataSystemManager()
        manager.start_full_system()

if __name__ == "__main__":
    main()