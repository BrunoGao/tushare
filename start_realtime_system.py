#!/usr/bin/env python3
import threading
import time
import logging
import subprocess
import sys
import os
import signal

# 配置日志
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)

def print_banner():
    print("""
🚀 领京万象股票分析系统 - 实时版本
====================================
WebSocket实时推送 + Flask API + 专业前端
支持技术指标分析、AI智能问答、实时数据监控
====================================
""")

class RealtimeSystemLauncher:
    """实时系统启动器"""
    
    def __init__(self):
        self.processes = []
        self.running = True
        
    def start_websocket_server(self):
        """启动WebSocket服务器"""
        try:
            logger.info("🚀 启动WebSocket服务器...")
            process = subprocess.Popen([
                sys.executable, 
                "api/websocket_server.py"
            ], stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
            self.processes.append(("WebSocket服务器", process))
            logger.info("✅ WebSocket服务器启动成功")
        except Exception as e:
            logger.error(f"❌ WebSocket服务器启动失败: {e}")
            
    def start_flask_api(self):
        """启动Flask API服务器"""
        try:
            logger.info("🚀 启动Flask API服务器...")
            process = subprocess.Popen([
                sys.executable, 
                "api/app.py"
            ], stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
            self.processes.append(("Flask API", process))
            logger.info("✅ Flask API服务器启动成功")
        except Exception as e:
            logger.error(f"❌ Flask API服务器启动失败: {e}")
            
    def check_dependencies(self):
        """检查依赖是否安装"""
        required_packages = [
            'websockets', 'tushare', 'pandas', 'numpy', 
            'flask', 'sqlalchemy', 'pymysql', 'openai'
        ]
        
        missing_packages = []
        for package in required_packages:
            try:
                __import__(package)
            except ImportError:
                missing_packages.append(package)
                
        if missing_packages:
            logger.error(f"❌ 缺少依赖包: {', '.join(missing_packages)}")
            logger.info("请运行: pip install " + " ".join(missing_packages))
            return False
            
        logger.info("✅ 所有依赖包检查通过")
        return True
        
    def check_database_connection(self):
        """检查数据库连接"""
        try:
            from utils.db_helper import db
            from sqlalchemy import text
            with db.engine.connect() as conn:
                conn.execute(text("SELECT 1"))
            logger.info("✅ 数据库连接正常")
            return True
        except Exception as e:
            logger.error(f"❌ 数据库连接失败: {e}")
            return False
            
    def setup_database(self):
        """初始化数据库"""
        try:
            from utils.db_helper import db
            logger.info("🔧 初始化数据库表结构...")
            db.create_tables()
            logger.info("✅ 数据库初始化完成")
            return True
        except Exception as e:
            logger.error(f"❌ 数据库初始化失败: {e}")
            return False
            
    def monitor_processes(self):
        """监控进程状态"""
        while self.running:
            for name, process in self.processes:
                if process.poll() is not None:
                    logger.error(f"❌ {name} 进程异常退出，返回码: {process.returncode}")
                    # 这里可以添加重启逻辑
                    
            time.sleep(5)
            
    def signal_handler(self, signum, frame):
        """信号处理"""
        logger.info("🛑 接收到停止信号，正在关闭系统...")
        self.stop_all()
        
    def stop_all(self):
        """停止所有进程"""
        self.running = False
        
        for name, process in self.processes:
            try:
                logger.info(f"🛑 正在停止 {name}...")
                process.terminate()
                process.wait(timeout=5)
                logger.info(f"✅ {name} 已停止")
            except subprocess.TimeoutExpired:
                logger.warning(f"⚠️ {name} 强制终止")
                process.kill()
            except Exception as e:
                logger.error(f"❌ 停止 {name} 失败: {e}")
                
        sys.exit(0)
        
    def start_system(self):
        """启动整个系统"""
        print_banner()
        
        # 检查依赖
        if not self.check_dependencies():
            return False
            
        # 检查数据库
        if not self.check_database_connection():
            logger.warning("⚠️ 数据库连接失败，请检查配置")
            return False
            
        # 初始化数据库
        if not self.setup_database():
            return False
            
        # 注册信号处理
        signal.signal(signal.SIGINT, self.signal_handler)
        signal.signal(signal.SIGTERM, self.signal_handler)
        
        try:
            # 启动WebSocket服务器
            self.start_websocket_server()
            time.sleep(2)
            
            # 启动Flask API
            self.start_flask_api()
            time.sleep(2)
            
            logger.info("🎉 系统启动完成！")
            logger.info("📊 WebSocket服务器: ws://localhost:8765")
            logger.info("🌐 API服务器: http://localhost:5005")
            logger.info("🎯 前端界面: http://localhost:5005/frontend/realtime_dashboard.html")
            logger.info("📋 按 Ctrl+C 停止系统")
            
            # 启动进程监控
            monitor_thread = threading.Thread(target=self.monitor_processes, daemon=True)
            monitor_thread.start()
            
            # 保持主进程运行
            while self.running:
                time.sleep(1)
                
        except KeyboardInterrupt:
            logger.info("🛑 用户中断，正在关闭系统...")
            self.stop_all()
        except Exception as e:
            logger.error(f"❌ 系统启动失败: {e}")
            self.stop_all()
            return False
            
        return True

def main():
    """主函数"""
    launcher = RealtimeSystemLauncher()
    
    if len(sys.argv) > 1:
        command = sys.argv[1]
        
        if command == "deps":
            # 安装依赖
            logger.info("📦 安装系统依赖...")
            subprocess.run([sys.executable, "-m", "pip", "install", "-r", "requirements.txt"])
            
        elif command == "init":
            # 初始化系统
            launcher.check_dependencies()
            launcher.setup_database()
            
        elif command == "test":
            # 测试连接
            launcher.check_dependencies()
            launcher.check_database_connection()
            
        else:
            print("用法: python start_realtime_system.py [deps|init|test]")
            print("  deps - 安装依赖")
            print("  init - 初始化系统")
            print("  test - 测试连接")
            
    else:
        # 启动系统
        launcher.start_system()

if __name__ == "__main__":
    main() 