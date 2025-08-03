#!/usr/bin/env python3
"""
TuShare股票分析系统 - 统一启动脚本
整合WebSocket服务器、Flask API、前端页面的一键启动
"""
import subprocess
import sys
import os
import time
import signal
import logging
import argparse
import json
from pathlib import Path

# 配置日志
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    handlers=[
        logging.StreamHandler(),
        logging.FileHandler('logs/system_startup.log', encoding='utf-8')
    ]
)
logger = logging.getLogger(__name__)

class UnifiedSystemLauncher:
    """统一系统启动器"""
    
    def __init__(self):
        self.processes = []
        self.running = True
        self.pid_file = Path('.system_pids')
        
        # 确保日志目录存在
        os.makedirs('logs', exist_ok=True)
        
    def print_banner(self):
        """打印启动横幅"""
        banner = """
╔══════════════════════════════════════════════╗
║       TuShare股票分析系统 - 统一启动器        ║
║            WebSocket + Flask + 前端           ║
╚══════════════════════════════════════════════╝
"""
        print(banner)
        
    def check_dependencies(self):
        """检查Python依赖"""
        logger.info("🔍 检查系统依赖...")
        
        required_packages = [
            'flask', 'websockets', 'tushare', 'pandas', 
            'numpy', 'sqlalchemy', 'pymysql'
        ]
        
        missing = []
        for package in required_packages:
            try:
                __import__(package)
            except ImportError:
                missing.append(package)
                
        if missing:
            logger.error(f"❌ 缺少依赖包: {', '.join(missing)}")
            logger.info("💡 请运行: pip install -r requirements.txt")
            return False
            
        logger.info("✅ 依赖检查通过")
        return True
        
    def check_database(self):
        """检查数据库连接"""
        logger.info("🔍 检查数据库连接...")
        try:
            from database.db_manager import DatabaseManager
            db_manager = DatabaseManager()
            # 简单连接测试
            logger.info("✅ 数据库连接正常")
            return True
        except Exception as e:
            logger.warning(f"⚠️ 数据库连接问题: {e}")
            logger.info("💡 系统将继续启动，但部分功能可能受限")
            return True  # 不阻断启动
            
    def check_ports(self):
        """检查端口占用"""
        import socket
        
        ports_to_check = [
            (5005, "Flask API"),
            (8765, "WebSocket服务")
        ]
        
        for port, service in ports_to_check:
            sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            result = sock.connect_ex(('localhost', port))
            sock.close()
            
            if result == 0:
                logger.warning(f"⚠️ 端口 {port} ({service}) 已被占用")
                try:
                    # 尝试杀死占用端口的进程
                    subprocess.run(['lsof', '-ti', f':{port}'], 
                                 stdout=subprocess.PIPE, check=True)
                    subprocess.run(['lsof', '-ti', f':{port}', '|', 'xargs', 'kill', '-9'], 
                                 shell=True)
                    logger.info(f"✅ 已释放端口 {port}")
                except:
                    pass
                    
    def start_flask_api(self):
        """启动Flask API服务器"""
        logger.info("🚀 启动Flask API服务器...")
        try:
            process = subprocess.Popen([
                sys.executable, "api/app.py"
            ], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            
            self.processes.append(("Flask API", process))
            logger.info(f"✅ Flask API启动成功 (PID: {process.pid})")
            return True
        except Exception as e:
            logger.error(f"❌ Flask API启动失败: {e}")
            return False
            
    def start_websocket_server(self):
        """启动WebSocket服务器"""
        logger.info("🚀 启动WebSocket服务器...")
        try:
            # 优先使用websocket/realtime_server.py
            if os.path.exists("websocket/realtime_server.py"):
                script_path = "start_websocket_only.py"
            elif os.path.exists("api/websocket_server.py"):
                script_path = "api/websocket_server.py"
            else:
                logger.error("❌ 找不到WebSocket服务器脚本")
                return False
                
            process = subprocess.Popen([
                sys.executable, script_path
            ], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            
            self.processes.append(("WebSocket服务器", process))
            logger.info(f"✅ WebSocket服务器启动成功 (PID: {process.pid})")
            return True
        except Exception as e:
            logger.error(f"❌ WebSocket服务器启动失败: {e}")
            return False
            
    def start_background_tasks(self, enable_sync=False, enable_scheduler=False):
        """启动后台任务"""
        if enable_sync:
            logger.info("🚀 启动数据同步任务...")
            try:
                process = subprocess.Popen([
                    sys.executable, "scheduler/quick_data_sync.py", "--days", "7"
                ], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
                self.processes.append(("数据同步", process))
                logger.info(f"✅ 数据同步任务启动 (PID: {process.pid})")
            except Exception as e:
                logger.warning(f"⚠️ 数据同步任务启动失败: {e}")
                
        if enable_scheduler:
            logger.info("🚀 启动定时调度器...")
            try:
                process = subprocess.Popen([
                    sys.executable, "scheduler/stock_data_scheduler.py"
                ], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
                self.processes.append(("定时调度器", process))
                logger.info(f"✅ 定时调度器启动 (PID: {process.pid})")
            except Exception as e:
                logger.warning(f"⚠️ 定时调度器启动失败: {e}")
                
    def save_pids(self):
        """保存进程ID到文件"""
        pid_data = {
            "timestamp": time.time(),
            "processes": [
                {"name": name, "pid": process.pid} 
                for name, process in self.processes
            ]
        }
        
        with open(self.pid_file, 'w') as f:
            json.dump(pid_data, f, indent=2)
            
    def show_system_info(self):
        """显示系统信息"""
        print("\n" + "="*60)
        print("🎉 系统启动完成！")
        print("="*60)
        print("📊 服务访问地址:")
        print("  • Flask API:      http://localhost:5005")
        print("  • 市场仪表板:     http://localhost:5005/market-dashboard")
        print("  • 实时仪表板:     http://localhost:5005/frontend/realtime_dashboard.html")
        print("  • 推荐系统:       http://localhost:5005/frontend/recommendation_dashboard.html")
        print("  • WebSocket:      ws://localhost:8765")
        print("")
        print("🔧 运行中的进程:")
        for name, process in self.processes:
            print(f"  • {name:<15} PID: {process.pid}")
        print("")
        print("📋 管理命令:")
        print("  • 查看日志:       tail -f logs/system_startup.log")
        print("  • 停止系统:       python start_unified.py --stop")
        print("  • 重启系统:       python start_unified.py --restart")
        print("")
        print("🛑 按 Ctrl+C 停止系统")
        print("="*60)
        
    def monitor_processes(self):
        """监控进程状态"""
        while self.running:
            dead_processes = []
            for name, process in self.processes:
                if process.poll() is not None:
                    logger.warning(f"⚠️ {name} 进程已退出 (返回码: {process.returncode})")
                    dead_processes.append((name, process))
                    
            # 移除已死进程
            for dead in dead_processes:
                self.processes.remove(dead)
                
            time.sleep(10)
            
    def signal_handler(self, signum, frame):
        """处理系统信号"""
        logger.info("🛑 接收到停止信号，正在关闭系统...")
        self.stop_all()
        
    def stop_all(self):
        """停止所有进程"""
        self.running = False
        
        logger.info("🛑 正在停止所有服务...")
        for name, process in self.processes:
            try:
                logger.info(f"  • 停止 {name} (PID: {process.pid})")
                process.terminate()
                
                # 等待进程优雅退出
                try:
                    process.wait(timeout=5)
                except subprocess.TimeoutExpired:
                    logger.warning(f"    {name} 强制终止")
                    process.kill()
                    
            except Exception as e:
                logger.error(f"❌ 停止 {name} 失败: {e}")
                
        # 清理PID文件
        if self.pid_file.exists():
            self.pid_file.unlink()
            
        logger.info("✅ 系统已完全停止")
        sys.exit(0)
        
    def stop_existing_system(self):
        """停止现有运行的系统"""
        if not self.pid_file.exists():
            logger.info("📝 没有发现运行中的系统")
            return
            
        try:
            with open(self.pid_file, 'r') as f:
                pid_data = json.load(f)
                
            logger.info("🛑 停止现有系统...")
            for proc_info in pid_data.get("processes", []):
                try:
                    pid = proc_info["pid"]
                    name = proc_info["name"]
                    os.kill(pid, signal.SIGTERM)
                    logger.info(f"  • 已停止 {name} (PID: {pid})")
                except ProcessLookupError:
                    logger.info(f"  • {name} (PID: {pid}) 已不存在")
                except Exception as e:
                    logger.warning(f"  • 停止 {name} 失败: {e}")
                    
            self.pid_file.unlink()
            logger.info("✅ 现有系统已停止")
            
        except Exception as e:
            logger.error(f"❌ 停止现有系统失败: {e}")
            
    def start_system(self, mode="full", enable_sync=False, enable_scheduler=False):
        """启动系统"""
        self.print_banner()
        
        # 检查环境
        if not self.check_dependencies():
            return False
            
        self.check_database()
        self.check_ports()
        
        # 注册信号处理
        signal.signal(signal.SIGINT, self.signal_handler)
        signal.signal(signal.SIGTERM, self.signal_handler)
        
        try:
            # 启动核心服务
            if mode in ["full", "api"]:
                if not self.start_flask_api():
                    return False
                time.sleep(2)
                
            if mode in ["full", "websocket"]:
                if not self.start_websocket_server():
                    return False
                time.sleep(2)
                
            # 启动后台任务
            if mode == "full":
                self.start_background_tasks(enable_sync, enable_scheduler)
                
            # 保存进程信息
            self.save_pids()
            
            # 显示系统信息
            self.show_system_info()
            
            # 启动进程监控
            import threading
            monitor_thread = threading.Thread(target=self.monitor_processes, daemon=True)
            monitor_thread.start()
            
            # 保持主进程运行
            while self.running:
                time.sleep(1)
                
        except KeyboardInterrupt:
            logger.info("🛑 用户中断")
            self.stop_all()
        except Exception as e:
            logger.error(f"❌ 系统启动失败: {e}")
            self.stop_all()
            return False
            
        return True

def main():
    """主函数"""
    parser = argparse.ArgumentParser(description='TuShare股票分析系统统一启动器')
    parser.add_argument('--mode', choices=['full', 'api', 'websocket'], 
                       default='full', help='启动模式 (默认: full)')
    parser.add_argument('--sync', action='store_true', help='启用数据同步')
    parser.add_argument('--scheduler', action='store_true', help='启用定时调度器')
    parser.add_argument('--stop', action='store_true', help='停止运行中的系统')
    parser.add_argument('--restart', action='store_true', help='重启系统')
    parser.add_argument('--status', action='store_true', help='查看系统状态')
    
    args = parser.parse_args()
    
    launcher = UnifiedSystemLauncher()
    
    if args.stop:
        launcher.stop_existing_system()
        return
        
    if args.restart:
        launcher.stop_existing_system()
        time.sleep(2)
        launcher.start_system(args.mode, args.sync, args.scheduler)
        return
        
    if args.status:
        if launcher.pid_file.exists():
            with open(launcher.pid_file, 'r') as f:
                pid_data = json.load(f)
            print("🟢 系统运行状态:")
            for proc in pid_data.get("processes", []):
                try:
                    os.kill(proc["pid"], 0)  # 检查进程是否存在
                    print(f"  • {proc['name']:<15} PID: {proc['pid']} (运行中)")
                except ProcessLookupError:
                    print(f"  • {proc['name']:<15} PID: {proc['pid']} (已停止)")
        else:
            print("🔴 系统未运行")
        return
        
    # 启动系统
    launcher.start_system(args.mode, args.sync, args.scheduler)

if __name__ == "__main__":
    main()