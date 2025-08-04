#!/usr/bin/env python3
"""
服务器控制脚本 - 启动、停止、重启Flask应用
"""

import os
import sys
import signal
import subprocess
import time
from pathlib import Path

def get_pid_file():
    """获取PID文件路径"""
    return Path('server.pid')

def is_running():
    """检查服务器是否正在运行"""
    pid_file = get_pid_file()
    if not pid_file.exists():
        return False
    
    try:
        with open(pid_file, 'r') as f:
            pid = int(f.read().strip())
        
        # 检查进程是否存在
        os.kill(pid, 0)
        return True
    except (OSError, ValueError):
        # 进程不存在，删除无效的PID文件
        if pid_file.exists():
            pid_file.unlink()
        return False

def start_server():
    """启动服务器"""
    if is_running():
        print("⚠️  服务器已在运行中")
        return
    
    print("🚀 启动ljwx-stock服务器...")
    
    # 使用更简单的单进程模式
    cmd = [
        'python', 'wsgi_app.py'
    ]
    
    try:
        # 后台启动
        process = subprocess.Popen(
            cmd,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            preexec_fn=os.setsid
        )
        
        # 保存PID
        with open(get_pid_file(), 'w') as f:
            f.write(str(process.pid))
        
        print(f"✅ 服务器已启动 (PID: {process.pid})")
        print("🌐 访问地址: http://localhost:5005")
        print("🛑 停止服务: python server_control.py stop")
        
    except Exception as e:
        print(f"❌ 启动失败: {e}")

def stop_server():
    """停止服务器"""
    if not is_running():
        print("⚠️  服务器未运行")
        return
    
    pid_file = get_pid_file()
    try:
        with open(pid_file, 'r') as f:
            pid = int(f.read().strip())
        
        print(f"🛑 停止服务器 (PID: {pid})...")
        
        # 发送SIGTERM信号
        os.killpg(os.getpgid(pid), signal.SIGTERM)
        
        # 等待进程结束
        for i in range(10):
            try:
                os.kill(pid, 0)
                time.sleep(0.5)
            except OSError:
                break
        else:
            # 如果进程仍然存在，强制杀死
            print("强制停止...")
            os.killpg(os.getpgid(pid), signal.SIGKILL)
        
        # 删除PID文件
        if pid_file.exists():
            pid_file.unlink()
        
        print("✅ 服务器已停止")
        
    except Exception as e:
        print(f"❌ 停止失败: {e}")
        # 清理PID文件
        if pid_file.exists():
            pid_file.unlink()

def restart_server():
    """重启服务器"""
    print("🔄 重启服务器...")
    stop_server()
    time.sleep(1)
    start_server()

def status_server():
    """显示服务器状态"""
    if is_running():
        pid_file = get_pid_file()
        with open(pid_file, 'r') as f:
            pid = int(f.read().strip())
        print(f"✅ 服务器正在运行 (PID: {pid})")
        print("🌐 访问地址: http://localhost:5005")
    else:
        print("❌ 服务器未运行")

def main():
    """主函数"""
    if len(sys.argv) < 2:
        print("📋 使用方法:")
        print("  python server_control.py start   - 启动服务器")
        print("  python server_control.py stop    - 停止服务器")
        print("  python server_control.py restart - 重启服务器")
        print("  python server_control.py status  - 查看状态")
        return
    
    command = sys.argv[1].lower()
    
    if command == 'start':
        start_server()
    elif command == 'stop':
        stop_server()
    elif command == 'restart':
        restart_server()
    elif command == 'status':
        status_server()
    else:
        print(f"❌ 未知命令: {command}")
        print("支持的命令: start, stop, restart, status")

if __name__ == '__main__':
    main()