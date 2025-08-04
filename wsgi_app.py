#!/usr/bin/env python3
"""
WSGI入口点 - 用于生产环境部署
"""

import os
import sys
from dotenv import load_dotenv

# 加载环境变量
load_dotenv()

# 添加项目路径
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

# 设置环境变量
os.environ['FLASK_ENV'] = 'production'

from unified_app import UnifiedStockApp

# 创建应用实例
app_instance = UnifiedStockApp()
application = app_instance.app

# 为了支持SocketIO，我们需要使用socketio应用包装器
socketio_app = app_instance.socketio

if __name__ == "__main__":
    # 如果直接运行此文件，使用SocketIO的run方法
    print("🚀 ljwx-stock 服务器启动中...")
    print("🌐 服务器地址: http://localhost:5005")
    print("🛑 按 Ctrl+C 停止服务")
    print("=" * 50)
    
    try:
        socketio_app.run(application, host='0.0.0.0', port=5005, debug=False, allow_unsafe_werkzeug=True)
    except KeyboardInterrupt:
        print("\n👋 服务已停止")
    except Exception as e:
        print(f"❌ 启动失败: {e}")