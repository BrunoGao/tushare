#!/usr/bin/env python3
import asyncio,threading,time,logging
from websocket.realtime_server import RealtimeDataServer
from frontend.cache_optimizer import cache_optimizer
from api.app import app
import config

def start_flask_app():
    """启动Flask API服务器"""
    print("🚀 启动Flask API服务器...")
    app.run(host=config.API_HOST,port=config.API_PORT,debug=False,use_reloader=False)

def start_websocket_server():
    """启动WebSocket服务器"""
    print("🔌 启动WebSocket服务器...")
    server=RealtimeDataServer()
    
    # 设置事件循环
    loop=asyncio.new_event_loop()
    asyncio.set_event_loop(loop)
    
    # 启动服务器
    start_server=server.start_server(config.WS_HOST,config.WS_PORT)
    loop.run_until_complete(start_server)
    
    # 启动广播任务
    loop.create_task(server.broadcast_updates())
    
    print(f"✅ WebSocket服务器已启动: ws://{config.WS_HOST}:{config.WS_PORT}")
    loop.run_forever()

def main():
    """主启动函数"""
    print("="*60)
    print("🚀 领京万象 - 实时股票分析系统")
    print("="*60)
    
    # 配置日志
    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    )
    
    # 启动缓存优化器
    cache_optimizer.start_preload_worker()
    
    # 启动Flask服务器(在单独线程中)
    flask_thread=threading.Thread(target=start_flask_app,daemon=True)
    flask_thread.start()
    
    # 给Flask一点启动时间
    time.sleep(2)
    
    print(f"📊 Web界面: http://localhost:{config.API_PORT}")
    print(f"🔌 WebSocket: ws://localhost:{config.WS_PORT}")
    print("="*60)
    
    # 启动WebSocket服务器(主线程)
    try:
        start_websocket_server()
    except KeyboardInterrupt:
        print("\n👋 服务器已停止")

if __name__=="__main__":
    main() 