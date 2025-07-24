#!/usr/bin/env python3
import asyncio,logging
from websocket.realtime_server import RealtimeDataServer
from frontend.cache_optimizer import cache_optimizer
import config

def main():
    """只启动WebSocket服务器"""
    print("🔌 启动WebSocket实时服务器...")
    
    # 配置日志
    logging.basicConfig(level=logging.INFO,format='%(asctime)s - %(name)s - %(levelname)s - %(message)s')
    
    # 启动缓存优化器
    cache_optimizer.start_preload_worker()
    
    # 创建WebSocket服务器
    server=RealtimeDataServer()
    
    print(f"✅ WebSocket服务器启动: ws://{config.WS_HOST}:{config.WS_PORT}")
    print("📊 Flask API应该已在另一个终端运行: http://localhost:5005")
    print("="*60)
    
    # 启动服务器
    async def start_all():
        start_server=server.start_server(config.WS_HOST,config.WS_PORT)
        await start_server
        # 启动广播任务
        asyncio.create_task(server.broadcast_updates())
        
    loop=asyncio.get_event_loop()
    loop.run_until_complete(start_all())
    
    try:
        loop.run_forever()
    except KeyboardInterrupt:
        print("\n👋 WebSocket服务器已停止")

if __name__=="__main__":
    main() 