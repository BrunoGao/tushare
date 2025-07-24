#!/usr/bin/env python3
import asyncio,websockets,json,sys

async def test_websocket():
    """测试WebSocket连接和订阅功能"""
    uri="ws://localhost:8765"
    
    try:
        print("🔌 正在连接WebSocket服务器...")
        async with websockets.connect(uri) as websocket:
            print("✅ WebSocket连接成功!")
            
            # 发送订阅请求
            subscribe_msg={"type":"subscribe","code":"000001.SZ"}
            await websocket.send(json.dumps(subscribe_msg))
            print(f"📤 发送订阅请求: {subscribe_msg}")
            
            # 接收3条消息
            for i in range(3):
                try:
                    response=await asyncio.wait_for(websocket.recv(),timeout=5)
                    data=json.loads(response)
                    print(f"📥 收到消息 {i+1}: {data.get('type','unknown')}")
                    
                    if data.get('type')=='realtime_data':
                        price_data=data.get('data',{}).get('price',{})
                        print(f"   💰 股价: {price_data.get('current','N/A')}")
                        print(f"   📊 涨跌: {price_data.get('change','N/A')}")
                    
                except asyncio.TimeoutError:
                    print(f"⏰ 等待消息 {i+1} 超时")
                    
            print("✅ WebSocket功能测试完成!")
            
    except Exception as e:
        print(f"❌ WebSocket连接失败: {e}")
        return False
        
    return True

if __name__=="__main__":
    success=asyncio.run(test_websocket())
    sys.exit(0 if success else 1) 