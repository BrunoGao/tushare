import asyncio,websockets,json,logging,threading,time,sys,os
from datetime import datetime,timedelta
from typing import Dict,Set,Any

# 添加项目根目录到路径
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from database.db_manager import DatabaseManager
try:
    from analysis.technical_indicators import TechnicalIndicators
except ImportError:
    # 如果talib有问题，使用简化版技术指标
    print("⚠️ WebSocket使用简化版技术指标（talib不可用）")
    class TechnicalIndicators:
        def calculate_all_indicators(self, df):
            # 简化版技术指标计算
            df['ma5'] = df['close'].rolling(5).mean()
            df['ma20'] = df['close'].rolling(20).mean()
            df['ma60'] = df['close'].rolling(60).mean()
            df['rsi'] = 50  # 简化RSI
            df['macd'] = 0  # 简化MACD
            df['macd_signal'] = 0
            df['kdj_k'] = 50  # 简化KDJ
            df['kdj_d'] = 50
            return df
            
from frontend.cache_optimizer import cache_optimizer
from config import Config

class RealtimeDataServer:
    def __init__(self):
        self.db=DatabaseManager()
        self.tech_calc=TechnicalIndicators()
        self.clients:Set[websockets.WebSocketServerProtocol]=set()  #连接的客户端
        self.subscriptions:Dict[str,Set[websockets.WebSocketServerProtocol]]={}  #订阅关系
        self.data_cache:Dict[str,Any]={}  #数据缓存
        self.last_update:Dict[str,datetime]={}  #上次更新时间
        self.logger=logging.getLogger(__name__)
        
    async def register(self,websocket,path=None):
        """注册新客户端连接"""
        self.clients.add(websocket)
        self.logger.info(f"客户端连接: {websocket.remote_address}")
        try:
            await self.handle_client(websocket)
        except websockets.exceptions.ConnectionClosed:
            pass
        finally:
            self.clients.discard(websocket)
            self.unsubscribe_all(websocket)
            
    async def handle_client(self,websocket):
        """处理客户端消息"""
        async for message in websocket:
            try:
                data=json.loads(message)
                await self.process_message(websocket,data)
            except Exception as e:
                self.logger.error(f"处理消息错误: {e}")
                await websocket.send(json.dumps({"error":str(e)}))
                
    async def process_message(self,websocket,data):
        """处理具体消息类型"""
        msg_type=data.get("type")
        if msg_type=="subscribe":
            await self.subscribe_stock(websocket,data.get("code"))
        elif msg_type=="unsubscribe":
            await self.unsubscribe_stock(websocket,data.get("code"))
        elif msg_type=="get_realtime":
            await self.send_realtime_data(websocket,data.get("code"))
            
    async def subscribe_stock(self,websocket,stock_code):
        """订阅股票实时数据"""
        if stock_code not in self.subscriptions:
            self.subscriptions[stock_code]=set()
        self.subscriptions[stock_code].add(websocket)
        await websocket.send(json.dumps({"type":"subscribed","code":stock_code}))
        await self.send_realtime_data(websocket,stock_code)  #立即发送当前数据
        
    async def unsubscribe_stock(self,websocket,stock_code):
        """取消订阅"""
        if stock_code in self.subscriptions:
            self.subscriptions[stock_code].discard(websocket)
            
    def unsubscribe_all(self,websocket):
        """取消所有订阅"""
        for subscribers in self.subscriptions.values():
            subscribers.discard(websocket)
            
    async def send_realtime_data(self,websocket,stock_code):
        """发送实时数据"""
        try:
            data=await self.get_latest_data(stock_code)
            await websocket.send(json.dumps({
                "type":"realtime_data",
                "code":stock_code,
                "data":data,
                "timestamp":datetime.now().isoformat()
            }))
        except Exception as e:
            self.logger.error(f"发送实时数据错误: {e}")
            
    async def get_latest_data(self,stock_code):
        """获取最新数据(含技术指标) - 使用缓存优化器"""
        cache_key=f"{stock_code}_latest"
        now=datetime.now()
        
        #优先使用缓存优化器
        cached_data=cache_optimizer.get_fast_data(stock_code)
        if cached_data and 'realtime_data' in cached_data:
            return cached_data['realtime_data']
        
        #检查本地缓存(30秒内有效)
        if cache_key in self.data_cache and cache_key in self.last_update:
            if now-self.last_update[cache_key]<timedelta(seconds=30):
                return self.data_cache[cache_key]
                
        #获取最新数据
        with self.db.engine.connect() as conn:
            #获取最新价格数据
            price_sql=f"""
            SELECT trade_date,open,high,low,close,vol,amount 
            FROM daily_data WHERE ts_code='{stock_code}' 
            ORDER BY trade_date DESC LIMIT 30
            """
            df=self.db.fetch_data(price_sql)
            
            if df.empty:
                return {"error":"无数据"}
                
            #计算技术指标
            df=self.tech_calc.calculate_all_indicators(df)
            latest=df.iloc[0]
            
            #构建返回数据
            result={
                "price":{
                    "current":float(latest['close']),
                    "open":float(latest['open']),
                    "high":float(latest['high']),
                    "low":float(latest['low']),
                    "volume":int(latest['vol']),
                    "change":float(latest['close']-latest['open']),
                    "change_pct":float((latest['close']-latest['open'])/latest['open']*100)
                },
                "indicators":{
                    "ma5":float(latest.get('ma5',0)),
                    "ma20":float(latest.get('ma20',0)),
                    "ma60":float(latest.get('ma60',0)),
                    "rsi":float(latest.get('rsi',0)),
                    "macd":float(latest.get('macd',0)),
                    "macd_signal":float(latest.get('macd_signal',0)),
                    "kdj_k":float(latest.get('kdj_k',0)),
                    "kdj_d":float(latest.get('kdj_d',0))
                },
                "trend_data":[{  #最近10天趋势
                    "date":row['trade_date'],
                    "close":float(row['close']),
                    "ma5":float(row.get('ma5',0)),
                    "ma20":float(row.get('ma20',0))
                } for _,row in df.head(10).iterrows()]
            }
            
            #更新缓存
            self.data_cache[cache_key]=result
            self.last_update[cache_key]=now
            return result
            
    async def broadcast_updates(self):
        """定期广播更新给所有订阅者"""
        while True:
            try:
                for stock_code,subscribers in self.subscriptions.items():
                    if subscribers:
                        data=await self.get_latest_data(stock_code)
                        message=json.dumps({
                            "type":"realtime_update",
                            "code":stock_code,
                            "data":data,
                            "timestamp":datetime.now().isoformat()
                        })
                        #并发发送给所有订阅者
                        await asyncio.gather(*[
                            self.safe_send(ws,message) for ws in subscribers.copy()
                        ],return_exceptions=True)
                await asyncio.sleep(10)  # 10秒更新间隔
            except Exception as e:
                self.logger.error(f"广播更新错误: {e}")
                await asyncio.sleep(5)
                
    async def safe_send(self,websocket,message):
        """安全发送消息"""
        try:
            await websocket.send(message)
        except websockets.exceptions.ConnectionClosed:
            self.clients.discard(websocket)
        except Exception as e:
            self.logger.error(f"发送消息失败: {e}")
            
    def start_server(self,host="0.0.0.0",port=8765):
        """启动WebSocket服务器"""
        logging.basicConfig(level=logging.INFO)
        self.logger.info(f"启动WebSocket服务器 {host}:{port}")
        
        return websockets.serve(self.register,host,port)

if __name__=="__main__":
    async def main():
        server=RealtimeDataServer()
        start_server=server.start_server()
        
        # 启动广播任务
        asyncio.create_task(server.broadcast_updates())
        
        # 启动WebSocket服务器
        await start_server
        
        # 保持运行
        await asyncio.Future()  # run forever
    
    asyncio.run(main())