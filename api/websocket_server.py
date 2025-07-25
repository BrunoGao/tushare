import asyncio
import websockets
import json
import logging
import threading
import time
from datetime import datetime, timedelta
from typing import Dict, Set, Any, List
import sys, os
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import config
from utils.db_helper import db
from utils.technical_indicators import TechnicalIndicators
from utils.llm_analyzer import LLMAnalyzer
import tushare as ts

logger = logging.getLogger(__name__)

class WebSocketServer:
    """WebSocket实时数据服务器"""
    
    def __init__(self):
        self.clients: Set[websockets.WebSocketServerProtocol] = set()  # 连接的客户端
        self.subscriptions: Dict[str, Set[websockets.WebSocketServerProtocol]] = {}  # 订阅关系
        self.data_cache: Dict[str, Any] = {}  # 数据缓存
        self.last_update: Dict[str, datetime] = {}  # 上次更新时间
        self.tech_analyzer = TechnicalIndicators()
        self.llm_analyzer = LLMAnalyzer()
        
        # 初始化TuShare
        ts.set_token(config.TS_TOKEN)
        self.pro = ts.pro_api()
        
    async def register_client(self, websocket, path=None):
        """注册新客户端连接"""
        self.clients.add(websocket)
        remote_addr = websocket.remote_address if hasattr(websocket, 'remote_address') else 'unknown'
        logger.info(f"🔌 客户端连接: {remote_addr}, 总连接数: {len(self.clients)}")
        
        try:
            # 发送欢迎消息
            await websocket.send(json.dumps({
                "type": "welcome",
                "message": "WebSocket连接成功",
                "timestamp": datetime.now().isoformat(),
                "server_info": {
                    "name": "LJWX股票分析系统",
                    "version": "2.0",
                    "features": ["实时数据", "技术分析", "AI问答"]
                }
            }))
            
            await self.handle_client_messages(websocket)
            
        except websockets.exceptions.ConnectionClosed:
            logger.info(f"📞 客户端断开连接: {remote_addr}")
        except Exception as e:
            logger.error(f"❌ 处理客户端连接异常: {e}")
        finally:
            self.clients.discard(websocket)
            self.unsubscribe_all(websocket)
            
    async def handle_client_messages(self, websocket):
        """处理客户端消息"""
        async for message in websocket:
            try:
                data = json.loads(message)
                await self.process_message(websocket, data)
            except json.JSONDecodeError:
                await self.send_error(websocket, "消息格式错误")
            except Exception as e:
                logger.error(f"❌ 处理消息异常: {e}")
                await self.send_error(websocket, str(e))
                
    async def process_message(self, websocket, data: Dict):
        """处理不同类型的消息"""
        message_type = data.get('type')
        
        if message_type == 'subscribe':
            await self.handle_subscribe(websocket, data)
        elif message_type == 'unsubscribe':
            await self.handle_unsubscribe(websocket, data)
        elif message_type == 'get_realtime':
            await self.handle_get_realtime(websocket, data)
        elif message_type == 'get_technical':
            await self.handle_get_technical(websocket, data)
        elif message_type == 'ai_query':
            await self.handle_ai_query(websocket, data)
        elif message_type == 'ping':
            await websocket.send(json.dumps({"type": "pong", "timestamp": datetime.now().isoformat()}))
        else:
            await self.send_error(websocket, f"未知消息类型: {message_type}")
            
    async def handle_subscribe(self, websocket, data: Dict):
        """处理订阅请求"""
        try:
            stock_codes = data.get('stock_codes', [])
            if not stock_codes:
                await self.send_error(websocket, "请提供股票代码")
                return
                
            for code in stock_codes:
                if code not in self.subscriptions:
                    self.subscriptions[code] = set()
                self.subscriptions[code].add(websocket)
                
            await websocket.send(json.dumps({
                "type": "subscribe_success",
                "stock_codes": stock_codes,
                "message": f"成功订阅 {len(stock_codes)} 只股票",
                "timestamp": datetime.now().isoformat()
            }))
            
            # 立即发送一次数据
            for code in stock_codes:
                await self.send_stock_data(code)
                
        except Exception as e:
            await self.send_error(websocket, f"订阅失败: {e}")
            
    async def handle_unsubscribe(self, websocket, data: Dict):
        """处理取消订阅"""
        try:
            stock_codes = data.get('stock_codes', [])
            
            for code in stock_codes:
                if code in self.subscriptions and websocket in self.subscriptions[code]:
                    self.subscriptions[code].discard(websocket)
                    if not self.subscriptions[code]:
                        del self.subscriptions[code]
                        
            await websocket.send(json.dumps({
                "type": "unsubscribe_success",
                "stock_codes": stock_codes,
                "timestamp": datetime.now().isoformat()
            }))
            
        except Exception as e:
            await self.send_error(websocket, f"取消订阅失败: {e}")
            
    async def handle_get_realtime(self, websocket, data: Dict):
        """获取实时数据"""
        try:
            ts_code = data.get('ts_code')
            if not ts_code:
                await self.send_error(websocket, "请提供股票代码")
                return
                
            realtime_data = await self.get_realtime_data(ts_code)
            
            await websocket.send(json.dumps({
                "type": "realtime_data",
                "ts_code": ts_code,
                "data": realtime_data,
                "timestamp": datetime.now().isoformat()
            }))
            
        except Exception as e:
            await self.send_error(websocket, f"获取实时数据失败: {e}")
            
    async def handle_get_technical(self, websocket, data: Dict):
        """获取技术分析数据"""
        try:
            ts_code = data.get('ts_code')
            days = data.get('days', 60)
            
            if not ts_code:
                await self.send_error(websocket, "请提供股票代码")
                return
                
            technical_data = await self.get_technical_analysis(ts_code, days)
            
            await websocket.send(json.dumps({
                "type": "technical_data",
                "ts_code": ts_code,
                "data": technical_data,
                "timestamp": datetime.now().isoformat()
            }))
            
        except Exception as e:
            await self.send_error(websocket, f"获取技术分析失败: {e}")
            
    async def handle_ai_query(self, websocket, data: Dict):
        """处理AI查询"""
        try:
            query = data.get('query')
            ts_code = data.get('ts_code')
            
            if not query:
                await self.send_error(websocket, "请提供查询内容")
                return
                
            # 获取相关数据作为上下文
            context_data = {}
            if ts_code:
                context_data = await self.get_stock_context(ts_code)
                
            # 调用LLM分析
            response = self.llm_analyzer.natural_language_query(query, context_data)
            
            await websocket.send(json.dumps({
                "type": "ai_response",
                "query": query,
                "response": response,
                "ts_code": ts_code,
                "timestamp": datetime.now().isoformat()
            }))
            
        except Exception as e:
            await self.send_error(websocket, f"AI查询失败: {e}")
            
    async def get_realtime_data(self, ts_code: str) -> Dict:
        """获取实时数据"""
        try:
            # 检查缓存
            cache_key = f"realtime_{ts_code}"
            if cache_key in self.data_cache:
                cache_time = self.last_update.get(cache_key, datetime.min)
                if datetime.now() - cache_time < timedelta(seconds=30):  # 30秒缓存
                    return self.data_cache[cache_key]
                    
            # 获取实时数据
            df = self.pro.realtime(ts_code=ts_code)
            if df.empty:
                return {"error": "无数据"}
                
            latest = df.iloc[0]
            
            # 获取股票基本信息
            with db.engine.connect() as conn:
                from sqlalchemy import text
                result = conn.execute(text("SELECT name, industry, sector FROM stock_basic WHERE ts_code = :ts_code"), 
                                    {'ts_code': ts_code})
                stock_info = result.fetchone()
                
            realtime_data = {
                "ts_code": ts_code,
                "name": stock_info[0] if stock_info else ts_code,
                "industry": stock_info[1] if stock_info else "",
                "sector": stock_info[2] if stock_info else "",
                "current_price": float(latest.get('price', 0)),
                "change_amount": float(latest.get('change', 0)),
                "change_pct": float(latest.get('pct_chg', 0)),
                "volume": int(latest.get('vol', 0)),
                "amount": float(latest.get('amount', 0)),
                "turnover_rate": float(latest.get('turnover_rate', 0)),
                "pe_ratio": float(latest.get('pe', 0)),
                "pb_ratio": float(latest.get('pb', 0)),
                "total_mv": float(latest.get('total_mv', 0)),
                "update_time": datetime.now().isoformat()
            }
            
            # 更新缓存
            self.data_cache[cache_key] = realtime_data
            self.last_update[cache_key] = datetime.now()
            
            return realtime_data
            
        except Exception as e:
            logger.error(f"❌ 获取实时数据失败: {e}")
            return {"error": str(e)}
            
    async def get_technical_analysis(self, ts_code: str, days: int = 60) -> Dict:
        """获取技术分析数据"""
        try:
            # 获取历史数据
            end_date = datetime.now().strftime('%Y%m%d')
            start_date = (datetime.now() - timedelta(days=days)).strftime('%Y%m%d')
            df = db.query_stock_data(ts_code, start_date, end_date)
            
            if df.empty:
                return {"error": "无历史数据"}
                
            # 计算技术指标
            df_with_indicators = self.tech_analyzer.calculate_all_indicators(df)
            signals = self.tech_analyzer.generate_signals(df_with_indicators)
            
            # 获取最新数据
            latest = df_with_indicators.iloc[-1]
            latest_signals = signals.iloc[-1] if not signals.empty else {}
            
            def safe_float(val, default=0):
                try:
                    import pandas as pd
                    if pd.isna(val) or val is None:
                        return default
                    return float(val)
                except (ValueError, TypeError):
                    return default
                    
            technical_data = {
                "ts_code": ts_code,
                "trade_date": latest['trade_date'].strftime('%Y-%m-%d'),
                "indicators": {
                    "close": safe_float(latest['close']),
                    "ma5": safe_float(latest.get('ma5')),
                    "ma10": safe_float(latest.get('ma10')),
                    "ma20": safe_float(latest.get('ma20')),
                    "ma60": safe_float(latest.get('ma60')),
                    "ema12": safe_float(latest.get('ema12')),
                    "ema26": safe_float(latest.get('ema26')),
                    "macd": safe_float(latest.get('macd')),
                    "macd_signal": safe_float(latest.get('macd_signal')),
                    "macd_histogram": safe_float(latest.get('macd_histogram')),
                    "rsi": safe_float(latest.get('rsi')),
                    "kdj_k": safe_float(latest.get('kdj_k')),
                    "kdj_d": safe_float(latest.get('kdj_d')),
                    "kdj_j": safe_float(latest.get('kdj_j')),
                    "boll_upper": safe_float(latest.get('boll_upper')),
                    "boll_middle": safe_float(latest.get('boll_middle')),
                    "boll_lower": safe_float(latest.get('boll_lower'))
                },
                "signals": {
                    "ma_golden_cross": int(latest_signals.get('ma_golden_cross', 0)),
                    "ma_death_cross": int(latest_signals.get('ma_death_cross', 0)),
                    "macd_golden": int(latest_signals.get('macd_golden', 0)),
                    "macd_death": int(latest_signals.get('macd_death', 0)),
                    "rsi_overbought": int(latest_signals.get('rsi_overbought', 0)),
                    "rsi_oversold": int(latest_signals.get('rsi_oversold', 0))
                }
            }
            
            return technical_data
            
        except Exception as e:
            logger.error(f"❌ 获取技术分析失败: {e}")
            return {"error": str(e)}
            
    async def get_stock_context(self, ts_code: str) -> Dict:
        """获取股票上下文信息"""
        try:
            context = {}
            
            # 获取基本信息
            with db.engine.connect() as conn:
                from sqlalchemy import text
                result = conn.execute(text("SELECT * FROM stock_basic WHERE ts_code = :ts_code"), 
                                    {'ts_code': ts_code})
                stock_info = result.fetchone()
                if stock_info:
                    context['basic_info'] = dict(stock_info._mapping)
                    
            # 获取技术分析数据
            technical_data = await self.get_technical_analysis(ts_code, 30)
            if 'error' not in technical_data:
                context['technical'] = technical_data
                
            # 获取实时数据
            realtime_data = await self.get_realtime_data(ts_code)
            if 'error' not in realtime_data:
                context['realtime'] = realtime_data
                
            return context
            
        except Exception as e:
            logger.error(f"❌ 获取股票上下文失败: {e}")
            return {}
            
    async def send_stock_data(self, ts_code: str):
        """向订阅者推送股票数据"""
        if ts_code not in self.subscriptions:
            return
            
        try:
            # 获取实时数据和技术分析
            realtime_data = await self.get_realtime_data(ts_code)
            technical_data = await self.get_technical_analysis(ts_code, 30)
            
            message = {
                "type": "stock_update",
                "ts_code": ts_code,
                "realtime": realtime_data,
                "technical": technical_data,
                "timestamp": datetime.now().isoformat()
            }
            
            # 发送给所有订阅者
            disconnected = set()
            for websocket in self.subscriptions[ts_code]:
                try:
                    await websocket.send(json.dumps(message))
                except websockets.exceptions.ConnectionClosed:
                    disconnected.add(websocket)
                except Exception as e:
                    logger.error(f"❌ 发送数据失败: {e}")
                    disconnected.add(websocket)
                    
            # 清理断开的连接
            for websocket in disconnected:
                self.subscriptions[ts_code].discard(websocket)
                
        except Exception as e:
            logger.error(f"❌ 推送股票数据失败: {e}")
            
    async def send_error(self, websocket, error_message: str):
        """发送错误消息"""
        try:
            await websocket.send(json.dumps({
                "type": "error",
                "message": error_message,
                "timestamp": datetime.now().isoformat()
            }))
        except Exception as e:
            logger.error(f"❌ 发送错误消息失败: {e}")
            
    def unsubscribe_all(self, websocket):
        """取消客户端的所有订阅"""
        for stock_code in list(self.subscriptions.keys()):
            if websocket in self.subscriptions[stock_code]:
                self.subscriptions[stock_code].discard(websocket)
                if not self.subscriptions[stock_code]:
                    del self.subscriptions[stock_code]
                    
    async def start_data_pusher(self):
        """启动数据推送任务"""
        while True:
            try:
                for stock_code in list(self.subscriptions.keys()):
                    if self.subscriptions[stock_code]:  # 有订阅者
                        await self.send_stock_data(stock_code)
                        
                await asyncio.sleep(config.REALTIME_UPDATE_INTERVAL)  # 10秒推送一次
                
            except Exception as e:
                logger.error(f"❌ 数据推送任务异常: {e}")
                await asyncio.sleep(5)
                
    async def start_server(self):
        """启动WebSocket服务器"""
        logger.info(f"🚀 启动WebSocket服务器: {config.WS_HOST}:{config.WS_PORT}")
        
        # 启动数据推送任务
        asyncio.create_task(self.start_data_pusher())
        
        # 启动WebSocket服务器
        async with websockets.serve(self.register_client, config.WS_HOST, config.WS_PORT, max_size=2**20, ping_interval=30):
            logger.info(f"✅ WebSocket服务器已启动，等待连接...")
            await asyncio.Future()  # 保持运行

# 创建全局实例
websocket_server = WebSocketServer()

if __name__ == "__main__":
    # 配置日志
    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    )
    
    try:
        asyncio.run(websocket_server.start_server())
    except KeyboardInterrupt:
        logger.info("🛑 WebSocket服务器已停止")
    except Exception as e:
        logger.error(f"❌ WebSocket服务器启动失败: {e}") 