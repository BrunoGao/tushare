import json,time,threading
from typing import Dict,Any,List
from datetime import datetime,timedelta
from database.db_manager import DatabaseManager
try:
    from analysis.technical_indicators import TechnicalIndicators
except ImportError:
    # 如果talib有问题，使用简化版技术指标
    print("⚠️ 缓存优化器使用简化版技术指标（talib不可用）")
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

class CacheOptimizer:
    """前端缓存和预加载优化器"""
    
    def __init__(self):
        self.db=DatabaseManager()
        self.tech_calc=TechnicalIndicators()
        self.cache:Dict[str,Any]={}  #内存缓存
        self.cache_time:Dict[str,datetime]={}  #缓存时间
        self.preload_list:List[str]=[]  #预加载股票列表
        self.cache_ttl=300  #缓存TTL(5分钟)
        
    def get_hot_stocks(self)->List[str]:
        """获取热门股票列表用于预加载"""
        hot_stocks=['000001.SZ','000002.SZ','600036.SH','600519.SH','000858.SZ']
        return hot_stocks
        
    def is_cache_valid(self,key:str)->bool:
        """检查缓存是否有效"""
        if key not in self.cache_time:
            return False
        return datetime.now()-self.cache_time[key]<timedelta(seconds=self.cache_ttl)
        
    def get_cached_data(self,key:str)->Any:
        """获取缓存数据"""
        if self.is_cache_valid(key):
            return self.cache.get(key)
        return None
        
    def set_cache(self,key:str,data:Any):
        """设置缓存"""
        self.cache[key]=data
        self.cache_time[key]=datetime.now()
        
    def preload_stock_data(self,stock_code:str):
        """预加载单个股票数据"""
        try:
            cache_key=f"stock_data_{stock_code}"
            
            # 检查是否已缓存
            if self.is_cache_valid(cache_key):
                return
                
            # 获取数据 - 先检查表是否存在
            try:
                with self.db.engine.connect() as conn:
                    sql=f"""
                    SELECT trade_date,open,high,low,close,vol,amount 
                    FROM daily_data WHERE ts_code='{stock_code}' 
                    ORDER BY trade_date DESC LIMIT 250
                    """
                    df=self.db.fetch_data(sql)
            except Exception as db_e:
                print(f"⚠️ 数据库查询失败 {stock_code}: {db_e}")
                # 使用模拟数据
                df=self.create_mock_data(stock_code)
                print(f"📊 使用模拟数据: {stock_code}")
                
                if not df.empty:
                    # 计算技术指标
                    df=self.tech_calc.calculate_all_indicators(df)
                    
                    # 准备前端数据格式
                    chart_data=self.prepare_chart_data(df,stock_code)
                    realtime_data=self.prepare_realtime_data(df.iloc[0])
                    
                    # 缓存数据
                    cache_data={
                        'chart_data':chart_data,
                        'realtime_data':realtime_data,
                        'raw_data':df.to_dict('records')[:30],  #只缓存最近30天
                        'timestamp':datetime.now().isoformat()
                    }
                    
                    self.set_cache(cache_key,cache_data)
                    print(f"✅ 预加载完成: {stock_code}")
                    
        except Exception as e:
            print(f"❌ 预加载失败 {stock_code}: {e}")
            
    def prepare_chart_data(self,df,stock_code:str)->Dict:
        """准备ECharts图表数据格式"""
        data=[]
        for _,row in df.iterrows():
            data.append({
                'date':row['trade_date'],
                'open':float(row['open']),
                'high':float(row['high']),
                'low':float(row['low']),
                'close':float(row['close']),
                'volume':int(row['vol']),
                'ma5':float(row.get('ma5',0)),
                'ma20':float(row.get('ma20',0)),
                'ma60':float(row.get('ma60',0)),
                'rsi':float(row.get('rsi',0)),
                'macd':float(row.get('macd',0)),
                'kdj_k':float(row.get('kdj_k',0))
            })
        return {'code':stock_code,'data':data}
        
    def prepare_realtime_data(self,latest_row)->Dict:
        """准备实时数据格式"""
        return {
            'price':{
                'current':float(latest_row['close']),
                'open':float(latest_row['open']),
                'high':float(latest_row['high']),
                'low':float(latest_row['low']),
                'volume':int(latest_row['vol']),
                'change':float(latest_row['close']-latest_row['open']),
                'change_pct':float((latest_row['close']-latest_row['open'])/latest_row['open']*100)
            },
            'indicators':{
                'ma5':float(latest_row.get('ma5',0)),
                'ma20':float(latest_row.get('ma20',0)),
                'ma60':float(latest_row.get('ma60',0)),
                'rsi':float(latest_row.get('rsi',0)),
                'macd':float(latest_row.get('macd',0)),
                'kdj_k':float(latest_row.get('kdj_k',0))
            }
        }
        
    def start_preload_worker(self):
        """启动预加载工作线程"""
        def worker():
            while True:
                try:
                    hot_stocks=self.get_hot_stocks()
                    for stock_code in hot_stocks:
                        self.preload_stock_data(stock_code)
                        time.sleep(1)  # 避免频繁请求
                    time.sleep(60)  # 每分钟更新一次
                except Exception as e:
                    print(f"预加载工作线程错误: {e}")
                    time.sleep(30)
                    
        thread=threading.Thread(target=worker,daemon=True)
        thread.start()
        print("🔄 预加载工作线程已启动")
        
    def get_fast_data(self,stock_code:str)->Dict:
        """快速获取数据(优先使用缓存)"""
        cache_key=f"stock_data_{stock_code}"
        cached=self.get_cached_data(cache_key)
        
        if cached:
            print(f"⚡ 使用缓存数据: {stock_code}")
            return cached
            
        # 缓存未命中，实时加载
        print(f"🔄 实时加载数据: {stock_code}")
        self.preload_stock_data(stock_code)
        return self.get_cached_data(cache_key) or {}
        
    def create_mock_data(self,stock_code:str):
        """创建模拟股票数据用于演示"""
        import pandas as pd
        import numpy as np
        
        # 基础价格（不同股票不同起始价格）
        base_prices={'000001.SZ':12.50,'000002.SZ':25.80,'600036.SH':45.20,'600519.SH':1800.0,'000858.SZ':18.30}
        base_price=base_prices.get(stock_code,20.0)
        
        # 生成30天模拟数据
        dates=[datetime.now()-timedelta(days=i) for i in range(29,-1,-1)]
        
        data=[]
        current_price=base_price
        for date in dates:
            # 模拟价格波动(-3%到+3%)
            change=np.random.uniform(-0.03,0.03)
            current_price*=(1+change)
            
            # 生成OHLC数据
            open_price=current_price*(1+np.random.uniform(-0.01,0.01))
            high_price=max(open_price,current_price)*(1+np.random.uniform(0,0.02))
            low_price=min(open_price,current_price)*(1-np.random.uniform(0,0.02))
            volume=np.random.randint(100000,1000000)
            
            data.append({
                'trade_date':date.strftime('%Y%m%d'),
                'open':round(open_price,2),
                'high':round(high_price,2), 
                'low':round(low_price,2),
                'close':round(current_price,2),
                'vol':volume,
                'amount':volume*current_price
            })
            
        return pd.DataFrame(data)
        
    def clear_old_cache(self):
        """清理过期缓存"""
        now=datetime.now()
        expired_keys=[]
        
        for key,cache_time in self.cache_time.items():
            if now-cache_time>timedelta(seconds=self.cache_ttl):
                expired_keys.append(key)
                
        for key in expired_keys:
            self.cache.pop(key,None)
            self.cache_time.pop(key,None)
            
        if expired_keys:
            print(f"🗑️ 清理过期缓存: {len(expired_keys)}个")

# 全局缓存优化器实例
cache_optimizer=CacheOptimizer() 