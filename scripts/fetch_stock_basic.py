import tushare as ts
import pandas as pd
import logging
from datetime import datetime
import time
import sys, os
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import config
from utils.db_helper import db

logging.basicConfig(level=getattr(logging, config.LOG_LEVEL), format=config.LOG_FORMAT)
logger = logging.getLogger(__name__)

class StockBasicFetcher:
    def __init__(self):
        ts.set_token(config.TS_TOKEN)
        self.pro = ts.pro_api()
        
    def fetch_all_stocks(self) -> bool:  # 获取所有股票基本信息
        start_time = datetime.now()
        try:
            logger.info("开始获取股票基本信息...")
            
            # 获取沪深A股
            exchanges = ['SSE', 'SZSE']  # 上交所、深交所
            all_data = []
            
            for exchange in exchanges:
                logger.info(f"获取{exchange}股票信息...")
                df = self.pro.stock_basic(exchange=exchange, list_status='L')  # 只获取上市股票
                if not df.empty:
                    df['exchange'] = exchange
                    all_data.append(df)
                    logger.info(f"获取{exchange}股票{len(df)}只")
                time.sleep(0.2)  # 防止超频
            
            if not all_data:
                logger.error("未获取到任何股票数据")
                return False
                
            # 合并数据
            final_df = pd.concat(all_data, ignore_index=True)
            logger.info(f"共获取股票{len(final_df)}只")
            
            # 数据清洗
            final_df['list_date'] = pd.to_datetime(final_df['list_date'], format='%Y%m%d', errors='coerce')
            if 'delist_date' in final_df.columns:
                final_df['delist_date'] = pd.to_datetime(final_df['delist_date'], format='%Y%m%d', errors='coerce')
            else:
                final_df['delist_date'] = None
                
            # 确保所有必需字段都存在
            required_columns = ['ts_code', 'symbol', 'name', 'area', 'industry', 'fullname', 'enname', 
                              'market', 'exchange', 'curr_type', 'list_status', 'list_date', 'delist_date', 'is_hs']
            for col in required_columns:
                if col not in final_df.columns:
                    final_df[col] = None
                    
            # 选择需要的列
            final_df = final_df[required_columns]
            
            # 批量插入数据库
            final_df.to_sql('stock_basic', con=db.engine, if_exists='replace', index=False, chunksize=config.CHUNK_SIZE)
            
            duration = int((datetime.now() - start_time).total_seconds())
            db.log_operation('fetch_basic', 'stock_basic_fetch', 'SUCCESS', f'成功获取{len(final_df)}只股票信息', duration)
            logger.info(f"股票基本信息获取完成，耗时{duration}秒")
            return True
            
        except Exception as e:
            duration = int((datetime.now() - start_time).total_seconds())
            error_msg = f"获取股票基本信息失败: {e}"
            logger.error(error_msg)
            try:
                db.log_operation('fetch_basic', 'stock_basic_fetch', 'FAILED', error_msg, duration)
            except:
                pass  # 忽略日志记录失败
            return False
            
    def update_stock_info(self, ts_code: str) -> bool:  # 更新单只股票信息
        try:
            df = self.pro.stock_basic(ts_code=ts_code)
            if not df.empty:
                df['list_date'] = pd.to_datetime(df['list_date'], format='%Y%m%d', errors='coerce')
                if 'delist_date' in df.columns:
                    df['delist_date'] = pd.to_datetime(df['delist_date'], format='%Y%m%d', errors='coerce')
                else:
                    df['delist_date'] = None
                df.to_sql('stock_basic', con=db.engine, if_exists='append', index=False)
                logger.info(f"更新股票{ts_code}信息成功")
                return True
        except Exception as e: logger.error(f"更新股票{ts_code}信息失败: {e}")
        return False

if __name__ == "__main__":
    fetcher = StockBasicFetcher()
    success = fetcher.fetch_all_stocks()
    sys.exit(0 if success else 1) 