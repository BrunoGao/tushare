import tushare as ts
import pandas as pd
import logging
from datetime import datetime, date, timedelta
from concurrent.futures import ThreadPoolExecutor, as_completed
import time
import sys, os
from typing import List, Tuple
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import config
from utils.db_helper import db

logging.basicConfig(level=getattr(logging, config.LOG_LEVEL), format=config.LOG_FORMAT)
logger = logging.getLogger(__name__)

class StockDailyFetcher:
    def __init__(self):
        ts.set_token(config.TS_TOKEN)
        self.pro = ts.pro_api()
        self.request_count = 0
        self.last_minute = datetime.now().minute
        
    def rate_limit_check(self):  # 请求频率控制
        current_minute = datetime.now().minute
        if current_minute != self.last_minute:
            self.request_count = 0
            self.last_minute = current_minute
        if self.request_count >= config.TS_RATE_LIMIT:
            time.sleep(61 - datetime.now().second)
            self.request_count = 0
        self.request_count += 1
        
    def fetch_stock_daily_range(self, ts_code: str, start_date: str, end_date: str) -> bool:  # 获取单只股票指定时间段数据
        try:
            self.rate_limit_check()
            df = self.pro.daily(ts_code=ts_code, start_date=start_date, end_date=end_date)
            if df.empty: return True
            
            # 数据清洗与转换
            df['trade_date'] = pd.to_datetime(df['trade_date'], format='%Y%m%d')
            df['change_pct'] = df['pct_chg'] / 100 if 'pct_chg' in df.columns else None
            df = df.rename(columns={'vol': 'vol', 'amount': 'amount'})
            
            # 按月分表存储
            for month_group in df.groupby(df['trade_date'].dt.to_period('M')):
                month_data = month_group[1]
                year, month = month_group[0].year, month_group[0].month
                
                # 确保月表存在
                db.create_monthly_table(year, month)
                table_name = f"stock_daily_{year:04d}{month:02d}"
                
                # 转换日期格式并插入
                month_data = month_data.copy()
                month_data['trade_date'] = month_data['trade_date'].dt.date
                month_data.to_sql(table_name, con=db.engine, if_exists='append', index=False, chunksize=config.CHUNK_SIZE)
                
            logger.info(f"完成{ts_code} {start_date}-{end_date} 数据获取，共{len(df)}条")
            return True
            
        except Exception as e:
            logger.error(f"获取{ts_code} {start_date}-{end_date}数据失败: {e}")
            return False
            
    def batch_fetch_by_date_range(self, start_date: str, end_date: str, batch_name: str = None) -> int:  # 批量获取指定时间段数据
        start_time = datetime.now()
        success_count = 0
        
        try:
            # 获取股票列表
            stock_list = db.get_stock_list()
            if not stock_list:
                logger.error("未获取到股票列表")
                return 0
                
            logger.info(f"开始批量获取{len(stock_list)}只股票 {start_date}-{end_date} 数据")
            
            # 多线程并发获取
            with ThreadPoolExecutor(max_workers=config.TS_THREAD_COUNT) as executor:
                futures = {executor.submit(self.fetch_stock_daily_range, ts_code, start_date, end_date): ts_code 
                          for ts_code in stock_list}
                
                for future in as_completed(futures):
                    ts_code = futures[future]
                    try:
                        if future.result():
                            success_count += 1
                            if success_count % 100 == 0:
                                logger.info(f"已完成{success_count}/{len(stock_list)}只股票")
                    except Exception as e:
                        logger.error(f"处理{ts_code}时发生异常: {e}")
                        
            duration = int((datetime.now() - start_time).total_seconds())
            operation_name = batch_name or f"daily_fetch_{start_date}_{end_date}"
            db.log_operation('fetch_daily', operation_name, 'SUCCESS', f'成功获取{success_count}/{len(stock_list)}只股票数据', duration)
            logger.info(f"批量获取完成，成功{success_count}/{len(stock_list)}只股票，耗时{duration}秒")
            
        except Exception as e:
            duration = int((datetime.now() - start_time).total_seconds())
            error_msg = f"批量获取数据失败: {e}"
            logger.error(error_msg)
            db.log_operation('fetch_daily', 'daily_fetch_batch', 'FAILED', error_msg, duration)
            
        return success_count
        
    def fetch_3day_plan(self) -> bool:  # 3天获取计划执行
        logger.info("开始执行3天历史数据获取计划")
        
        # 分批计划
        batches = [
            ('20100101', '20151231', 'batch_2010_2015'),  # 第1天
            ('20160101', '20201231', 'batch_2016_2020'),  # 第2天
            ('20210101', datetime.now().strftime('%Y%m%d'), 'batch_2021_now')  # 第3天
        ]
        
        total_success = 0
        for i, (start_date, end_date, batch_name) in enumerate(batches, 1):
            logger.info(f"执行第{i}批次: {batch_name} ({start_date} - {end_date})")
            success_count = self.batch_fetch_by_date_range(start_date, end_date, batch_name)
            total_success += success_count
            logger.info(f"第{i}批次完成，成功获取{success_count}只股票数据")
            
        logger.info(f"3天计划执行完成，总计成功获取{total_success}只股票历史数据")
        return total_success > 0
        
    def daily_update(self) -> int:  # 每日增量更新
        yesterday = (datetime.now() - timedelta(days=1)).strftime('%Y%m%d')
        today = datetime.now().strftime('%Y%m%d')
        logger.info(f"开始每日增量更新: {yesterday}")
        return self.batch_fetch_by_date_range(yesterday, today, 'daily_update')

if __name__ == "__main__":
    fetcher = StockDailyFetcher()
    
    # 根据命令行参数选择执行模式
    if len(sys.argv) > 1:
        mode = sys.argv[1]
        if mode == 'plan':
            success = fetcher.fetch_3day_plan()
        elif mode == 'update':
            success = fetcher.daily_update() > 0
        elif mode == 'range' and len(sys.argv) >= 4:
            start_date, end_date = sys.argv[2], sys.argv[3]
            success = fetcher.batch_fetch_by_date_range(start_date, end_date) > 0
        else:
            print("用法: python fetch_stock_daily.py [plan|update|range start_date end_date]")
            sys.exit(1)
    else:
        success = fetcher.fetch_3day_plan()
        
    sys.exit(0 if success else 1) 