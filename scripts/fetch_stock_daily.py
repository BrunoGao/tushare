import tushare as ts
import pandas as pd
import logging
from datetime import datetime, date, timedelta
from concurrent.futures import ThreadPoolExecutor, as_completed
import time
import sys, os
from typing import List, Tuple
from tqdm import tqdm
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import config
from utils.db_helper import db
from sqlalchemy import text

# 配置详细日志 - 强制设置为INFO级别以显示所有进度信息
logging.basicConfig(
    level=logging.INFO,  # 强制设置为INFO级别
    format='%(asctime)s - %(levelname)s - %(message)s',
    handlers=[
        logging.StreamHandler(sys.stdout),  # 输出到控制台
        logging.FileHandler('logs/fetch_daily.log', encoding='utf-8')
    ],
    force=True  # 强制重新配置
)
logger = logging.getLogger(__name__)

# 添加控制台处理器，确保实时输出
console_handler = logging.StreamHandler(sys.stdout)
console_handler.setLevel(logging.INFO)
console_formatter = logging.Formatter('%(asctime)s - %(levelname)s - %(message)s')
console_handler.setFormatter(console_formatter)
logger.addHandler(console_handler)
logger.setLevel(logging.INFO)

class StockDailyFetcher:
    def __init__(self):
        ts.set_token(config.TS_TOKEN)
        self.pro = ts.pro_api()
        self.request_count = 0
        self.last_minute = datetime.now().minute
        self.total_processed = 0
        self.total_success = 0
        
    def rate_limit_check(self):  # 请求频率控制
        current_minute = datetime.now().minute
        if current_minute != self.last_minute:
            self.request_count = 0
            self.last_minute = current_minute
        if self.request_count >= config.TS_RATE_LIMIT:
            sleep_time = 61 - datetime.now().second
            logger.info(f"⏳ 达到频率限制，等待{sleep_time}秒...")
            time.sleep(sleep_time)
            self.request_count = 0
        self.request_count += 1
        
    def fetch_stock_daily_range(self, ts_code: str, start_date: str, end_date: str) -> bool:  # 获取单只股票指定时间段数据
        try:
            self.rate_limit_check()
            logger.info(f"🔍 正在获取 {ts_code} {start_date}-{end_date} 数据...")
            df = self.pro.daily(ts_code=ts_code, start_date=start_date, end_date=end_date)
            if df.empty: 
                logger.info(f"📭 {ts_code} 无数据")
                return True
            
            logger.info(f"📊 {ts_code} 获取到 {len(df)} 条数据")
            
            # 数据清洗与转换
            df['trade_date'] = pd.to_datetime(df['trade_date'], format='%Y%m%d')
            df['change_pct'] = df['pct_chg'] / 100 if 'pct_chg' in df.columns else None
            df = df.rename(columns={'vol': 'vol', 'amount': 'amount'})
            
            # 按月分表存储，实现断点续传
            saved_count = 0
            for month_group in df.groupby(df['trade_date'].dt.to_period('M')):
                month_data = month_group[1]
                year, month = month_group[0].year, month_group[0].month
                
                # 确保月表存在
                if not db.create_monthly_table(year, month):
                    logger.error(f"❌ 创建表 stock_daily_{year:04d}{month:02d} 失败")
                    continue
                    
                table_name = f"stock_daily_{year:04d}{month:02d}"
                
                # 检查已存在的数据，避免重复插入
                existing_dates = self.get_existing_dates(table_name, ts_code, month_data['trade_date'].min(), month_data['trade_date'].max())
                if existing_dates:
                    # 过滤掉已存在的数据
                    month_data = month_data[~month_data['trade_date'].dt.date.isin(existing_dates)]
                    if month_data.empty:
                        logger.info(f"⏭️  {ts_code} {year}年{month}月数据已存在，跳过")
                        continue
                    logger.info(f"🔄 {ts_code} {year}年{month}月过滤后需插入{len(month_data)}条数据")
                
                # 转换日期格式并插入
                month_data = month_data.copy()
                month_data['trade_date'] = month_data['trade_date'].dt.date
                
                try:
                    month_data.to_sql(table_name, con=db.engine, if_exists='append', index=False, chunksize=config.CHUNK_SIZE)
                    saved_count += len(month_data)
                    logger.info(f"💾 {ts_code} {year}年{month}月数据已保存到{table_name}({len(month_data)}条)")
                except Exception as e:
                    logger.error(f"❌ {ts_code} {year}年{month}月数据保存失败: {e}")
                    continue
                
            logger.info(f"✅ {ts_code} 数据获取完成，共保存{saved_count}条记录")
            return True
            
        except Exception as e:
            logger.error(f"❌ 获取{ts_code} {start_date}-{end_date}数据失败: {e}")
            return False
    
    def get_existing_dates(self, table_name: str, ts_code: str, start_date: date, end_date: date) -> List[date]:
        """获取表中已存在的日期，用于断点续传"""
        try:
            with db.engine.connect() as conn:
                sql = f"SELECT trade_date FROM {table_name} WHERE ts_code = :ts_code AND trade_date BETWEEN :start_date AND :end_date"
                result = conn.execute(text(sql), {
                    'ts_code': ts_code,
                    'start_date': start_date,
                    'end_date': end_date
                })
                existing_dates = [row[0] for row in result]
                return existing_dates
        except Exception as e:
            logger.warning(f"⚠️ 查询已存在数据失败: {e}")
            return []
            
    def fetch_all_stocks_10years(self) -> bool:  # 一次性获取所有股票最近10年数据
        logger.info("🚀 开始执行一次性获取所有股票最近10年数据")
        start_time = datetime.now()
        
        try:
            # 步骤1: 获取股票列表
            logger.info("📋 步骤1: 正在获取股票列表...")
            stock_list = db.get_stock_list()
            if not stock_list:
                logger.error("❌ 未获取到股票列表")
                return False
            logger.info(f"✅ 步骤1完成: 获取到 {len(stock_list)} 只股票")
            
            # 步骤2: 计算时间范围
            end_date = datetime.now().strftime('%Y%m%d')
            start_date = (datetime.now() - timedelta(days=365*10)).strftime('%Y%m%d')
            logger.info(f"📅 步骤2: 时间范围 {start_date} - {end_date} (最近10年)")
            
            # 步骤3: 批量获取数据
            logger.info(f"🚀 步骤3: 开始批量获取{len(stock_list)}只股票数据")
            logger.info(f"📊 预计耗时: {len(stock_list) * 0.2 / 60:.1f}分钟")
            
            success_count = 0
            error_count = 0
            total_saved = 0
            
            # 使用tqdm显示进度
            with tqdm(total=len(stock_list), desc="获取股票数据", unit="只", 
                     bar_format='{l_bar}{bar}| {n_fmt}/{total_fmt} [{elapsed}<{remaining}, {rate_fmt}]') as pbar:
                # 多线程并发获取
                with ThreadPoolExecutor(max_workers=config.TS_THREAD_COUNT) as executor:
                    futures = {executor.submit(self.fetch_stock_daily_range, ts_code, start_date, end_date): ts_code 
                              for ts_code in stock_list}
                    
                    for future in as_completed(futures):
                        ts_code = futures[future]
                        try:
                            if future.result():
                                success_count += 1
                                logger.info(f"✅ {ts_code} 处理成功")
                            else:
                                error_count += 1
                                logger.error(f"❌ {ts_code} 处理失败")
                            pbar.update(1)
                            pbar.set_postfix({
                                '成功': success_count, 
                                '失败': error_count,
                                '成功率': f'{success_count/(success_count+error_count)*100:.1f}%'
                            })
                        except Exception as e:
                            error_count += 1
                            logger.error(f"❌ 处理{ts_code}时发生异常: {e}")
                            pbar.update(1)
                            
            # 步骤4: 统计结果
            duration = int((datetime.now() - start_time).total_seconds())
            logger.info(f"✅ 步骤4: 批量获取完成")
            logger.info(f"📊 统计结果:")
            logger.info(f"   - 总股票数: {len(stock_list)}")
            logger.info(f"   - 成功获取: {success_count}")
            logger.info(f"   - 获取失败: {error_count}")
            logger.info(f"   - 成功率: {success_count/(success_count+error_count)*100:.1f}%")
            logger.info(f"   - 总耗时: {duration}秒 ({duration/60:.1f}分钟)")
            
            # 记录操作日志
            try:
                db.log_operation('fetch_daily', 'all_stocks_10years', 'SUCCESS', 
                               f'成功获取{success_count}/{len(stock_list)}只股票数据', duration)
            except Exception as e:
                logger.warning(f"⚠️ 记录日志失败: {e}")
            
            return success_count > 0
            
        except Exception as e:
            duration = int((datetime.now() - start_time).total_seconds())
            error_msg = f"批量获取数据失败: {e}"
            logger.error(f"❌ {error_msg}")
            try:
                db.log_operation('fetch_daily', 'all_stocks_10years', 'FAILED', error_msg, duration)
            except:
                pass
            return False
        
    def fetch_3day_plan(self) -> bool:  # 3天获取计划执行
        logger.info("🚀 开始执行3天历史数据获取计划")
        plan_start_time = datetime.now()
        
        # 分批计划 - 优化为更合理的时间段
        batches = [
            ('20200101', '20221231', 'batch_2020_2022', '2020-2022年数据'),  # 第1天：近3年数据
            ('20230101', '20241231', 'batch_2023_2024', '2023-2024年数据'),  # 第2天：最近2年数据
            ('20250101', datetime.now().strftime('%Y%m%d'), 'batch_2025_now', '2025年至今数据')  # 第3天：今年数据
        ]
        
        total_success = 0
        total_stocks = 0
        
        for i, (start_date, end_date, batch_name, description) in enumerate(batches, 1):
            logger.info(f"📅 执行第{i}批次: {description} ({start_date} - {end_date})")
            batch_start_time = datetime.now()
            
            success_count = self.batch_fetch_by_date_range(start_date, end_date, batch_name)
            total_success += success_count
            
            batch_duration = int((datetime.now() - batch_start_time).total_seconds())
            logger.info(f"✅ 第{i}批次完成，成功获取{success_count}只股票数据，耗时{batch_duration}秒")
            
            # 批次间休息，避免API限制
            if i < len(batches):
                logger.info("⏸️  批次间休息30秒...")
                time.sleep(30)
            
        plan_duration = int((datetime.now() - plan_start_time).total_seconds())
        logger.info(f"🎉 3天计划执行完成，总计成功获取{total_success}只股票历史数据，总耗时{plan_duration}秒")
        
        # 记录总体执行结果
        try:
            db.log_operation('fetch_daily', '3day_plan_complete', 'SUCCESS', 
                           f'3天计划完成，成功获取{total_success}只股票数据', plan_duration)
        except:
            pass
            
        return total_success > 0
        
    def batch_fetch_by_date_range(self, start_date: str, end_date: str, batch_name: str = None) -> int:  # 批量获取指定时间段数据
        start_time = datetime.now()
        success_count = 0
        error_count = 0
        
        try:
            # 获取股票列表
            logger.info(f"📋 正在获取股票列表...")
            stock_list = db.get_stock_list()
            if not stock_list:
                logger.error("❌ 未获取到股票列表")
                return 0
                
            logger.info(f"🚀 开始批量获取{len(stock_list)}只股票 {start_date}-{end_date} 数据")
            logger.info(f"📊 预计耗时: {len(stock_list) * 0.2 / 60:.1f}分钟")
            
            # 使用tqdm显示进度
            with tqdm(total=len(stock_list), desc=f"获取{start_date}-{end_date}", unit="只", 
                     bar_format='{l_bar}{bar}| {n_fmt}/{total_fmt} [{elapsed}<{remaining}, {rate_fmt}]') as pbar:
                # 多线程并发获取
                with ThreadPoolExecutor(max_workers=config.TS_THREAD_COUNT) as executor:
                    futures = {executor.submit(self.fetch_stock_daily_range, ts_code, start_date, end_date): ts_code 
                              for ts_code in stock_list}
                    
                    for future in as_completed(futures):
                        ts_code = futures[future]
                        try:
                            if future.result():
                                success_count += 1
                            else:
                                error_count += 1
                            pbar.update(1)
                            pbar.set_postfix({
                                '成功': success_count, 
                                '失败': error_count,
                                '成功率': f'{success_count/(success_count+error_count)*100:.1f}%'
                            })
                        except Exception as e:
                            error_count += 1
                            logger.error(f"❌ 处理{ts_code}时发生异常: {e}")
                            pbar.update(1)
                            
            duration = int((datetime.now() - start_time).total_seconds())
            operation_name = batch_name or f"daily_fetch_{start_date}_{end_date}"
            db.log_operation('fetch_daily', operation_name, 'SUCCESS', f'成功获取{success_count}/{len(stock_list)}只股票数据', duration)
            logger.info(f"✅ 批量获取完成，成功{success_count}/{len(stock_list)}只股票，失败{error_count}只，耗时{duration}秒")
            
        except Exception as e:
            duration = int((datetime.now() - start_time).total_seconds())
            error_msg = f"批量获取数据失败: {e}"
            logger.error(f"❌ {error_msg}")
            try:
                db.log_operation('fetch_daily', 'daily_fetch_batch', 'FAILED', error_msg, duration)
            except:
                pass
            
        return success_count
        
    def daily_update(self) -> int:  # 每日增量更新
        yesterday = (datetime.now() - timedelta(days=1)).strftime('%Y%m%d')
        today = datetime.now().strftime('%Y%m%d')
        logger.info(f"📈 开始每日增量更新: {yesterday}")
        return self.batch_fetch_by_date_range(yesterday, today, 'daily_update')
        
    def quick_test(self, limit: int = 10) -> bool:  # 快速测试功能
        """快速测试数据获取功能"""
        logger.info(f"🧪 开始快速测试，获取前{limit}只股票最近30天数据")
        
        try:
            stock_list = db.get_stock_list()[:limit]
            if not stock_list:
                logger.error("❌ 未获取到股票列表")
                return False
                
            end_date = datetime.now().strftime('%Y%m%d')
            start_date = (datetime.now() - timedelta(days=30)).strftime('%Y%m%d')
            
            success_count = 0
            for ts_code in tqdm(stock_list, desc="测试获取", unit="只"):
                if self.fetch_stock_daily_range(ts_code, start_date, end_date):
                    success_count += 1
                    
            logger.info(f"✅ 快速测试完成，成功获取{success_count}/{len(stock_list)}只股票数据")
            return success_count > 0
            
        except Exception as e:
            logger.error(f"❌ 快速测试失败: {e}")
            return False

if __name__ == "__main__":
    fetcher = StockDailyFetcher()
    
    # 根据命令行参数选择执行模式
    if len(sys.argv) > 1:
        mode = sys.argv[1]
        if mode == 'plan':
            success = fetcher.fetch_3day_plan()
        elif mode == 'update':
            success = fetcher.daily_update() > 0
        elif mode == 'test':
            limit = int(sys.argv[2]) if len(sys.argv) > 2 else 10
            success = fetcher.quick_test(limit)
        elif mode == 'range' and len(sys.argv) >= 4:
            start_date, end_date = sys.argv[2], sys.argv[3]
            success = fetcher.batch_fetch_by_date_range(start_date, end_date) > 0
        elif mode == '10years':
            success = fetcher.fetch_all_stocks_10years()
        else:
            print("用法: python fetch_stock_daily.py [plan|update|test|range start_date end_date|10years]")
            sys.exit(1)
    else:
        # 默认执行10年数据获取
        success = fetcher.fetch_all_stocks_10years()
        
    sys.exit(0 if success else 1) 