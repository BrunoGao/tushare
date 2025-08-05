#!/usr/bin/env python3
"""
异步获取股票全量数据脚本
支持多线程并发、断点续传、进度监控
"""
import asyncio
import aiohttp
import logging
import tushare as ts
import pandas as pd
from datetime import datetime, timedelta
from concurrent.futures import ThreadPoolExecutor, as_completed
from database.db_manager import DatabaseManager
import config
import json
import os
import time
from typing import List, Dict, Tuple
from tqdm import tqdm
import threading
from queue import Queue

# 配置日志
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler('logs/fetch_full_data.log'),
        logging.StreamHandler()
    ]
)
logger = logging.getLogger(__name__)

class AsyncStockDataFetcher:
    def __init__(self, max_workers=10, batch_size=50):
        """
        初始化异步数据获取器
        :param max_workers: 最大并发线程数
        :param batch_size: 每批处理的股票数量
        """
        ts.set_token(config.TS_TOKEN)
        self.pro = ts.pro_api()
        self.db_manager = DatabaseManager()
        self.max_workers = max_workers
        self.batch_size = batch_size
        
        # 进度跟踪
        self.progress_file = 'logs/fetch_progress.json'
        self.completed_stocks = set()
        self.failed_stocks = set()
        self.total_stocks = 0
        
        # 频率控制
        self.request_lock = threading.Lock()
        self.request_count = 0
        self.last_minute = datetime.now().minute
        
        # 统计信息
        self.stats = {
            'total_records': 0,
            'success_stocks': 0,
            'failed_stocks': 0,
            'start_time': None,
            'tables_created': set()
        }
        
        self.load_progress()

    def load_progress(self):
        """加载进度文件"""
        if os.path.exists(self.progress_file):
            try:
                with open(self.progress_file, 'r') as f:
                    progress = json.load(f)
                    self.completed_stocks = set(progress.get('completed', []))
                    self.failed_stocks = set(progress.get('failed', []))
                    self.stats.update(progress.get('stats', {}))
                logger.info(f"📁 加载进度: 已完成 {len(self.completed_stocks)} 只股票")
            except Exception as e:
                logger.warning(f"⚠️ 无法加载进度文件: {e}")

    def save_progress(self):
        """保存进度文件"""
        try:
            progress = {
                'completed': list(self.completed_stocks),
                'failed': list(self.failed_stocks),
                'stats': self.stats,
                'last_update': datetime.now().isoformat()
            }
            os.makedirs('logs', exist_ok=True)
            with open(self.progress_file, 'w') as f:
                json.dump(progress, f, indent=2)
        except Exception as e:
            logger.error(f"❌ 保存进度失败: {e}")

    def rate_limit_control(self):
        """频率控制 - 每分钟最多500次请求"""
        with self.request_lock:
            current_minute = datetime.now().minute
            if current_minute != self.last_minute:
                self.request_count = 0
                self.last_minute = current_minute
                
            if self.request_count >= config.TS_RATE_LIMIT:
                sleep_time = 61 - datetime.now().second
                logger.info(f"⏱️ 达到频率限制，等待 {sleep_time} 秒...")
                time.sleep(sleep_time)
                self.request_count = 0
                self.last_minute = datetime.now().minute
                
            self.request_count += 1

    def create_monthly_table(self, year: int, month: int) -> str:
        """创建月度分表"""
        table_name = f"stock_daily_{year:04d}{month:02d}"
        
        if table_name in self.stats['tables_created']:
            return table_name
            
        try:
            create_sql = f"""
            CREATE TABLE IF NOT EXISTS {table_name} (
                id BIGINT AUTO_INCREMENT PRIMARY KEY,
                ts_code VARCHAR(20) NOT NULL COMMENT '股票代码',
                trade_date DATE NOT NULL COMMENT '交易日期',
                open DECIMAL(10,3) COMMENT '开盘价',
                high DECIMAL(10,3) COMMENT '最高价',
                low DECIMAL(10,3) COMMENT '最低价',
                close DECIMAL(10,3) COMMENT '收盘价',
                pre_close DECIMAL(10,3) COMMENT '昨收价',
                `change` DECIMAL(10,3) COMMENT '涨跌额',
                pct_chg DECIMAL(8,4) COMMENT '涨跌幅',
                vol BIGINT COMMENT '成交量(手)',
                amount DECIMAL(20,3) COMMENT '成交额(千元)',
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                UNIQUE KEY uk_code_date (ts_code, trade_date),
                INDEX idx_code (ts_code),
                INDEX idx_date (trade_date),
                INDEX idx_code_date (ts_code, trade_date)
            ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='股票日线数据表-{year:04d}{month:02d}'
            """
            
            self.db_manager.execute_sql(create_sql)
            self.stats['tables_created'].add(table_name)
            logger.debug(f"✅ 创建表 {table_name}")
            return table_name
            
        except Exception as e:
            logger.error(f"❌ 创建表 {table_name} 失败: {e}")
            return None

    def fetch_stock_daily_data(self, ts_code: str, start_date: str, end_date: str) -> Dict:
        """获取单只股票的日线数据"""
        try:
            self.rate_limit_control()
            
            # 获取数据
            df = self.pro.daily(
                ts_code=ts_code,
                start_date=start_date,
                end_date=end_date
            )
            
            if df.empty:
                return {'success': False, 'ts_code': ts_code, 'error': '无数据', 'records': 0}
                
            # 数据预处理
            df['trade_date'] = pd.to_datetime(df['trade_date'], format='%Y%m%d')
            df = df.sort_values('trade_date')
            
            # 按月分组插入
            total_records = 0
            for (year, month), group in df.groupby([df['trade_date'].dt.year, df['trade_date'].dt.month]):
                table_name = self.create_monthly_table(year, month)
                if table_name:
                    success = self.db_manager.insert_dataframe(table_name, group, replace=False)
                    if success:
                        total_records += len(group)
                    
            return {
                'success': True, 
                'ts_code': ts_code, 
                'records': total_records,
                'date_range': f"{df['trade_date'].min().date()} ~ {df['trade_date'].max().date()}"
            }
            
        except Exception as e:
            return {'success': False, 'ts_code': ts_code, 'error': str(e), 'records': 0}

    def fetch_stock_batch(self, stock_batch: List[Tuple[str, str]]) -> List[Dict]:
        """批量获取股票数据"""
        results = []
        
        # 计算日期范围（最近3年）
        end_date = datetime.now().strftime('%Y%m%d')
        start_date = (datetime.now() - timedelta(days=3*365)).strftime('%Y%m%d')
        
        for ts_code, name in stock_batch:
            if ts_code in self.completed_stocks:
                continue
                
            result = self.fetch_stock_daily_data(ts_code, start_date, end_date)
            result['name'] = name
            results.append(result)
            
            if result['success']:
                self.completed_stocks.add(ts_code)
                self.stats['success_stocks'] += 1
                self.stats['total_records'] += result['records']
                logger.info(f"✅ {ts_code} ({name}): {result['records']} 条记录")
            else:
                self.failed_stocks.add(ts_code)
                self.stats['failed_stocks'] += 1
                logger.error(f"❌ {ts_code} ({name}): {result['error']}")
                
            # 小延时避免过于频繁的请求
            time.sleep(0.05)
            
        return results

    async def fetch_all_stocks_async(self):
        """异步获取所有股票数据"""
        logger.info("🚀 开始异步获取全量股票数据...")
        self.stats['start_time'] = datetime.now().isoformat()
        
        try:
            # 获取股票列表
            logger.info("📋 获取股票列表...")
            stock_df = self.db_manager.fetch_data("""
                SELECT ts_code, name FROM stock_basic 
                ORDER BY ts_code
            """)
            
            if stock_df.empty:
                logger.error("❌ 未找到股票数据")
                return False
                
            # 过滤已完成的股票
            remaining_stocks = [
                (row['ts_code'], row['name']) 
                for _, row in stock_df.iterrows() 
                if row['ts_code'] not in self.completed_stocks
            ]
            
            self.total_stocks = len(stock_df)
            remaining_count = len(remaining_stocks)
            
            logger.info(f"📊 总股票数: {self.total_stocks}")
            logger.info(f"📊 已完成: {len(self.completed_stocks)}")
            logger.info(f"📊 待处理: {remaining_count}")
            
            if remaining_count == 0:
                logger.info("🎉 所有股票数据已获取完成！")
                return True
                
            # 分批处理
            batches = [
                remaining_stocks[i:i + self.batch_size] 
                for i in range(0, remaining_count, self.batch_size)
            ]
            
            logger.info(f"🔄 将分 {len(batches)} 批处理，每批 {self.batch_size} 只股票")
            
            # 使用线程池并发处理
            with ThreadPoolExecutor(max_workers=self.max_workers) as executor:
                # 提交所有批次任务
                future_to_batch = {
                    executor.submit(self.fetch_stock_batch, batch): i 
                    for i, batch in enumerate(batches)
                }
                
                # 使用tqdm显示进度
                with tqdm(total=len(batches), desc="处理批次") as pbar:
                    for future in as_completed(future_to_batch):
                        batch_idx = future_to_batch[future]
                        try:
                            results = future.result()
                            pbar.set_postfix({
                                '成功': self.stats['success_stocks'],
                                '失败': self.stats['failed_stocks'],
                                '记录数': self.stats['total_records']
                            })
                            pbar.update(1)
                            
                            # 定期保存进度
                            if batch_idx % 10 == 0:
                                self.save_progress()
                                
                        except Exception as e:
                            logger.error(f"❌ 批次 {batch_idx} 处理失败: {e}")
                            pbar.update(1)
            
            # 最终保存进度
            self.save_progress()
            
            # 统计结果
            end_time = datetime.now()
            duration = (end_time - datetime.fromisoformat(self.stats['start_time'])).total_seconds()
            
            logger.info("=" * 60)
            logger.info("🎉 全量数据获取完成！")
            logger.info(f"📊 总股票数: {self.total_stocks}")
            logger.info(f"✅ 成功获取: {self.stats['success_stocks']}")
            logger.info(f"❌ 获取失败: {self.stats['failed_stocks']}")
            logger.info(f"📈 总记录数: {self.stats['total_records']:,}")
            logger.info(f"🕒 总耗时: {duration/60:.1f} 分钟")
            logger.info(f"📋 创建表数: {len(self.stats['tables_created'])}")
            logger.info("=" * 60)
            
            return True
            
        except Exception as e:
            logger.error(f"❌ 异步获取失败: {e}")
            self.save_progress()
            return False

    def get_progress_stats(self) -> Dict:
        """获取进度统计"""
        return {
            'total_stocks': self.total_stocks,
            'completed_stocks': len(self.completed_stocks),
            'failed_stocks': len(self.failed_stocks),
            'completion_rate': len(self.completed_stocks) / max(self.total_stocks, 1) * 100,
            'total_records': self.stats['total_records'],
            'tables_created': len(self.stats['tables_created'])
        }

async def main():
    """主函数"""
    print("=" * 60)
    print("🚀 异步股票全量数据获取程序")
    print("=" * 60)
    
    # 创建数据获取器
    fetcher = AsyncStockDataFetcher(
        max_workers=8,  # 并发线程数
        batch_size=30   # 每批股票数
    )
    
    # 显示当前进度
    stats = fetcher.get_progress_stats()
    if stats['completed_stocks'] > 0:
        print(f"📊 当前进度: {stats['completed_stocks']}/{stats['total_stocks']} "
              f"({stats['completion_rate']:.1f}%)")
        print(f"📈 已获取记录: {stats['total_records']:,} 条")
        print(f"📋 已创建表: {stats['tables_created']} 个")
        print("-" * 60)
    
    # 开始获取数据
    success = await fetcher.fetch_all_stocks_async()
    
    # 最终统计
    final_stats = fetcher.get_progress_stats()
    print("\n" + "=" * 60)
    if success:
        print("🎉 全量数据获取成功完成！")
    else:
        print("⚠️ 数据获取过程中出现问题")
    
    print(f"📊 最终统计:")
    print(f"  总股票数: {final_stats['total_stocks']}")
    print(f"  成功获取: {final_stats['completed_stocks']}")
    print(f"  获取失败: {final_stats['failed_stocks']}")
    print(f"  完成率: {final_stats['completion_rate']:.1f}%")
    print(f"  总记录数: {final_stats['total_records']:,}")
    print("=" * 60)
    
    return 0 if success else 1

if __name__ == "__main__":
    # 确保日志目录存在
    os.makedirs('logs', exist_ok=True)
    
    # 运行异步程序
    exit_code = asyncio.run(main())
    exit(exit_code)