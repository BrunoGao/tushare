#!/usr/bin/env python3
"""
快速数据同步工具 - 立即获取最新股票数据
"""

import sys
import os
import logging
from datetime import datetime, timedelta
import pandas as pd
import tushare as ts
from concurrent.futures import ThreadPoolExecutor, as_completed

# 添加项目路径
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import config
from database.db_manager import DatabaseManager

logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

class QuickDataSync:
    """快速数据同步"""
    
    def __init__(self):
        self.db = DatabaseManager()
        self.ts_api = ts.pro_api(config.TS_TOKEN)
        
    def sync_latest_data(self, days: int = 30):
        """同步最新数据"""
        logger.info(f"🚀 开始同步最近 {days} 天的股票数据")
        
        # 1. 更新股票基本信息
        self._update_stock_basic()
        
        # 2. 获取股票列表
        stocks = self._get_stock_list()
        logger.info(f"📊 获取到 {len(stocks)} 只股票")
        
        # 3. 并行更新日线数据
        self._parallel_update_daily_data(stocks, days)
        
        logger.info("✅ 数据同步完成")
    
    def _update_stock_basic(self):
        """更新股票基本信息"""
        logger.info("📋 更新股票基本信息...")
        
        try:
            # 获取A股基本信息
            stock_basic = self.ts_api.stock_basic(exchange='', list_status='L')
            
            if not stock_basic.empty:
                # 清空旧数据并插入新数据
                self.db.execute_sql("TRUNCATE TABLE stock_basic")
                self.db.insert_dataframe('stock_basic', stock_basic)
                logger.info(f"✅ 股票基本信息更新完成: {len(stock_basic)} 条")
            else:
                logger.warning("⚠️ 未获取到股票基本信息")
                
        except Exception as e:
            logger.error(f"❌ 更新股票基本信息失败: {e}")
    
    def _get_stock_list(self) -> list:
        """获取股票列表"""
        try:
            sql = """
            SELECT ts_code, name, industry, list_date 
            FROM stock_basic 
            WHERE list_status = 'L' 
            AND ts_code LIKE '%.SZ' OR ts_code LIKE '%.SH'
            ORDER BY ts_code
            """
            df = self.db.fetch_data(sql)
            return df['ts_code'].tolist() if not df.empty else []
        except Exception as e:
            logger.error(f"获取股票列表失败: {e}")
            return []
    
    def _parallel_update_daily_data(self, stocks: list, days: int):
        """并行更新日线数据"""
        logger.info(f"🔄 开始并行更新 {len(stocks)} 只股票的日线数据")
        
        # 计算日期范围
        end_date = datetime.now().strftime('%Y%m%d')
        start_date = (datetime.now() - timedelta(days=days)).strftime('%Y%m%d')
        
        # 分批处理，避免API限制
        batch_size = config.TS_BATCH_SIZE
        total_updated = 0
        
        for i in range(0, len(stocks), batch_size):
            batch = stocks[i:i + batch_size]
            batch_num = i // batch_size + 1
            total_batches = (len(stocks) + batch_size - 1) // batch_size
            
            logger.info(f"📦 处理批次 {batch_num}/{total_batches} ({len(batch)} 只股票)")
            
            # 使用线程池并行处理
            with ThreadPoolExecutor(max_workers=config.TS_THREAD_COUNT) as executor:
                future_to_stock = {
                    executor.submit(self._update_single_stock, ts_code, start_date, end_date): ts_code 
                    for ts_code in batch
                }
                
                batch_success = 0
                for future in as_completed(future_to_stock):
                    ts_code = future_to_stock[future]
                    try:
                        success = future.result(timeout=30)
                        if success:
                            batch_success += 1
                    except Exception as e:
                        logger.error(f"❌ 更新 {ts_code} 失败: {e}")
                
                total_updated += batch_success
                logger.info(f"✅ 批次 {batch_num} 完成: {batch_success}/{len(batch)} 成功")
            
            # 批次间短暂休息
            if batch_num < total_batches:
                import time
                time.sleep(0.5)
        
        logger.info(f"🎉 数据更新完成: {total_updated}/{len(stocks)} 成功")
    
    def _update_single_stock(self, ts_code: str, start_date: str, end_date: str) -> bool:
        """更新单只股票数据"""
        try:
            # 获取日线数据
            df = self.ts_api.daily(
                ts_code=ts_code, 
                start_date=start_date, 
                end_date=end_date
            )
            
            if not df.empty:
                # 添加数据处理字段
                df['change'] = df['close'] - df['pre_close']
                df['change_pct'] = df['pct_chg']
                df['created_at'] = datetime.now()
                
                # 插入数据库 (使用 REPLACE INTO 避免重复)
                self.db.insert_dataframe('daily_data', df, replace=True)
                return True
            else:
                logger.warning(f"⚠️ {ts_code} 无数据")
                return False
                
        except Exception as e:
            logger.error(f"❌ 更新 {ts_code} 失败: {e}")
            return False
    
    def sync_specific_stocks(self, stock_codes: list, days: int = 30):
        """同步指定股票数据"""
        logger.info(f"🎯 同步指定股票数据: {stock_codes}")
        self._parallel_update_daily_data(stock_codes, days)
    
    def get_data_summary(self):
        """获取数据概览"""
        try:
            # 股票总数
            stock_count_sql = "SELECT COUNT(*) as count FROM stock_basic WHERE list_status = 'L'"
            stock_count = self.db.fetch_data(stock_count_sql).iloc[0]['count']
            
            # 最新数据日期
            latest_date_sql = "SELECT MAX(trade_date) as latest_date FROM daily_data"
            latest_date = self.db.fetch_data(latest_date_sql).iloc[0]['latest_date']
            
            # 数据总量
            data_count_sql = "SELECT COUNT(*) as count FROM daily_data"
            data_count = self.db.fetch_data(data_count_sql).iloc[0]['count']
            
            # 今日更新股票数
            today = datetime.now().strftime('%Y-%m-%d')
            today_update_sql = f"SELECT COUNT(DISTINCT ts_code) as count FROM daily_data WHERE DATE(created_at) = '{today}'"
            today_updates = self.db.fetch_data(today_update_sql).iloc[0]['count']
            
            summary = {
                'total_stocks': stock_count,
                'latest_data_date': str(latest_date),
                'total_data_records': data_count,
                'today_updated_stocks': today_updates
            }
            
            logger.info("📊 数据概览:")
            logger.info(f"   总股票数: {summary['total_stocks']}")
            logger.info(f"   最新数据日期: {summary['latest_data_date']}")
            logger.info(f"   总数据记录: {summary['total_data_records']:,}")
            logger.info(f"   今日更新股票: {summary['today_updated_stocks']}")
            
            return summary
            
        except Exception as e:
            logger.error(f"获取数据概览失败: {e}")
            return {}

def main():
    """主函数"""
    import argparse
    
    parser = argparse.ArgumentParser(description='股票数据快速同步工具')
    parser.add_argument('--days', type=int, default=30, help='同步天数 (默认30天)')
    parser.add_argument('--stocks', nargs='*', help='指定股票代码 (可选)')
    parser.add_argument('--summary', action='store_true', help='显示数据概览')
    
    args = parser.parse_args()
    
    sync = QuickDataSync()
    
    try:
        if args.summary:
            sync.get_data_summary()
        elif args.stocks:
            sync.sync_specific_stocks(args.stocks, args.days)
        else:
            sync.sync_latest_data(args.days)
            sync.get_data_summary()
            
    except KeyboardInterrupt:
        logger.info("❌ 用户中断操作")
    except Exception as e:
        logger.error(f"❌ 同步失败: {e}")

if __name__ == "__main__":
    main()