#!/usr/bin/env python3
"""
股东数据抓取脚本
包括股东人数、前十大股东、前十大流通股东、股东增减持等数据
"""

import os
import sys
import time
import json
import pandas as pd
import pymysql
from datetime import datetime, timedelta
from typing import Dict, List, Optional
import logging
from concurrent.futures import ThreadPoolExecutor, as_completed
import tushare as ts
import threading

# 添加项目根目录到路径
sys.path.append(os.path.dirname(os.path.abspath(__file__)))
from config import Config

# 配置日志
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler('logs/holder_data_fetch.log'),
        logging.StreamHandler()
    ]
)
logger = logging.getLogger(__name__)

class HolderDataFetcher:
    """股东数据抓取器"""
    
    def __init__(self):
        self.ts_pro = ts.pro_api(Config.TS_TOKEN)
        self.db_config = {
            'host': Config.DB_HOST,
            'port': Config.DB_PORT,
            'user': Config.DB_USER,
            'password': Config.DB_PASSWORD,
            'database': Config.DB_NAME,
            'charset': 'utf8mb4'
        }
        self.progress_file = 'logs/holder_fetch_progress.json'
        self.stats = {
            'start_time': datetime.now().isoformat(),
            'processed_tables': 0,
            'total_records': 0,
            'failed_operations': [],
            'processed_stocks': []
        }
        self.lock = threading.Lock()
        
    def get_db_connection(self):
        """获取数据库连接"""
        return pymysql.connect(**self.db_config)
    
    def create_tables(self):
        """创建所需的数据表"""
        tables_sql = {
            'holder_number': """
                CREATE TABLE IF NOT EXISTS holder_number (
                    id INT AUTO_INCREMENT PRIMARY KEY,
                    ts_code VARCHAR(20) NOT NULL COMMENT '股票代码',
                    ann_date DATE COMMENT '公告日期',
                    end_date DATE COMMENT '截止日期',
                    holder_num INT COMMENT '股东总数',
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    UNIQUE KEY uk_code_date (ts_code, end_date),
                    INDEX idx_ts_code (ts_code),
                    INDEX idx_end_date (end_date),
                    INDEX idx_holder_num (holder_num)
                ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='股东人数表'
            """,
            
            'top10_holders': """
                CREATE TABLE IF NOT EXISTS top10_holders (
                    id INT AUTO_INCREMENT PRIMARY KEY,
                    ts_code VARCHAR(20) NOT NULL COMMENT '股票代码',
                    ann_date DATE COMMENT '公告日期',
                    end_date DATE COMMENT '截止日期',
                    holder_name VARCHAR(200) COMMENT '股东名称',
                    hold_amount DECIMAL(20,2) COMMENT '持有数量(万股)',
                    hold_ratio DECIMAL(10,6) COMMENT '持有比例(%)',
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    INDEX idx_ts_code (ts_code),
                    INDEX idx_end_date (end_date),
                    INDEX idx_holder_name (holder_name),
                    INDEX idx_hold_ratio (hold_ratio)
                ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='前十大股东表'
            """,
            
            'top10_floatholders': """
                CREATE TABLE IF NOT EXISTS top10_floatholders (
                    id INT AUTO_INCREMENT PRIMARY KEY,
                    ts_code VARCHAR(20) NOT NULL COMMENT '股票代码',
                    ann_date DATE COMMENT '公告日期',
                    end_date DATE COMMENT '截止日期',
                    holder_name VARCHAR(200) COMMENT '股东名称',
                    hold_amount DECIMAL(20,2) COMMENT '持有数量(万股)',
                    hold_ratio DECIMAL(10,6) COMMENT '持有比例(%)',
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    INDEX idx_ts_code (ts_code),
                    INDEX idx_end_date (end_date),
                    INDEX idx_holder_name (holder_name),
                    INDEX idx_hold_ratio (hold_ratio)
                ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='前十大流通股东表'
            """,
            
            'holder_trade': """
                CREATE TABLE IF NOT EXISTS holder_trade (
                    id INT AUTO_INCREMENT PRIMARY KEY,
                    ts_code VARCHAR(20) NOT NULL COMMENT '股票代码',
                    ann_date DATE COMMENT '公告日期',
                    holder_name VARCHAR(200) COMMENT '股东名称',
                    holder_type VARCHAR(20) COMMENT '股东类型',
                    in_de VARCHAR(2) COMMENT '类型:增持/减持',
                    change_vol DECIMAL(20,2) COMMENT '变动数量(万股)',
                    change_ratio DECIMAL(10,6) COMMENT '占总股本比例(%)',
                    after_share DECIMAL(20,2) COMMENT '变动后持股数(万股)',
                    after_ratio DECIMAL(10,6) COMMENT '变动后占总股本比例(%)',
                    avg_price DECIMAL(10,3) COMMENT '平均价格',
                    total_share DECIMAL(20,2) COMMENT '总股本(万股)',
                    begin_date DATE COMMENT '变动开始日期',
                    close_date DATE COMMENT '变动结束日期',
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    INDEX idx_ts_code (ts_code),
                    INDEX idx_ann_date (ann_date),
                    INDEX idx_holder_name (holder_name),
                    INDEX idx_in_de (in_de)
                ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='股东增减持表'
            """
        }
        
        conn = self.get_db_connection()
        try:
            with conn.cursor() as cursor:
                for table_name, sql in tables_sql.items():
                    cursor.execute(sql)
                    logger.info(f"✅ 创建/检查表: {table_name}")
                conn.commit()
                logger.info("🎯 所有股东相关表创建完成")
        except Exception as e:
            logger.error(f"❌ 创建表失败: {e}")
            raise
        finally:
            conn.close()
    
    def get_stock_list(self) -> List[str]:
        """获取股票列表"""
        try:
            conn = self.get_db_connection()
            with conn.cursor() as cursor:
                cursor.execute("SELECT ts_code FROM stock_basic WHERE ts_code IS NOT NULL LIMIT 100")  # 限制数量以避免超时
                results = cursor.fetchall()
                return [row[0] for row in results]
        except Exception as e:
            logger.error(f"❌ 获取股票列表失败: {e}")
            return []
        finally:
            conn.close()
    
    def fetch_holder_number(self, ts_code: str) -> int:
        """抓取单只股票的股东人数数据"""
        try:
            # 获取最近一年的股东人数数据
            df = self.ts_pro.stk_holdernumber(
                ts_code=ts_code,
                start_date=(datetime.now() - timedelta(days=365)).strftime('%Y%m%d'),
                end_date=datetime.now().strftime('%Y%m%d')
            )
            
            if df.empty:
                return 0
            
            conn = self.get_db_connection()
            try:
                insert_sql = """
                    INSERT IGNORE INTO holder_number 
                    (ts_code, ann_date, end_date, holder_num)
                    VALUES (%s, %s, %s, %s)
                """
                
                records = []
                for _, row in df.iterrows():
                    records.append((
                        ts_code,
                        row.get('ann_date'),
                        row.get('end_date'),
                        row.get('holder_num')
                    ))
                
                if records:
                    with conn.cursor() as cursor:
                        cursor.executemany(insert_sql, records)
                        conn.commit()
                    return len(records)
                
                return 0
                
            except Exception as e:
                logger.warning(f"⚠️ {ts_code} 股东人数数据插入失败: {e}")
                conn.rollback()
                return 0
            finally:
                conn.close()
                
        except Exception as e:
            logger.warning(f"⚠️ {ts_code} 股东人数数据抓取失败: {e}")
            return 0
    
    def fetch_top10_holders(self, ts_code: str) -> int:
        """抓取单只股票的前十大股东数据"""
        try:
            # 获取最近一年的前十大股东数据
            df = self.ts_pro.top10_holders(
                ts_code=ts_code,
                start_date=(datetime.now() - timedelta(days=365)).strftime('%Y%m%d'),
                end_date=datetime.now().strftime('%Y%m%d')
            )
            
            if df.empty:
                return 0
            
            conn = self.get_db_connection()
            try:
                insert_sql = """
                    INSERT IGNORE INTO top10_holders 
                    (ts_code, ann_date, end_date, holder_name, hold_amount, hold_ratio)
                    VALUES (%s, %s, %s, %s, %s, %s)
                """
                
                records = []
                for _, row in df.iterrows():
                    records.append((
                        ts_code,
                        row.get('ann_date'),
                        row.get('end_date'),
                        row.get('holder_name'),
                        row.get('hold_amount'),
                        row.get('hold_ratio')
                    ))
                
                if records:
                    with conn.cursor() as cursor:
                        cursor.executemany(insert_sql, records)
                        conn.commit()
                    return len(records)
                
                return 0
                
            except Exception as e:
                logger.warning(f"⚠️ {ts_code} 前十大股东数据插入失败: {e}")
                conn.rollback()
                return 0
            finally:
                conn.close()
                
        except Exception as e:
            logger.warning(f"⚠️ {ts_code} 前十大股东数据抓取失败: {e}")
            return 0
    
    def fetch_top10_floatholders(self, ts_code: str) -> int:
        """抓取单只股票的前十大流通股东数据"""
        try:
            # 获取最近一年的前十大流通股东数据
            df = self.ts_pro.top10_floatholders(
                ts_code=ts_code,
                start_date=(datetime.now() - timedelta(days=365)).strftime('%Y%m%d'),
                end_date=datetime.now().strftime('%Y%m%d')
            )
            
            if df.empty:
                return 0
            
            conn = self.get_db_connection()
            try:
                insert_sql = """
                    INSERT IGNORE INTO top10_floatholders 
                    (ts_code, ann_date, end_date, holder_name, hold_amount, hold_ratio)
                    VALUES (%s, %s, %s, %s, %s, %s)
                """
                
                records = []
                for _, row in df.iterrows():
                    records.append((
                        ts_code,
                        row.get('ann_date'),
                        row.get('end_date'),
                        row.get('holder_name'),
                        row.get('hold_amount'),
                        row.get('hold_ratio')
                    ))
                
                if records:
                    with conn.cursor() as cursor:
                        cursor.executemany(insert_sql, records)
                        conn.commit()
                    return len(records)
                
                return 0
                
            except Exception as e:
                logger.warning(f"⚠️ {ts_code} 前十大流通股东数据插入失败: {e}")
                conn.rollback()
                return 0
            finally:
                conn.close()
                
        except Exception as e:
            logger.warning(f"⚠️ {ts_code} 前十大流通股东数据抓取失败: {e}")
            return 0
    
    def fetch_holder_trade(self, ts_code: str) -> int:
        """抓取单只股票的股东增减持数据"""
        try:
            # 获取最近一年的股东增减持数据
            df = self.ts_pro.stk_holdertrade(
                ts_code=ts_code,
                start_date=(datetime.now() - timedelta(days=365)).strftime('%Y%m%d'),
                end_date=datetime.now().strftime('%Y%m%d')
            )
            
            if df.empty:
                return 0
            
            conn = self.get_db_connection()
            try:
                insert_sql = """
                    INSERT IGNORE INTO holder_trade 
                    (ts_code, ann_date, holder_name, holder_type, in_de, change_vol, 
                     change_ratio, after_share, after_ratio, avg_price, total_share, 
                     begin_date, close_date)
                    VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s)
                """
                
                records = []
                for _, row in df.iterrows():
                    records.append((
                        ts_code,
                        row.get('ann_date'),
                        row.get('holder_name'),
                        row.get('holder_type'),
                        row.get('in_de'),
                        row.get('change_vol'),
                        row.get('change_ratio'),
                        row.get('after_share'),
                        row.get('after_ratio'),
                        row.get('avg_price'),
                        row.get('total_share'),
                        row.get('begin_date'),
                        row.get('close_date')
                    ))
                
                if records:
                    with conn.cursor() as cursor:
                        cursor.executemany(insert_sql, records)
                        conn.commit()
                    return len(records)
                
                return 0
                
            except Exception as e:
                logger.warning(f"⚠️ {ts_code} 股东增减持数据插入失败: {e}")
                conn.rollback()
                return 0
            finally:
                conn.close()
                
        except Exception as e:
            logger.warning(f"⚠️ {ts_code} 股东增减持数据抓取失败: {e}")
            return 0
    
    def process_single_stock(self, ts_code: str) -> int:
        """处理单只股票的所有股东数据"""
        logger.info(f"📊 处理股票: {ts_code}")
        
        total_records = 0
        
        try:
            # 抓取股东人数
            total_records += self.fetch_holder_number(ts_code)
            time.sleep(0.2)  # API限频
            
            # 抓取前十大股东
            total_records += self.fetch_top10_holders(ts_code)
            time.sleep(0.2)
            
            # 抓取前十大流通股东
            total_records += self.fetch_top10_floatholders(ts_code)
            time.sleep(0.2)
            
            # 抓取股东增减持
            total_records += self.fetch_holder_trade(ts_code)
            time.sleep(0.2)
            
            with self.lock:
                self.stats['total_records'] += total_records
                self.stats['processed_stocks'].append(ts_code)
            
            if total_records > 0:
                logger.info(f"✅ {ts_code} 完成: {total_records}条记录")
            
            return total_records
            
        except Exception as e:
            logger.error(f"❌ {ts_code} 处理失败: {e}")
            with self.lock:
                self.stats['failed_operations'].append({
                    'ts_code': ts_code,
                    'error': str(e),
                    'time': datetime.now().isoformat()
                })
            return 0
    
    def save_progress(self):
        """保存进度"""
        try:
            os.makedirs('logs', exist_ok=True)
            with open(self.progress_file, 'w') as f:
                json.dump(self.stats, f, indent=2, ensure_ascii=False)
        except Exception as e:
            logger.error(f"❌ 保存进度失败: {e}")
    
    def run(self, max_stocks: int = 100):
        """执行股东数据抓取"""
        logger.info("🎯 开始股东数据抓取任务...")
        
        try:
            # 创建数据表
            self.create_tables()
            
            # 获取股票列表
            stock_codes = self.get_stock_list()[:max_stocks]  # 限制处理数量
            logger.info(f"📊 将处理 {len(stock_codes)} 只股票")
            
            # 使用线程池处理
            with ThreadPoolExecutor(max_workers=3) as executor:
                futures = []
                
                for ts_code in stock_codes:
                    future = executor.submit(self.process_single_stock, ts_code)
                    futures.append(future)
                
                # 等待完成并收集结果
                for i, future in enumerate(as_completed(futures), 1):
                    try:
                        result = future.result()
                        if i % 10 == 0:  # 每10只股票保存一次进度
                            self.save_progress()
                            logger.info(f"📈 进度: {i}/{len(stock_codes)} ({i/len(stock_codes)*100:.1f}%)")
                    
                    except Exception as e:
                        logger.error(f"❌ 任务执行失败: {e}")
            
            # 最终统计
            self.stats['end_time'] = datetime.now().isoformat()
            self.stats['duration'] = (datetime.now() - datetime.fromisoformat(self.stats['start_time'])).total_seconds()
            
            logger.info("🎉 股东数据抓取任务完成!")
            logger.info(f"📊 处理股票数: {len(self.stats['processed_stocks'])}")
            logger.info(f"📈 总记录数: {self.stats['total_records']:,}")
            logger.info(f"⏱️ 耗时: {self.stats['duration']:.2f}秒")
            
            if self.stats['failed_operations']:
                logger.warning(f"⚠️ 失败操作数: {len(self.stats['failed_operations'])}")
            
            self.save_progress()
            
        except Exception as e:
            logger.error(f"❌ 股东数据抓取任务失败: {e}")
            self.stats['error'] = str(e)
            self.save_progress()
            raise

def main():
    """主函数"""
    import argparse
    
    parser = argparse.ArgumentParser(description='股东数据抓取')
    parser.add_argument('--max-stocks', type=int, default=100, help='最大处理股票数量')
    
    args = parser.parse_args()
    
    try:
        fetcher = HolderDataFetcher()
        fetcher.run(args.max_stocks)
    except KeyboardInterrupt:
        logger.info("👋 用户中断程序")
    except Exception as e:
        logger.error(f"❌ 程序执行失败: {e}")
        return 1
    return 0

if __name__ == "__main__":
    exit(main())