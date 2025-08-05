#!/usr/bin/env python3
"""
资金流向数据抓取脚本
包括个股资金流、板块资金流、沪深港通资金流、融资融券等数据
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
        logging.FileHandler('logs/moneyflow_data_fetch.log'),
        logging.StreamHandler()
    ]
)
logger = logging.getLogger(__name__)

class MoneyflowDataFetcher:
    """资金流向数据抓取器"""
    
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
        self.progress_file = 'logs/moneyflow_fetch_progress.json'
        self.stats = {
            'start_time': datetime.now().isoformat(),
            'processed_tables': 0,
            'total_records': 0,
            'failed_operations': [],
            'processed_dates': []
        }
        self.lock = threading.Lock()
        
    def get_db_connection(self):
        """获取数据库连接"""
        return pymysql.connect(**self.db_config)
    
    def create_tables(self):
        """创建所需的数据表"""
        tables_sql = {
            'stock_moneyflow': """
                CREATE TABLE IF NOT EXISTS stock_moneyflow (
                    id INT AUTO_INCREMENT PRIMARY KEY,
                    ts_code VARCHAR(20) NOT NULL COMMENT '股票代码',
                    trade_date DATE NOT NULL COMMENT '交易日期',
                    buy_sm_amount DECIMAL(20,2) COMMENT '小单买入金额(万元)',
                    buy_sm_vol INT COMMENT '小单买入量(手)',
                    sell_sm_amount DECIMAL(20,2) COMMENT '小单卖出金额(万元)',
                    sell_sm_vol INT COMMENT '小单卖出量(手)',
                    buy_md_amount DECIMAL(20,2) COMMENT '中单买入金额(万元)',
                    buy_md_vol INT COMMENT '中单买入量(手)',
                    sell_md_amount DECIMAL(20,2) COMMENT '中单卖出金额(万元)',
                    sell_md_vol INT COMMENT '中单卖出量(手)',
                    buy_lg_amount DECIMAL(20,2) COMMENT '大单买入金额(万元)',
                    buy_lg_vol INT COMMENT '大单买入量(手)',
                    sell_lg_amount DECIMAL(20,2) COMMENT '大单卖出金额(万元)',
                    sell_lg_vol INT COMMENT '大单卖出量(手)',
                    buy_elg_amount DECIMAL(20,2) COMMENT '特大单买入金额(万元)',
                    buy_elg_vol INT COMMENT '特大单买入量(手)',
                    sell_elg_amount DECIMAL(20,2) COMMENT '特大单卖出金额(万元)',
                    sell_elg_vol INT COMMENT '特大单卖出量(手)',
                    net_mf_amount DECIMAL(20,2) COMMENT '净流入金额(万元)',
                    net_mf_vol INT COMMENT '净流入量(手)',
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    UNIQUE KEY uk_code_date (ts_code, trade_date),
                    INDEX idx_trade_date (trade_date),
                    INDEX idx_ts_code (ts_code),
                    INDEX idx_net_mf (net_mf_amount)
                ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='个股资金流向表'
            """,
            
            'sector_moneyflow': """
                CREATE TABLE IF NOT EXISTS sector_moneyflow (
                    id INT AUTO_INCREMENT PRIMARY KEY,
                    trade_date DATE NOT NULL COMMENT '交易日期',
                    sector_name VARCHAR(100) NOT NULL COMMENT '板块名称',
                    sector_code VARCHAR(20) COMMENT '板块代码',
                    buy_amount DECIMAL(20,2) COMMENT '买入金额(万元)',
                    sell_amount DECIMAL(20,2) COMMENT '卖出金额(万元)',
                    net_amount DECIMAL(20,2) COMMENT '净流入金额(万元)',
                    buy_vol INT COMMENT '买入量(手)',
                    sell_vol INT COMMENT '卖出量(手)',
                    net_vol INT COMMENT '净流入量(手)',
                    stocks_count INT COMMENT '成分股数量',
                    rise_count INT COMMENT '上涨家数',
                    fall_count INT COMMENT '下跌家数',
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    UNIQUE KEY uk_sector_date (sector_name, trade_date),
                    INDEX idx_trade_date (trade_date),
                    INDEX idx_sector (sector_name),
                    INDEX idx_net_amount (net_amount)
                ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='板块资金流向表'
            """,
            
            'hsgt_flow': """
                CREATE TABLE IF NOT EXISTS hsgt_flow (
                    id INT AUTO_INCREMENT PRIMARY KEY,
                    trade_date DATE NOT NULL COMMENT '交易日期',
                    ggt_ss DECIMAL(20,2) COMMENT '港股通（上海）当日成交金额(万元)',
                    ggt_sz DECIMAL(20,2) COMMENT '港股通（深圳）当日成交金额(万元)',
                    hgt DECIMAL(20,2) COMMENT '沪股通当日成交金额(万元)',
                    sgt DECIMAL(20,2) COMMENT '深股通当日成交金额(万元)',
                    north_money DECIMAL(20,2) COMMENT '北向资金成交金额(万元)',
                    south_money DECIMAL(20,2) COMMENT '南向资金成交金额(万元)',
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    UNIQUE KEY uk_trade_date (trade_date),
                    INDEX idx_north_money (north_money),
                    INDEX idx_south_money (south_money)
                ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='沪深港通资金流向表'
            """,
            
            'margin_detail': """
                CREATE TABLE IF NOT EXISTS margin_detail (
                    id INT AUTO_INCREMENT PRIMARY KEY,
                    trade_date DATE NOT NULL COMMENT '交易日期',
                    ts_code VARCHAR(20) NOT NULL COMMENT '股票代码',
                    name VARCHAR(100) COMMENT '股票名称',
                    rzye DECIMAL(20,2) COMMENT '融资余额(万元)',
                    rzmre DECIMAL(20,2) COMMENT '融资买入额(万元)',
                    rzche DECIMAL(20,2) COMMENT '融资偿还额(万元)',
                    rqye DECIMAL(20,2) COMMENT '融券余额(万元)',
                    rqmcl INT COMMENT '融券卖出量(股)',
                    rzrqye DECIMAL(20,2) COMMENT '融资融券余额(万元)',
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    UNIQUE KEY uk_code_date (ts_code, trade_date),
                    INDEX idx_trade_date (trade_date),
                    INDEX idx_ts_code (ts_code),
                    INDEX idx_rzrqye (rzrqye)
                ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='融资融券明细表'
            """
        }
        
        conn = self.get_db_connection()
        try:
            with conn.cursor() as cursor:
                for table_name, sql in tables_sql.items():
                    cursor.execute(sql)
                    logger.info(f"✅ 创建/检查表: {table_name}")
                conn.commit()
                logger.info("🎯 所有资金流相关表创建完成")
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
                cursor.execute("SELECT ts_code FROM stock_basic WHERE ts_code IS NOT NULL")
                results = cursor.fetchall()
                return [row[0] for row in results]
        except Exception as e:
            logger.error(f"❌ 获取股票列表失败: {e}")
            return []
        finally:
            conn.close()
    
    def fetch_stock_moneyflow_batch(self, date_str: str, stock_codes: List[str]) -> int:
        """批量抓取个股资金流数据"""
        logger.info(f"📊 抓取日期 {date_str} 的个股资金流数据, 股票数: {len(stock_codes)}")
        
        success_count = 0
        conn = self.get_db_connection()
        
        try:
            insert_sql = """
                INSERT IGNORE INTO stock_moneyflow 
                (ts_code, trade_date, buy_sm_amount, buy_sm_vol, sell_sm_amount, sell_sm_vol,
                 buy_md_amount, buy_md_vol, sell_md_amount, sell_md_vol,
                 buy_lg_amount, buy_lg_vol, sell_lg_amount, sell_lg_vol,
                 buy_elg_amount, buy_elg_vol, sell_elg_amount, sell_elg_vol,
                 net_mf_amount, net_mf_vol)
                VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s)
            """
            
            # 分批处理股票
            batch_size = 100
            for i in range(0, len(stock_codes), batch_size):
                batch_codes = stock_codes[i:i+batch_size]
                
                try:
                    # 获取资金流数据
                    df = self.ts_pro.moneyflow(
                        trade_date=date_str,
                        start_date=date_str,
                        end_date=date_str
                    )
                    
                    if not df.empty:
                        # 过滤出当前批次的股票
                        batch_df = df[df['ts_code'].isin(batch_codes)]
                        
                        records = []
                        for _, row in batch_df.iterrows():
                            records.append((
                                row.get('ts_code'),
                                date_str,
                                row.get('buy_sm_amount'),
                                row.get('buy_sm_vol'),
                                row.get('sell_sm_amount'),
                                row.get('sell_sm_vol'),
                                row.get('buy_md_amount'),
                                row.get('buy_md_vol'),
                                row.get('sell_md_amount'),
                                row.get('sell_md_vol'),
                                row.get('buy_lg_amount'),
                                row.get('buy_lg_vol'),
                                row.get('sell_lg_amount'),
                                row.get('sell_lg_vol'),
                                row.get('buy_elg_amount'),
                                row.get('buy_elg_vol'),
                                row.get('sell_elg_amount'),
                                row.get('sell_elg_vol'),
                                row.get('net_mf_amount'),
                                row.get('net_mf_vol')
                            ))
                        
                        if records:
                            with conn.cursor() as cursor:
                                cursor.executemany(insert_sql, records)
                                conn.commit()
                                success_count += len(records)
                    
                    # API限频
                    time.sleep(0.5)
                    
                except Exception as e:
                    logger.warning(f"⚠️ 批次 {i//batch_size + 1} 抓取失败: {e}")
                    continue
            
            with self.lock:
                self.stats['total_records'] += success_count
            
            logger.info(f"✅ {date_str} 个股资金流数据抓取完成: {success_count}条记录")
            return success_count
            
        except Exception as e:
            logger.error(f"❌ 抓取 {date_str} 个股资金流数据失败: {e}")
            return 0
        finally:
            conn.close()
    
    def fetch_hsgt_flow(self, start_date: str, end_date: str) -> int:
        """抓取沪深港通资金流数据"""
        logger.info(f"🚀 抓取沪深港通资金流数据: {start_date} ~ {end_date}")
        
        try:
            # 获取沪深港通资金流
            df = self.ts_pro.hsgt_top10(
                trade_date='',
                start_date=start_date,
                end_date=end_date,
                market_type='1'  # 1-沪市 2-深市 3-港股通(沪) 4-港股通(深)
            )
            
            if df.empty:
                logger.warning("⚠️ 未获取到沪深港通资金流数据")
                return 0
            
            conn = self.get_db_connection()
            try:
                insert_sql = """
                    INSERT IGNORE INTO hsgt_flow 
                    (trade_date, ggt_ss, ggt_sz, hgt, sgt, north_money, south_money)
                    VALUES (%s, %s, %s, %s, %s, %s, %s)
                """
                
                # 按日期汇总数据
                daily_flow = df.groupby('trade_date').agg({
                    'buy_amount': 'sum',
                    'sell_amount': 'sum'
                }).reset_index()
                
                records = []
                for _, row in daily_flow.iterrows():
                    records.append((
                        row['trade_date'],
                        0,  # ggt_ss - 需要从其他接口获取
                        0,  # ggt_sz - 需要从其他接口获取
                        row.get('buy_amount', 0),  # hgt近似
                        0,  # sgt - 需要从其他接口获取
                        row.get('buy_amount', 0),  # north_money
                        row.get('sell_amount', 0)   # south_money
                    ))
                
                with conn.cursor() as cursor:
                    cursor.executemany(insert_sql, records)
                    conn.commit()
                    
                record_count = len(records)
                self.stats['total_records'] += record_count
                logger.info(f"✅ 沪深港通资金流数据抓取完成: {record_count}条记录")
                return record_count
                
            except Exception as e:
                logger.error(f"❌ 沪深港通资金流数据插入失败: {e}")
                conn.rollback()
                return 0
            finally:
                conn.close()
                
        except Exception as e:
            logger.error(f"❌ 抓取沪深港通资金流数据失败: {e}")
            self.stats['failed_operations'].append({
                'operation': 'fetch_hsgt_flow',
                'error': str(e),
                'time': datetime.now().isoformat()
            })
            return 0
    
    def fetch_margin_detail(self, trade_date: str) -> int:
        """抓取融资融券数据"""
        logger.info(f"🚀 抓取融资融券数据: {trade_date}")
        
        try:
            # 获取融资融券数据
            df = self.ts_pro.margin(trade_date=trade_date)
            
            if df.empty:
                logger.warning(f"⚠️ {trade_date} 未获取到融资融券数据")
                return 0
            
            conn = self.get_db_connection()
            try:
                insert_sql = """
                    INSERT IGNORE INTO margin_detail 
                    (trade_date, ts_code, name, rzye, rzmre, rzche, rqye, rqmcl, rzrqye)
                    VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s)
                """
                
                records = []
                for _, row in df.iterrows():
                    records.append((
                        trade_date,
                        row.get('ts_code'),
                        row.get('name'),
                        row.get('rzye'),
                        row.get('rzmre'),
                        row.get('rzche'),
                        row.get('rqye'),
                        row.get('rqmcl'),
                        row.get('rzrqye')
                    ))
                
                with conn.cursor() as cursor:
                    cursor.executemany(insert_sql, records)
                    conn.commit()
                    
                record_count = len(records)
                self.stats['total_records'] += record_count
                logger.info(f"✅ {trade_date} 融资融券数据抓取完成: {record_count}条记录")
                return record_count
                
            except Exception as e:
                logger.error(f"❌ {trade_date} 融资融券数据插入失败: {e}")
                conn.rollback()
                return 0
            finally:
                conn.close()
                
        except Exception as e:
            logger.error(f"❌ 抓取 {trade_date} 融资融券数据失败: {e}")
            self.stats['failed_operations'].append({
                'operation': f'fetch_margin_detail_{trade_date}',
                'error': str(e),
                'time': datetime.now().isoformat()
            })
            return 0
    
    def get_trade_dates(self, start_date: str, end_date: str) -> List[str]:
        """获取交易日期列表"""
        try:
            df = self.ts_pro.trade_cal(
                exchange='SSE',
                start_date=start_date,
                end_date=end_date,
                is_open='1'
            )
            return df['cal_date'].tolist()
        except Exception as e:
            logger.error(f"❌ 获取交易日期失败: {e}")
            return []
    
    def save_progress(self):
        """保存进度"""
        try:
            os.makedirs('logs', exist_ok=True)
            with open(self.progress_file, 'w') as f:
                json.dump(self.stats, f, indent=2, ensure_ascii=False)
        except Exception as e:
            logger.error(f"❌ 保存进度失败: {e}")
    
    def run(self, start_date: str = None, end_date: str = None):
        """执行资金流数据抓取"""
        logger.info("🎯 开始资金流数据抓取任务...")
        
        # 默认抓取最近30天的数据
        if not start_date:
            start_date = (datetime.now() - timedelta(days=30)).strftime('%Y%m%d')
        if not end_date:
            end_date = datetime.now().strftime('%Y%m%d')
        
        logger.info(f"📅 抓取时间范围: {start_date} ~ {end_date}")
        
        try:
            # 创建数据表
            self.create_tables()
            
            # 获取股票列表
            stock_codes = self.get_stock_list()
            logger.info(f"📊 获取到 {len(stock_codes)} 只股票")
            
            # 获取交易日期
            trade_dates = self.get_trade_dates(start_date, end_date)
            logger.info(f"📅 获取到 {len(trade_dates)} 个交易日")
            
            # 并发抓取数据
            with ThreadPoolExecutor(max_workers=3) as executor:
                futures = []
                
                # 提交个股资金流任务
                for trade_date in trade_dates[-10:]:  # 最近10个交易日
                    future = executor.submit(
                        self.fetch_stock_moneyflow_batch,
                        trade_date,
                        stock_codes
                    )
                    futures.append(future)
                
                # 提交融资融券任务
                for trade_date in trade_dates[-5:]:  # 最近5个交易日
                    future = executor.submit(
                        self.fetch_margin_detail,
                        trade_date
                    )
                    futures.append(future)
                
                # 等待所有任务完成
                for future in as_completed(futures):
                    try:
                        result = future.result()
                        self.save_progress()
                    except Exception as e:
                        logger.error(f"❌ 任务执行失败: {e}")
            
            # 抓取沪深港通资金流
            self.fetch_hsgt_flow(start_date, end_date)
            
            # 最终统计
            self.stats['end_time'] = datetime.now().isoformat()
            self.stats['duration'] = (datetime.now() - datetime.fromisoformat(self.stats['start_time'])).total_seconds()
            
            logger.info("🎉 资金流数据抓取任务完成!")
            logger.info(f"📊 处理表数: {self.stats['processed_tables']}")
            logger.info(f"📈 总记录数: {self.stats['total_records']:,}")
            logger.info(f"⏱️ 耗时: {self.stats['duration']:.2f}秒")
            
            if self.stats['failed_operations']:
                logger.warning(f"⚠️ 失败操作数: {len(self.stats['failed_operations'])}")
            
            self.save_progress()
            
        except Exception as e:
            logger.error(f"❌ 资金流数据抓取任务失败: {e}")
            self.stats['error'] = str(e)
            self.save_progress()
            raise

def main():
    """主函数"""
    import argparse
    
    parser = argparse.ArgumentParser(description='资金流数据抓取')
    parser.add_argument('--start-date', help='开始日期 (YYYYMMDD)')
    parser.add_argument('--end-date', help='结束日期 (YYYYMMDD)')
    
    args = parser.parse_args()
    
    try:
        fetcher = MoneyflowDataFetcher()
        fetcher.run(args.start_date, args.end_date)
    except KeyboardInterrupt:
        logger.info("👋 用户中断程序")
    except Exception as e:
        logger.error(f"❌ 程序执行失败: {e}")
        return 1
    return 0

if __name__ == "__main__":
    exit(main())