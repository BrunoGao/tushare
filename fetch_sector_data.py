#!/usr/bin/env python3
"""
板块数据抓取脚本
包括行业分类、概念板块、地域分类等数据
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

# 添加项目根目录到路径
sys.path.append(os.path.dirname(os.path.abspath(__file__)))
from config import Config

# 配置日志
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler('logs/sector_data_fetch.log'),
        logging.StreamHandler()
    ]
)
logger = logging.getLogger(__name__)

class SectorDataFetcher:
    """板块数据抓取器"""
    
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
        self.progress_file = 'logs/sector_fetch_progress.json'
        self.stats = {
            'start_time': datetime.now().isoformat(),
            'processed_tables': 0,
            'total_records': 0,
            'failed_operations': []
        }
        
    def get_db_connection(self):
        """获取数据库连接"""
        return pymysql.connect(**self.db_config)
    
    def create_tables(self):
        """创建所需的数据表"""
        tables_sql = {
            'industry_classify': """
                CREATE TABLE IF NOT EXISTS industry_classify (
                    id INT AUTO_INCREMENT PRIMARY KEY,
                    ts_code VARCHAR(20) NOT NULL COMMENT '股票代码',
                    symbol VARCHAR(20) COMMENT '股票简称',
                    name VARCHAR(100) COMMENT '股票名称',
                    industry VARCHAR(100) COMMENT '所属行业',
                    industry_code VARCHAR(20) COMMENT '行业代码',
                    level VARCHAR(10) COMMENT '行业级别',
                    src VARCHAR(10) COMMENT '数据源',
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
                    INDEX idx_ts_code (ts_code),
                    INDEX idx_industry (industry),
                    INDEX idx_industry_code (industry_code)
                ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='行业分类表'
            """,
            
            'concept_classify': """
                CREATE TABLE IF NOT EXISTS concept_classify (
                    id INT AUTO_INCREMENT PRIMARY KEY,
                    ts_code VARCHAR(20) NOT NULL COMMENT '股票代码',
                    name VARCHAR(100) COMMENT '股票名称',
                    concept_name VARCHAR(100) COMMENT '概念名称',
                    concept_code VARCHAR(20) COMMENT '概念代码',
                    src VARCHAR(10) COMMENT '数据源',
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
                    INDEX idx_ts_code (ts_code),
                    INDEX idx_concept (concept_name),
                    INDEX idx_concept_code (concept_code)
                ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='概念分类表'
            """,
            
            'area_classify': """
                CREATE TABLE IF NOT EXISTS area_classify (
                    id INT AUTO_INCREMENT PRIMARY KEY,
                    ts_code VARCHAR(20) NOT NULL COMMENT '股票代码',
                    name VARCHAR(100) COMMENT '股票名称',
                    area VARCHAR(50) COMMENT '所属地域',
                    province VARCHAR(50) COMMENT '省份',
                    city VARCHAR(50) COMMENT '城市',
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
                    INDEX idx_ts_code (ts_code),
                    INDEX idx_area (area),
                    INDEX idx_province (province)
                ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='地域分类表'
            """,
            
            'market_segment': """
                CREATE TABLE IF NOT EXISTS market_segment (
                    id INT AUTO_INCREMENT PRIMARY KEY,
                    ts_code VARCHAR(20) NOT NULL COMMENT '股票代码',
                    name VARCHAR(100) COMMENT '股票名称',
                    market VARCHAR(20) COMMENT '市场类型',
                    segment VARCHAR(50) COMMENT '市场板块',
                    is_hs VARCHAR(1) COMMENT '是否沪深港通',
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
                    INDEX idx_ts_code (ts_code),
                    INDEX idx_market (market),
                    INDEX idx_segment (segment)
                ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='市场板块表'
            """
        }
        
        conn = self.get_db_connection()
        try:
            with conn.cursor() as cursor:
                for table_name, sql in tables_sql.items():
                    cursor.execute(sql)
                    logger.info(f"✅ 创建/检查表: {table_name}")
                conn.commit()
                logger.info("🎯 所有板块相关表创建完成")
        except Exception as e:
            logger.error(f"❌ 创建表失败: {e}")
            raise
        finally:
            conn.close()
    
    def fetch_industry_classify(self):
        """抓取行业分类数据"""
        logger.info("🚀 开始抓取行业分类数据...")
        
        try:
            # 获取行业分类
            df = self.ts_pro.stock_company(
                exchange='',
                fields='ts_code,chairman,manager,secretary,reg_capital,setup_date,province'
            )
            
            if df.empty:
                logger.warning("⚠️ 未获取到行业分类数据")
                return 0
            
            # 获取详细行业信息
            industry_df = self.ts_pro.hs_const(hs_type='A')
            
            conn = self.get_db_connection()
            try:
                with conn.cursor() as cursor:
                    # 清空现有数据
                    cursor.execute("DELETE FROM industry_classify")
                    
                    # 批量插入数据
                    insert_sql = """
                        INSERT INTO industry_classify 
                        (ts_code, symbol, name, industry, industry_code, level, src)
                        VALUES (%s, %s, %s, %s, %s, %s, %s)
                    """
                    
                    records = []
                    for _, row in industry_df.iterrows():
                        records.append((
                            row.get('ts_code', ''),
                            row.get('symbol', ''),
                            row.get('name', ''),
                            row.get('industry', ''),
                            row.get('industry_code', ''),
                            row.get('level', ''),
                            'tushare'
                        ))
                    
                    cursor.executemany(insert_sql, records)
                    conn.commit()
                    
                    record_count = len(records)
                    self.stats['total_records'] += record_count
                    logger.info(f"✅ 行业分类数据抓取完成: {record_count}条记录")
                    return record_count
                    
            except Exception as e:
                logger.error(f"❌ 行业分类数据插入失败: {e}")
                conn.rollback()
                return 0
            finally:
                conn.close()
                
        except Exception as e:
            logger.error(f"❌ 抓取行业分类数据失败: {e}")
            self.stats['failed_operations'].append({
                'operation': 'fetch_industry_classify',
                'error': str(e),
                'time': datetime.now().isoformat()
            })
            return 0
    
    def fetch_concept_classify(self):
        """抓取概念分类数据"""
        logger.info("🚀 开始抓取概念分类数据...")
        
        try:
            # 获取概念分类
            df = self.ts_pro.concept()
            
            if df.empty:
                logger.warning("⚠️ 未获取到概念分类数据")
                return 0
            
            # 获取概念成分股
            concept_detail_records = []
            
            for _, concept_row in df.iterrows():
                try:
                    concept_code = concept_row['code']
                    concept_name = concept_row['name']
                    
                    # 获取该概念的成分股
                    detail_df = self.ts_pro.concept_detail(id=concept_code)
                    time.sleep(0.2)  # API限频
                    
                    for _, detail_row in detail_df.iterrows():
                        concept_detail_records.append((
                            detail_row.get('ts_code', ''),
                            detail_row.get('name', ''),
                            concept_name,
                            concept_code,
                            'tushare'
                        ))
                        
                except Exception as e:
                    logger.warning(f"⚠️ 获取概念{concept_name}成分股失败: {e}")
                    continue
            
            conn = self.get_db_connection()
            try:
                with conn.cursor() as cursor:
                    # 清空现有数据
                    cursor.execute("DELETE FROM concept_classify")
                    
                    # 批量插入数据
                    insert_sql = """
                        INSERT INTO concept_classify 
                        (ts_code, name, concept_name, concept_code, src)
                        VALUES (%s, %s, %s, %s, %s)
                    """
                    
                    cursor.executemany(insert_sql, concept_detail_records)
                    conn.commit()
                    
                    record_count = len(concept_detail_records)
                    self.stats['total_records'] += record_count
                    logger.info(f"✅ 概念分类数据抓取完成: {record_count}条记录")
                    return record_count
                    
            except Exception as e:
                logger.error(f"❌ 概念分类数据插入失败: {e}")
                conn.rollback()
                return 0
            finally:
                conn.close()
                
        except Exception as e:
            logger.error(f"❌ 抓取概念分类数据失败: {e}")
            self.stats['failed_operations'].append({
                'operation': 'fetch_concept_classify',
                'error': str(e),
                'time': datetime.now().isoformat()
            })
            return 0
    
    def fetch_area_classify(self):
        """抓取地域分类数据"""
        logger.info("🚀 开始抓取地域分类数据...")
        
        try:
            # 直接使用已有的股票基本信息，避免重复调用API
            conn = self.get_db_connection()
            
            try:
                with conn.cursor() as cursor:
                    # 从stock_basic表获取地域信息
                    cursor.execute("""
                        SELECT ts_code, name, area, industry 
                        FROM stock_basic 
                        WHERE list_status = 'L'
                    """)
                    
                    stock_data = cursor.fetchall()
                    
                    if not stock_data:
                        logger.warning("⚠️ 未获取到股票基本信息")
                        return 0
                    
                    # 清空现有数据
                    cursor.execute("DELETE FROM area_classify")
                    
                    # 批量插入数据
                    insert_sql = """
                        INSERT INTO area_classify 
                        (ts_code, name, area, province, city)
                        VALUES (%s, %s, %s, %s, %s)
                    """
                    
                    records = []
                    for row in stock_data:
                        ts_code, name, area, industry = row
                        records.append((
                            ts_code,
                            name,
                            area or '',  # 地域
                            area or '',  # 省份 (使用area字段)
                            ''  # 城市 (暂时为空)
                        ))
                    
                    if records:
                        cursor.executemany(insert_sql, records)
                        conn.commit()
                        
                        record_count = len(records)
                        self.stats['total_records'] += record_count
                        logger.info(f"✅ 地域分类数据抓取完成: {record_count}条记录")
                        return record_count
                    else:
                        logger.warning("⚠️ 没有数据需要插入")
                        return 0
                    
            except Exception as e:
                logger.error(f"❌ 地域分类数据插入失败: {e}")
                conn.rollback()
                return 0
            finally:
                conn.close()
                
        except Exception as e:
            logger.error(f"❌ 抓取地域分类数据失败: {e}")
            self.stats['failed_operations'].append({
                'operation': 'fetch_area_classify',
                'error': str(e),
                'time': datetime.now().isoformat()
            })
            return 0
    
    def fetch_market_segment(self):
        """抓取市场板块数据"""
        logger.info("🚀 开始抓取市场板块数据...")
        
        try:
            # 获取沪深港通成分股
            hsgt_df = self.ts_pro.hs_const(hs_type='SH')  # 沪股通
            hk_df = self.ts_pro.hs_const(hs_type='SZ')    # 深股通
            
            # 获取股票基本信息
            basic_df = self.ts_pro.stock_basic(
                exchange='',
                list_status='L',
                fields='ts_code,symbol,name,area,industry,market,list_date'
            )
            
            conn = self.get_db_connection()
            try:
                with conn.cursor() as cursor:
                    # 清空现有数据
                    cursor.execute("DELETE FROM market_segment")
                    
                    # 批量插入数据
                    insert_sql = """
                        INSERT INTO market_segment 
                        (ts_code, name, market, segment, is_hs)
                        VALUES (%s, %s, %s, %s, %s)
                    """
                    
                    records = []
                    hsgt_codes = set(hsgt_df['ts_code'].tolist() + hk_df['ts_code'].tolist())
                    
                    for _, row in basic_df.iterrows():
                        ts_code = row.get('ts_code', '')
                        is_hs = 'Y' if ts_code in hsgt_codes else 'N'
                        
                        records.append((
                            ts_code,
                            row.get('name', ''),
                            row.get('market', ''),
                            row.get('industry', ''),  # 使用行业作为板块
                            is_hs
                        ))
                    
                    cursor.executemany(insert_sql, records)
                    conn.commit()
                    
                    record_count = len(records)
                    self.stats['total_records'] += record_count
                    logger.info(f"✅ 市场板块数据抓取完成: {record_count}条记录")
                    return record_count
                    
            except Exception as e:
                logger.error(f"❌ 市场板块数据插入失败: {e}")
                conn.rollback()
                return 0
            finally:
                conn.close()
                
        except Exception as e:
            logger.error(f"❌ 抓取市场板块数据失败: {e}")
            self.stats['failed_operations'].append({
                'operation': 'fetch_market_segment',
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
    
    def run(self):
        """执行板块数据抓取"""
        logger.info("🎯 开始板块数据抓取任务...")
        
        try:
            # 创建数据表
            self.create_tables()
            
            # 按优先级抓取数据
            tasks = [
                ('行业分类', self.fetch_industry_classify),
                ('概念分类', self.fetch_concept_classify),
                ('地域分类', self.fetch_area_classify),
                ('市场板块', self.fetch_market_segment)
            ]
            
            for task_name, task_func in tasks:
                logger.info(f"📊 正在处理: {task_name}")
                
                try:
                    record_count = task_func()
                    self.stats['processed_tables'] += 1
                    logger.info(f"✅ {task_name}完成: {record_count}条记录")
                    
                    # 保存进度
                    self.save_progress()
                    
                    # API限频
                    time.sleep(1)
                    
                except Exception as e:
                    logger.error(f"❌ {task_name}失败: {e}")
                    self.stats['failed_operations'].append({
                        'task': task_name,
                        'error': str(e),
                        'time': datetime.now().isoformat()
                    })
            
            # 最终统计
            self.stats['end_time'] = datetime.now().isoformat()
            self.stats['duration'] = (datetime.now() - datetime.fromisoformat(self.stats['start_time'])).total_seconds()
            
            logger.info("🎉 板块数据抓取任务完成!")
            logger.info(f"📊 处理表数: {self.stats['processed_tables']}")
            logger.info(f"📈 总记录数: {self.stats['total_records']:,}")
            logger.info(f"⏱️ 耗时: {self.stats['duration']:.2f}秒")
            
            if self.stats['failed_operations']:
                logger.warning(f"⚠️ 失败操作数: {len(self.stats['failed_operations'])}")
            
            self.save_progress()
            
        except Exception as e:
            logger.error(f"❌ 板块数据抓取任务失败: {e}")
            self.stats['error'] = str(e)
            self.save_progress()
            raise

def main():
    """主函数"""
    try:
        fetcher = SectorDataFetcher()
        fetcher.run()
    except KeyboardInterrupt:
        logger.info("👋 用户中断程序")
    except Exception as e:
        logger.error(f"❌ 程序执行失败: {e}")
        return 1
    return 0

if __name__ == "__main__":
    exit(main())