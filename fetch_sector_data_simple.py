#!/usr/bin/env python3
"""
简化版板块数据抓取脚本
优化性能，避免超时问题
"""

import os
import sys
import time
import json
import pandas as pd
import pymysql
from datetime import datetime
from typing import Dict, List, Optional
import logging
import tushare as ts

# 添加项目根目录到路径
sys.path.append(os.path.dirname(os.path.abspath(__file__)))
from config import Config

# 配置日志
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler('logs/sector_simple_fetch.log'),
        logging.StreamHandler()
    ]
)
logger = logging.getLogger(__name__)

class SimpleSectorDataFetcher:
    """简化版板块数据抓取器"""
    
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
        self.progress_file = 'logs/sector_simple_progress.json'
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
            'industry_classify_simple': """
                CREATE TABLE IF NOT EXISTS industry_classify_simple (
                    id INT AUTO_INCREMENT PRIMARY KEY,
                    ts_code VARCHAR(20) NOT NULL COMMENT '股票代码',
                    name VARCHAR(100) COMMENT '股票名称',
                    industry VARCHAR(100) COMMENT '所属行业',
                    area VARCHAR(50) COMMENT '地域',
                    market VARCHAR(20) COMMENT '市场',
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
                    UNIQUE KEY uk_ts_code (ts_code),
                    INDEX idx_industry (industry),
                    INDEX idx_area (area)
                ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='简化行业分类表'
            """,
            
            'concept_classify_simple': """
                CREATE TABLE IF NOT EXISTS concept_classify_simple (
                    id INT AUTO_INCREMENT PRIMARY KEY,
                    concept_code VARCHAR(20) NOT NULL COMMENT '概念代码',
                    concept_name VARCHAR(100) NOT NULL COMMENT '概念名称',
                    stock_count INT DEFAULT 0 COMMENT '成分股数量',
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    UNIQUE KEY uk_concept_code (concept_code),
                    INDEX idx_concept_name (concept_name)
                ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='简化概念分类表'
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
    
    def fetch_industry_classify_simple(self):
        """简化版行业分类数据抓取"""
        logger.info("🚀 开始抓取行业分类数据(简化版)...")
        
        try:
            conn = self.get_db_connection()
            
            try:
                with conn.cursor() as cursor:
                    # 直接从stock_basic表获取数据
                    cursor.execute("""
                        SELECT ts_code, name, industry, area, market 
                        FROM stock_basic 
                        WHERE ts_code IS NOT NULL AND name IS NOT NULL
                    """)
                    
                    stock_data = cursor.fetchall()
                    
                    if not stock_data:
                        logger.warning("⚠️ 未获取到股票基本信息")
                        return 0
                    
                    # 清空现有数据
                    cursor.execute("DELETE FROM industry_classify_simple")
                    
                    # 批量插入数据
                    insert_sql = """
                        INSERT INTO industry_classify_simple 
                        (ts_code, name, industry, area, market)
                        VALUES (%s, %s, %s, %s, %s)
                    """
                    
                    records = []
                    for row in stock_data:
                        ts_code, name, industry, area, market = row
                        records.append((
                            ts_code,
                            name,
                            industry or '',
                            area or '',
                            market or ''
                        ))
                    
                    if records:
                        cursor.executemany(insert_sql, records)
                        conn.commit()
                        
                        record_count = len(records)
                        self.stats['total_records'] += record_count
                        logger.info(f"✅ 行业分类数据抓取完成: {record_count}条记录")
                        return record_count
                    else:
                        logger.warning("⚠️ 没有数据需要插入")
                        return 0
                    
            except Exception as e:
                logger.error(f"❌ 行业分类数据处理失败: {e}")
                conn.rollback()
                return 0
            finally:
                conn.close()
                
        except Exception as e:
            logger.error(f"❌ 抓取行业分类数据失败: {e}")
            self.stats['failed_operations'].append({
                'operation': 'fetch_industry_classify_simple',
                'error': str(e),
                'time': datetime.now().isoformat()
            })
            return 0
    
    def fetch_concept_classify_simple(self):
        """简化版概念分类数据抓取"""
        logger.info("🚀 开始抓取概念分类数据(简化版)...")
        
        try:
            # 只获取概念列表，不获取成分股详情
            df = self.ts_pro.concept()
            
            if df.empty:
                logger.warning("⚠️ 未获取到概念分类数据")
                return 0
            
            conn = self.get_db_connection()
            try:
                with conn.cursor() as cursor:
                    # 清空现有数据
                    cursor.execute("DELETE FROM concept_classify_simple")
                    
                    # 批量插入数据
                    insert_sql = """
                        INSERT INTO concept_classify_simple 
                        (concept_code, concept_name, stock_count)
                        VALUES (%s, %s, %s)
                    """
                    
                    records = []
                    for _, row in df.iterrows():
                        records.append((
                            row.get('code', ''),
                            row.get('name', ''),
                            0  # 暂时设为0，后续可以单独更新
                        ))
                    
                    cursor.executemany(insert_sql, records)
                    conn.commit()
                    
                    record_count = len(records)
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
                'operation': 'fetch_concept_classify_simple',
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
        """执行简化版板块数据抓取"""
        logger.info("🎯 开始简化版板块数据抓取任务...")
        
        try:
            # 创建数据表
            self.create_tables()
            
            # 按优先级抓取数据
            tasks = [
                ('行业分类(简化)', self.fetch_industry_classify_simple),
                ('概念分类(简化)', self.fetch_concept_classify_simple)
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
            
            logger.info("🎉 简化版板块数据抓取任务完成!")
            logger.info(f"📊 处理表数: {self.stats['processed_tables']}")
            logger.info(f"📈 总记录数: {self.stats['total_records']:,}")
            logger.info(f"⏱️ 耗时: {self.stats['duration']:.2f}秒")
            
            if self.stats['failed_operations']:
                logger.warning(f"⚠️ 失败操作数: {len(self.stats['failed_operations'])}")
            
            self.save_progress()
            
        except Exception as e:
            logger.error(f"❌ 简化版板块数据抓取任务失败: {e}")
            self.stats['error'] = str(e)
            self.save_progress()
            raise

def main():
    """主函数"""
    try:
        fetcher = SimpleSectorDataFetcher()
        fetcher.run()
    except KeyboardInterrupt:
        logger.info("👋 用户中断程序")
    except Exception as e:
        logger.error(f"❌ 程序执行失败: {e}")
        return 1
    return 0

if __name__ == "__main__":
    exit(main())