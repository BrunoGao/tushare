#!/usr/bin/env python3
"""
获取最近数据的简化脚本
"""
import logging
import tushare as ts
import pandas as pd
from datetime import datetime, timedelta
from database.db_manager import DatabaseManager
import config

# 配置日志
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)

def fetch_recent_daily_data():
    """获取最近30天的股票日线数据"""
    logger.info("🚀 开始获取最近30天股票日线数据...")
    
    try:
        # 初始化
        ts.set_token(config.TS_TOKEN)
        pro = ts.pro_api()
        db_manager = DatabaseManager()
        
        # 获取股票列表
        logger.info("📋 获取股票列表...")
        stock_df = db_manager.fetch_data("SELECT ts_code, name FROM stock_basic LIMIT 10")
        if stock_df.empty:
            logger.error("❌ 未找到股票数据")
            return False
            
        logger.info(f"📋 获取到 {len(stock_df)} 只股票")
        
        # 计算日期范围（最近30天）
        end_date = datetime.now().strftime('%Y%m%d')
        start_date = (datetime.now() - timedelta(days=30)).strftime('%Y%m%d')
        
        logger.info(f"📅 数据范围: {start_date} ~ {end_date}")
        
        # 为每只股票获取数据
        success_count = 0
        total_count = len(stock_df)
        
        for idx, row in stock_df.iterrows():
            ts_code = row['ts_code']
            name = row['name']
            
            try:
                logger.info(f"📈 [{idx+1}/{total_count}] 获取 {ts_code} ({name}) 数据...")
                
                # 获取日线数据
                daily_df = pro.daily(
                    ts_code=ts_code,
                    start_date=start_date,
                    end_date=end_date
                )
                
                if not daily_df.empty:
                    # 数据预处理
                    daily_df['trade_date'] = pd.to_datetime(daily_df['trade_date'], format='%Y%m%d')
                    
                    # 确定目标表名（使用当前月份）
                    current_month = datetime.now().strftime('%Y%m')
                    table_name = f"stock_daily_{current_month}"
                    
                    # 创建月度表（如果不存在）
                    create_table_sql = f"""
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
                        INDEX idx_date (trade_date)
                    ) COMMENT='股票日线数据表-{current_month}'
                    """
                    db_manager.execute_sql(create_table_sql)
                    
                    # 插入数据
                    db_manager.insert_dataframe(table_name, daily_df, replace=False)
                    success_count += 1
                    logger.info(f"✅ {ts_code} 数据插入成功: {len(daily_df)} 条记录")
                    
                else:
                    logger.warning(f"⚠️ {ts_code} 无数据")
                    
            except Exception as e:
                logger.error(f"❌ {ts_code} 数据获取失败: {e}")
                
            # 简单的频率控制
            import time
            time.sleep(0.1)
            
        logger.info(f"🎉 数据获取完成！成功: {success_count}/{total_count}")
        return True
        
    except Exception as e:
        logger.error(f"❌ 数据获取失败: {e}")
        return False

if __name__ == "__main__":
    success = fetch_recent_daily_data()
    exit(0 if success else 1)