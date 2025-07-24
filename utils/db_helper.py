import logging
from datetime import datetime, date
from typing import List, Dict, Any, Optional
from sqlalchemy import create_engine, text, MetaData, Table, Column, String, Integer, Date, DateTime, BigInteger, DECIMAL, Text, TIMESTAMP
from sqlalchemy.exc import SQLAlchemyError
from sqlalchemy.orm import sessionmaker
import pandas as pd
import config

class DatabaseHelper:
    def __init__(self):
        try:
            connection_string = f'mysql+pymysql://{config.DB_USER}:{config.DB_PASSWORD}@{config.DB_HOST}:{config.DB_PORT}/{config.DB_NAME}?charset={config.DB_CHARSET}'
            self.engine = create_engine(
                connection_string, 
                pool_size=10, 
                max_overflow=20, 
                pool_pre_ping=True, 
                pool_recycle=3600,
                echo=False  # 设置为True可查看SQL语句
            )
            self.Session = sessionmaker(bind=self.engine)
            self.logger = logging.getLogger(__name__)
            
            # 测试连接
            with self.engine.connect() as conn:
                conn.execute(text("SELECT 1"))
            self.logger.info(f"✅ 数据库连接成功: {config.DB_HOST}:{config.DB_PORT}/{config.DB_NAME}")
            
        except Exception as e:
            self.logger.error(f"❌ 数据库连接失败: {e}")
            raise

    def create_monthly_table(self, year: int, month: int) -> bool:  # 创建月度分表
        table_name = f"stock_daily_{year:04d}{month:02d}"
        
        # 检查表是否存在
        try:
            with self.engine.connect() as conn:
                result = conn.execute(text(f"SHOW TABLES LIKE '{table_name}'"))
                table_exists = result.fetchone() is not None
                
                if table_exists:
                    # 检查必需字段是否存在
                    required_fields = ['change', 'pct_chg', 'change_pct']
                    for field in required_fields:
                        result = conn.execute(text(f"SHOW COLUMNS FROM {table_name} LIKE '{field}'"))
                        field_exists = result.fetchone() is not None
                        
                        if not field_exists:
                            # 添加缺失字段
                            self.logger.info(f"🔧 为表 {table_name} 添加 {field} 字段")
                            if field == 'change':
                                conn.execute(text(f"ALTER TABLE {table_name} ADD COLUMN `{field}` DECIMAL(10,3) AFTER pre_close"))
                            elif field == 'pct_chg':
                                conn.execute(text(f"ALTER TABLE {table_name} ADD COLUMN `{field}` DECIMAL(8,4) AFTER `change`"))
                            elif field == 'change_pct':
                                conn.execute(text(f"ALTER TABLE {table_name} ADD COLUMN `{field}` DECIMAL(8,4) AFTER pct_chg"))
                            conn.commit()
                            self.logger.info(f"✅ 表 {table_name} {field} 字段添加成功")
                    return True
                else:
                    # 创建新表
                    sql = f"""CREATE TABLE {table_name} (
                        id BIGINT AUTO_INCREMENT PRIMARY KEY,
                        ts_code VARCHAR(20) NOT NULL,
                        trade_date DATE NOT NULL,
                        open DECIMAL(10,3), high DECIMAL(10,3), low DECIMAL(10,3), close DECIMAL(10,3),
                        pre_close DECIMAL(10,3), `change` DECIMAL(10,3), pct_chg DECIMAL(8,4), change_pct DECIMAL(8,4),
                        vol BIGINT, amount DECIMAL(20,3),
                        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                        UNIQUE KEY uk_code_date (ts_code, trade_date),
                        INDEX idx_code (ts_code), INDEX idx_date (trade_date)
                    ) COMMENT='股票日线数据表-{year}年{month}月'"""
                    conn.execute(text(sql))
                    conn.commit()
                    self.logger.info(f"📋 表 {table_name} 创建成功")
                    return True
                    
        except Exception as e: 
            self.logger.error(f"❌ 处理表{table_name}失败: {e}")
            return False

    def insert_batch_data(self, table_name: str, data: List[Dict]) -> int:  # 批量插入数据
        if not data: return 0
        try:
            df = pd.DataFrame(data)
            df.to_sql(table_name, con=self.engine, if_exists='append', index=False, chunksize=config.CHUNK_SIZE, method='multi')
            self.logger.debug(f"💾 批量插入 {table_name} 成功，{len(data)}条记录")
            return len(data)
        except Exception as e: 
            self.logger.error(f"❌ 批量插入{table_name}失败: {e}")
            return 0

    def get_table_name_by_date(self, trade_date: date) -> str:  # 根据日期获取表名
        return f"stock_daily_{trade_date.year:04d}{trade_date.month:02d}"

    def query_stock_data(self, ts_code: str, start_date: str = None, end_date: str = None, limit: int = None) -> pd.DataFrame:  # 查询股票数据
        try:
            if start_date and end_date:
                start_dt, end_dt = datetime.strptime(start_date, '%Y%m%d').date(), datetime.strptime(end_date, '%Y%m%d').date()
                tables = [self.get_table_name_by_date(date(y, m, 1)) for y in range(start_dt.year, end_dt.year + 1) for m in range(1, 13)]
                sql = f"SELECT * FROM ({' UNION ALL '.join([f'SELECT * FROM {t} WHERE ts_code=:ts_code AND trade_date BETWEEN :start_date AND :end_date' for t in set(tables)])}) AS combined ORDER BY trade_date {'DESC' if limit else ''}"
                if limit: sql += f" LIMIT {limit}"
                return pd.read_sql(sql, self.engine, params={'ts_code': ts_code, 'start_date': start_dt, 'end_date': end_dt})
            return pd.DataFrame()
        except Exception as e: 
            self.logger.error(f"❌ 查询数据失败: {e}")
            return pd.DataFrame()

    def get_stock_list(self) -> List[str]:  # 获取股票列表
        try:
            with self.engine.connect() as conn:
                result = conn.execute(text("SELECT ts_code FROM stock_basic WHERE list_status='L' ORDER BY ts_code"))
                stock_list = [row[0] for row in result]
                self.logger.info(f"📋 获取到 {len(stock_list)} 只股票")
                return stock_list
        except Exception as e:
            self.logger.error(f"❌ 获取股票列表失败: {e}")
            return []

    def log_operation(self, operation_type: str, operation_name: str, status: str, message: str, duration: int) -> bool:  # 记录操作日志
        try:
            with self.engine.connect() as conn:
                sql = """INSERT INTO system_log (operation_type, operation_name, status, message, duration, created_at) 
                        VALUES (:operation_type, :operation_name, :status, :message, :duration, NOW())"""
                conn.execute(text(sql), {
                    'operation_type': operation_type,
                    'operation_name': operation_name,
                    'status': status,
                    'message': message,
                    'duration': duration
                })
                conn.commit()
            return True
        except Exception as e:
            self.logger.error(f"❌ 记录日志失败: {e}")
            return False

# 全局数据库实例
db = DatabaseHelper() 