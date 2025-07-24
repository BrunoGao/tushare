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
        self.engine = create_engine(f'mysql+pymysql://{config.DB_USER}:{config.DB_PASSWORD}@{config.DB_HOST}:{config.DB_PORT}/{config.DB_NAME}?charset={config.DB_CHARSET}', 
                                   pool_size=10, max_overflow=20, pool_pre_ping=True, pool_recycle=3600)
        self.Session = sessionmaker(bind=self.engine)
        self.logger = logging.getLogger(__name__)

    def create_monthly_table(self, year: int, month: int) -> bool:  # 创建月度分表
        table_name = f"stock_daily_{year:04d}{month:02d}"
        sql = f"""CREATE TABLE IF NOT EXISTS {table_name} (
            id BIGINT AUTO_INCREMENT PRIMARY KEY,
            ts_code VARCHAR(20) NOT NULL,
            trade_date DATE NOT NULL,
            open DECIMAL(10,3), high DECIMAL(10,3), low DECIMAL(10,3), close DECIMAL(10,3),
            pre_close DECIMAL(10,3), change_pct DECIMAL(8,4), vol BIGINT, amount DECIMAL(20,3),
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            UNIQUE KEY uk_code_date (ts_code, trade_date),
            INDEX idx_code (ts_code), INDEX idx_date (trade_date)
        ) COMMENT='股票日线数据表-{year}年{month}月'"""
        try:
            with self.engine.connect() as conn: conn.execute(text(sql)); conn.commit()
            return True
        except Exception as e: self.logger.error(f"创建表{table_name}失败: {e}"); return False

    def insert_batch_data(self, table_name: str, data: List[Dict]) -> int:  # 批量插入数据
        if not data: return 0
        try:
            df = pd.DataFrame(data)
            df.to_sql(table_name, con=self.engine, if_exists='append', index=False, chunksize=config.CHUNK_SIZE, method='multi')
            return len(data)
        except Exception as e: self.logger.error(f"批量插入{table_name}失败: {e}"); return 0

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
        except Exception as e: self.logger.error(f"查询数据失败: {e}"); return pd.DataFrame()

    def log_operation(self, module: str, operation: str, status: str, message: str = None, duration: int = None):  # 记录操作日志
        try:
            with self.engine.connect() as conn:
                sql = "INSERT INTO system_log (module, operation, status, message, duration) VALUES (:module, :operation, :status, :message, :duration)"
                conn.execute(text(sql), {'module': module, 'operation': operation, 'status': status, 'message': message, 'duration': duration})
                conn.commit()
        except Exception as e: self.logger.error(f"记录日志失败: {e}")

    def get_stock_list(self, exchange: str = None) -> List[str]:  # 获取股票列表
        try:
            sql = "SELECT ts_code FROM stock_basic WHERE list_status='L'"
            if exchange: sql += f" AND exchange='{exchange}'"
            with self.engine.connect() as conn:
                result = conn.execute(text(sql))
                return [row[0] for row in result]
        except Exception as e: self.logger.error(f"获取股票列表失败: {e}"); return []

    def close(self): self.engine.dispose()  # 关闭数据库连接

db = DatabaseHelper()  # 全局数据库实例 