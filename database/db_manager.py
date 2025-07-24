from utils.db_helper import DatabaseHelper
import pandas as pd
from sqlalchemy import text

class DatabaseManager(DatabaseHelper):
    """数据库管理器 - 继承自DatabaseHelper，提供额外的便捷方法"""
    
    def __init__(self):
        super().__init__()
        
    def fetch_data(self,sql):
        """执行SQL查询并返回DataFrame"""
        try:
            with self.engine.connect() as conn:
                return pd.read_sql(sql,conn)
        except Exception as e:
            self.logger.error(f"查询失败: {e}")
            return pd.DataFrame()
            
    def execute_sql(self,sql):
        """执行SQL语句"""
        try:
            with self.engine.connect() as conn:
                result=conn.execute(text(sql))
                conn.commit()
                return result
        except Exception as e:
            self.logger.error(f"执行失败: {e}")
            return None 