from utils.db_helper import DatabaseHelper
import pandas as pd
from sqlalchemy import text

class DatabaseManager(DatabaseHelper):
    """数据库管理器 - 继承自DatabaseHelper，提供额外的便捷方法"""
    
    def __init__(self):
        super().__init__()
        
    def fetch_data(self, sql):
        """执行SQL查询并返回DataFrame"""
        try:
            with self.engine.connect() as conn:
                return pd.read_sql(sql, conn)
        except Exception as e:
            self.logger.error(f"查询失败: {e}")
            return pd.DataFrame()
            
    def execute_sql(self, sql):
        """执行SQL语句"""
        try:
            with self.engine.connect() as conn:
                result = conn.execute(text(sql))
                conn.commit()
                return result
        except Exception as e:
            self.logger.error(f"执行失败: {e}")
            return None
    
    def insert_dataframe(self, table_name: str, df: pd.DataFrame, replace: bool = False):
        """将DataFrame插入数据库表"""
        try:
            if df.empty:
                self.logger.warning(f"DataFrame为空，跳过插入 {table_name}")
                return False
                
            # 确定插入方式
            if_exists = 'replace' if replace else 'append'
            
            # 插入数据
            df.to_sql(
                name=table_name,
                con=self.engine,
                if_exists=if_exists,
                index=False,
                method='multi'
            )
            
            self.logger.info(f"成功插入 {len(df)} 条记录到 {table_name}")
            return True
            
        except Exception as e:
            self.logger.error(f"插入数据到 {table_name} 失败: {e}")
            return False
    
    def test_connection(self) -> bool:
        """测试数据库连接"""
        try:
            with self.engine.connect() as conn:
                conn.execute(text("SELECT 1"))
                return True
        except Exception as e:
            self.logger.error(f"数据库连接测试失败: {e}")
            return False 