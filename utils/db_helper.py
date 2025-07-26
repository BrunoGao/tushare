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

    def create_tables(self) -> bool:  # 创建所有必要的表
        """创建所有数据库表"""
        try:
            with self.engine.connect() as conn:
                # 股票基本信息表
                conn.execute(text("""
                    CREATE TABLE IF NOT EXISTS stock_basic (
                        ts_code VARCHAR(10) PRIMARY KEY COMMENT '股票代码',
                        symbol VARCHAR(10) NOT NULL COMMENT '股票简称代码',
                        name VARCHAR(20) NOT NULL COMMENT '股票名称',
                        area VARCHAR(20) COMMENT '地域',
                        industry VARCHAR(50) COMMENT '所属行业',
                        sector VARCHAR(50) COMMENT '所属板块',
                        market VARCHAR(10) COMMENT '市场类型',
                        list_date DATE COMMENT '上市日期',
                        list_status VARCHAR(2) DEFAULT 'L' COMMENT '上市状态',
                        exchange VARCHAR(10) COMMENT '交易所',
                        is_hs VARCHAR(2) COMMENT '是否沪深港通',
                        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                        updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
                        INDEX idx_industry (industry),
                        INDEX idx_sector (sector),
                        INDEX idx_list_status (list_status)
                    ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='股票基本信息表'
                """))
                
                # 技术指标汇总表
                conn.execute(text("""
                    CREATE TABLE IF NOT EXISTS stock_indicators (
                        id BIGINT PRIMARY KEY AUTO_INCREMENT,
                        ts_code VARCHAR(10) NOT NULL COMMENT '股票代码',
                        trade_date DATE NOT NULL COMMENT '交易日期',
                        close_price DECIMAL(10,2) COMMENT '收盘价',
                        ma5 DECIMAL(10,2) COMMENT '5日均线',
                        ma10 DECIMAL(10,2) COMMENT '10日均线',
                        ma20 DECIMAL(10,2) COMMENT '20日均线',
                        ma60 DECIMAL(10,2) COMMENT '60日均线',
                        ema12 DECIMAL(10,2) COMMENT '12日指数均线',
                        ema26 DECIMAL(10,2) COMMENT '26日指数均线',
                        macd DECIMAL(10,4) COMMENT 'MACD值',
                        macd_signal DECIMAL(10,4) COMMENT 'MACD信号线',
                        macd_histogram DECIMAL(10,4) COMMENT 'MACD柱状图',
                        rsi14 DECIMAL(10,2) COMMENT 'RSI14',
                        kdj_k DECIMAL(10,2) COMMENT 'KDJ-K值',
                        kdj_d DECIMAL(10,2) COMMENT 'KDJ-D值',
                        kdj_j DECIMAL(10,2) COMMENT 'KDJ-J值',
                        boll_upper DECIMAL(10,2) COMMENT '布林带上轨',
                        boll_middle DECIMAL(10,2) COMMENT '布林带中轨',
                        boll_lower DECIMAL(10,2) COMMENT '布林带下轨',
                        volume_ratio DECIMAL(10,2) COMMENT '量比',
                        strength_score DECIMAL(5,2) COMMENT '强弱度评分',
                        buy_signal TINYINT DEFAULT 0 COMMENT '买入信号',
                        sell_signal TINYINT DEFAULT 0 COMMENT '卖出信号',
                        support_price DECIMAL(10,2) COMMENT '支撑位',
                        resistance_price DECIMAL(10,2) COMMENT '阻力位',
                        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                        UNIQUE KEY uk_stock_date (ts_code, trade_date),
                        INDEX idx_trade_date (trade_date),
                        INDEX idx_strength_score (strength_score),
                        INDEX idx_signals (buy_signal, sell_signal)
                    ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='股票技术指标表'
                """))
                
                # 行业板块信息表
                conn.execute(text("""
                    CREATE TABLE IF NOT EXISTS industry_sectors (
                        id INT PRIMARY KEY AUTO_INCREMENT,
                        industry_name VARCHAR(50) NOT NULL COMMENT '行业名称',
                        sector_name VARCHAR(50) NOT NULL COMMENT '板块名称',
                        industry_code VARCHAR(20) NOT NULL COMMENT '行业代码',
                        sector_code VARCHAR(20) NOT NULL COMMENT '板块代码',
                        parent_industry VARCHAR(50) COMMENT '父行业',
                        description TEXT COMMENT '行业描述',
                        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                        UNIQUE KEY uk_industry_sector (industry_name, sector_name),
                        INDEX idx_industry_name (industry_name),
                        INDEX idx_sector_name (sector_name)
                    ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='行业板块信息表'
                """))
                
                # 实时数据表
                conn.execute(text("""
                    CREATE TABLE IF NOT EXISTS realtime_data (
                        ts_code VARCHAR(10) NOT NULL COMMENT '股票代码',
                        current_price DECIMAL(10,2) COMMENT '当前价',
                        change_amount DECIMAL(10,2) COMMENT '涨跌额',
                        change_pct DECIMAL(10,2) COMMENT '涨跌幅',
                        volume BIGINT COMMENT '成交量',
                        amount DECIMAL(15,2) COMMENT '成交额',
                        turnover_rate DECIMAL(10,2) COMMENT '换手率',
                        pe_ratio DECIMAL(10,2) COMMENT '市盈率',
                        pb_ratio DECIMAL(10,2) COMMENT '市净率',
                        total_mv DECIMAL(15,2) COMMENT '总市值',
                        circ_mv DECIMAL(15,2) COMMENT '流通市值',
                        update_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
                        PRIMARY KEY (ts_code),
                        INDEX idx_update_time (update_time),
                        INDEX idx_change_pct (change_pct)
                    ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='实时数据表'
                """))
                
                # 交易信号记录表
                conn.execute(text("""
                    CREATE TABLE IF NOT EXISTS trading_signals (
                        id BIGINT PRIMARY KEY AUTO_INCREMENT,
                        ts_code VARCHAR(10) NOT NULL COMMENT '股票代码',
                        signal_type ENUM('buy', 'sell', 'hold') NOT NULL COMMENT '信号类型',
                        signal_strength ENUM('weak', 'moderate', 'strong') DEFAULT 'moderate' COMMENT '信号强度',
                        trigger_conditions JSON COMMENT '触发条件',
                        price DECIMAL(10,2) COMMENT '信号价格',
                        volume BIGINT COMMENT '成交量',
                        confidence_score DECIMAL(5,2) COMMENT '置信度',
                        signal_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                        INDEX idx_ts_code (ts_code),
                        INDEX idx_signal_type (signal_type),
                        INDEX idx_signal_time (signal_time),
                        INDEX idx_confidence (confidence_score)
                    ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='交易信号记录表'
                """))
                
                conn.commit()
                self.logger.info("✅ 所有数据库表创建完成")
                return True
                
        except Exception as e:
            self.logger.error(f"❌ 创建数据库表失败: {e}")
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
                # 生成需要查询的表名
                tables = []
                current_date = start_dt.replace(day=1)
                while current_date <= end_dt:
                    table_name = f"stock_daily_{current_date.year:04d}{current_date.month:02d}"
                    tables.append(table_name)
                    # 下个月
                    if current_date.month == 12:
                        current_date = current_date.replace(year=current_date.year + 1, month=1)
                    else:
                        current_date = current_date.replace(month=current_date.month + 1)
                
                # 检查表是否存在并构建查询
                valid_tables = []
                with self.engine.connect() as conn:
                    for table in tables:
                        try:
                            conn.execute(text(f"SELECT 1 FROM {table} LIMIT 1"))
                            valid_tables.append(table)
                        except:
                            pass
                
                if not valid_tables:
                    return pd.DataFrame()
                
                # 构建单个查询（简化方法）
                if len(valid_tables) == 1:
                    sql = f"SELECT * FROM {valid_tables[0]} WHERE ts_code = %s AND trade_date BETWEEN %s AND %s ORDER BY trade_date"
                    if limit:
                        sql += f" LIMIT {limit}"
                    return pd.read_sql(sql, self.engine, params=(ts_code, start_dt, end_dt))
                else:
                    # 多表查询，分别查询然后合并
                    all_data = []
                    for table in valid_tables:
                        sql = f"SELECT * FROM {table} WHERE ts_code = %s AND trade_date BETWEEN %s AND %s"
                        df = pd.read_sql(sql, self.engine, params=(ts_code, start_dt, end_dt))
                        if not df.empty:
                            all_data.append(df)
                    
                    if all_data:
                        result_df = pd.concat(all_data, ignore_index=True)
                        result_df = result_df.sort_values('trade_date')
                        if limit:
                            result_df = result_df.head(limit)
                        return result_df
                    return pd.DataFrame()
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

    def upsert_dataframe(self, df, table_name: str, unique_cols: List[str], 
                        update_cols: List[str] = None) -> int:
        """批量插入或更新DataFrame数据
        
        Args:
            df: 要插入的DataFrame
            table_name: 目标表名
            unique_cols: 唯一键列名列表，用于判断是否重复
            update_cols: 需要更新的列名列表，为None时更新除unique_cols外的所有列
            
        Returns:
            int: 成功处理的记录数
        """
        if df.empty:
            return 0
            
        try:
            # 如果update_cols为None，则更新除unique_cols外的所有列
            if update_cols is None:
                update_cols = [col for col in df.columns if col not in unique_cols]
                
            # 构建ON DUPLICATE KEY UPDATE语句
            update_statements = []
            for col in update_cols:
                update_statements.append(f"`{col}` = VALUES(`{col}`)")
                
            # 生成列名列表（加上反引号防止关键字冲突）
            columns = [f"`{col}`" for col in df.columns]
            columns_str = ", ".join(columns)
            
            # 生成占位符
            placeholders = ", ".join(["%s"] * len(df.columns))
            
            # 构建完整的SQL语句
            sql = f"""
                INSERT INTO {table_name} ({columns_str})
                VALUES ({placeholders})
                ON DUPLICATE KEY UPDATE
                {", ".join(update_statements)}
            """
            
            # 准备数据
            data_tuples = []
            for _, row in df.iterrows():
                # 处理NaN值，转换为None
                row_data = []
                for value in row:
                    if pd.isna(value):
                        row_data.append(None)
                    else:
                        row_data.append(value)
                data_tuples.append(tuple(row_data))
            
            # 批量执行
            with self.engine.connect() as conn:
                cursor = conn.connection.cursor()
                cursor.executemany(sql, data_tuples)
                affected_rows = cursor.rowcount
                conn.commit()
                cursor.close()
                
            self.logger.info(f"✅ {table_name} 批量upsert完成: {affected_rows} 条记录")
            return affected_rows
            
        except Exception as e:
            self.logger.error(f"❌ {table_name} 批量upsert失败: {e}")
            return 0
            
    def execute_sql(self, sql: str, params: Dict = None) -> bool:
        """执行SQL语句
        
        Args:
            sql: SQL语句
            params: 参数字典
            
        Returns:
            bool: 执行是否成功
        """
        try:
            with self.engine.connect() as conn:
                if params:
                    conn.execute(text(sql), params)
                else:
                    conn.execute(text(sql))
                conn.commit()
            return True
        except Exception as e:
            self.logger.error(f"❌ 执行SQL失败: {e}")
            return False

# 全局数据库实例
db = DatabaseHelper() 