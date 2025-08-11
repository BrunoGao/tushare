#!/usr/bin/env python3
"""
数据库初始化脚本
根据优化方案创建所有必要的表结构和索引
"""

import mysql.connector
import sqlite3
import os
import logging
from datetime import datetime
from typing import Dict, Any

# 设置日志
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

class DatabaseInitializer:
    """数据库初始化器"""
    
    def __init__(self, config: Dict[str, Any]):
        self.config = config
        self.db_type = config.get('type', 'mysql')  # mysql 或 sqlite
        
    def initialize_database(self):
        """初始化完整的数据库结构"""
        logger.info("开始初始化数据库...")
        
        try:
            if self.db_type == 'mysql':
                self._init_mysql_database()
            else:
                self._init_sqlite_database()
                
            logger.info("数据库初始化完成！")
            
        except Exception as e:
            logger.error(f"数据库初始化失败: {e}")
            raise
    
    def _init_mysql_database(self):
        """初始化MySQL数据库"""
        # 连接MySQL
        connection = mysql.connector.connect(
            host=self.config['host'],
            port=self.config['port'],
            user=self.config['user'],
            password=self.config['password']
        )
        cursor = connection.cursor()
        
        try:
            # 创建数据库
            database_name = self.config['database']
            cursor.execute(f"CREATE DATABASE IF NOT EXISTS {database_name} CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci")
            cursor.execute(f"USE {database_name}")
            
            # 创建所有表
            self._create_mysql_tables(cursor)
            
            # 创建索引
            self._create_mysql_indexes(cursor)
            
            # 创建分区（如果需要）
            self._create_mysql_partitions(cursor)
            
            connection.commit()
            logger.info("MySQL数据库初始化完成")
            
        finally:
            cursor.close()
            connection.close()
    
    def _create_mysql_tables(self, cursor):
        """创建MySQL表结构"""
        
        # 股票基础信息表（支持A股和港股）
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS stock_basic (
                stock_code VARCHAR(10) PRIMARY KEY,
                stock_name VARCHAR(100) NOT NULL,
                industry VARCHAR(50),
                sector VARCHAR(50),
                market VARCHAR(10),
                stock_type VARCHAR(10) DEFAULT 'A股' COMMENT '股票类型：A股/港股',
                area VARCHAR(20) COMMENT '所属地区',
                list_date DATE,
                delist_date DATE,
                is_active BOOLEAN DEFAULT TRUE,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP
            ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='股票基础信息表（含A股港股）'
        """)
        
        # 日线行情数据表（分区表）
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS stock_daily (
                id BIGINT AUTO_INCREMENT PRIMARY KEY,
                stock_code VARCHAR(10) NOT NULL,
                trade_date DATE NOT NULL,
                open_price DECIMAL(10,3),
                high_price DECIMAL(10,3),
                low_price DECIMAL(10,3),
                close_price DECIMAL(10,3),
                pre_close DECIMAL(10,3),
                change_amt DECIMAL(10,3),
                change_pct DECIMAL(6,3),
                volume BIGINT,
                amount DECIMAL(15,2),
                turnover_rate DECIMAL(6,3),
                volume_ratio DECIMAL(6,3),
                pe DECIMAL(8,2),
                pb DECIMAL(6,3),
                total_share BIGINT,
                float_share BIGINT,
                free_share BIGINT,
                total_mv DECIMAL(15,2),
                circ_mv DECIMAL(15,2),
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                UNIQUE KEY uk_stock_date (stock_code, trade_date)
            ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
        """)
        
        # 技术指标缓存表
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS technical_indicators (
                id BIGINT AUTO_INCREMENT PRIMARY KEY,
                stock_code VARCHAR(10) NOT NULL,
                trade_date DATE NOT NULL,
                -- 移动平均线
                ma5 DECIMAL(10,3),
                ma10 DECIMAL(10,3),
                ma20 DECIMAL(10,3),
                ma30 DECIMAL(10,3),
                ma60 DECIMAL(10,3),
                ma120 DECIMAL(10,3),
                ma250 DECIMAL(10,3),
                -- RSI指标
                rsi6 DECIMAL(6,2),
                rsi12 DECIMAL(6,2),
                rsi24 DECIMAL(6,2),
                -- MACD指标
                macd_dif DECIMAL(8,4),
                macd_dea DECIMAL(8,4),
                macd_macd DECIMAL(8,4),
                -- KDJ指标
                kdj_k DECIMAL(6,2),
                kdj_d DECIMAL(6,2),
                kdj_j DECIMAL(6,2),
                -- 布林带
                boll_upper DECIMAL(10,3),
                boll_mid DECIMAL(10,3),
                boll_lower DECIMAL(10,3),
                -- 其他指标
                atr DECIMAL(8,4),
                cci DECIMAL(8,2),
                williams_r DECIMAL(6,2),
                obv BIGINT,
                vwap DECIMAL(10,3),
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
                UNIQUE KEY uk_stock_date (stock_code, trade_date)
            ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
        """)
        
        # 财务数据表
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS financial_data (
                id BIGINT AUTO_INCREMENT PRIMARY KEY,
                stock_code VARCHAR(10) NOT NULL,
                end_date DATE NOT NULL,
                ann_date DATE,
                report_type VARCHAR(10),
                -- 资产负债表
                total_assets DECIMAL(15,2),
                total_hldr_eqy_exc_min_int DECIMAL(15,2),
                total_liab DECIMAL(15,2),
                monetary_cap DECIMAL(15,2),
                accounts_receiv DECIMAL(15,2),
                inventories DECIMAL(15,2),
                fixed_assets DECIMAL(15,2),
                -- 利润表
                revenue DECIMAL(15,2),
                operate_profit DECIMAL(15,2),
                total_profit DECIMAL(15,2),
                n_income DECIMAL(15,2),
                n_income_attr_p DECIMAL(15,2),
                ebit DECIMAL(15,2),
                ebitda DECIMAL(15,2),
                -- 现金流量表
                c_fr_sale_sg DECIMAL(15,2),
                c_paid_goods_s DECIMAL(15,2),
                n_cashflow_act DECIMAL(15,2),
                n_cashflow_inv_act DECIMAL(15,2),
                n_cashflow_fin_act DECIMAL(15,2),
                c_cash_equ_end_period DECIMAL(15,2),
                -- 财务比率
                roe DECIMAL(6,3),
                roa DECIMAL(6,3),
                gross_margin DECIMAL(6,3),
                net_margin DECIMAL(6,3),
                debt_to_assets DECIMAL(6,3),
                current_ratio DECIMAL(6,3),
                quick_ratio DECIMAL(6,3),
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
                UNIQUE KEY uk_stock_enddate (stock_code, end_date)
            ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
        """)
        
        # 策略信号表
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS strategy_signals (
                id BIGINT AUTO_INCREMENT PRIMARY KEY,
                strategy_id VARCHAR(50) NOT NULL,
                stock_code VARCHAR(10) NOT NULL,
                signal_date DATE NOT NULL,
                signal_type ENUM('BUY', 'SELL', 'HOLD') NOT NULL,
                signal_strength DECIMAL(3,2),
                signal_price DECIMAL(10,3),
                -- 信号详情
                trigger_conditions JSON,
                indicator_values JSON,
                risk_metrics JSON,
                -- 执行状态
                execution_status ENUM('PENDING', 'EXECUTED', 'CANCELLED', 'EXPIRED') DEFAULT 'PENDING',
                execution_price DECIMAL(10,3),
                execution_time TIMESTAMP NULL,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP
            ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
        """)
        
        # 回测结果表
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS backtest_results (
                id VARCHAR(50) PRIMARY KEY,
                strategy_id VARCHAR(50) NOT NULL,
                stock_code VARCHAR(10),
                start_date DATE NOT NULL,
                end_date DATE NOT NULL,
                -- 基础参数
                initial_capital DECIMAL(15,2),
                commission DECIMAL(6,4),
                slippage DECIMAL(6,4),
                -- 收益指标
                total_return DECIMAL(8,4),
                annual_return DECIMAL(8,4),
                excess_return DECIMAL(8,4),
                -- 风险指标
                volatility DECIMAL(8,4),
                max_drawdown DECIMAL(8,4),
                sharpe_ratio DECIMAL(6,3),
                sortino_ratio DECIMAL(6,3),
                calmar_ratio DECIMAL(6,3),
                -- 交易统计
                total_trades INT,
                win_trades INT,
                lose_trades INT,
                win_rate DECIMAL(5,3),
                avg_win DECIMAL(8,4),
                avg_lose DECIMAL(8,4),
                profit_factor DECIMAL(6,3),
                -- 持仓统计
                max_position_size DECIMAL(5,3),
                avg_holding_days DECIMAL(6,1),
                turnover_rate DECIMAL(6,3),
                -- 详细数据
                equity_curve JSON,
                trade_records JSON,
                daily_returns JSON,
                position_details JSON,
                -- 性能指标
                backtest_duration DECIMAL(8,2),
                data_quality_score DECIMAL(3,2),
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
        """)
        
        # 数据同步日志表
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS data_sync_log (
                id BIGINT AUTO_INCREMENT PRIMARY KEY,
                sync_type VARCHAR(50) NOT NULL,
                sync_date DATE NOT NULL,
                start_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                end_time TIMESTAMP NULL,
                status ENUM('RUNNING', 'SUCCESS', 'FAILED') DEFAULT 'RUNNING',
                records_processed INT DEFAULT 0,
                records_success INT DEFAULT 0,
                records_failed INT DEFAULT 0,
                error_message TEXT,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
        """)
        
        logger.info("MySQL表结构创建完成")
    
    def _create_mysql_indexes(self, cursor):
        """创建MySQL索引"""
        indexes = [
            # stock_basic表索引
            "CREATE INDEX idx_stock_basic_industry ON stock_basic (industry)",
            "CREATE INDEX idx_stock_basic_sector ON stock_basic (sector)",
            "CREATE INDEX idx_stock_basic_market ON stock_basic (market)",
            "CREATE INDEX idx_stock_basic_stock_type ON stock_basic (stock_type)",
            "CREATE INDEX idx_stock_basic_area ON stock_basic (area)",
            "CREATE INDEX idx_stock_basic_active ON stock_basic (is_active)",
            "CREATE INDEX idx_stock_basic_list_date ON stock_basic (list_date)",
            
            # stock_daily表索引
            "CREATE INDEX idx_stock_daily_date ON stock_daily (trade_date)",
            "CREATE INDEX idx_stock_daily_code ON stock_daily (stock_code)",
            "CREATE INDEX idx_stock_daily_volume ON stock_daily (volume)",
            "CREATE INDEX idx_stock_daily_amount ON stock_daily (amount)",
            "CREATE INDEX idx_stock_daily_change ON stock_daily (change_pct)",
            "CREATE INDEX idx_stock_daily_range ON stock_daily (stock_code, trade_date, close_price)",
            "CREATE INDEX idx_stock_daily_change_volume ON stock_daily (trade_date, change_pct, volume)",
            
            # technical_indicators表索引
            "CREATE INDEX idx_tech_date ON technical_indicators (trade_date)",
            "CREATE INDEX idx_tech_rsi ON technical_indicators (rsi6, rsi12, rsi24)",
            "CREATE INDEX idx_tech_kdj ON technical_indicators (kdj_k, kdj_d, kdj_j)",
            "CREATE INDEX idx_tech_combo ON technical_indicators (stock_code, trade_date, rsi6, kdj_k, macd_dif)",
            
            # financial_data表索引
            "CREATE INDEX idx_financial_end_date ON financial_data (end_date)",
            "CREATE INDEX idx_financial_ann_date ON financial_data (ann_date)",
            "CREATE INDEX idx_financial_roe ON financial_data (roe)",
            "CREATE INDEX idx_financial_margin ON financial_data (net_margin)",
            "CREATE INDEX idx_financial_debt ON financial_data (debt_to_assets)",
            "CREATE INDEX idx_financial_metrics ON financial_data (end_date, roe, net_margin, debt_to_assets)",
            
            # strategy_signals表索引
            "CREATE INDEX idx_signals_strategy_date ON strategy_signals (strategy_id, signal_date)",
            "CREATE INDEX idx_signals_stock_date ON strategy_signals (stock_code, signal_date)",
            "CREATE INDEX idx_signals_type ON strategy_signals (signal_type)",
            "CREATE INDEX idx_signals_status ON strategy_signals (execution_status)",
            "CREATE INDEX idx_signals_strength ON strategy_signals (signal_strength)",
            
            # backtest_results表索引
            "CREATE INDEX idx_backtest_strategy_date ON backtest_results (strategy_id, start_date, end_date)",
            "CREATE INDEX idx_backtest_stock_date ON backtest_results (stock_code, start_date, end_date)",
            "CREATE INDEX idx_backtest_return ON backtest_results (annual_return)",
            "CREATE INDEX idx_backtest_sharpe ON backtest_results (sharpe_ratio)",
            "CREATE INDEX idx_backtest_drawdown ON backtest_results (max_drawdown)",
            
            # data_sync_log表索引
            "CREATE INDEX idx_sync_log_type_date ON data_sync_log (sync_type, sync_date)",
            "CREATE INDEX idx_sync_log_status ON data_sync_log (status)"
        ]
        
        for index_sql in indexes:
            try:
                cursor.execute(index_sql)
                logger.debug(f"创建索引: {index_sql[:50]}...")
            except mysql.connector.Error as e:
                if "Duplicate key name" not in str(e):
                    logger.warning(f"创建索引失败: {e}")
        
        logger.info("MySQL索引创建完成")
    
    def _create_mysql_partitions(self, cursor):
        """创建MySQL分区（如果不存在）"""
        try:
            # 检查stock_daily表是否已经分区
            cursor.execute("""
                SELECT COUNT(*) FROM information_schema.partitions 
                WHERE table_schema = DATABASE() AND table_name = 'stock_daily' AND partition_name IS NOT NULL
            """)
            partition_count = cursor.fetchone()[0]
            
            if partition_count == 0:
                # 添加分区
                cursor.execute("""
                    ALTER TABLE stock_daily PARTITION BY RANGE (YEAR(trade_date)) (
                        PARTITION p2020 VALUES LESS THAN (2021),
                        PARTITION p2021 VALUES LESS THAN (2022),
                        PARTITION p2022 VALUES LESS THAN (2023),
                        PARTITION p2023 VALUES LESS THAN (2024),
                        PARTITION p2024 VALUES LESS THAN (2025),
                        PARTITION p2025 VALUES LESS THAN (2026),
                        PARTITION pmax VALUES LESS THAN MAXVALUE
                    )
                """)
                
                # technical_indicators表分区
                cursor.execute("""
                    ALTER TABLE technical_indicators PARTITION BY RANGE (YEAR(trade_date)) (
                        PARTITION p2020 VALUES LESS THAN (2021),
                        PARTITION p2021 VALUES LESS THAN (2022),
                        PARTITION p2022 VALUES LESS THAN (2023),
                        PARTITION p2023 VALUES LESS THAN (2024),
                        PARTITION p2024 VALUES LESS THAN (2025),
                        PARTITION p2025 VALUES LESS THAN (2026),
                        PARTITION pmax VALUES LESS THAN MAXVALUE
                    )
                """)
                
                logger.info("MySQL表分区创建完成")
                
        except Exception as e:
            logger.warning(f"创建分区失败（可能已存在）: {e}")
    
    def _init_sqlite_database(self):
        """初始化SQLite数据库（开发和测试用）"""
        db_path = self.config.get('database', 'data/stock_analysis.db')
        os.makedirs(os.path.dirname(db_path), exist_ok=True)
        
        connection = sqlite3.connect(db_path)
        cursor = connection.cursor()
        
        try:
            # 创建SQLite表（简化版）
            self._create_sqlite_tables(cursor)
            connection.commit()
            logger.info("SQLite数据库初始化完成")
            
        finally:
            cursor.close()
            connection.close()
    
    def _create_sqlite_tables(self, cursor):
        """创建SQLite表结构（简化版）"""
        
        # 股票基础信息表（支持A股和港股）
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS stock_basic (
                stock_code TEXT PRIMARY KEY,
                stock_name TEXT NOT NULL,
                industry TEXT,
                sector TEXT,
                market TEXT,
                stock_type TEXT DEFAULT 'A股', -- 股票类型：A股/港股
                area TEXT, -- 所属地区
                list_date TEXT,
                delist_date TEXT,
                is_active INTEGER DEFAULT 1,
                created_at TEXT DEFAULT CURRENT_TIMESTAMP,
                updated_at TEXT DEFAULT CURRENT_TIMESTAMP
            )
        """)
        
        # 日线行情数据表
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS stock_daily (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                stock_code TEXT NOT NULL,
                trade_date TEXT NOT NULL,
                open_price REAL,
                high_price REAL,
                low_price REAL,
                close_price REAL,
                pre_close REAL,
                change_amt REAL,
                change_pct REAL,
                volume INTEGER,
                amount REAL,
                turnover_rate REAL,
                volume_ratio REAL,
                pe REAL,
                pb REAL,
                total_share INTEGER,
                float_share INTEGER,
                free_share INTEGER,
                total_mv REAL,
                circ_mv REAL,
                created_at TEXT DEFAULT CURRENT_TIMESTAMP,
                UNIQUE(stock_code, trade_date)
            )
        """)
        
        # 技术指标缓存表
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS technical_indicators (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                stock_code TEXT NOT NULL,
                trade_date TEXT NOT NULL,
                ma5 REAL, ma10 REAL, ma20 REAL, ma30 REAL, ma60 REAL, ma120 REAL, ma250 REAL,
                rsi6 REAL, rsi12 REAL, rsi24 REAL,
                macd_dif REAL, macd_dea REAL, macd_macd REAL,
                kdj_k REAL, kdj_d REAL, kdj_j REAL,
                boll_upper REAL, boll_mid REAL, boll_lower REAL,
                atr REAL, cci REAL, williams_r REAL, obv INTEGER, vwap REAL,
                created_at TEXT DEFAULT CURRENT_TIMESTAMP,
                updated_at TEXT DEFAULT CURRENT_TIMESTAMP,
                UNIQUE(stock_code, trade_date)
            )
        """)
        
        # 创建索引
        indexes = [
            "CREATE INDEX IF NOT EXISTS idx_stock_daily_date ON stock_daily (trade_date)",
            "CREATE INDEX IF NOT EXISTS idx_stock_daily_code ON stock_daily (stock_code)",
            "CREATE INDEX IF NOT EXISTS idx_tech_date ON technical_indicators (trade_date)",
            "CREATE INDEX IF NOT EXISTS idx_tech_code ON technical_indicators (stock_code)"
        ]
        
        for index_sql in indexes:
            cursor.execute(index_sql)
        
        logger.info("SQLite表结构创建完成")

def get_database_config():
    """获取数据库配置"""
    # 优先使用环境变量
    db_type = os.getenv('DB_TYPE', 'sqlite')
    
    if db_type == 'mysql':
        return {
            'type': 'mysql',
            'host': os.getenv('DB_HOST', 'localhost'),
            'port': int(os.getenv('DB_PORT', 3306)),
            'user': os.getenv('DB_USER', 'root'),
            'password': os.getenv('DB_PASSWORD', ''),
            'database': os.getenv('DB_NAME', 'stock_analysis')
        }
    else:
        return {
            'type': 'sqlite',
            'database': os.getenv('DB_PATH', 'data/stock_analysis.db')
        }

if __name__ == "__main__":
    """直接运行此脚本初始化数据库"""
    config = get_database_config()
    
    print("数据库初始化工具")
    print("=" * 50)
    print(f"数据库类型: {config['type']}")
    
    if config['type'] == 'mysql':
        print(f"MySQL主机: {config['host']}:{config['port']}")
        print(f"数据库名: {config['database']}")
    else:
        print(f"SQLite路径: {config['database']}")
    
    confirm = input("\n确认初始化数据库？(y/N): ")
    if confirm.lower() == 'y':
        initializer = DatabaseInitializer(config)
        initializer.initialize_database()
        print("\n✅ 数据库初始化完成！")
    else:
        print("❌ 取消初始化")