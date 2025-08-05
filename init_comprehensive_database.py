#!/usr/bin/env python3
"""
ljwx-stock数据库综合初始化脚本
创建全维度数据表并获取基础数据
"""
import logging
import sys
import traceback
from datetime import datetime
from database.db_manager import DatabaseManager
from utils.comprehensive_data_schema import ComprehensiveDataSchema
from scripts.fetch_stock_basic import StockBasicFetcher

# 配置日志
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler('logs/database_initialization.log'),
        logging.StreamHandler()
    ]
)
logger = logging.getLogger(__name__)

def init_database():
    """初始化ljwx-stock数据库"""
    logger.info("🚀 开始初始化ljwx-stock数据库...")
    
    try:
        # 1. 创建数据库连接
        db_manager = DatabaseManager()
        if not db_manager.test_connection():
            logger.error("❌ 数据库连接失败")
            return False
            
        logger.info("✅ 数据库连接成功")
        
        # 2. 创建基础表结构
        logger.info("📊 创建基础表结构...")
        if not db_manager.create_tables():
            logger.error("❌ 基础表创建失败")
            return False
        logger.info("✅ 基础表创建成功")
        
        # 3. 创建全维度数据表
        logger.info("🏗️ 创建全维度数据表...")
        if not ComprehensiveDataSchema.create_all_tables(db_manager):
            logger.error("❌ 全维度数据表创建失败")
            return False
        logger.info("✅ 全维度数据表创建成功")
        
        # 4. 获取股票基础数据
        logger.info("📈 开始获取股票基础数据...")
        try:
            fetcher = StockBasicFetcher()
            basic_success = fetcher.fetch_all_stocks()
            if not basic_success:
                logger.warning("⚠️ 股票基础数据获取失败，但继续执行")
            else:
                logger.info("✅ 股票基础数据获取成功")
        except Exception as e:
            logger.warning(f"⚠️ 初始化股票基础数据获取器失败: {e}")
        
        # 5. 获取历史行情数据（可选）
        logger.info("📊 开始获取历史行情数据...")
        try:
            from scheduler.historical_data_sync import HistoricalDataSync
            sync = HistoricalDataSync()
            sync.sync_last_n_years(1)  # 获取最近1年数据
            logger.info("✅ 历史行情数据获取成功")
        except Exception as e:
            logger.warning(f"⚠️ 历史行情数据获取失败: {e}")
        
        # 6. 记录系统日志
        log_sql = """
        INSERT INTO system_log (module, operation, status, message, duration)
        VALUES ('database_init', 'full_initialization', 'success', 
                'ljwx-stock数据库初始化完成', 0)
        """
        db_manager.execute_sql(log_sql)
        
        logger.info("🎉 ljwx-stock数据库初始化完成！")
        return True
        
    except Exception as e:
        logger.error(f"❌ 数据库初始化失败: {e}")
        logger.error(f"错误详情: {traceback.format_exc()}")
        
        # 记录失败日志
        try:
            db_manager = DatabaseManager()
            log_sql = """
            INSERT INTO system_log (module, operation, status, message, duration)
            VALUES ('database_init', 'full_initialization', 'failed', 
                    '{}', 0)
            """.format(str(e).replace("'", "''"))
            db_manager.execute_sql(log_sql)
        except:
            pass
            
        return False

def verify_database():
    """验证数据库完整性"""
    logger.info("🔍 开始验证数据库完整性...")
    
    try:
        db_manager = DatabaseManager()
        
        # 检查基础表
        basic_tables = [
            'stock_basic', 'recommend_result', 'technical_indicators', 'system_log'
        ]
        
        # 检查全维度表
        comprehensive_tables = [
            't_stock_basic', 't_industry_classification', 't_balance_sheet',
            't_income_statement', 't_financial_indicators', 't_money_flow'
        ]
        
        all_tables = basic_tables + comprehensive_tables
        
        missing_tables = []
        for table in all_tables:
            sql = f"SHOW TABLES LIKE '{table}'"
            result = db_manager.fetch_data(sql)
            if result.empty:
                missing_tables.append(table)
                
        if missing_tables:
            logger.warning(f"⚠️ 缺失表: {missing_tables}")
            return False
        
        # 检查数据量
        for table in ['stock_basic', 't_stock_basic']:
            try:
                sql = f"SELECT COUNT(*) as count FROM {table}"
                result = db_manager.fetch_data(sql)
                count = result.iloc[0]['count'] if not result.empty else 0
                logger.info(f"📊 {table}: {count} 条记录")
            except:
                logger.info(f"📊 {table}: 表存在但无数据")
        
        logger.info("✅ 数据库验证完成")
        return True
        
    except Exception as e:
        logger.error(f"❌ 数据库验证失败: {e}")
        return False

def main():
    """主函数"""
    print("=" * 60)
    print("🏦 ljwx-stock 数据库初始化程序")
    print("=" * 60)
    
    start_time = datetime.now()
    
    # 执行初始化
    success = init_database()
    
    if success:
        # 验证数据库
        verify_success = verify_database()
        
        end_time = datetime.now()
        duration = (end_time - start_time).total_seconds()
        
        print("\n" + "=" * 60)
        if verify_success:
            print("🎉 数据库初始化和验证成功完成！")
        else:
            print("⚠️ 数据库初始化完成，但验证发现问题")
        print(f"⏱️ 总耗时: {duration:.2f} 秒")
        print("=" * 60)
        
        return 0 if verify_success else 1
        
    else:
        print("\n" + "=" * 60)
        print("❌ 数据库初始化失败！")
        print("=" * 60)
        return 1

if __name__ == "__main__":
    sys.exit(main())