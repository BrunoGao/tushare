#!/usr/bin/env python3
"""
数据完整性验证系统 - 检查历史数据同步状态和数据完整性
"""

import sys
import os
import pandas as pd
from datetime import datetime, timedelta
import logging

# 添加项目路径
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import config
from database.db_manager import DatabaseManager

# 配置日志
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)

class DataIntegrityValidator:
    """数据完整性验证器"""
    
    def __init__(self):
        self.db = DatabaseManager()
    
    def validate_all_data(self):
        """验证所有数据的完整性"""
        logger.info("🔍 开始数据完整性验证")
        
        validation_results = {
            'basic_data': self.validate_basic_data(),
            'daily_data': self.validate_daily_data(),
            'technical_indicators': self.validate_technical_indicators(),
            'basic_indicators': self.validate_basic_indicators(),
            'market_data': self.validate_market_data()
        }
        
        self.print_validation_summary(validation_results)
        return validation_results
    
    def validate_basic_data(self):
        """验证基础数据"""
        logger.info("📊 验证股票基础数据...")
        
        try:
            # 股票基本信息
            basic_count = self.db.fetch_data("SELECT COUNT(*) as count FROM stock_basic").iloc[0]['count']
            
            # A股筛选
            a_stock_count = self.db.fetch_data("""
                SELECT COUNT(*) as count 
                FROM stock_basic 
                WHERE (ts_code LIKE '%%.SZ' OR ts_code LIKE '%%.SH')
            """).iloc[0]['count']
            
            result = {
                'total_stocks': basic_count,
                'a_stocks': a_stock_count,
                'status': 'pass' if a_stock_count > 4000 else 'warning'
            }
            
            logger.info(f"✅ 基础数据: {basic_count} 只股票 ({a_stock_count} 只A股)")
            return result
            
        except Exception as e:
            logger.error(f"❌ 基础数据验证失败: {e}")
            return {'status': 'error', 'error': str(e)}
    
    def validate_daily_data(self):
        """验证日线数据"""
        logger.info("📈 验证日线数据...")
        
        try:
            # 获取所有月度表
            monthly_tables = self.db.fetch_data("""
                SELECT table_name
                FROM information_schema.tables 
                WHERE table_schema = 'ljwx_stock' 
                AND table_name LIKE 'stock_daily_%%'
                ORDER BY table_name DESC
            """)
            
            total_records = 0
            table_details = []
            
            for _, row in monthly_tables.iterrows():
                table_name = row['table_name']
                
                try:
                    # 获取精确行数和日期范围
                    table_stats = self.db.fetch_data(f"""
                        SELECT 
                            COUNT(*) as actual_count,
                            COUNT(DISTINCT ts_code) as stock_count,
                            MIN(trade_date) as earliest_date,
                            MAX(trade_date) as latest_date
                        FROM {table_name}
                    """)
                    
                    if not table_stats.empty:
                        stats = table_stats.iloc[0]
                        table_details.append({
                            'table': table_name,
                            'records': stats['actual_count'],
                            'stocks': stats['stock_count'],
                            'date_range': f"{stats['earliest_date']} - {stats['latest_date']}"
                        })
                        total_records += stats['actual_count']
                        
                except Exception as e:
                    logger.debug(f"表统计失败 {table_name}: {e}")
                    table_details.append({
                        'table': table_name,
                        'records': 0,
                        'stocks': 'unknown',
                        'date_range': 'unknown'
                    })
            
            result = {
                'total_records': total_records,
                'total_tables': len(table_details),
                'table_details': table_details,
                'status': 'pass' if total_records > 1000000 else 'warning'  # 100万条记录为基准
            }
            
            logger.info(f"✅ 日线数据: {total_records:,} 条记录, {len(table_details)} 个月表")
            return result
            
        except Exception as e:
            logger.error(f"❌ 日线数据验证失败: {e}")
            return {'status': 'error', 'error': str(e)}
    
    def validate_technical_indicators(self):
        """验证技术指标数据"""
        logger.info("📊 验证技术指标数据...")
        
        try:
            # 技术指标总数
            indicator_count = self.db.fetch_data("SELECT COUNT(*) as count FROM technical_indicators").iloc[0]['count']
            
            # 检查各种指标的完整性
            indicator_stats = self.db.fetch_data("""
                SELECT 
                    COUNT(*) as total_records,
                    COUNT(DISTINCT ts_code) as stock_count,
                    COUNT(ma5) as ma5_count,
                    COUNT(ma10) as ma10_count,
                    COUNT(ma20) as ma20_count,
                    COUNT(rsi6) as rsi6_count,
                    COUNT(rsi12) as rsi12_count,
                    COUNT(macd_dif) as macd_count,
                    COUNT(kdj_k) as kdj_count,
                    COUNT(boll_upper) as boll_count,
                    MIN(trade_date) as earliest_date,
                    MAX(trade_date) as latest_date
                FROM technical_indicators
            """)
            
            if not indicator_stats.empty:
                stats = indicator_stats.iloc[0]
                result = {
                    'total_records': indicator_count,
                    'stock_count': stats['stock_count'],
                    'indicator_coverage': {
                        'ma5': stats['ma5_count'],
                        'ma10': stats['ma10_count'],
                        'ma20': stats['ma20_count'],
                        'rsi6': stats['rsi6_count'],
                        'rsi12': stats['rsi12_count'],
                        'macd': stats['macd_count'],
                        'kdj': stats['kdj_count'],
                        'boll': stats['boll_count']
                    },
                    'date_range': f"{stats['earliest_date']} - {stats['latest_date']}",
                    'status': 'pass' if indicator_count > 10000 else 'warning'
                }
                
                logger.info(f"✅ 技术指标: {indicator_count:,} 条记录, {stats['stock_count']} 只股票")
                return result
            else:
                return {'status': 'warning', 'message': '无技术指标数据'}
                
        except Exception as e:
            logger.error(f"❌ 技术指标验证失败: {e}")
            return {'status': 'error', 'error': str(e)}
    
    def validate_basic_indicators(self):
        """验证基本指标数据（PE、PB等）"""
        logger.info("📊 验证基本指标数据...")
        
        try:
            indicator_count = self.db.fetch_data("SELECT COUNT(*) as count FROM stock_indicators").iloc[0]['count']
            
            # 检查指标完整性
            indicator_stats = self.db.fetch_data("""
                SELECT 
                    COUNT(*) as total_records,
                    COUNT(DISTINCT ts_code) as stock_count,
                    MIN(trade_date) as earliest_date,
                    MAX(trade_date) as latest_date
                FROM stock_indicators
            """)
            
            if not indicator_stats.empty:
                stats = indicator_stats.iloc[0]
                result = {
                    'total_records': indicator_count,
                    'stock_count': stats['stock_count'],
                    'date_range': f"{stats['earliest_date']} - {stats['latest_date']}",
                    'status': 'pass' if indicator_count > 50000 else 'warning'
                }
                
                logger.info(f"✅ 基本指标: {indicator_count:,} 条记录, {stats['stock_count']} 只股票")
                return result
            else:
                return {'status': 'warning', 'message': '无基本指标数据'}
                
        except Exception as e:
            logger.error(f"❌ 基本指标验证失败: {e}")
            return {'status': 'error', 'error': str(e)}
    
    def validate_market_data(self):
        """验证市场数据（涨跌停、龙虎榜等）"""
        logger.info("📊 验证市场数据...")
        
        market_tables = [
            ('龙虎榜', 't_dragon_tiger_list'),
            ('涨跌停', 't_limit_price'),
            ('停复牌', 't_suspend'),
            ('概念板块', 't_concept'),
            ('概念成分股', 't_concept_detail'),
            ('行业板块', 'industry_sectors'),
            ('资金流', 't_money_flow')
        ]
        
        result = {'tables': {}, 'status': 'pass'}
        total_market_records = 0
        
        for name, table in market_tables:
            try:
                count = self.db.fetch_data(f'SELECT COUNT(*) as count FROM {table}').iloc[0]['count']
                result['tables'][name] = {
                    'records': count,
                    'status': 'pass' if count > 0 else 'warning'
                }
                total_market_records += count
                logger.info(f"✅ {name}: {count:,} 条记录")
                
            except Exception as e:
                result['tables'][name] = {
                    'records': 0,
                    'status': 'error',
                    'error': str(e)
                }
                logger.warning(f"⚠️ {name}: 表不存在或无数据")
        
        result['total_records'] = total_market_records
        return result
    
    def print_validation_summary(self, results):
        """打印验证摘要"""
        logger.info("=" * 80)
        logger.info("📊 数据完整性验证报告")
        logger.info("=" * 80)
        
        # 基础数据
        if results['basic_data']['status'] != 'error':
            basic = results['basic_data']
            logger.info(f"📈 股票基础数据: {basic['a_stocks']:,} 只A股")
        
        # 日线数据
        if results['daily_data']['status'] != 'error':
            daily = results['daily_data']
            logger.info(f"📊 日线历史数据: {daily['total_records']:,} 条记录")
            logger.info(f"   分布在 {daily['total_tables']} 个月度表中")
        
        # 技术指标
        if results['technical_indicators']['status'] != 'error':
            tech = results['technical_indicators']
            logger.info(f"📈 技术指标数据: {tech['total_records']:,} 条记录")
            if 'stock_count' in tech:
                logger.info(f"   覆盖 {tech['stock_count']} 只股票")
        
        # 基本指标
        if results['basic_indicators']['status'] != 'error':
            basic_ind = results['basic_indicators']
            logger.info(f"💰 基本指标数据: {basic_ind['total_records']:,} 条记录")
        
        # 市场数据
        if results['market_data']['status'] != 'error':
            market = results['market_data']
            logger.info(f"🎯 市场数据总计: {market['total_records']:,} 条记录")
        
        # 计算总记录数
        total_records = 0
        for key, data in results.items():
            if isinstance(data, dict) and data.get('status') != 'error':
                if 'total_records' in data:
                    total_records += data['total_records']
        
        logger.info("=" * 80)
        logger.info(f"🎉 数据库总记录数: {total_records:,}")
        logger.info("=" * 80)
    
    def get_sync_progress(self):
        """获取同步进度"""
        try:
            # 检查后台进程
            import subprocess
            result = subprocess.run(['ps', 'aux'], capture_output=True, text=True)
            
            historical_running = 'historical_data_sync.py' in result.stdout
            
            progress_info = {
                'historical_sync_running': historical_running,
                'timestamp': datetime.now().strftime('%Y-%m-%d %H:%M:%S')
            }
            
            if historical_running:
                logger.info("🔄 历史数据同步正在后台运行")
            else:
                logger.info("⏸️ 历史数据同步未在运行")
            
            return progress_info
            
        except Exception as e:
            logger.error(f"获取同步进度失败: {e}")
            return {'error': str(e)}

def main():
    """主函数"""
    import argparse
    
    parser = argparse.ArgumentParser(description='数据完整性验证系统')
    parser.add_argument('--progress', action='store_true', help='检查同步进度')
    parser.add_argument('--full', action='store_true', help='完整验证报告')
    
    args = parser.parse_args()
    
    validator = DataIntegrityValidator()
    
    try:
        if args.progress:
            progress = validator.get_sync_progress()
            print(f"同步状态: {progress}")
        else:
            # 默认执行完整验证
            results = validator.validate_all_data()
            
            # 显示关键统计
            print("\n📊 快速数据概览:")
            
            for key, result in results.items():
                if result['status'] != 'error':
                    if 'total_records' in result:
                        print(f"   {key}: {result['total_records']:,} 条记录")
            
    except Exception as e:
        logger.error(f"❌ 验证异常: {e}")

if __name__ == "__main__":
    main()