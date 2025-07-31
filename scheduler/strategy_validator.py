#!/usr/bin/env python3
"""
投资策略验证系统 - 验证是否支持App中所有的投资策略
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

class StrategyValidator:
    """投资策略验证器"""
    
    def __init__(self):
        self.db = DatabaseManager()
    
    def validate_all_strategies(self):
        """验证所有投资策略"""
        logger.info("🎯 开始投资策略验证")
        
        strategies = [
            ("涨幅大于5%的非新股", self.validate_price_increase_strategy),
            ("昨天涨停不含新股", self.validate_limit_up_strategy),
            ("均线多头排列的股票", self.validate_ma_bullish_strategy),
            ("MACD与KDJ双金叉", self.validate_macd_kdj_strategy),
            ("今天的新股开板/上市", self.validate_new_stock_strategy),
            ("最近10天有2次涨停", self.validate_multiple_limit_up_strategy),
            ("市盈率小于15的股票", self.validate_pe_ratio_strategy)
        ]
        
        results = {}
        
        for strategy_name, strategy_func in strategies:
            logger.info(f"🔍 验证策略: {strategy_name}")
            try:
                result = strategy_func()
                results[strategy_name] = result
                
                if result['supported']:
                    logger.info(f"✅ {strategy_name}: 支持 ({result.get('count', 0)} 只股票)")
                else:
                    logger.warning(f"⚠️ {strategy_name}: 不完全支持 - {result.get('reason', '未知原因')}")
                    
            except Exception as e:
                logger.error(f"❌ {strategy_name}: 验证失败 - {e}")
                results[strategy_name] = {'supported': False, 'error': str(e)}
        
        self.print_strategy_summary(results)
        return results
    
    def validate_price_increase_strategy(self):
        """验证涨幅大于5%的非新股策略"""
        try:
            # 获取最新交易日期
            latest_date = self.db.fetch_data("""
                SELECT MAX(trade_date) as latest_date 
                FROM stock_daily_202507
            """).iloc[0]['latest_date']
            
            if not latest_date:
                return {'supported': False, 'reason': '无最新日线数据'}
            
            # 查找涨幅大于5%的股票
            sql = f"""
            SELECT d.ts_code, d.close, d.pct_chg, b.name, b.list_date
            FROM stock_daily_202507 d
            JOIN stock_basic b ON d.ts_code = b.ts_code
            WHERE d.trade_date = '{latest_date}'
              AND d.pct_chg > 5.0
              AND DATEDIFF(CURDATE(), STR_TO_DATE(b.list_date, '%%Y%%m%%d')) > 365
            ORDER BY d.pct_chg DESC
            LIMIT 20
            """
            
            result_df = self.db.fetch_data(sql)
            
            return {
                'supported': True,
                'count': len(result_df),
                'sample_data': result_df.head(5).to_dict('records') if not result_df.empty else []
            }
            
        except Exception as e:
            return {'supported': False, 'reason': f'查询失败: {e}'}
    
    def validate_limit_up_strategy(self):
        """验证昨天涨停不含新股策略"""
        try:
            # 检查涨跌停数据
            limit_count = self.db.fetch_data("SELECT COUNT(*) as count FROM t_limit_price").iloc[0]['count']
            
            if limit_count == 0:
                return {'supported': False, 'reason': '无涨跌停历史数据'}
            
            # 获取最近的涨停数据
            sql = """
            SELECT l.ts_code, l.name, l.close, l.pct_chg, b.list_date
            FROM t_limit_price l
            JOIN stock_basic b ON l.ts_code = b.ts_code
            WHERE l.limit_type = 'UP'
              AND DATEDIFF(CURDATE(), STR_TO_DATE(b.list_date, '%Y%m%d')) > 365
            ORDER BY l.trade_date DESC
            LIMIT 10
            """
            
            result_df = self.db.fetch_data(sql)
            
            return {
                'supported': True,
                'count': len(result_df),
                'total_limit_data': limit_count,
                'sample_data': result_df.head(3).to_dict('records') if not result_df.empty else []
            }
            
        except Exception as e:
            return {'supported': False, 'reason': f'查询失败: {e}'}
    
    def validate_ma_bullish_strategy(self):
        """验证均线多头排列策略"""
        try:
            # 检查技术指标数据
            indicator_count = self.db.fetch_data("SELECT COUNT(*) as count FROM technical_indicators").iloc[0]['count']
            
            if indicator_count == 0:
                return {'supported': False, 'reason': '无技术指标数据'}
            
            # 查找均线多头排列的股票 (MA5 > MA10 > MA20)
            sql = """
            SELECT t.ts_code, t.close, t.ma5, t.ma10, t.ma20, b.name
            FROM technical_indicators t
            JOIN stock_basic b ON t.ts_code = b.ts_code
            WHERE t.ma5 IS NOT NULL 
              AND t.ma10 IS NOT NULL 
              AND t.ma20 IS NOT NULL
              AND t.ma5 > t.ma10 
              AND t.ma10 > t.ma20
            ORDER BY t.trade_date DESC
            LIMIT 10
            """
            
            result_df = self.db.fetch_data(sql)
            
            return {
                'supported': True,
                'count': len(result_df),
                'total_indicator_data': indicator_count,
                'sample_data': result_df.head(5).to_dict('records') if not result_df.empty else []
            }
            
        except Exception as e:
            return {'supported': False, 'reason': f'查询失败: {e}'}
    
    def validate_macd_kdj_strategy(self):
        """验证MACD与KDJ双金叉策略"""
        try:
            # 检查MACD和KDJ数据
            macd_count = self.db.fetch_data("""
                SELECT COUNT(*) as count FROM technical_indicators 
                WHERE macd_dif IS NOT NULL AND macd_dea IS NOT NULL
            """).iloc[0]['count']
            
            kdj_count = self.db.fetch_data("""
                SELECT COUNT(*) as count FROM technical_indicators 
                WHERE kdj_k IS NOT NULL AND kdj_d IS NOT NULL
            """).iloc[0]['count']
            
            if macd_count == 0:
                return {'supported': False, 'reason': '无MACD指标数据'}
            
            if kdj_count == 0:
                return {'supported': False, 'reason': '无KDJ指标数据'}
            
            # 查找双金叉信号 (简化版：MACD DIF > DEA, KDJ K > D)
            sql = """
            SELECT t.ts_code, t.close, t.macd_dif, t.macd_dea, t.kdj_k, t.kdj_d, b.name
            FROM technical_indicators t
            JOIN stock_basic b ON t.ts_code = b.ts_code
            WHERE t.macd_dif IS NOT NULL 
              AND t.macd_dea IS NOT NULL
              AND t.kdj_k IS NOT NULL 
              AND t.kdj_d IS NOT NULL
              AND t.macd_dif > t.macd_dea
              AND t.kdj_k > t.kdj_d
            ORDER BY t.trade_date DESC
            LIMIT 10
            """
            
            result_df = self.db.fetch_data(sql)
            
            return {
                'supported': True,
                'count': len(result_df),
                'macd_data_count': macd_count,
                'kdj_data_count': kdj_count,
                'sample_data': result_df.head(3).to_dict('records') if not result_df.empty else []
            }
            
        except Exception as e:
            return {'supported': False, 'reason': f'查询失败: {e}'}
    
    def validate_new_stock_strategy(self):
        """验证新股开板/上市策略"""
        try:
            # 检查最近上市的股票
            recent_stocks = self.db.fetch_data("""
                SELECT ts_code, name, list_date, industry
                FROM stock_basic
                WHERE STR_TO_DATE(list_date, '%Y%m%d') >= DATE_SUB(CURDATE(), INTERVAL 90 DAY)
                ORDER BY list_date DESC
                LIMIT 10
            """)
            
            return {
                'supported': True,
                'count': len(recent_stocks),
                'note': '支持基于上市日期筛选新股',
                'sample_data': recent_stocks.head(5).to_dict('records') if not recent_stocks.empty else []
            }
            
        except Exception as e:
            return {'supported': False, 'reason': f'查询失败: {e}'}
    
    def validate_multiple_limit_up_strategy(self):
        """验证最近10天有2次涨停策略"""
        try:
            # 检查涨停数据的时间跨度
            limit_stats = self.db.fetch_data("""
                SELECT 
                    COUNT(*) as total_count,
                    MIN(trade_date) as earliest_date,
                    MAX(trade_date) as latest_date,
                    COUNT(DISTINCT trade_date) as trading_days
                FROM t_limit_price
                WHERE limit_type = 'UP'
            """)
            
            if limit_stats.empty or limit_stats.iloc[0]['total_count'] == 0:
                return {'supported': False, 'reason': '无涨停历史数据'}
            
            stats = limit_stats.iloc[0]
            
            # 查找多次涨停的股票（简化查询）
            sql = """
            SELECT ts_code, COUNT(*) as limit_count
            FROM t_limit_price
            WHERE limit_type = 'UP'
              AND trade_date >= DATE_SUB(CURDATE(), INTERVAL 10 DAY)
            GROUP BY ts_code
            HAVING COUNT(*) >= 2
            ORDER BY limit_count DESC
            LIMIT 5
            """
            
            result_df = self.db.fetch_data(sql)
            
            return {
                'supported': True,
                'count': len(result_df),
                'data_span': f"{stats['earliest_date']} - {stats['latest_date']}",
                'trading_days': stats['trading_days'],
                'sample_data': result_df.to_dict('records') if not result_df.empty else []
            }
            
        except Exception as e:
            return {'supported': False, 'reason': f'查询失败: {e}'}
    
    def validate_pe_ratio_strategy(self):
        """验证市盈率小于15的股票策略"""
        try:
            # 检查基本指标数据
            indicator_count = self.db.fetch_data("SELECT COUNT(*) as count FROM stock_indicators").iloc[0]['count']
            
            if indicator_count == 0:
                return {'supported': False, 'reason': '无基本指标数据（PE、PB等）'}
            
            # 尝试查找PE相关字段
            columns = self.db.fetch_data("""
                SELECT COLUMN_NAME 
                FROM INFORMATION_SCHEMA.COLUMNS 
                WHERE TABLE_NAME = 'stock_indicators' 
                AND TABLE_SCHEMA = 'ljwx_stock'
            """)['COLUMN_NAME'].tolist()
            
            pe_columns = [col for col in columns if 'pe' in col.lower()]
            
            return {
                'supported': indicator_count > 0,
                'count': indicator_count,
                'available_columns': pe_columns,
                'note': '基本指标数据表存在，需确认PE字段名称'
            }
            
        except Exception as e:
            return {'supported': False, 'reason': f'查询失败: {e}'}
    
    def print_strategy_summary(self, results):
        """打印策略验证摘要"""
        logger.info("=" * 80)
        logger.info("🎯 投资策略支持情况汇总")
        logger.info("=" * 80)
        
        supported_count = 0
        total_count = len(results)
        
        for strategy_name, result in results.items():
            status = "✅ 支持" if result.get('supported') else "❌ 不支持"
            logger.info(f"{status} {strategy_name}")
            
            if result.get('supported'):
                supported_count += 1
                if 'count' in result:
                    logger.info(f"   数据量: {result['count']} 条")
            else:
                logger.info(f"   原因: {result.get('reason', '未知')}")
        
        logger.info("=" * 80)
        logger.info(f"📊 策略支持率: {supported_count}/{total_count} ({supported_count/total_count*100:.1f}%)")
        logger.info("=" * 80)
    
    def get_missing_data_recommendations(self):
        """获取缺失数据的建议"""
        recommendations = [
            "📈 完善历史涨跌停数据获取（运行中）",
            "💰 获取完整的基本指标数据（PE、PB、PS等）",
            "🔄 确保技术指标数据覆盖更多股票",
            "📊 增加新股开板监控数据",
            "⏰ 建立实时数据更新机制"
        ]
        
        logger.info("🔧 数据完善建议:")
        for rec in recommendations:
            logger.info(f"   {rec}")

def main():
    """主函数"""
    validator = StrategyValidator()
    
    try:
        # 验证所有策略
        results = validator.validate_all_strategies()
        
        # 给出改进建议
        validator.get_missing_data_recommendations()
        
        # 统计支持情况
        supported = sum(1 for r in results.values() if r.get('supported'))
        total = len(results)
        
        print(f"\n🎯 策略验证完成: {supported}/{total} 个策略已支持")
        
    except Exception as e:
        logger.error(f"❌ 策略验证异常: {e}")

if __name__ == "__main__":
    main()