#!/usr/bin/env python3
"""
每日数据更新脚本 - 自动获取当日最新股票数据
"""
import sys
import os
import logging
from datetime import datetime, timedelta
import tushare as ts

# 添加项目路径
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import config
from database.db_manager import DatabaseManager

logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

class DailyDataUpdater:
    """每日数据更新器"""
    
    def __init__(self):
        self.db = DatabaseManager()
        self.ts_api = ts.pro_api(config.TS_TOKEN)
        
    def _get_table_name_for_date(self, trade_date):
        """根据交易日期获取对应的分表名称"""
        if isinstance(trade_date, str):
            if len(trade_date) == 8:  # YYYYMMDD格式
                year_month = trade_date[:6]
            else:  # YYYY-MM-DD格式
                year_month = trade_date.replace('-', '')[:6]
        else:
            year_month = trade_date.strftime('%Y%m')
        
        return f'stock_daily_{year_month}'
    
    def update_today_data(self):
        """更新今日数据"""
        today = datetime.now().strftime('%Y%m%d')
        logger.info(f"🔄 开始更新今日数据: {today}")
        
        # 获取需要更新的股票列表
        stocks = self._get_active_stocks()
        logger.info(f"📊 需要更新 {len(stocks)} 只股票")
        
        success_count = 0
        for ts_code in stocks:
            try:
                # 获取最近3天数据确保包含今天
                start_date = (datetime.now() - timedelta(days=3)).strftime('%Y%m%d')
                df = self.ts_api.daily(ts_code=ts_code, start_date=start_date, end_date=today)
                
                if not df.empty:
                    # 添加处理字段
                    df['change'] = df['close'] - df['pre_close']
                    df['change_pct'] = df['pct_chg']
                    
                    # 按月份分组数据，分别插入对应表
                    success = False
                    grouped = df.groupby(df['trade_date'].str[:6])  # 按年月分组
                    
                    for year_month, month_data in grouped:
                        table_name = f'stock_daily_{year_month}'
                        if self.db.insert_dataframe(table_name, month_data, replace=False):
                            success = True
                    
                    if success:
                        latest_date = df['trade_date'].max()
                        latest_close = df[df['trade_date'] == latest_date]['close'].iloc[0]
                        logger.info(f"✅ {ts_code}: {len(df)}条记录, 最新: {latest_date} {latest_close}")
                        success_count += 1
                    else:
                        logger.error(f"❌ {ts_code}: 数据插入失败")
                else:
                    logger.warning(f"⚠️ {ts_code}: 无数据")
                    
            except Exception as e:
                logger.error(f"❌ {ts_code}: {e}")
        
        logger.info(f"✅ 数据更新完成: {success_count}/{len(stocks)} 成功")
        return success_count
    
    def _get_active_stocks(self):
        """获取活跃股票列表"""
        try:
            # 从stock_basic表获取主要股票
            sql = """
            SELECT ts_code FROM stock_basic 
            WHERE ts_code IN (
                '000001.SZ', '000002.SZ', '600036.SH', '600000.SH', 
                '601318.SH', '000858.SZ', '002415.SZ', '600519.SH',
                '601012.SH', '601857.SH', '601398.SH', '002230.SZ',
                '601858.SH', '000300.SZ', '600887.SH'
            )
            ORDER BY ts_code
            """
            df = self.db.fetch_data(sql)
            if not df.empty:
                return df['ts_code'].tolist()
            else:
                # 如果数据库中没有，返回默认列表
                return [
                    '000001.SZ', '000002.SZ', '600036.SH', '600000.SH', 
                    '601318.SH', '000858.SZ', '002415.SZ', '600519.SH',
                    '601012.SH', '601857.SH', '601398.SH', '002230.SZ',
                    '601858.SH'
                ]
        except Exception as e:
            logger.error(f"获取股票列表失败: {e}")
            return []
    
    def get_update_summary(self):
        """获取更新摘要"""
        try:
            current_table = self._get_table_name_for_date(datetime.now().strftime('%Y%m%d'))
            
            # 今日更新统计 - 检查最新交易日期而不是创建时间
            today_updates = self.db.fetch_data(f"""
                SELECT COUNT(DISTINCT ts_code) as updated_stocks,
                       MAX(trade_date) as latest_date
                FROM {current_table}
                WHERE trade_date >= DATE_SUB(CURDATE(), INTERVAL 3 DAY)
            """)
            
            # 最新数据统计
            latest_data = self.db.fetch_data(f"""
                SELECT COUNT(DISTINCT ts_code) as total_stocks,
                       MAX(trade_date) as latest_trade_date,
                       COUNT(*) as total_records
                FROM {current_table}
            """)
            
            if not today_updates.empty and not latest_data.empty:
                summary = {
                    'today_updated_stocks': today_updates.iloc[0]['updated_stocks'],
                    'latest_date': today_updates.iloc[0]['latest_date'],
                    'total_stocks': latest_data.iloc[0]['total_stocks'],
                    'latest_trade_date': latest_data.iloc[0]['latest_trade_date'],
                    'total_records': latest_data.iloc[0]['total_records']
                }
                
                logger.info("📊 更新摘要:")
                logger.info(f"   今日更新股票: {summary['today_updated_stocks']}")
                logger.info(f"   最新交易日: {summary['latest_trade_date']}")
                logger.info(f"   总股票数: {summary['total_stocks']}")
                logger.info(f"   总记录数: {summary['total_records']:,}")
                
                return summary
            
        except Exception as e:
            logger.error(f"获取更新摘要失败: {e}")
            return {}

def main():
    """主函数"""
    updater = DailyDataUpdater()
    
    try:
        # 更新今日数据
        success_count = updater.update_today_data()
        
        # 显示摘要
        updater.get_update_summary()
        
        if success_count > 0:
            logger.info("🎉 每日数据更新成功完成！")
        else:
            logger.warning("⚠️ 没有数据被更新")
            
    except Exception as e:
        logger.error(f"❌ 每日数据更新失败: {e}")

if __name__ == "__main__":
    main()