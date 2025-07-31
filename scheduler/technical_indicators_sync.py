#!/usr/bin/env python3
"""
技术指标和市场数据完整获取系统
包含：技术指标、龙虎榜、涨跌停、停复牌等完整市场数据
"""

import sys
import os
import time
import logging
import numpy as np
import pandas as pd
from datetime import datetime, timedelta
from concurrent.futures import ThreadPoolExecutor, as_completed
from threading import Lock
import tushare as ts

# 添加项目路径
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import config
from database.db_manager import DatabaseManager

# 配置日志
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler('logs/technical_indicators.log'),
        logging.StreamHandler()
    ]
)
logger = logging.getLogger(__name__)

class TechnicalIndicatorSync:
    """技术指标和市场数据同步器"""
    
    def __init__(self):
        self.db = DatabaseManager()
        self.ts_api = ts.pro_api(config.TS_TOKEN)
        self.stats_lock = Lock()
        
        # 确保日志目录存在
        os.makedirs('logs', exist_ok=True)
    
    def sync_all_technical_data(self):
        """同步所有技术数据"""
        logger.info("🚀 开始技术指标和市场数据同步")
        
        tasks = [
            ("技术指标数据", self.sync_technical_indicators),
            ("龙虎榜数据", self.sync_dragon_tiger_list),
            ("涨跌停数据", self.sync_limit_price_data),
            ("停复牌数据", self.sync_suspend_data),
            ("每日基本指标", self.sync_daily_basic_indicators)
        ]
        
        success_count = 0
        total_tasks = len(tasks)
        
        for task_name, task_func in tasks:
            logger.info(f"📊 开始同步: {task_name}")
            try:
                success = task_func()
                if success:
                    success_count += 1
                    logger.info(f"✅ {task_name} 同步完成")
                else:
                    logger.warning(f"⚠️ {task_name} 同步失败")
            except Exception as e:
                logger.error(f"❌ {task_name} 同步异常: {e}")
            
            # 任务间休息
            time.sleep(2)
        
        logger.info(f"🎉 技术数据同步完成: {success_count}/{total_tasks} 成功")
        return success_count == total_tasks
    
    def sync_technical_indicators(self):
        """同步技术指标数据"""
        try:
            logger.info("📈 开始计算和同步技术指标...")
            
            # 获取当前月的日线数据表名
            current_table = f'stock_daily_{datetime.now().strftime("%Y%m")}'
            
            # 获取最近的股票日线数据
            recent_data = self.db.fetch_data(f"""
                SELECT ts_code, trade_date, close, high, low, vol, amount
                FROM {current_table}
                WHERE trade_date >= DATE_SUB(CURDATE(), INTERVAL 30 DAY)
                ORDER BY ts_code, trade_date
            """)
            
            if recent_data.empty:
                logger.warning("⚠️ 无日线数据，跳过技术指标计算")
                return False
            
            logger.info(f"📊 处理 {len(recent_data)} 条日线数据计算技术指标")
            
            # 按股票分组计算技术指标
            all_indicators = []
            
            for ts_code, group_data in recent_data.groupby('ts_code'):
                if len(group_data) < 10:  # 数据不够计算指标
                    continue
                
                try:
                    indicators = self._calculate_technical_indicators(ts_code, group_data)
                    if not indicators.empty:
                        all_indicators.append(indicators)
                except Exception as e:
                    logger.debug(f"技术指标计算失败 {ts_code}: {e}")
                    continue
            
            if all_indicators:
                combined_indicators = pd.concat(all_indicators, ignore_index=True)
                combined_indicators['created_at'] = datetime.now()
                
                success = self.db.insert_dataframe('technical_indicators', combined_indicators, replace=True)
                if success:
                    logger.info(f"✅ 技术指标: {len(combined_indicators)} 条记录")
                    return True
            
            return False
            
        except Exception as e:
            logger.error(f"技术指标同步失败: {e}")
            return False
    
    def _calculate_technical_indicators(self, ts_code, data):
        """计算单只股票的技术指标"""
        try:
            # 确保数据按日期排序
            data = data.sort_values('trade_date').reset_index(drop=True)
            
            if len(data) < 5:
                return pd.DataFrame()
            
            # 准备价格数据
            close = data['close'].values
            high = data['high'].values  
            low = data['low'].values
            volume = data['vol'].values
            
            indicators_list = []
            
            # 对每一天计算指标（取最新的几天）
            for i in range(max(20, len(data)-10), len(data)):  # 最后10天的指标
                if i < 20:  # 需要足够的历史数据
                    continue
                    
                trade_date = data.iloc[i]['trade_date']
                
                # 计算各种技术指标
                indicators = {
                    'ts_code': ts_code,
                    'trade_date': trade_date,
                    'close': close[i],
                    
                    # 移动平均线
                    'ma5': np.mean(close[max(0, i-4):i+1]) if i >= 4 else None,
                    'ma10': np.mean(close[max(0, i-9):i+1]) if i >= 9 else None,
                    'ma20': np.mean(close[max(0, i-19):i+1]) if i >= 19 else None,
                    
                    # RSI相对强弱指标
                    'rsi6': self._calculate_rsi(close[max(0, i-6):i+1]) if i >= 6 else None,
                    'rsi12': self._calculate_rsi(close[max(0, i-12):i+1]) if i >= 12 else None,
                    
                    # MACD
                    'macd_dif': None,  # 简化处理
                    'macd_dea': None,
                    'macd_macd': None,
                    
                    # 布林带
                    'boll_upper': None,
                    'boll_mid': np.mean(close[max(0, i-19):i+1]) if i >= 19 else None,
                    'boll_lower': None,
                    
                    # KDJ指标
                    'kdj_k': None,
                    'kdj_d': None, 
                    'kdj_j': None,
                    
                    # 成交量指标
                    'vol_ratio': volume[i] / np.mean(volume[max(0, i-4):i+1]) if i >= 4 and np.mean(volume[max(0, i-4):i+1]) > 0 else None,
                }
                
                # 计算布林带
                if i >= 19:
                    period_data = close[i-19:i+1]
                    mid = np.mean(period_data)
                    std = np.std(period_data)
                    indicators['boll_upper'] = mid + 2 * std
                    indicators['boll_lower'] = mid - 2 * std
                
                indicators_list.append(indicators)
            
            return pd.DataFrame(indicators_list)
            
        except Exception as e:
            logger.debug(f"计算技术指标失败 {ts_code}: {e}")
            return pd.DataFrame()
    
    def _calculate_rsi(self, prices):
        """计算RSI指标"""
        try:
            if len(prices) < 2:
                return None
                
            deltas = np.diff(prices)
            gains = np.where(deltas > 0, deltas, 0)
            losses = np.where(deltas < 0, -deltas, 0)
            
            if len(gains) == 0:
                return 50
                
            avg_gain = np.mean(gains)
            avg_loss = np.mean(losses)
            
            if avg_loss == 0:
                return 100
                
            rs = avg_gain / avg_loss
            rsi = 100 - (100 / (1 + rs))
            
            return round(rsi, 2)
        except:
            return None
    
    def sync_dragon_tiger_list(self):
        """同步龙虎榜数据"""
        try:
            logger.info("🐉 获取龙虎榜数据...")
            
            # 获取最近10个交易日的龙虎榜
            all_dragon_data = []
            
            for i in range(15):  # 扩大范围确保获取到数据
                trade_date = (datetime.now() - timedelta(days=i)).strftime('%Y%m%d')
                
                # 跳过周末
                date_obj = datetime.strptime(trade_date, '%Y%m%d')
                if date_obj.weekday() >= 5:  # 周六日
                    continue
                
                try:
                    # 获取每日龙虎榜
                    dragon_df = self.ts_api.top_list(trade_date=trade_date)
                    
                    if not dragon_df.empty:
                        dragon_df['created_at'] = datetime.now()
                        all_dragon_data.append(dragon_df)
                        logger.info(f"📅 {trade_date}: {len(dragon_df)} 条龙虎榜记录")
                    
                    time.sleep(0.3)  # API限制
                    
                except Exception as e:
                    if "没有数据" not in str(e) and "暂停" not in str(e):
                        logger.debug(f"龙虎榜 {trade_date}: {e}")
                    continue
            
            if all_dragon_data:
                combined_df = pd.concat(all_dragon_data, ignore_index=True)
                # 去重处理
                combined_df = combined_df.drop_duplicates(subset=['trade_date', 'ts_code'], keep='last')
                
                # 修复字段名称映射
                if 'pct_change' in combined_df.columns:
                    combined_df = combined_df.rename(columns={'pct_change': 'pct_chg'})
                
                success = self.db.insert_dataframe('t_dragon_tiger_list', combined_df, replace=False)
                if success:
                    logger.info(f"✅ 龙虎榜数据: {len(combined_df)} 条记录")
                    return True
            
            logger.info("📊 龙虎榜: 近期无新数据")
            return True  # 无数据也算成功
            
        except Exception as e:
            logger.error(f"龙虎榜数据同步失败: {e}")
            return False
    
    def sync_limit_price_data(self):
        """同步涨跌停数据"""
        try:
            logger.info("📊 获取涨跌停数据...")
            
            # 获取最近5个交易日的涨跌停数据
            all_limit_data = []
            
            for i in range(7):
                trade_date = (datetime.now() - timedelta(days=i)).strftime('%Y%m%d')
                
                # 跳过周末
                date_obj = datetime.strptime(trade_date, '%Y%m%d')
                if date_obj.weekday() >= 5:
                    continue
                
                try:
                    # 获取涨停股
                    limit_up = self.ts_api.limit_list(trade_date=trade_date, limit_type='U')
                    if not limit_up.empty:
                        limit_up['limit_type'] = 'UP'
                        limit_up['trade_date'] = trade_date
                        all_limit_data.append(limit_up)
                    
                    time.sleep(0.2)
                    
                    # 获取跌停股
                    limit_down = self.ts_api.limit_list(trade_date=trade_date, limit_type='D')
                    if not limit_down.empty:
                        limit_down['limit_type'] = 'DOWN'
                        limit_down['trade_date'] = trade_date
                        all_limit_data.append(limit_down)
                    
                    time.sleep(0.2)
                    
                except Exception as e:
                    if "没有数据" not in str(e):
                        logger.debug(f"涨跌停 {trade_date}: {e}")
                    continue
            
            if all_limit_data:
                combined_df = pd.concat(all_limit_data, ignore_index=True)
                combined_df['created_at'] = datetime.now()
                
                success = self.db.insert_dataframe('t_limit_price', combined_df, replace=False)
                if success:
                    logger.info(f"✅ 涨跌停数据: {len(combined_df)} 条记录")
                    return True
            
            logger.info("📊 涨跌停: 近期无数据")
            return True
            
        except Exception as e:
            logger.error(f"涨跌停数据同步失败: {e}")
            return False
    
    def sync_suspend_data(self):
        """同步停复牌数据"""
        try:
            logger.info("⏸️ 获取停复牌数据...")
            
            # 获取最近30天的停复牌数据
            end_date = datetime.now().strftime('%Y%m%d')
            start_date = (datetime.now() - timedelta(days=30)).strftime('%Y%m%d')
            
            suspend_df = self.ts_api.suspend_d(start_date=start_date, end_date=end_date)
            
            if not suspend_df.empty:
                suspend_df['created_at'] = datetime.now()
                success = self.db.insert_dataframe('t_suspend', suspend_df, replace=False)
                if success:
                    logger.info(f"✅ 停复牌数据: {len(suspend_df)} 条记录")
                    return True
            
            return False
            
        except Exception as e:
            logger.error(f"停复牌数据同步失败: {e}")
            return False
    
    def sync_daily_basic_indicators(self):
        """同步每日基本指标"""
        try:
            logger.info("📈 获取每日基本指标...")
            
            # 获取最近3个交易日的基本指标
            for i in range(5):
                trade_date = (datetime.now() - timedelta(days=i)).strftime('%Y%m%d')
                
                # 跳过周末
                date_obj = datetime.strptime(trade_date, '%Y%m%d')
                if date_obj.weekday() >= 5:
                    continue
                
                try:
                    # 获取每日基本指标（PE、PB等）
                    basic_df = self.ts_api.daily_basic(trade_date=trade_date)
                    
                    if not basic_df.empty:
                        basic_df['created_at'] = datetime.now()
                        success = self.db.insert_dataframe('stock_indicators', basic_df, replace=False)
                        if success:
                            logger.info(f"📅 {trade_date} 基本指标: {len(basic_df)} 条记录")
                    
                    time.sleep(0.5)  # API限制
                    break  # 只获取最新一天的数据
                    
                except Exception as e:
                    if "没有数据" not in str(e):
                        logger.debug(f"基本指标 {trade_date}: {e}")
                    continue
            
            return True
            
        except Exception as e:
            logger.error(f"每日基本指标同步失败: {e}")
            return False
    
    def get_technical_summary(self):
        """获取技术数据摘要"""
        try:
            tables_info = [
                ('技术指标', 'technical_indicators'),
                ('每日基本指标', 'stock_indicators'),
                ('龙虎榜', 't_dragon_tiger_list'),
                ('涨跌停', 't_limit_price'),
                ('停复牌', 't_suspend')
            ]
            
            logger.info("📊 技术数据摘要:")
            total_records = 0
            
            for name, table in tables_info:
                try:
                    count = self.db.fetch_data(f'SELECT COUNT(*) as count FROM {table}').iloc[0]['count']
                    logger.info(f"   {name}: {count:,} 条记录")
                    total_records += count
                except:
                    logger.info(f"   {name}: 无数据")
            
            logger.info(f"📈 技术数据总计: {total_records:,} 条记录")
            
        except Exception as e:
            logger.error(f"获取技术数据摘要失败: {e}")

def main():
    """主函数"""
    import argparse
    
    parser = argparse.ArgumentParser(description='技术指标和市场数据同步系统')
    parser.add_argument('--type', choices=['all', 'indicators', 'dragon', 'limit', 'suspend', 'basic'], 
                        default='all', help='同步数据类型')
    
    args = parser.parse_args()
    
    syncer = TechnicalIndicatorSync()
    
    try:
        if args.type == 'all':
            success = syncer.sync_all_technical_data()
        elif args.type == 'indicators':
            success = syncer.sync_technical_indicators()
        elif args.type == 'dragon':
            success = syncer.sync_dragon_tiger_list()
        elif args.type == 'limit':
            success = syncer.sync_limit_price_data()
        elif args.type == 'suspend':
            success = syncer.sync_suspend_data()
        elif args.type == 'basic':
            success = syncer.sync_daily_basic_indicators()
        
        # 显示摘要
        syncer.get_technical_summary()
        
        if success:
            logger.info("🎉 技术数据同步成功完成！")
        else:
            logger.warning("⚠️ 技术数据同步未完全成功")
            
    except KeyboardInterrupt:
        logger.info("⏸️ 用户中断操作")
    except Exception as e:
        logger.error(f"❌ 系统异常: {e}")

if __name__ == "__main__":
    main()