#!/usr/bin/env python3
"""
历史数据全量同步系统 - 获取所有股票过去5年完整数据
包含：日线数据、技术指标、基本指标、涨跌停、停复牌等全套数据
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
from tqdm import tqdm

# 添加项目路径
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import config
from database.db_manager import DatabaseManager

# 配置日志
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler('logs/historical_sync.log'),
        logging.StreamHandler()
    ]
)
logger = logging.getLogger(__name__)

class HistoricalDataSync:
    """历史数据全量同步器 - 5年完整数据"""
    
    def __init__(self):
        self.db = DatabaseManager()
        self.ts_api = ts.pro_api(config.TS_TOKEN)
        self.stats_lock = Lock()
        
        # 时间范围 - 过去5年
        self.end_date = datetime.now().strftime('%Y%m%d')
        self.start_date = (datetime.now() - timedelta(days=5*365)).strftime('%Y%m%d')
        
        # 同步统计
        self.stats = {
            'total_stocks': 0,
            'completed_stocks': 0,
            'failed_stocks': 0,
            'total_records': 0,
            'start_time': None
        }
        
        # 确保日志目录存在
        os.makedirs('logs', exist_ok=True)
        
        logger.info(f"📅 数据同步时间范围: {self.start_date} - {self.end_date}")
    
    def sync_all_historical_data(self):
        """同步所有历史数据"""
        logger.info("🚀 开始历史数据全量同步 (5年)")
        self.stats['start_time'] = datetime.now()
        
        try:
            # 1. 更新股票基本信息
            self._update_stock_basic()
            
            # 2. 获取所有股票代码
            all_stocks = self._get_all_stocks()
            if not all_stocks:
                logger.error("❌ 无法获取股票列表")
                return False
            
            self.stats['total_stocks'] = len(all_stocks)
            logger.info(f"📊 需要同步 {len(all_stocks)} 只股票的5年历史数据")
            
            # 3. 分阶段同步不同类型的数据
            stages = [
                ("日线基础数据", self._sync_daily_data_batch, all_stocks),
                ("技术指标数据", self._sync_technical_indicators_batch, all_stocks),
                ("基本指标数据", self._sync_basic_indicators_batch, None),
                ("涨跌停数据", self._sync_limit_data_batch, None),
                ("停复牌数据", self._sync_suspend_data_batch, None),
                ("分红送股数据", self._sync_dividend_data_batch, None)
            ]
            
            for stage_name, stage_func, stock_list in stages:
                logger.info(f"🔄 开始阶段: {stage_name}")
                try:
                    if stock_list:
                        success = stage_func(stock_list)
                    else:
                        success = stage_func()
                    
                    if success:
                        logger.info(f"✅ {stage_name} 同步完成")
                    else:
                        logger.warning(f"⚠️ {stage_name} 同步部分失败")
                        
                except Exception as e:
                    logger.error(f"❌ {stage_name} 同步异常: {e}")
                
                # 阶段间休息
                time.sleep(5)
            
            # 4. 输出最终统计
            self._print_final_stats()
            return True
            
        except Exception as e:
            logger.error(f"❌ 历史数据同步异常: {e}")
            return False
    
    def _update_stock_basic(self):
        """更新股票基本信息"""
        logger.info("📋 更新股票基本信息...")
        
        try:
            # 获取所有股票基本信息
            stock_basic = self.ts_api.stock_basic(exchange='', list_status='L')
            
            if not stock_basic.empty:
                # 过滤A股
                a_stocks = stock_basic[
                    (stock_basic['ts_code'].str.endswith('.SZ')) | 
                    (stock_basic['ts_code'].str.endswith('.SH'))
                ]
                
                success = self.db.insert_dataframe('stock_basic', a_stocks, replace=True)
                if success:
                    logger.info(f"✅ 股票基本信息: {len(a_stocks)} 只A股")
                else:
                    logger.error("❌ 股票基本信息更新失败")
            
        except Exception as e:
            logger.error(f"❌ 更新股票基本信息失败: {e}")
    
    def _get_all_stocks(self):
        """获取所有A股代码"""
        try:
            sql = """
            SELECT ts_code, name, list_date, industry 
            FROM stock_basic 
            WHERE (ts_code LIKE '%%.SZ' OR ts_code LIKE '%%.SH')
            ORDER BY ts_code
            """
            df = self.db.fetch_data(sql)
            
            if not df.empty:
                return df['ts_code'].tolist()
            else:
                return []
                
        except Exception as e:
            logger.error(f"❌ 获取股票列表失败: {e}")
            return []
    
    def _sync_daily_data_batch(self, stock_list):
        """批量同步日线数据"""
        logger.info(f"📈 开始同步 {len(stock_list)} 只股票的日线数据")
        
        success_count = 0
        batch_size = 20
        
        for i in range(0, len(stock_list), batch_size):
            batch = stock_list[i:i + batch_size]
            
            with ThreadPoolExecutor(max_workers=5) as executor:
                future_to_stock = {
                    executor.submit(self._sync_single_stock_daily, ts_code): ts_code 
                    for ts_code in batch
                }
                
                for future in tqdm(as_completed(future_to_stock), 
                                 total=len(batch), 
                                 desc=f"日线数据批次 {i//batch_size + 1}"):
                    ts_code = future_to_stock[future]
                    try:
                        success = future.result(timeout=60)
                        if success:
                            success_count += 1
                            with self.stats_lock:
                                self.stats['completed_stocks'] += 1
                    except Exception as e:
                        logger.error(f"❌ {ts_code} 日线数据同步失败: {e}")
                        with self.stats_lock:
                            self.stats['failed_stocks'] += 1
            
            # 批次间休息
            time.sleep(3)
            logger.info(f"📊 已完成 {min(i + batch_size, len(stock_list))}/{len(stock_list)} 只股票")
        
        logger.info(f"✅ 日线数据同步完成: {success_count}/{len(stock_list)}")
        return success_count > len(stock_list) * 0.8  # 80%成功率认为成功
    
    def _sync_single_stock_daily(self, ts_code):
        """同步单只股票的日线数据"""
        try:
            # 获取5年日线数据
            df = self.ts_api.daily(
                ts_code=ts_code,
                start_date=self.start_date,
                end_date=self.end_date
            )
            
            if not df.empty:
                # 添加计算字段
                df['change'] = df['close'] - df['pre_close']
                df['change_pct'] = df['pct_chg']
                
                # 按月份分组，插入不同的分表
                success_count = 0
                grouped = df.groupby(df['trade_date'].str[:6])
                
                for year_month, month_data in grouped:
                    table_name = f'stock_daily_{year_month}'
                    
                    # 确保表存在
                    self._ensure_monthly_table_exists(table_name)
                    
                    success = self.db.insert_dataframe(table_name, month_data, replace=False)
                    if success:
                        success_count += 1
                        with self.stats_lock:
                            self.stats['total_records'] += len(month_data)
                
                return success_count > 0
            else:
                return True  # 无数据也算成功
                
        except Exception as e:
            logger.debug(f"❌ {ts_code} 日线数据获取失败: {e}")
            return False
    
    def _ensure_monthly_table_exists(self, table_name):
        """确保月度分表存在"""
        try:
            # 检查表是否存在
            check_sql = f"""
            SELECT COUNT(*) as count 
            FROM information_schema.tables 
            WHERE table_schema = 'ljwx_stock' AND table_name = '{table_name}'
            """
            
            result = self.db.fetch_data(check_sql)
            if result.iloc[0]['count'] == 0:
                # 创建新的月度表
                create_sql = f"""
                CREATE TABLE {table_name} LIKE stock_daily_202507
                """
                self.db.execute_sql(create_sql)
                logger.info(f"📅 创建月度表: {table_name}")
                
        except Exception as e:
            logger.debug(f"月度表检查失败 {table_name}: {e}")
    
    def _sync_technical_indicators_batch(self, stock_list):
        """批量同步技术指标"""
        logger.info("📊 开始计算和同步技术指标")
        
        try:
            # 获取所有日线数据计算技术指标
            all_daily_data = self._get_all_daily_data_for_indicators()
            
            if all_daily_data.empty:
                logger.warning("⚠️ 无日线数据用于计算技术指标")
                return False
            
            logger.info(f"📈 基于 {len(all_daily_data)} 条日线数据计算技术指标")
            
            # 按股票分组计算指标
            all_indicators = []
            
            for ts_code, group_data in tqdm(all_daily_data.groupby('ts_code'), 
                                          desc="计算技术指标"):
                if len(group_data) < 30:  # 至少需要30天数据
                    continue
                
                try:
                    indicators = self._calculate_complete_technical_indicators(ts_code, group_data)
                    if not indicators.empty:
                        all_indicators.append(indicators)
                except Exception as e:
                    logger.debug(f"技术指标计算失败 {ts_code}: {e}")
                    continue
            
            if all_indicators:
                combined_indicators = pd.concat(all_indicators, ignore_index=True)
                combined_indicators['created_at'] = datetime.now()
                
                # 分批插入避免内存问题
                batch_size = 10000
                success_count = 0
                
                for i in range(0, len(combined_indicators), batch_size):
                    batch_data = combined_indicators[i:i + batch_size]
                    success = self.db.insert_dataframe('technical_indicators', batch_data, replace=True)
                    if success:
                        success_count += len(batch_data)
                
                logger.info(f"✅ 技术指标: {success_count:,} 条记录")
                return True
            
            return False
            
        except Exception as e:
            logger.error(f"技术指标同步失败: {e}")
            return False
    
    def _get_all_daily_data_for_indicators(self):
        """获取所有日线数据用于技术指标计算"""
        try:
            # 获取近2年的数据用于计算指标（需要足够的历史数据）
            tables_to_query = []
            for i in range(24):  # 过去24个月
                date = datetime.now() - timedelta(days=30*i)
                table_name = f"stock_daily_{date.strftime('%Y%m')}"
                tables_to_query.append(table_name)
            
            all_data = []
            for table_name in tables_to_query:
                try:
                    sql = f"""
                    SELECT ts_code, trade_date, open, high, low, close, vol, amount
                    FROM {table_name}
                    ORDER BY ts_code, trade_date
                    """
                    data = self.db.fetch_data(sql)
                    if not data.empty:
                        all_data.append(data)
                except:
                    continue
            
            if all_data:
                return pd.concat(all_data, ignore_index=True)
            else:
                return pd.DataFrame()
                
        except Exception as e:
            logger.error(f"获取日线数据失败: {e}")
            return pd.DataFrame()
    
    def _calculate_complete_technical_indicators(self, ts_code, data):
        """计算完整的技术指标，包括MACD和KDJ"""
        try:
            # 确保数据按日期排序
            data = data.sort_values('trade_date').reset_index(drop=True)
            
            if len(data) < 30:
                return pd.DataFrame()
            
            # 准备价格数据
            close = data['close'].values
            high = data['high'].values
            low = data['low'].values
            volume = data['vol'].values
            
            indicators_list = []
            
            # 对每一天计算指标
            for i in range(26, len(data)):  # 从第26天开始，确保有足够数据计算MACD
                trade_date = data.iloc[i]['trade_date']
                
                # 基础技术指标
                indicators = {
                    'ts_code': ts_code,
                    'trade_date': trade_date,
                    'close': close[i],
                    
                    # 移动平均线
                    'ma5': np.mean(close[max(0, i-4):i+1]) if i >= 4 else None,
                    'ma10': np.mean(close[max(0, i-9):i+1]) if i >= 9 else None,
                    'ma20': np.mean(close[max(0, i-19):i+1]) if i >= 19 else None,
                    'ma60': np.mean(close[max(0, i-59):i+1]) if i >= 59 else None,
                    
                    # RSI相对强弱指标
                    'rsi6': self._calculate_rsi(close[max(0, i-6):i+1]) if i >= 6 else None,
                    'rsi12': self._calculate_rsi(close[max(0, i-12):i+1]) if i >= 12 else None,
                    'rsi24': self._calculate_rsi(close[max(0, i-24):i+1]) if i >= 24 else None,
                    
                    # 成交量指标
                    'vol_ratio': volume[i] / np.mean(volume[max(0, i-4):i+1]) if i >= 4 and np.mean(volume[max(0, i-4):i+1]) > 0 else None,
                }
                
                # MACD指标计算
                if i >= 25:  # 确保有足够数据计算MACD
                    macd_data = self._calculate_macd(close[:i+1])
                    indicators.update(macd_data)
                
                # KDJ指标计算
                if i >= 8:  # KDJ需要9天数据
                    kdj_data = self._calculate_kdj(high[max(0, i-8):i+1], 
                                                 low[max(0, i-8):i+1], 
                                                 close[max(0, i-8):i+1])
                    indicators.update(kdj_data)
                
                # 布林带指标
                if i >= 19:
                    boll_data = self._calculate_bollinger_bands(close[max(0, i-19):i+1])
                    indicators.update(boll_data)
                
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
    
    def _calculate_macd(self, prices):
        """计算MACD指标"""
        try:
            if len(prices) < 26:
                return {'macd_dif': None, 'macd_dea': None, 'macd_macd': None}
            
            # 计算EMA
            ema12 = self._calculate_ema(prices, 12)
            ema26 = self._calculate_ema(prices, 26)
            
            # DIF线
            dif = ema12 - ema26
            
            # DEA线（DIF的9日EMA）
            if len(prices) >= 34:  # 26 + 9 - 1
                dif_series = []
                for i in range(25, len(prices)):
                    temp_ema12 = self._calculate_ema(prices[:i+1], 12)
                    temp_ema26 = self._calculate_ema(prices[:i+1], 26)
                    dif_series.append(temp_ema12 - temp_ema26)
                
                dea = self._calculate_ema(np.array(dif_series), 9)
                macd = 2 * (dif - dea)
                
                return {
                    'macd_dif': round(dif, 4),
                    'macd_dea': round(dea, 4),
                    'macd_macd': round(macd, 4)
                }
            
            return {
                'macd_dif': round(dif, 4),
                'macd_dea': None,
                'macd_macd': None
            }
            
        except:
            return {'macd_dif': None, 'macd_dea': None, 'macd_macd': None}
    
    def _calculate_ema(self, prices, period):
        """计算指数移动平均"""
        try:
            alpha = 2 / (period + 1)
            ema = prices[0]
            
            for price in prices[1:]:
                ema = alpha * price + (1 - alpha) * ema
                
            return ema
        except:
            return None
    
    def _calculate_kdj(self, high, low, close):
        """计算KDJ指标"""
        try:
            if len(high) < 9:
                return {'kdj_k': None, 'kdj_d': None, 'kdj_j': None}
            
            # 计算RSV
            lowest_low = np.min(low)
            highest_high = np.max(high)
            
            if highest_high == lowest_low:
                rsv = 50
            else:
                rsv = (close[-1] - lowest_low) / (highest_high - lowest_low) * 100
            
            # 简化的KDJ计算
            k = rsv * 0.33 + 50 * 0.67  # 简化处理
            d = k * 0.33 + 50 * 0.67
            j = 3 * k - 2 * d
            
            return {
                'kdj_k': round(k, 2),
                'kdj_d': round(d, 2),
                'kdj_j': round(j, 2)
            }
            
        except:
            return {'kdj_k': None, 'kdj_d': None, 'kdj_j': None}
    
    def _calculate_bollinger_bands(self, prices):
        """计算布林带指标"""
        try:
            if len(prices) < 20:
                return {'boll_upper': None, 'boll_mid': None, 'boll_lower': None}
            
            mid = np.mean(prices)
            std = np.std(prices)
            
            return {
                'boll_upper': round(mid + 2 * std, 2),
                'boll_mid': round(mid, 2),
                'boll_lower': round(mid - 2 * std, 2)
            }
            
        except:
            return {'boll_upper': None, 'boll_mid': None, 'boll_lower': None}
    
    def _sync_basic_indicators_batch(self):
        """批量同步基本指标数据（PE、PB等）"""
        logger.info("📊 开始同步历史基本指标数据")
        
        try:
            # 获取过去1年的基本指标数据
            total_records = 0
            
            # 按月获取数据
            for i in range(60):  # 过去5年，每月获取一次
                target_date = datetime.now() - timedelta(days=30*i)
                trade_date = target_date.strftime('%Y%m%d')
                
                try:
                    # 获取该日期的基本指标
                    basic_df = self.ts_api.daily_basic(trade_date=trade_date)
                    
                    if not basic_df.empty:
                        basic_df['created_at'] = datetime.now()
                        success = self.db.insert_dataframe('stock_indicators', basic_df, replace=False)
                        if success:
                            total_records += len(basic_df)
                            logger.info(f"📅 {trade_date} 基本指标: {len(basic_df)} 条记录")
                    
                    time.sleep(0.5)  # API限制
                    
                except Exception as e:
                    if "没有数据" not in str(e):
                        logger.debug(f"基本指标 {trade_date}: {e}")
                    continue
            
            logger.info(f"✅ 历史基本指标同步完成: {total_records:,} 条记录")
            return total_records > 0
            
        except Exception as e:
            logger.error(f"历史基本指标同步失败: {e}")
            return False
    
    def _sync_limit_data_batch(self):
        """批量同步涨跌停历史数据"""
        logger.info("📊 开始同步历史涨跌停数据")
        
        try:
            total_records = 0
            
            # 获取过去1年的涨跌停数据
            for i in range(365):
                trade_date = (datetime.now() - timedelta(days=i)).strftime('%Y%m%d')
                
                # 跳过周末
                date_obj = datetime.strptime(trade_date, '%Y%m%d')
                if date_obj.weekday() >= 5:
                    continue
                
                try:
                    all_limit_data = []
                    
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
                    
                    if all_limit_data:
                        combined_df = pd.concat(all_limit_data, ignore_index=True)
                        combined_df['created_at'] = datetime.now()
                        
                        success = self.db.insert_dataframe('t_limit_price', combined_df, replace=False)
                        if success:
                            total_records += len(combined_df)
                    
                    time.sleep(0.2)
                    
                except Exception as e:
                    if "没有数据" not in str(e):
                        logger.debug(f"涨跌停 {trade_date}: {e}")
                    continue
                
                # 每100天输出一次进度
                if i % 100 == 0:
                    logger.info(f"📅 涨跌停数据进度: {i}/365 天, 已获取 {total_records} 条记录")
            
            logger.info(f"✅ 历史涨跌停数据: {total_records:,} 条记录")
            return True
            
        except Exception as e:
            logger.error(f"历史涨跌停数据同步失败: {e}")
            return False
    
    def _sync_suspend_data_batch(self):
        """批量同步停复牌历史数据"""
        logger.info("📊 开始同步历史停复牌数据")
        
        try:
            # 获取过去5年的停复牌数据
            suspend_df = self.ts_api.suspend_d(
                start_date=self.start_date, 
                end_date=self.end_date
            )
            
            if not suspend_df.empty:
                suspend_df['created_at'] = datetime.now()
                success = self.db.insert_dataframe('t_suspend', suspend_df, replace=False)
                if success:
                    logger.info(f"✅ 历史停复牌数据: {len(suspend_df):,} 条记录")
                    return True
            
            return False
            
        except Exception as e:
            logger.error(f"历史停复牌数据同步失败: {e}")
            return False
    
    def _sync_dividend_data_batch(self):
        """批量同步分红送股历史数据"""
        logger.info("📊 开始同步历史分红送股数据")
        
        try:
            # 获取过去5年的分红数据
            dividend_df = self.ts_api.dividend(
                start_date=self.start_date,
                end_date=self.end_date
            )
            
            if not dividend_df.empty:
                dividend_df['created_at'] = datetime.now()
                success = self.db.insert_dataframe('t_dividend', dividend_df, replace=False)
                if success:
                    logger.info(f"✅ 历史分红数据: {len(dividend_df):,} 条记录")
                    return True
            
            return False
            
        except Exception as e:
            logger.error(f"历史分红数据同步失败: {e}")
            return False
    
    def _print_final_stats(self):
        """打印最终统计信息"""
        end_time = datetime.now()
        duration = end_time - self.stats['start_time']
        
        logger.info("=" * 80)
        logger.info("🎉 历史数据全量同步完成！（5年数据）")
        logger.info(f"📊 总股票数: {self.stats['total_stocks']}")
        logger.info(f"✅ 成功同步: {self.stats['completed_stocks']}")
        logger.info(f"❌ 失败数量: {self.stats['failed_stocks']}")
        logger.info(f"📈 总记录数: {self.stats['total_records']:,}")
        logger.info(f"⏱️ 总耗时: {duration}")
        logger.info(f"🚀 同步速度: {self.stats['total_records'] / duration.total_seconds():.2f} 记录/秒")
        
        # 数据库统计
        self._print_database_summary()
    
    def _print_database_summary(self):
        """打印数据库汇总信息"""
        try:
            tables_info = [
                ('股票基本信息', 'stock_basic'),
                ('技术指标', 'technical_indicators'),
                ('基本指标', 'stock_indicators'),
                ('涨跌停', 't_limit_price'),
                ('停复牌', 't_suspend'),
                ('分红送股', 't_dividend')
            ]
            
            logger.info("📊 数据库汇总统计:")
            total_records = 0
            
            for name, table in tables_info:
                try:
                    count = self.db.fetch_data(f'SELECT COUNT(*) as count FROM {table}').iloc[0]['count']
                    logger.info(f"   {name}: {count:,} 条记录")
                    total_records += count
                except:
                    logger.info(f"   {name}: 表不存在或无数据")
            
            # 统计所有月度分表
            monthly_tables = self.db.fetch_data("""
                SELECT table_name, table_rows
                FROM information_schema.tables 
                WHERE table_schema = 'ljwx_stock' 
                AND table_name LIKE 'stock_daily_%'
                ORDER BY table_name
            """)
            
            if not monthly_tables.empty:
                monthly_total = monthly_tables['table_rows'].sum()
                logger.info(f"   日线数据 ({len(monthly_tables)}个月表): {monthly_total:,} 条记录")
                total_records += monthly_total
            
            logger.info(f"📈 数据库总计: {total_records:,} 条记录")
            
        except Exception as e:
            logger.error(f"获取数据库统计失败: {e}")

def main():
    """主函数"""
    import argparse
    
    parser = argparse.ArgumentParser(description='历史数据全量同步系统 (5年)')
    parser.add_argument('--test', action='store_true', help='测试模式')
    
    args = parser.parse_args()
    
    syncer = HistoricalDataSync()
    
    try:
        if args.test:
            logger.info("🧪 测试模式 - 仅同步少量数据")
            # 在测试模式下可以限制数据范围
            syncer.start_date = (datetime.now() - timedelta(days=30)).strftime('%Y%m%d')
        
        success = syncer.sync_all_historical_data()
        
        if success:
            logger.info("🎉 历史数据全量同步成功完成！")
        else:
            logger.warning("⚠️ 历史数据同步未完全成功")
            
    except KeyboardInterrupt:
        logger.info("⏸️ 用户中断操作")
    except Exception as e:
        logger.error(f"❌ 系统异常: {e}")

if __name__ == "__main__":
    main()