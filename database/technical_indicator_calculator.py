#!/usr/bin/env python3
"""
技术指标预计算系统
实现高效的技术指标批量计算和缓存
"""

import os
import time
import logging
import pandas as pd
import numpy as np
import mysql.connector
import sqlite3
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Any, Tuple
from dataclasses import dataclass
from concurrent.futures import ThreadPoolExecutor, ProcessPoolExecutor, as_completed
import talib
from numba import jit
import warnings

# 忽略警告
warnings.filterwarnings('ignore')

# 设置日志
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

@dataclass
class IndicatorCalculationResult:
    """指标计算结果"""
    success: bool
    stock_code: str
    calculated_indicators: Dict[str, Any]
    calculation_time: float
    error_message: Optional[str] = None

class TechnicalIndicatorCalculator:
    """技术指标计算器"""
    
    def __init__(self, db_config: Dict[str, Any]):
        """
        初始化技术指标计算器
        
        Args:
            db_config: 数据库配置
        """
        self.db_config = db_config
        self.db_type = db_config.get('type', 'mysql')
        self.batch_size = 100
        
        # 支持的指标列表
        self.supported_indicators = {
            # 移动平均线
            'ma': [5, 10, 20, 30, 60, 120, 250],
            'ema': [12, 26],
            
            # 趋势指标
            'macd': {'fast': 12, 'slow': 26, 'signal': 9},
            'adx': {'period': 14},
            'aroon': {'period': 14},
            
            # 摆荡指标
            'rsi': [6, 12, 24],
            'stoch': {'k_period': 9, 'd_period': 3},
            'kdj': {'k_period': 9, 'd_period': 3, 'j_period': 3},
            'williams_r': {'period': 14},
            'cci': {'period': 20},
            
            # 波动率指标
            'bollinger': {'period': 20, 'std': 2},
            'atr': {'period': 14},
            'kelch': {'period': 20, 'multiplier': 2},
            
            # 成交量指标
            'obv': {},
            'vwap': {},
            'mfi': {'period': 14},
            'ad_line': {},
            
            # 其他指标
            'pivot_points': {},
            'fibonacci': {},
            'ichimoku': {'conversion': 9, 'base': 26, 'span': 52}
        }
        
        # 缓存配置
        self.use_cache = True
        self.cache_duration = 3600  # 1小时缓存
        
    def get_connection(self):
        """获取数据库连接"""
        if self.db_type == 'mysql':
            return mysql.connector.connect(
                host=self.db_config['host'],
                port=self.db_config['port'],
                user=self.db_config['user'],
                password=self.db_config['password'],
                database=self.db_config['database']
            )
        else:
            return sqlite3.connect(self.db_config['database'])
    
    def calculate_all_indicators_batch(self, stock_codes: List[str] = None, 
                                     trade_date: str = None, 
                                     indicators: List[str] = None,
                                     parallel: bool = True) -> List[IndicatorCalculationResult]:
        """
        批量计算技术指标
        
        Args:
            stock_codes: 股票代码列表，为空则计算所有活跃股票
            trade_date: 交易日期，为空则计算最新数据
            indicators: 要计算的指标列表，为空则计算所有支持的指标
            parallel: 是否并行计算
            
        Returns:
            计算结果列表
        """
        start_time = time.time()
        
        if not stock_codes:
            stock_codes = self._get_active_stock_codes()
        
        if not trade_date:
            trade_date = self._get_last_trade_date()
        
        if not indicators:
            indicators = list(self.supported_indicators.keys())
        
        logger.info(f"开始批量计算技术指标: {len(stock_codes)}只股票, {len(indicators)}个指标")
        
        results = []
        
        if parallel:
            # 并行计算
            with ProcessPoolExecutor(max_workers=min(4, os.cpu_count())) as executor:
                # 分批提交任务
                futures = []
                for i in range(0, len(stock_codes), self.batch_size):
                    batch_codes = stock_codes[i:i + self.batch_size]
                    future = executor.submit(
                        self._calculate_batch_indicators_worker,
                        batch_codes, trade_date, indicators
                    )
                    futures.append(future)
                
                # 收集结果
                for future in as_completed(futures):
                    try:
                        batch_results = future.result()
                        results.extend(batch_results)
                    except Exception as e:
                        logger.error(f"批次计算失败: {e}")
        else:
            # 串行计算
            for i in range(0, len(stock_codes), self.batch_size):
                batch_codes = stock_codes[i:i + self.batch_size]
                batch_results = self._calculate_batch_indicators(batch_codes, trade_date, indicators)
                results.extend(batch_results)
        
        # 批量保存结果
        self._batch_save_indicators(results, trade_date)
        
        duration = time.time() - start_time
        success_count = sum(1 for r in results if r.success)
        
        logger.info(f"技术指标计算完成: 成功{success_count}/{len(results)}, 耗时{duration:.2f}秒")
        
        return results
    
    def _calculate_batch_indicators_worker(self, stock_codes: List[str], 
                                         trade_date: str, 
                                         indicators: List[str]) -> List[IndicatorCalculationResult]:
        """并行计算工作进程"""
        return self._calculate_batch_indicators(stock_codes, trade_date, indicators)
    
    def _calculate_batch_indicators(self, stock_codes: List[str], 
                                  trade_date: str, 
                                  indicators: List[str]) -> List[IndicatorCalculationResult]:
        """计算一批股票的技术指标"""
        results = []
        
        for stock_code in stock_codes:
            try:
                result = self._calculate_stock_indicators(stock_code, trade_date, indicators)
                results.append(result)
            except Exception as e:
                logger.error(f"计算{stock_code}指标失败: {e}")
                results.append(IndicatorCalculationResult(
                    success=False,
                    stock_code=stock_code,
                    calculated_indicators={},
                    calculation_time=0,
                    error_message=str(e)
                ))
        
        return results
    
    def _calculate_stock_indicators(self, stock_code: str, 
                                  trade_date: str, 
                                  indicators: List[str]) -> IndicatorCalculationResult:
        """计算单只股票的技术指标"""
        start_time = time.time()
        
        try:
            # 获取历史数据
            stock_data = self._get_stock_historical_data(stock_code, trade_date)
            
            if stock_data.empty or len(stock_data) < 250:  # 需要至少一年的数据
                return IndicatorCalculationResult(
                    success=False,
                    stock_code=stock_code,
                    calculated_indicators={},
                    calculation_time=time.time() - start_time,
                    error_message="历史数据不足"
                )
            
            # 计算各类指标
            calculated_indicators = {}
            
            for indicator in indicators:
                try:
                    if indicator in self.supported_indicators:
                        indicator_values = self._calculate_single_indicator(
                            stock_data, indicator, self.supported_indicators[indicator]
                        )
                        calculated_indicators.update(indicator_values)
                except Exception as e:
                    logger.warning(f"计算{stock_code}的{indicator}指标失败: {e}")
                    continue
            
            return IndicatorCalculationResult(
                success=True,
                stock_code=stock_code,
                calculated_indicators=calculated_indicators,
                calculation_time=time.time() - start_time
            )
            
        except Exception as e:
            return IndicatorCalculationResult(
                success=False,
                stock_code=stock_code,
                calculated_indicators={},
                calculation_time=time.time() - start_time,
                error_message=str(e)
            )
    
    def _calculate_single_indicator(self, data: pd.DataFrame, 
                                   indicator: str, 
                                   params: Any) -> Dict[str, float]:
        """计算单个技术指标"""
        result = {}
        
        # 提取价格数据
        high = data['high_price'].values
        low = data['low_price'].values
        close = data['close_price'].values
        open_price = data['open_price'].values
        volume = data['volume'].values
        
        try:
            if indicator == 'ma':
                # 移动平均线
                for period in params:
                    ma_values = talib.SMA(close, timeperiod=period)
                    result[f'ma{period}'] = ma_values[-1] if not np.isnan(ma_values[-1]) else None
            
            elif indicator == 'ema':
                # 指数移动平均线
                for period in params:
                    ema_values = talib.EMA(close, timeperiod=period)
                    result[f'ema{period}'] = ema_values[-1] if not np.isnan(ema_values[-1]) else None
            
            elif indicator == 'rsi':
                # RSI指标
                for period in params:
                    rsi_values = talib.RSI(close, timeperiod=period)
                    result[f'rsi{period}'] = rsi_values[-1] if not np.isnan(rsi_values[-1]) else None
            
            elif indicator == 'macd':
                # MACD指标
                macd_dif, macd_dea, macd_hist = talib.MACD(
                    close, 
                    fastperiod=params['fast'],
                    slowperiod=params['slow'],
                    signalperiod=params['signal']
                )
                result.update({
                    'macd_dif': macd_dif[-1] if not np.isnan(macd_dif[-1]) else None,
                    'macd_dea': macd_dea[-1] if not np.isnan(macd_dea[-1]) else None,
                    'macd_macd': macd_hist[-1] if not np.isnan(macd_hist[-1]) else None
                })
            
            elif indicator == 'kdj':
                # KDJ指标（基于随机指标计算）
                k_values, d_values = talib.STOCH(
                    high, low, close,
                    fastk_period=params['k_period'],
                    slowk_period=params['d_period'],
                    slowd_period=params['j_period']
                )
                if not np.isnan(k_values[-1]) and not np.isnan(d_values[-1]):
                    j_value = 3 * k_values[-1] - 2 * d_values[-1]
                    result.update({
                        'kdj_k': k_values[-1],
                        'kdj_d': d_values[-1],
                        'kdj_j': j_value
                    })
            
            elif indicator == 'bollinger':
                # 布林带
                upper, middle, lower = talib.BBANDS(
                    close,
                    timeperiod=params['period'],
                    nbdevup=params['std'],
                    nbdevdn=params['std']
                )
                result.update({
                    'boll_upper': upper[-1] if not np.isnan(upper[-1]) else None,
                    'boll_mid': middle[-1] if not np.isnan(middle[-1]) else None,
                    'boll_lower': lower[-1] if not np.isnan(lower[-1]) else None
                })
            
            elif indicator == 'atr':
                # 真实波动幅度
                atr_values = talib.ATR(high, low, close, timeperiod=params['period'])
                result['atr'] = atr_values[-1] if not np.isnan(atr_values[-1]) else None
            
            elif indicator == 'williams_r':
                # 威廉指标
                wr_values = talib.WILLR(high, low, close, timeperiod=params['period'])
                result['williams_r'] = wr_values[-1] if not np.isnan(wr_values[-1]) else None
            
            elif indicator == 'cci':
                # 商品通道指数
                cci_values = talib.CCI(high, low, close, timeperiod=params['period'])
                result['cci'] = cci_values[-1] if not np.isnan(cci_values[-1]) else None
            
            elif indicator == 'obv':
                # 能量潮
                obv_values = talib.OBV(close, volume)
                result['obv'] = obv_values[-1] if not np.isnan(obv_values[-1]) else None
            
            elif indicator == 'vwap':
                # 成交量加权平均价
                vwap_value = self._calculate_vwap(data)
                result['vwap'] = vwap_value
            
            elif indicator == 'adx':
                # 平均趋向指数
                adx_values = talib.ADX(high, low, close, timeperiod=params['period'])
                result['adx'] = adx_values[-1] if not np.isnan(adx_values[-1]) else None
            
            elif indicator == 'mfi':
                # 资金流量指数
                mfi_values = talib.MFI(high, low, close, volume, timeperiod=params['period'])
                result['mfi'] = mfi_values[-1] if not np.isnan(mfi_values[-1]) else None
            
            elif indicator == 'stoch':
                # 随机指标
                slowk, slowd = talib.STOCH(
                    high, low, close,
                    fastk_period=params['k_period'],
                    slowk_period=params['d_period'],
                    slowd_period=params['d_period']
                )
                result.update({
                    'stoch_k': slowk[-1] if not np.isnan(slowk[-1]) else None,
                    'stoch_d': slowd[-1] if not np.isnan(slowd[-1]) else None
                })
            
        except Exception as e:
            logger.warning(f"计算{indicator}指标时发生错误: {e}")
        
        return result
    
    @jit(nopython=True)
    def _calculate_rsi_fast(self, prices: np.ndarray, period: int) -> np.ndarray:
        """快速RSI计算（使用numba加速）"""
        n = len(prices)
        rsi = np.full(n, np.nan)
        
        if n < period + 1:
            return rsi
        
        # 计算价格变化
        deltas = np.diff(prices)
        gains = np.where(deltas > 0, deltas, 0.0)
        losses = np.where(deltas < 0, -deltas, 0.0)
        
        # 初始平均值
        avg_gain = np.mean(gains[:period])
        avg_loss = np.mean(losses[:period])
        
        if avg_loss == 0:
            rsi[period] = 100.0
        else:
            rs = avg_gain / avg_loss
            rsi[period] = 100.0 - (100.0 / (1.0 + rs))
        
        # 滚动计算
        for i in range(period + 1, n):
            avg_gain = ((avg_gain * (period - 1)) + gains[i - 1]) / period
            avg_loss = ((avg_loss * (period - 1)) + losses[i - 1]) / period
            
            if avg_loss == 0:
                rsi[i] = 100.0
            else:
                rs = avg_gain / avg_loss
                rsi[i] = 100.0 - (100.0 / (1.0 + rs))
        
        return rsi
    
    def _calculate_vwap(self, data: pd.DataFrame) -> Optional[float]:
        """计算成交量加权平均价"""
        try:
            if len(data) == 0:
                return None
            
            # 典型价格
            typical_price = (data['high_price'] + data['low_price'] + data['close_price']) / 3
            
            # VWAP计算
            volume_price = (typical_price * data['volume']).sum()
            total_volume = data['volume'].sum()
            
            if total_volume == 0:
                return None
            
            return volume_price / total_volume
            
        except Exception as e:
            logger.warning(f"计算VWAP失败: {e}")
            return None
    
    def _get_stock_historical_data(self, stock_code: str, end_date: str, 
                                 days: int = 300) -> pd.DataFrame:
        """获取股票历史数据"""
        try:
            connection = self.get_connection()
            cursor = connection.cursor()
            
            # 计算开始日期
            end_dt = datetime.strptime(end_date, '%Y-%m-%d')
            start_dt = end_dt - timedelta(days=days)
            start_date = start_dt.strftime('%Y-%m-%d')
            
            # 提取股票代码（去掉交易所后缀）
            if '.' in stock_code:
                clean_stock_code = stock_code.split('.')[0]
            else:
                clean_stock_code = stock_code
            
            if self.db_type == 'mysql':
                sql = """
                SELECT trade_date, open_price, high_price, low_price, close_price, 
                       pre_close, volume, amount, change_pct
                FROM stock_daily 
                WHERE stock_code = %s AND trade_date >= %s AND trade_date <= %s
                ORDER BY trade_date
                """
                cursor.execute(sql, (clean_stock_code, start_date, end_date))
            else:
                sql = """
                SELECT trade_date, open_price, high_price, low_price, close_price,
                       pre_close, volume, amount, change_pct
                FROM stock_daily 
                WHERE stock_code = ? AND trade_date >= ? AND trade_date <= ?
                ORDER BY trade_date
                """
                cursor.execute(sql, (clean_stock_code, start_date, end_date))
            
            rows = cursor.fetchall()
            
            if not rows:
                return pd.DataFrame()
            
            # 构建DataFrame
            columns = ['trade_date', 'open_price', 'high_price', 'low_price', 'close_price',
                      'pre_close', 'volume', 'amount', 'change_pct']
            
            df = pd.DataFrame(rows, columns=columns)
            df['trade_date'] = pd.to_datetime(df['trade_date'])
            
            # 确保数值类型
            numeric_columns = ['open_price', 'high_price', 'low_price', 'close_price',
                             'pre_close', 'volume', 'amount', 'change_pct']
            for col in numeric_columns:
                df[col] = pd.to_numeric(df[col], errors='coerce')
            
            # 过滤无效数据
            df = df.dropna(subset=['open_price', 'high_price', 'low_price', 'close_price'])
            df = df[df['close_price'] > 0]
            
            return df
            
        except Exception as e:
            logger.error(f"获取{stock_code}历史数据失败: {e}")
            return pd.DataFrame()
        finally:
            if 'connection' in locals():
                connection.close()
    
    def _batch_save_indicators(self, results: List[IndicatorCalculationResult], 
                             trade_date: str) -> bool:
        """批量保存指标计算结果"""
        try:
            connection = self.get_connection()
            cursor = connection.cursor()
            
            # 成功的结果
            success_results = [r for r in results if r.success and r.calculated_indicators]
            
            if not success_results:
                logger.warning("没有成功的指标计算结果需要保存")
                return False
            
            if self.db_type == 'mysql':
                sql = """
                INSERT INTO technical_indicators 
                (stock_code, trade_date, ma5, ma10, ma20, ma30, ma60, ma120, ma250,
                 rsi6, rsi12, rsi24, macd_dif, macd_dea, macd_macd,
                 kdj_k, kdj_d, kdj_j, boll_upper, boll_mid, boll_lower,
                 atr, cci, williams_r, obv, vwap)
                VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, 
                        %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s)
                ON DUPLICATE KEY UPDATE
                ma5 = VALUES(ma5), ma10 = VALUES(ma10), ma20 = VALUES(ma20),
                ma30 = VALUES(ma30), ma60 = VALUES(ma60), ma120 = VALUES(ma120), ma250 = VALUES(ma250),
                rsi6 = VALUES(rsi6), rsi12 = VALUES(rsi12), rsi24 = VALUES(rsi24),
                macd_dif = VALUES(macd_dif), macd_dea = VALUES(macd_dea), macd_macd = VALUES(macd_macd),
                kdj_k = VALUES(kdj_k), kdj_d = VALUES(kdj_d), kdj_j = VALUES(kdj_j),
                boll_upper = VALUES(boll_upper), boll_mid = VALUES(boll_mid), boll_lower = VALUES(boll_lower),
                atr = VALUES(atr), cci = VALUES(cci), williams_r = VALUES(williams_r),
                obv = VALUES(obv), vwap = VALUES(vwap),
                updated_at = CURRENT_TIMESTAMP
                """
            else:
                sql = """
                INSERT OR REPLACE INTO technical_indicators 
                (stock_code, trade_date, ma5, ma10, ma20, ma30, ma60, ma120, ma250,
                 rsi6, rsi12, rsi24, macd_dif, macd_dea, macd_macd,
                 kdj_k, kdj_d, kdj_j, boll_upper, boll_mid, boll_lower,
                 atr, cci, williams_r, obv, vwap)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                """
            
            success_count = 0
            
            for result in success_results:
                try:
                    # 提取股票代码（去掉交易所后缀）
                    if '.' in result.stock_code:
                        clean_stock_code = result.stock_code.split('.')[0]
                    else:
                        clean_stock_code = result.stock_code
                    
                    # 构建参数元组
                    params = (
                        clean_stock_code,
                        trade_date,
                        result.calculated_indicators.get('ma5'),
                        result.calculated_indicators.get('ma10'),
                        result.calculated_indicators.get('ma20'),
                        result.calculated_indicators.get('ma30'),
                        result.calculated_indicators.get('ma60'),
                        result.calculated_indicators.get('ma120'),
                        result.calculated_indicators.get('ma250'),
                        result.calculated_indicators.get('rsi6'),
                        result.calculated_indicators.get('rsi12'),
                        result.calculated_indicators.get('rsi24'),
                        result.calculated_indicators.get('macd_dif'),
                        result.calculated_indicators.get('macd_dea'),
                        result.calculated_indicators.get('macd_macd'),
                        result.calculated_indicators.get('kdj_k'),
                        result.calculated_indicators.get('kdj_d'),
                        result.calculated_indicators.get('kdj_j'),
                        result.calculated_indicators.get('boll_upper'),
                        result.calculated_indicators.get('boll_mid'),
                        result.calculated_indicators.get('boll_lower'),
                        result.calculated_indicators.get('atr'),
                        result.calculated_indicators.get('cci'),
                        result.calculated_indicators.get('williams_r'),
                        result.calculated_indicators.get('obv'),
                        result.calculated_indicators.get('vwap')
                    )
                    
                    cursor.execute(sql, params)
                    success_count += 1
                    
                except Exception as e:
                    logger.warning(f"保存{result.stock_code}指标失败: {e}")
                    continue
            
            connection.commit()
            logger.info(f"成功保存{success_count}条技术指标记录")
            
            return True
            
        except Exception as e:
            logger.error(f"批量保存技术指标失败: {e}")
            return False
        finally:
            if 'connection' in locals():
                connection.close()
    
    def get_indicators_for_stock(self, stock_code: str, 
                               trade_date: str = None) -> Dict[str, Any]:
        """
        获取股票的技术指标
        
        Args:
            stock_code: 股票代码
            trade_date: 交易日期，为空则获取最新数据
            
        Returns:
            技术指标字典
        """
        try:
            connection = self.get_connection()
            cursor = connection.cursor()
            
            if not trade_date:
                trade_date = self._get_last_trade_date()
            
            # 提取股票代码
            if '.' in stock_code:
                clean_stock_code = stock_code.split('.')[0]
            else:
                clean_stock_code = stock_code
            
            if self.db_type == 'mysql':
                sql = """
                SELECT * FROM technical_indicators 
                WHERE stock_code = %s AND trade_date = %s
                """
                cursor.execute(sql, (clean_stock_code, trade_date))
            else:
                sql = """
                SELECT * FROM technical_indicators 
                WHERE stock_code = ? AND trade_date = ?
                """
                cursor.execute(sql, (clean_stock_code, trade_date))
            
            row = cursor.fetchone()
            
            if not row:
                return {}
            
            # 获取列名
            if self.db_type == 'mysql':
                columns = [desc[0] for desc in cursor.description]
            else:
                columns = [desc[0] for desc in cursor.description]
            
            # 构建结果字典
            indicators = dict(zip(columns, row))
            
            # 移除非指标字段
            indicators.pop('id', None)
            indicators.pop('stock_code', None)
            indicators.pop('trade_date', None)
            indicators.pop('created_at', None)
            indicators.pop('updated_at', None)
            
            return indicators
            
        except Exception as e:
            logger.error(f"获取{stock_code}技术指标失败: {e}")
            return {}
        finally:
            if 'connection' in locals():
                connection.close()
    
    def _get_active_stock_codes(self) -> List[str]:
        """获取活跃股票代码列表"""
        try:
            connection = self.get_connection()
            cursor = connection.cursor()
            
            if self.db_type == 'mysql':
                cursor.execute("""
                    SELECT CONCAT(stock_code, CASE 
                        WHEN market IN ('gem', 'star') THEN '.SZ'
                        WHEN SUBSTRING(stock_code, 1, 1) IN ('0', '3') THEN '.SZ'
                        ELSE '.SH'
                    END) as ts_code
                    FROM stock_basic 
                    WHERE is_active = 1
                    ORDER BY stock_code
                """)
            else:
                cursor.execute("""
                    SELECT stock_code || 
                           CASE 
                               WHEN substr(stock_code, 1, 1) IN ('0', '3') THEN '.SZ'
                               ELSE '.SH'
                           END as ts_code
                    FROM stock_basic 
                    WHERE is_active = 1
                    ORDER BY stock_code
                """)
            
            result = [row[0] for row in cursor.fetchall()]
            return result
            
        except Exception as e:
            logger.error(f"获取活跃股票列表失败: {e}")
            return []
        finally:
            if 'connection' in locals():
                connection.close()
    
    def _get_last_trade_date(self) -> str:
        """获取最后交易日"""
        try:
            connection = self.get_connection()
            cursor = connection.cursor()
            
            cursor.execute("""
                SELECT MAX(trade_date) FROM stock_daily
                WHERE trade_date <= CURDATE()
            """)
            
            result = cursor.fetchone()
            if result and result[0]:
                return result[0].strftime('%Y-%m-%d')
            
        except Exception as e:
            logger.warning(f"获取最后交易日失败: {e}")
        
        # 备选方案
        today = datetime.now()
        if today.weekday() == 0:  # 周一
            last_trade = today - timedelta(days=3)
        elif today.weekday() == 6:  # 周日
            last_trade = today - timedelta(days=2)
        else:
            last_trade = today - timedelta(days=1)
        
        return last_trade.strftime('%Y-%m-%d')
        
        if 'connection' in locals():
            connection.close()
    
    def get_calculation_statistics(self) -> Dict[str, Any]:
        """获取计算统计信息"""
        try:
            connection = self.get_connection()
            cursor = connection.cursor()
            
            # 统计总记录数
            cursor.execute("SELECT COUNT(*) FROM technical_indicators")
            total_records = cursor.fetchone()[0]
            
            # 统计最新日期
            cursor.execute("SELECT MAX(trade_date) FROM technical_indicators")
            latest_date = cursor.fetchone()[0]
            
            # 统计股票数量
            cursor.execute("SELECT COUNT(DISTINCT stock_code) FROM technical_indicators")
            stock_count = cursor.fetchone()[0]
            
            # 统计各指标的完整性
            indicator_columns = ['ma5', 'ma10', 'ma20', 'rsi6', 'rsi12', 'macd_dif', 'kdj_k', 'atr']
            completeness = {}
            
            for col in indicator_columns:
                cursor.execute(f"SELECT COUNT(*) FROM technical_indicators WHERE {col} IS NOT NULL")
                valid_count = cursor.fetchone()[0]
                completeness[col] = (valid_count / total_records * 100) if total_records > 0 else 0
            
            return {
                'total_records': total_records,
                'latest_date': latest_date.strftime('%Y-%m-%d') if latest_date else None,
                'stock_count': stock_count,
                'indicator_completeness': completeness,
                'last_updated': datetime.now().isoformat()
            }
            
        except Exception as e:
            logger.error(f"获取计算统计失败: {e}")
            return {'error': str(e)}
        finally:
            if 'connection' in locals():
                connection.close()

def main():
    """主函数 - 技术指标计算测试"""
    # 数据库配置
    db_config = {
        'type': 'sqlite',
        'database': 'data/stock_analysis.db'
    }
    
    # 创建计算器
    calculator = TechnicalIndicatorCalculator(db_config)
    
    print("=== 技术指标预计算系统测试 ===")
    
    # 测试单只股票指标计算
    print("\n1. 测试单只股票指标计算:")
    test_stocks = ['000001.SZ', '000002.SZ']
    
    results = calculator.calculate_all_indicators_batch(
        stock_codes=test_stocks,
        indicators=['ma', 'rsi', 'macd', 'kdj', 'bollinger'],
        parallel=False
    )
    
    for result in results:
        if result.success:
            print(f"  {result.stock_code}: 成功计算{len(result.calculated_indicators)}个指标")
            print(f"    计算耗时: {result.calculation_time:.3f}秒")
            # 显示部分指标
            for key, value in list(result.calculated_indicators.items())[:5]:
                print(f"    {key}: {value}")
        else:
            print(f"  {result.stock_code}: 计算失败 - {result.error_message}")
    
    # 获取统计信息
    print("\n2. 计算统计信息:")
    stats = calculator.get_calculation_statistics()
    print(f"  总记录数: {stats.get('total_records', 0)}")
    print(f"  覆盖股票数: {stats.get('stock_count', 0)}")
    print(f"  最新日期: {stats.get('latest_date', 'N/A')}")
    
    if 'indicator_completeness' in stats:
        print("  指标完整性:")
        for indicator, percentage in stats['indicator_completeness'].items():
            print(f"    {indicator}: {percentage:.1f}%")
    
    # 测试获取指标
    print("\n3. 测试获取股票指标:")
    if test_stocks:
        indicators = calculator.get_indicators_for_stock(test_stocks[0])
        if indicators:
            print(f"  {test_stocks[0]} 的技术指标:")
            for key, value in indicators.items():
                if value is not None:
                    print(f"    {key}: {value}")
        else:
            print(f"  未找到{test_stocks[0]}的技术指标数据")
    
    print("\n=== 测试完成 ===")

if __name__ == "__main__":
    main()