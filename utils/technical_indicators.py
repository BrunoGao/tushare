import pandas as pd
import numpy as np
from typing import Dict, List, Tuple, Optional
import talib
import logging

logger = logging.getLogger(__name__)

class TechnicalIndicators:
    """技术指标计算器 - 实现主流技术分析算法"""
    
    @staticmethod
    def sma(data: pd.Series, period: int = 20) -> pd.Series:  # 简单移动平均
        """简单移动平均线"""
        return data.rolling(window=period).mean()
    
    @staticmethod
    def ema(data: pd.Series, period: int = 20) -> pd.Series:  # 指数移动平均
        """指数移动平均线"""
        return data.ewm(span=period).mean()
    
    @staticmethod
    def macd(data: pd.Series, fast: int = 12, slow: int = 26, signal: int = 9) -> Dict:  # MACD指标
        """MACD指标计算"""
        ema_fast = data.ewm(span=fast).mean()
        ema_slow = data.ewm(span=slow).mean()
        macd_line = ema_fast - ema_slow
        signal_line = macd_line.ewm(span=signal).mean()
        histogram = macd_line - signal_line
        return {'macd': macd_line, 'signal': signal_line, 'histogram': histogram}
    
    @staticmethod
    def rsi(data: pd.Series, period: int = 14) -> pd.Series:  # RSI相对强弱指标
        """RSI相对强弱指标"""
        delta = data.diff()
        gain = (delta.where(delta > 0, 0)).rolling(window=period).mean()
        loss = (-delta.where(delta < 0, 0)).rolling(window=period).mean()
        rs = gain / loss
        return 100 - (100 / (1 + rs))
    
    @staticmethod
    def bollinger_bands(data: pd.Series, period: int = 20, std_dev: float = 2) -> Dict:  # 布林带
        """布林带指标"""
        sma = data.rolling(window=period).mean()
        std = data.rolling(window=period).std()
        upper = sma + (std * std_dev)
        lower = sma - (std * std_dev)
        return {'upper': upper, 'middle': sma, 'lower': lower}
    
    @staticmethod
    def kdj(high: pd.Series, low: pd.Series, close: pd.Series, n: int = 9, m1: int = 3, m2: int = 3) -> Dict:  # KDJ随机指标
        """KDJ随机指标"""
        lowest_low = low.rolling(window=n).min()
        highest_high = high.rolling(window=n).max()
        rsv = (close - lowest_low) / (highest_high - lowest_low) * 100
        k = rsv.ewm(com=m1-1).mean()
        d = k.ewm(com=m2-1).mean()
        j = 3 * k - 2 * d
        return {'k': k, 'd': d, 'j': j}
    
    @staticmethod
    def calculate_all_indicators(df: pd.DataFrame) -> pd.DataFrame:  # 计算所有技术指标
        """计算所有技术指标"""
        try:
            result = df.copy()
            
            # 移动平均线
            result['ma5'] = TechnicalIndicators.sma(df['close'], 5)
            result['ma10'] = TechnicalIndicators.sma(df['close'], 10)
            result['ma20'] = TechnicalIndicators.sma(df['close'], 20)
            result['ma60'] = TechnicalIndicators.sma(df['close'], 60)
            result['ema12'] = TechnicalIndicators.ema(df['close'], 12)
            result['ema26'] = TechnicalIndicators.ema(df['close'], 26)
            
            # MACD
            macd_data = TechnicalIndicators.macd(df['close'])
            result['macd'] = macd_data['macd']
            result['macd_signal'] = macd_data['signal']
            result['macd_histogram'] = macd_data['histogram']
            
            # RSI
            result['rsi'] = TechnicalIndicators.rsi(df['close'])
            
            # 布林带
            boll_data = TechnicalIndicators.bollinger_bands(df['close'])
            result['boll_upper'] = boll_data['upper']
            result['boll_middle'] = boll_data['middle']
            result['boll_lower'] = boll_data['lower']
            
            # KDJ
            kdj_data = TechnicalIndicators.kdj(df['high'], df['low'], df['close'])
            result['kdj_k'] = kdj_data['k']
            result['kdj_d'] = kdj_data['d']
            result['kdj_j'] = kdj_data['j']
            
            # 成交量指标
            result['vol_ma5'] = TechnicalIndicators.sma(df['vol'], 5)
            result['vol_ma10'] = TechnicalIndicators.sma(df['vol'], 10)
            
            logger.info(f"✅ 技术指标计算完成，数据行数: {len(result)}")
            return result
            
        except Exception as e:
            logger.error(f"❌ 技术指标计算失败: {e}")
            return df
    
    @staticmethod
    def generate_signals(df: pd.DataFrame) -> pd.DataFrame:  # 生成交易信号
        """生成交易信号"""
        try:
            result = df.copy()
            
            # 金叉死叉信号
            result['ma_golden_cross'] = ((result['ma5'] > result['ma10']) & 
                                       (result['ma5'].shift(1) <= result['ma10'].shift(1))).astype(int)
            result['ma_death_cross'] = ((result['ma5'] < result['ma10']) & 
                                      (result['ma5'].shift(1) >= result['ma10'].shift(1))).astype(int)
            
            # MACD信号
            result['macd_golden'] = ((result['macd'] > result['macd_signal']) & 
                                   (result['macd'].shift(1) <= result['macd_signal'].shift(1))).astype(int)
            result['macd_death'] = ((result['macd'] < result['macd_signal']) & 
                                  (result['macd'].shift(1) >= result['macd_signal'].shift(1))).astype(int)
            
            # RSI超买超卖
            result['rsi_overbought'] = (result['rsi'] > 70).astype(int)
            result['rsi_oversold'] = (result['rsi'] < 30).astype(int)
            
            # 布林带突破
            result['boll_breakthrough_up'] = (result['close'] > result['boll_upper']).astype(int)
            result['boll_breakthrough_down'] = (result['close'] < result['boll_lower']).astype(int)
            
            logger.info(f"✅ 交易信号生成完成")
            return result
            
        except Exception as e:
            logger.error(f"❌ 交易信号生成失败: {e}")
            return df 