#!/usr/bin/env python3
"""
简化版技术指标计算器
不依赖talib，使用纯Python实现
"""
import pandas as pd
import numpy as np
from typing import Dict, Optional
import logging

logger = logging.getLogger(__name__)

class SimpleTechnicalIndicators:
    """简化版技术指标计算器"""
    
    @staticmethod
    def sma(data: pd.Series, period: int) -> pd.Series:
        """简单移动平均"""
        return data.rolling(window=period).mean()
    
    @staticmethod
    def ema(data: pd.Series, period: int) -> pd.Series:
        """指数移动平均"""
        return data.ewm(span=period).mean()
    
    @staticmethod
    def rsi(data: pd.Series, period: int = 14) -> pd.Series:
        """相对强弱指数"""
        delta = data.diff()
        gain = (delta.where(delta > 0, 0)).rolling(window=period).mean()
        loss = (-delta.where(delta < 0, 0)).rolling(window=period).mean()
        rs = gain / loss
        return 100 - (100 / (1 + rs))
    
    @staticmethod
    def macd(data: pd.Series, fast: int = 12, slow: int = 26, signal: int = 9) -> Dict[str, pd.Series]:
        """MACD指标"""
        ema_fast = SimpleTechnicalIndicators.ema(data, fast)
        ema_slow = SimpleTechnicalIndicators.ema(data, slow)
        macd_line = ema_fast - ema_slow
        signal_line = SimpleTechnicalIndicators.ema(macd_line, signal)
        histogram = macd_line - signal_line
        
        return {
            'macd': macd_line,
            'macd_signal': signal_line,
            'macd_histogram': histogram
        }
    
    @staticmethod
    def bollinger_bands(data: pd.Series, period: int = 20, std_dev: float = 2) -> Dict[str, pd.Series]:
        """布林带"""
        sma = SimpleTechnicalIndicators.sma(data, period)
        std = data.rolling(window=period).std()
        
        return {
            'bb_upper': sma + (std * std_dev),
            'bb_middle': sma,
            'bb_lower': sma - (std * std_dev)
        }
    
    @staticmethod
    def stochastic(high: pd.Series, low: pd.Series, close: pd.Series, 
                   k_period: int = 14, d_period: int = 3) -> Dict[str, pd.Series]:
        """随机指标KDJ"""
        lowest_low = low.rolling(window=k_period).min()
        highest_high = high.rolling(window=k_period).max()
        
        k_percent = 100 * ((close - lowest_low) / (highest_high - lowest_low))
        d_percent = k_percent.rolling(window=d_period).mean()
        
        return {
            'k': k_percent,
            'd': d_percent,
            'j': 3 * k_percent - 2 * d_percent
        }
    
    @staticmethod
    def atr(high: pd.Series, low: pd.Series, close: pd.Series, period: int = 14) -> pd.Series:
        """平均真实波幅"""
        tr1 = high - low
        tr2 = abs(high - close.shift(1))
        tr3 = abs(low - close.shift(1))
        
        true_range = pd.concat([tr1, tr2, tr3], axis=1).max(axis=1)
        return true_range.rolling(window=period).mean()
    
    def calculate_all_indicators(self, df: pd.DataFrame) -> pd.DataFrame:
        """计算所有技术指标"""
        try:
            result_df = df.copy()
            
            # 移动平均线
            result_df['ma5'] = self.sma(df['close'], 5)
            result_df['ma10'] = self.sma(df['close'], 10)
            result_df['ma20'] = self.sma(df['close'], 20)
            result_df['ma60'] = self.sma(df['close'], 60)
            
            # RSI
            result_df['rsi14'] = self.rsi(df['close'], 14)
            result_df['rsi6'] = self.rsi(df['close'], 6)
            
            # MACD
            macd_data = self.macd(df['close'])
            result_df['macd'] = macd_data['macd']
            result_df['macd_signal'] = macd_data['macd_signal']
            result_df['macd_histogram'] = macd_data['macd_histogram']
            
            # 布林带
            bb_data = self.bollinger_bands(df['close'])
            result_df['bb_upper'] = bb_data['bb_upper']
            result_df['bb_middle'] = bb_data['bb_middle']
            result_df['bb_lower'] = bb_data['bb_lower']
            
            # KDJ
            if all(col in df.columns for col in ['high', 'low']):
                kdj_data = self.stochastic(df['high'], df['low'], df['close'])
                result_df['kdj_k'] = kdj_data['k']
                result_df['kdj_d'] = kdj_data['d']
                result_df['kdj_j'] = kdj_data['j']
            
            # ATR
            if all(col in df.columns for col in ['high', 'low']):
                result_df['atr'] = self.atr(df['high'], df['low'], df['close'])
            
            # 成交量指标
            if 'vol' in df.columns:
                result_df['vol_ma5'] = self.sma(df['vol'], 5)
                result_df['vol_ma20'] = self.sma(df['vol'], 20)
            
            # 价格变化
            result_df['price_change'] = df['close'].pct_change()
            result_df['price_change_5d'] = df['close'].pct_change(5)
            
            # 强弱度评分（简化版）
            result_df['strength_score'] = self._calculate_strength_score(result_df)
            
            return result_df
            
        except Exception as e:
            logger.error(f"计算技术指标失败: {e}")
            return df
    
    def _calculate_strength_score(self, df: pd.DataFrame) -> pd.Series:
        """计算强弱度评分"""
        try:
            score = pd.Series(50.0, index=df.index)  # 基础分50分
            
            # RSI评分
            if 'rsi14' in df.columns:
                rsi = df['rsi14']
                score += np.where(rsi > 70, 10, np.where(rsi < 30, -10, 0))
            
            # 均线评分
            if all(col in df.columns for col in ['close', 'ma5', 'ma20']):
                # 价格相对均线位置
                score += np.where(df['close'] > df['ma5'], 5, -5)
                score += np.where(df['close'] > df['ma20'], 5, -5)
                score += np.where(df['ma5'] > df['ma20'], 10, -10)
            
            # MACD评分
            if 'macd' in df.columns and 'macd_signal' in df.columns:
                score += np.where(df['macd'] > df['macd_signal'], 10, -10)
            
            # 成交量评分
            if 'vol' in df.columns and 'vol_ma20' in df.columns:
                vol_ratio = df['vol'] / df['vol_ma20']
                score += np.where(vol_ratio > 1.5, 5, np.where(vol_ratio < 0.5, -5, 0))
            
            # 限制评分范围
            score = np.clip(score, 0, 100)
            
            return score
            
        except Exception as e:
            logger.error(f"计算强弱度评分失败: {e}")
            return pd.Series(50.0, index=df.index)
    
    def generate_signals(self, df: pd.DataFrame) -> pd.DataFrame:
        """生成交易信号"""
        try:
            signals_df = pd.DataFrame(index=df.index)
            
            # 初始化信号
            signals_df['buy_signal'] = 0
            signals_df['sell_signal'] = 0
            signals_df['ma_golden_cross'] = 0
            signals_df['ma_death_cross'] = 0
            signals_df['macd_golden_cross'] = 0
            signals_df['macd_death_cross'] = 0
            
            # 均线金叉死叉
            if all(col in df.columns for col in ['ma5', 'ma20']):
                ma5_above_ma20 = df['ma5'] > df['ma20']
                ma5_above_ma20_prev = ma5_above_ma20.shift(1)
                
                signals_df['ma_golden_cross'] = ((ma5_above_ma20) & (~ma5_above_ma20_prev)).astype(int)
                signals_df['ma_death_cross'] = ((~ma5_above_ma20) & (ma5_above_ma20_prev)).astype(int)
            
            # MACD金叉死叉
            if all(col in df.columns for col in ['macd', 'macd_signal']):
                macd_above_signal = df['macd'] > df['macd_signal']
                macd_above_signal_prev = macd_above_signal.shift(1)
                
                signals_df['macd_golden_cross'] = ((macd_above_signal) & (~macd_above_signal_prev)).astype(int)
                signals_df['macd_death_cross'] = ((~macd_above_signal) & (macd_above_signal_prev)).astype(int)
            
            # 综合买入信号
            buy_conditions = []
            if 'ma_golden_cross' in signals_df.columns:
                buy_conditions.append(signals_df['ma_golden_cross'] == 1)
            if 'macd_golden_cross' in signals_df.columns:
                buy_conditions.append(signals_df['macd_golden_cross'] == 1)
            if 'rsi14' in df.columns:
                buy_conditions.append(df['rsi14'] < 30)  # RSI超卖
            
            if buy_conditions:
                signals_df['buy_signal'] = pd.concat(buy_conditions, axis=1).any(axis=1).astype(int)
            
            # 综合卖出信号
            sell_conditions = []
            if 'ma_death_cross' in signals_df.columns:
                sell_conditions.append(signals_df['ma_death_cross'] == 1)
            if 'macd_death_cross' in signals_df.columns:
                sell_conditions.append(signals_df['macd_death_cross'] == 1)
            if 'rsi14' in df.columns:
                sell_conditions.append(df['rsi14'] > 70)  # RSI超买
            
            if sell_conditions:
                signals_df['sell_signal'] = pd.concat(sell_conditions, axis=1).any(axis=1).astype(int)
            
            return signals_df
            
        except Exception as e:
            logger.error(f"生成交易信号失败: {e}")
            return pd.DataFrame(index=df.index)

# 创建全局实例
tech_indicator = SimpleTechnicalIndicators()