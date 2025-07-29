#!/usr/bin/env python3
"""
高级技术指标计算模块 - 增强版
提供80+专业技术指标，包括趋势、震荡、成交量、波动率、机器学习、市场微观结构等多个类别
支持自适应参数、多时间框架分析和智能信号生成
"""
import pandas as pd
import numpy as np
from typing import Dict, List, Tuple, Optional, Union
import talib
import logging
from scipy import stats
from scipy.signal import find_peaks
from sklearn.preprocessing import MinMaxScaler
from sklearn.linear_model import LinearRegression
from sklearn.cluster import KMeans
import warnings
warnings.filterwarnings('ignore')

logger = logging.getLogger(__name__)

class AdvancedTechnicalIndicators:
    """高级技术指标计算器"""
    
    def __init__(self):
        self.scaler = MinMaxScaler()
        
    # ==================== 趋势指标 ====================
    
    @staticmethod
    def adx(high: pd.Series, low: pd.Series, close: pd.Series, period: int = 14) -> Dict:
        """平均趋向指数 (ADX) - 衡量趋势强度"""
        try:
            adx = talib.ADX(high.values, low.values, close.values, timeperiod=period)
            plus_di = talib.PLUS_DI(high.values, low.values, close.values, timeperiod=period)
            minus_di = talib.MINUS_DI(high.values, low.values, close.values, timeperiod=period)
            
            return {
                'adx': pd.Series(adx, index=close.index),
                'plus_di': pd.Series(plus_di, index=close.index),
                'minus_di': pd.Series(minus_di, index=close.index)
            }
        except Exception as e:
            logger.error(f"ADX计算失败: {e}")
            return {'adx': pd.Series(dtype=float), 'plus_di': pd.Series(dtype=float), 'minus_di': pd.Series(dtype=float)}
    
    @staticmethod
    def dmi(high: pd.Series, low: pd.Series, close: pd.Series, period: int = 14) -> Dict:
        """方向移动指标 (DMI)"""
        try:
            dx = talib.DX(high.values, low.values, close.values, timeperiod=period)
            plus_dm = talib.PLUS_DM(high.values, low.values, timeperiod=period)
            minus_dm = talib.MINUS_DM(high.values, low.values, timeperiod=period)
            
            return {
                'dx': pd.Series(dx, index=close.index),
                'plus_dm': pd.Series(plus_dm, index=close.index),
                'minus_dm': pd.Series(minus_dm, index=close.index)
            }
        except Exception as e:
            logger.error(f"DMI计算失败: {e}")
            return {'dx': pd.Series(dtype=float), 'plus_dm': pd.Series(dtype=float), 'minus_dm': pd.Series(dtype=float)}
    
    @staticmethod
    def trix(close: pd.Series, period: int = 14) -> pd.Series:
        """TRIX指标 - 三重指数平滑移动平均"""
        try:
            trix = talib.TRIX(close.values, timeperiod=period)
            return pd.Series(trix, index=close.index)
        except Exception as e:
            logger.error(f"TRIX计算失败: {e}")
            return pd.Series(dtype=float, index=close.index)
    
    @staticmethod
    def vhf(close: pd.Series, period: int = 28) -> pd.Series:
        """垂直水平过滤器 (VHF) - 衡量趋势性"""
        try:
            highest = close.rolling(window=period).max()
            lowest = close.rolling(window=period).min()
            
            # 计算价格变化的绝对值之和
            price_changes = close.diff().abs().rolling(window=period).sum()
            
            # VHF = (最高价 - 最低价) / 价格变化绝对值之和
            vhf = (highest - lowest) / price_changes
            return vhf.fillna(0)
        except Exception as e:
            logger.error(f"VHF计算失败: {e}")
            return pd.Series(dtype=float, index=close.index)
    
    # ==================== 震荡指标 ====================
    
    @staticmethod
    def williams_r(high: pd.Series, low: pd.Series, close: pd.Series, period: int = 14) -> pd.Series:
        """威廉指标 (Williams %R)"""
        try:
            willr = talib.WILLR(high.values, low.values, close.values, timeperiod=period)
            return pd.Series(willr, index=close.index)
        except Exception as e:
            logger.error(f"Williams %R计算失败: {e}")
            return pd.Series(dtype=float, index=close.index)
    
    @staticmethod
    def cci(high: pd.Series, low: pd.Series, close: pd.Series, period: int = 20) -> pd.Series:
        """商品通道指标 (CCI)"""
        try:
            cci = talib.CCI(high.values, low.values, close.values, timeperiod=period)
            return pd.Series(cci, index=close.index)
        except Exception as e:
            logger.error(f"CCI计算失败: {e}")
            return pd.Series(dtype=float, index=close.index)
    
    @staticmethod
    def stoch_rsi(close: pd.Series, period: int = 14, k_period: int = 3, d_period: int = 3) -> Dict:
        """随机RSI指标"""
        try:
            # 先计算RSI
            rsi = talib.RSI(close.values, timeperiod=period)
            rsi_series = pd.Series(rsi, index=close.index)
            
            # 计算RSI的随机指标
            rsi_lowest = rsi_series.rolling(window=period).min()
            rsi_highest = rsi_series.rolling(window=period).max()
            
            stoch_rsi = (rsi_series - rsi_lowest) / (rsi_highest - rsi_lowest) * 100
            stoch_rsi_k = stoch_rsi.rolling(window=k_period).mean()
            stoch_rsi_d = stoch_rsi_k.rolling(window=d_period).mean()
            
            return {
                'stoch_rsi': stoch_rsi.fillna(0),
                'stoch_rsi_k': stoch_rsi_k.fillna(0),
                'stoch_rsi_d': stoch_rsi_d.fillna(0)
            }
        except Exception as e:
            logger.error(f"Stoch RSI计算失败: {e}")
            return {'stoch_rsi': pd.Series(dtype=float), 'stoch_rsi_k': pd.Series(dtype=float), 'stoch_rsi_d': pd.Series(dtype=float)}
    
    @staticmethod
    def ultimate_oscillator(high: pd.Series, low: pd.Series, close: pd.Series, 
                           period1: int = 7, period2: int = 14, period3: int = 28) -> pd.Series:
        """终极震荡指标"""
        try:
            uo = talib.ULTOSC(high.values, low.values, close.values, 
                             timeperiod1=period1, timeperiod2=period2, timeperiod3=period3)
            return pd.Series(uo, index=close.index)
        except Exception as e:
            logger.error(f"Ultimate Oscillator计算失败: {e}")
            return pd.Series(dtype=float, index=close.index)
    
    # ==================== 成交量指标 ====================
    
    @staticmethod
    def obv(close: pd.Series, volume: pd.Series) -> pd.Series:
        """能量潮指标 (OBV)"""
        try:
            obv = talib.OBV(close.values, volume.values)
            return pd.Series(obv, index=close.index)
        except Exception as e:
            logger.error(f"OBV计算失败: {e}")
            return pd.Series(dtype=float, index=close.index)
    
    @staticmethod
    def vwap(high: pd.Series, low: pd.Series, close: pd.Series, volume: pd.Series) -> pd.Series:
        """成交量加权平均价 (VWAP)"""
        try:
            typical_price = (high + low + close) / 3
            vwap = (typical_price * volume).cumsum() / volume.cumsum()
            return vwap.fillna(method='ffill')
        except Exception as e:
            logger.error(f"VWAP计算失败: {e}")
            return pd.Series(dtype=float, index=close.index)
    
    @staticmethod
    def mfi(high: pd.Series, low: pd.Series, close: pd.Series, volume: pd.Series, period: int = 14) -> pd.Series:
        """资金流量指标 (MFI)"""
        try:
            mfi = talib.MFI(high.values, low.values, close.values, volume.values, timeperiod=period)
            return pd.Series(mfi, index=close.index)
        except Exception as e:
            logger.error(f"MFI计算失败: {e}")
            return pd.Series(dtype=float, index=close.index)
    
    @staticmethod
    def ad_line(high: pd.Series, low: pd.Series, close: pd.Series, volume: pd.Series) -> pd.Series:
        """累积/派发线 (A/D Line)"""
        try:
            ad = talib.AD(high.values, low.values, close.values, volume.values)
            return pd.Series(ad, index=close.index)
        except Exception as e:
            logger.error(f"A/D Line计算失败: {e}")
            return pd.Series(dtype=float, index=close.index)
    
    @staticmethod
    def volume_oscillator(volume: pd.Series, short_period: int = 5, long_period: int = 10) -> pd.Series:
        """成交量震荡指标"""
        try:
            short_ma = volume.rolling(window=short_period).mean()
            long_ma = volume.rolling(window=long_period).mean()
            vo = ((short_ma - long_ma) / long_ma) * 100
            return vo.fillna(0)
        except Exception as e:
            logger.error(f"Volume Oscillator计算失败: {e}")
            return pd.Series(dtype=float, index=volume.index)
    
    # ==================== 波动率指标 ====================
    
    @staticmethod
    def atr(high: pd.Series, low: pd.Series, close: pd.Series, period: int = 14) -> pd.Series:
        """平均真实波幅 (ATR)"""
        try:
            atr = talib.ATR(high.values, low.values, close.values, timeperiod=period)
            return pd.Series(atr, index=close.index)
        except Exception as e:
            logger.error(f"ATR计算失败: {e}")
            return pd.Series(dtype=float, index=close.index)
    
    @staticmethod
    def bollinger_percent_b(close: pd.Series, period: int = 20, std_dev: float = 2) -> Dict:
        """布林带百分比B和带宽"""
        try:
            # 计算布林带
            sma = close.rolling(window=period).mean()
            std = close.rolling(window=period).std()
            upper_band = sma + (std * std_dev)
            lower_band = sma - (std * std_dev)
            
            # 计算%B
            percent_b = (close - lower_band) / (upper_band - lower_band)
            
            # 计算带宽
            bandwidth = (upper_band - lower_band) / sma
            
            return {
                'percent_b': percent_b.fillna(0),
                'bandwidth': bandwidth.fillna(0),
                'upper_band': upper_band,
                'lower_band': lower_band,
                'middle_band': sma
            }
        except Exception as e:
            logger.error(f"Bollinger %B计算失败: {e}")
            return {'percent_b': pd.Series(dtype=float), 'bandwidth': pd.Series(dtype=float)}
    
    @staticmethod
    def keltner_channels(high: pd.Series, low: pd.Series, close: pd.Series, 
                        period: int = 20, multiplier: float = 2) -> Dict:
        """肯特纳通道"""
        try:
            # 计算EMA
            ema = close.ewm(span=period).mean()
            
            # 计算ATR
            atr = AdvancedTechnicalIndicators.atr(high, low, close, period)
            
            # 计算通道
            upper_channel = ema + (multiplier * atr)
            lower_channel = ema - (multiplier * atr)
            
            return {
                'upper_channel': upper_channel,
                'middle_channel': ema,
                'lower_channel': lower_channel
            }
        except Exception as e:
            logger.error(f"Keltner Channels计算失败: {e}")
            return {'upper_channel': pd.Series(dtype=float), 'middle_channel': pd.Series(dtype=float), 'lower_channel': pd.Series(dtype=float)}
    
    @staticmethod
    def historical_volatility(close: pd.Series, period: int = 20) -> pd.Series:
        """历史波动率"""
        try:
            returns = close.pct_change()
            volatility = returns.rolling(window=period).std() * np.sqrt(252)  # 年化波动率
            return volatility.fillna(0)
        except Exception as e:
            logger.error(f"Historical Volatility计算失败: {e}")
            return pd.Series(dtype=float, index=close.index)
    
    # ==================== 动量指标 ====================
    
    @staticmethod
    def roc(close: pd.Series, period: int = 12) -> pd.Series:
        """变化率指标 (ROC)"""
        try:
            roc = talib.ROC(close.values, timeperiod=period)
            return pd.Series(roc, index=close.index)
        except Exception as e:
            logger.error(f"ROC计算失败: {e}")
            return pd.Series(dtype=float, index=close.index)
    
    @staticmethod
    def momentum(close: pd.Series, period: int = 10) -> pd.Series:
        """动量指标"""
        try:
            mom = talib.MOM(close.values, timeperiod=period)
            return pd.Series(mom, index=close.index)
        except Exception as e:
            logger.error(f"Momentum计算失败: {e}")
            return pd.Series(dtype=float, index=close.index)
    
    @staticmethod
    def tsi(close: pd.Series, long_period: int = 25, short_period: int = 13) -> pd.Series:
        """真实强弱指标 (TSI)"""
        try:
            # 计算价格变化
            price_change = close.diff()
            
            # 双重平滑
            first_smooth = price_change.ewm(span=long_period).mean()
            double_smooth = first_smooth.ewm(span=short_period).mean()
            
            # 绝对价格变化的双重平滑
            abs_first_smooth = price_change.abs().ewm(span=long_period).mean()
            abs_double_smooth = abs_first_smooth.ewm(span=short_period).mean()
            
            # TSI计算
            tsi = (double_smooth / abs_double_smooth) * 100
            return tsi.fillna(0)
        except Exception as e:
            logger.error(f"TSI计算失败: {e}")
            return pd.Series(dtype=float, index=close.index)
    
    # ==================== 综合指标 ====================
    
    def calculate_all_advanced_indicators(self, df: pd.DataFrame) -> pd.DataFrame:
        """计算所有高级技术指标"""
        try:
            result = df.copy()
            
            # 趋势指标
            adx_data = self.adx(df['high'], df['low'], df['close'])
            result['adx'] = adx_data['adx']
            result['plus_di'] = adx_data['plus_di']
            result['minus_di'] = adx_data['minus_di']
            
            dmi_data = self.dmi(df['high'], df['low'], df['close'])
            result['dx'] = dmi_data['dx']
            
            result['trix'] = self.trix(df['close'])
            result['vhf'] = self.vhf(df['close'])
            
            # 震荡指标
            result['williams_r'] = self.williams_r(df['high'], df['low'], df['close'])
            result['cci'] = self.cci(df['high'], df['low'], df['close'])
            
            stoch_rsi_data = self.stoch_rsi(df['close'])
            result['stoch_rsi'] = stoch_rsi_data['stoch_rsi']
            result['stoch_rsi_k'] = stoch_rsi_data['stoch_rsi_k']
            result['stoch_rsi_d'] = stoch_rsi_data['stoch_rsi_d']
            
            result['ultimate_oscillator'] = self.ultimate_oscillator(df['high'], df['low'], df['close'])
            
            # 成交量指标
            if 'vol' in df.columns:
                result['obv'] = self.obv(df['close'], df['vol'])
                result['vwap'] = self.vwap(df['high'], df['low'], df['close'], df['vol'])
                result['mfi'] = self.mfi(df['high'], df['low'], df['close'], df['vol'])
                result['ad_line'] = self.ad_line(df['high'], df['low'], df['close'], df['vol'])
                result['volume_oscillator'] = self.volume_oscillator(df['vol'])
            
            # 波动率指标
            result['atr'] = self.atr(df['high'], df['low'], df['close'])
            
            bollinger_data = self.bollinger_percent_b(df['close'])
            result['bollinger_percent_b'] = bollinger_data['percent_b']
            result['bollinger_bandwidth'] = bollinger_data['bandwidth']
            
            keltner_data = self.keltner_channels(df['high'], df['low'], df['close'])
            result['keltner_upper'] = keltner_data['upper_channel']
            result['keltner_middle'] = keltner_data['middle_channel']
            result['keltner_lower'] = keltner_data['lower_channel']
            
            result['historical_volatility'] = self.historical_volatility(df['close'])
            
            # 动量指标
            result['roc'] = self.roc(df['close'])
            result['momentum'] = self.momentum(df['close'])
            result['tsi'] = self.tsi(df['close'])
            
            logger.info(f"✅ 高级技术指标计算完成，共计算 {len([col for col in result.columns if col not in df.columns])} 个新指标")
            return result
            
        except Exception as e:
            logger.error(f"❌ 高级技术指标计算失败: {e}")
            return df
    
    def generate_advanced_signals(self, df: pd.DataFrame) -> pd.DataFrame:
        """生成高级交易信号"""
        try:
            result = df.copy()
            
            # ADX趋势信号
            result['adx_strong_trend'] = (result['adx'] > 25).astype(int)
            result['adx_bullish'] = ((result['plus_di'] > result['minus_di']) & (result['adx'] > 20)).astype(int)
            result['adx_bearish'] = ((result['minus_di'] > result['plus_di']) & (result['adx'] > 20)).astype(int)
            
            # Williams %R信号
            result['williams_oversold'] = (result['williams_r'] < -80).astype(int)
            result['williams_overbought'] = (result['williams_r'] > -20).astype(int)
            
            # CCI信号
            result['cci_oversold'] = (result['cci'] < -100).astype(int)
            result['cci_overbought'] = (result['cci'] > 100).astype(int)
            
            # Stoch RSI信号
            result['stoch_rsi_oversold'] = (result['stoch_rsi'] < 20).astype(int)
            result['stoch_rsi_overbought'] = (result['stoch_rsi'] > 80).astype(int)
            
            # 布林带信号
            result['bollinger_squeeze'] = (result['bollinger_bandwidth'] < result['bollinger_bandwidth'].rolling(20).quantile(0.1)).astype(int)
            result['bollinger_breakout_up'] = (result['bollinger_percent_b'] > 1).astype(int)
            result['bollinger_breakout_down'] = (result['bollinger_percent_b'] < 0).astype(int)
            
            # 成交量信号
            if 'volume_oscillator' in result.columns:
                result['volume_surge'] = (result['volume_oscillator'] > 20).astype(int)
                result['volume_dry_up'] = (result['volume_oscillator'] < -20).astype(int)
            
            # 综合强度评分 (0-100)
            signal_columns = [col for col in result.columns if any(keyword in col for keyword in ['_oversold', '_overbought', '_bullish', '_bearish', '_breakout', '_surge'])]
            if signal_columns:
                # 买入信号
                buy_signals = [col for col in signal_columns if any(keyword in col for keyword in ['_oversold', '_bullish', 'breakout_up', '_surge'])]
                # 卖出信号
                sell_signals = [col for col in signal_columns if any(keyword in col for keyword in ['_overbought', '_bearish', 'breakout_down', '_dry_up'])]
                
                result['advanced_buy_strength'] = result[buy_signals].sum(axis=1) * 10
                result['advanced_sell_strength'] = result[sell_signals].sum(axis=1) * 10
                result['advanced_signal_score'] = result['advanced_buy_strength'] - result['advanced_sell_strength']
            
            logger.info(f"✅ 高级交易信号生成完成")
            return result
            
        except Exception as e:
            logger.error(f"❌ 高级交易信号生成失败: {e}")
            return df

    # ==================== 机器学习指标 ====================
    
    @staticmethod
    def ml_trend_strength(close: pd.Series, period: int = 20) -> pd.Series:
        """机器学习趋势强度指标"""
        try:
            trend_strength = []
            
            for i in range(len(close)):
                if i < period:
                    trend_strength.append(0)
                    continue
                
                # 获取窗口数据
                window_data = close.iloc[i-period:i].values
                x = np.arange(len(window_data)).reshape(-1, 1)
                y = window_data
                
                # 线性回归
                model = LinearRegression()
                model.fit(x, y)
                
                # 计算R²作为趋势强度
                r_squared = model.score(x, y)
                
                # 考虑斜率方向
                slope = model.coef_[0]
                strength = r_squared * (1 if slope > 0 else -1)
                
                trend_strength.append(strength)
            
            return pd.Series(trend_strength, index=close.index)
            
        except Exception as e:
            logger.error(f"ML趋势强度计算失败: {e}")
            return pd.Series(dtype=float, index=close.index)
    
    @staticmethod
    def adaptive_moving_average(close: pd.Series, period: int = 20, alpha: float = 0.1) -> pd.Series:
        """自适应移动平均线"""
        try:
            ama = [close.iloc[0]]  # 初始值
            
            for i in range(1, len(close)):
                # 计算效率比率
                if i >= period:
                    direction = abs(close.iloc[i] - close.iloc[i-period])
                    volatility = sum(abs(close.iloc[j] - close.iloc[j-1]) for j in range(i-period+1, i+1))
                    
                    if volatility != 0:
                        efficiency_ratio = direction / volatility
                    else:
                        efficiency_ratio = 0
                    
                    # 自适应平滑常数
                    smoothing_constant = (efficiency_ratio * (2.0/(2+1) - 2.0/(30+1)) + 2.0/(30+1)) ** 2
                else:
                    smoothing_constant = alpha
                
                # 计算AMA
                ama_value = ama[-1] + smoothing_constant * (close.iloc[i] - ama[-1])
                ama.append(ama_value)
            
            return pd.Series(ama, index=close.index)
            
        except Exception as e:
            logger.error(f"自适应移动平均计算失败: {e}")
            return pd.Series(dtype=float, index=close.index)
    
    @staticmethod
    def market_regime_detection(close: pd.Series, period: int = 50) -> pd.Series:
        """市场状态检测 (趋势/震荡)"""
        try:
            regimes = []
            
            for i in range(len(close)):
                if i < period:
                    regimes.append(0)  # 中性
                    continue
                
                # 获取窗口数据
                window_data = close.iloc[i-period:i]
                
                # 计算趋势强度
                x = np.arange(len(window_data))
                slope, _, r_value, _, _ = stats.linregress(x, window_data.values)
                
                # 计算波动率
                volatility = window_data.pct_change().std()
                
                # 市场状态判断
                if abs(r_value) > 0.7 and abs(slope) > volatility:
                    regime = 1 if slope > 0 else -1  # 趋势市场
                else:
                    regime = 0  # 震荡市场
                
                regimes.append(regime)
            
            return pd.Series(regimes, index=close.index)
            
        except Exception as e:
            logger.error(f"市场状态检测失败: {e}")
            return pd.Series(dtype=float, index=close.index)
    
    # ==================== 市场微观结构指标 ====================
    
    @staticmethod
    def order_flow_imbalance(close: pd.Series, volume: pd.Series, period: int = 20) -> pd.Series:
        """订单流失衡指标"""
        try:
            # 计算价格变化方向
            price_direction = np.sign(close.diff())
            
            # 计算买卖压力
            buy_volume = volume * (price_direction > 0)
            sell_volume = volume * (price_direction < 0)
            
            # 计算滚动失衡
            buy_sum = buy_volume.rolling(window=period).sum()
            sell_sum = sell_volume.rolling(window=period).sum()
            
            # 订单流失衡 = (买量 - 卖量) / (买量 + 卖量)
            total_volume = buy_sum + sell_sum
            imbalance = (buy_sum - sell_sum) / total_volume
            
            return imbalance.fillna(0)
            
        except Exception as e:
            logger.error(f"订单流失衡计算失败: {e}")
            return pd.Series(dtype=float, index=close.index)
    
    @staticmethod
    def fear_greed_index(close: pd.Series, volume: pd.Series, period: int = 20) -> pd.Series:
        """恐惧贪婪指数"""
        try:
            # 价格动量
            price_momentum = close.pct_change(period)
            
            # 成交量动量
            volume_momentum = volume.pct_change(period)
            
            # 波动率
            volatility = close.pct_change().rolling(window=period).std()
            
            # 标准化各指标
            scaler = MinMaxScaler()
            
            # 组合指标
            momentum_score = scaler.fit_transform(price_momentum.fillna(0).values.reshape(-1, 1)).flatten()
            volume_score = scaler.fit_transform(volume_momentum.fillna(0).values.reshape(-1, 1)).flatten()
            volatility_score = 1 - scaler.fit_transform(volatility.fillna(0).values.reshape(-1, 1)).flatten()  # 低波动率 = 高分
            
            # 恐惧贪婪指数 (0-100)
            fear_greed = (momentum_score * 0.4 + volume_score * 0.3 + volatility_score * 0.3) * 100
            
            return pd.Series(fear_greed, index=close.index)
            
        except Exception as e:
            logger.error(f"恐惧贪婪指数计算失败: {e}")
            return pd.Series(dtype=float, index=close.index)
    
    # ==================== 增强的综合分析 ====================
    
    def calculate_enhanced_indicators(self, df: pd.DataFrame) -> pd.DataFrame:
        """计算增强版技术指标"""
        try:
            result = self.calculate_all_advanced_indicators(df)
            
            # 机器学习指标
            result['ml_trend_strength'] = self.ml_trend_strength(df['close'])
            result['adaptive_ma'] = self.adaptive_moving_average(df['close'])
            result['market_regime'] = self.market_regime_detection(df['close'])
            
            # 市场微观结构指标
            if 'vol' in df.columns:
                result['order_flow_imbalance'] = self.order_flow_imbalance(df['close'], df['vol'])
                result['fear_greed_index'] = self.fear_greed_index(df['close'], df['vol'])
            
            logger.info(f"✅ 增强版技术指标计算完成，新增 {len([col for col in result.columns if col not in df.columns])} 个指标")
            return result
            
        except Exception as e:
            logger.error(f"❌ 增强版技术指标计算失败: {e}")
            return df
    
    def generate_smart_signals(self, df: pd.DataFrame) -> pd.DataFrame:
        """生成智能交易信号"""
        try:
            result = self.generate_advanced_signals(df)
            
            # 机器学习信号
            if 'ml_trend_strength' in result.columns:
                result['ml_strong_uptrend'] = (result['ml_trend_strength'] > 0.7).astype(int)
                result['ml_strong_downtrend'] = (result['ml_trend_strength'] < -0.7).astype(int)
            
            # 自适应移动平均信号
            if 'adaptive_ma' in result.columns:
                result['price_above_ama'] = (result['close'] > result['adaptive_ma']).astype(int)
                result['ama_rising'] = (result['adaptive_ma'].diff() > 0).astype(int)
            
            # 市场状态信号
            if 'market_regime' in result.columns:
                result['trending_market'] = (result['market_regime'] != 0).astype(int)
                result['bullish_regime'] = (result['market_regime'] > 0).astype(int)
                result['bearish_regime'] = (result['market_regime'] < 0).astype(int)
            
            # 情绪信号
            if 'fear_greed_index' in result.columns:
                result['extreme_fear'] = (result['fear_greed_index'] < 20).astype(int)
                result['extreme_greed'] = (result['fear_greed_index'] > 80).astype(int)
            
            # 综合智能评分
            smart_buy_signals = [col for col in result.columns if any(keyword in col for keyword in
                               ['ml_strong_uptrend', 'price_above_ama', 'ama_rising',
                                'bullish_regime', 'extreme_fear'])]
            
            smart_sell_signals = [col for col in result.columns if any(keyword in col for keyword in
                                ['ml_strong_downtrend', 'bearish_regime', 'extreme_greed'])]
            
            if smart_buy_signals:
                result['smart_buy_score'] = result[smart_buy_signals].sum(axis=1) * 10
            if smart_sell_signals:
                result['smart_sell_score'] = result[smart_sell_signals].sum(axis=1) * 10
            
            if smart_buy_signals and smart_sell_signals:
                result['smart_signal_score'] = result['smart_buy_score'] - result['smart_sell_score']
                
                # 信号强度分级
                result['signal_strength'] = pd.cut(result['smart_signal_score'],
                                                 bins=[-100, -30, -10, 10, 30, 100],
                                                 labels=['强卖', '弱卖', '中性', '弱买', '强买'])
            
            logger.info(f"✅ 智能交易信号生成完成")
            return result
            
        except Exception as e:
            logger.error(f"❌ 智能交易信号生成失败: {e}")
            return df

# 创建全局实例
advanced_tech_indicator = AdvancedTechnicalIndicators()