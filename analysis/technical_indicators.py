import pandas as pd
import numpy as np
import talib
from typing import Dict, List, Tuple, Optional
import logging
import sys, os
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import config
from utils.db_helper import db
from sqlalchemy import text

logger = logging.getLogger(__name__)

class TechnicalIndicators:
    """专业技术指标计算器"""
    
    def __init__(self):
        self.indicators = {}
        
    def calculate_all_indicators(self, df: pd.DataFrame) -> pd.DataFrame:
        """计算所有主流技术指标"""
        if len(df) < 30:  # 至少需要30个交易日数据
            logger.warning(f"数据不足30个交易日({len(df)}条)，技术指标可能不准确")
        elif len(df) < 60:  # 建议60个交易日以上
            logger.info(f"数据量{len(df)}条，建议增加历史数据以提高技术指标准确性")
            
        df = df.sort_values('trade_date').copy()
        
        # 基础价格数据
        high, low, close, volume = df['high'].values, df['low'].values, df['close'].values, df['vol'].values
        open_price = df['open'].values
        
        # 1. 移动平均线系列
        df['ma5'] = talib.SMA(close, timeperiod=5)
        df['ma10'] = talib.SMA(close, timeperiod=10) 
        df['ma20'] = talib.SMA(close, timeperiod=20)
        df['ma60'] = talib.SMA(close, timeperiod=60)
        df['ma120'] = talib.SMA(close, timeperiod=120)
        df['ema12'] = talib.EMA(close, timeperiod=12)
        df['ema26'] = talib.EMA(close, timeperiod=26)
        
        # 2. MACD指标
        df['macd'], df['macd_signal'], df['macd_hist'] = talib.MACD(close, fastperiod=12, slowperiod=26, signalperiod=9)
        
        # 3. RSI相对强弱指标
        df['rsi6'] = talib.RSI(close, timeperiod=6)
        df['rsi14'] = talib.RSI(close, timeperiod=14)
        df['rsi24'] = talib.RSI(close, timeperiod=24)
        
        # 4. KDJ随机指标
        df['k'], df['d'] = talib.STOCH(high, low, close, fastk_period=9, slowk_period=3, slowd_period=3)
        df['j'] = 3 * df['k'] - 2 * df['d']
        
        # 5. 布林带
        df['bb_upper'], df['bb_middle'], df['bb_lower'] = talib.BBANDS(close, timeperiod=20, nbdevup=2, nbdevdn=2)
        df['bb_width'] = (df['bb_upper'] - df['bb_lower']) / df['bb_middle']
        df['bb_position'] = (close - df['bb_lower']) / (df['bb_upper'] - df['bb_lower'])
        
        # 6. 威廉指标
        df['wr10'] = talib.WILLR(high, low, close, timeperiod=10)
        df['wr20'] = talib.WILLR(high, low, close, timeperiod=20)
        
        # 7. CCI商品通道指标
        df['cci14'] = talib.CCI(high, low, close, timeperiod=14)
        df['cci20'] = talib.CCI(high, low, close, timeperiod=20)
        
        # 8. 成交量指标
        df['obv'] = talib.OBV(close, volume)  # 能量潮
        df['ad'] = talib.AD(high, low, close, volume)  # 累积/派发线
        df['vr'] = self.calculate_vr(df)  # 量比
        
        # 9. 动量指标
        df['mom10'] = talib.MOM(close, timeperiod=10)
        df['roc10'] = talib.ROC(close, timeperiod=10)
        df['roc20'] = talib.ROC(close, timeperiod=20)
        
        # 10. ATR真实波动幅度
        df['atr14'] = talib.ATR(high, low, close, timeperiod=14)
        df['atr20'] = talib.ATR(high, low, close, timeperiod=20)
        
        # 11. 趋势指标
        df['adx'] = talib.ADX(high, low, close, timeperiod=14)  # 平均趋向指标
        df['di_plus'] = talib.PLUS_DI(high, low, close, timeperiod=14)
        df['di_minus'] = talib.MINUS_DI(high, low, close, timeperiod=14)
        
        # 12. 价格形态
        df = self.calculate_price_patterns(df)
        
        # 13. 自定义指标
        df = self.calculate_custom_indicators(df)
        
        return df
        
    def calculate_vr(self, df: pd.DataFrame, period: int = 24) -> pd.Series:
        """计算量比VR"""
        df = df.copy()
        df['price_change'] = df['close'].diff()
        
        # 上涨日成交量
        up_volume = df['vol'].where(df['price_change'] > 0, 0)
        # 下跌日成交量  
        down_volume = df['vol'].where(df['price_change'] < 0, 0)
        # 平盘日成交量
        flat_volume = df['vol'].where(df['price_change'] == 0, 0)
        
        # VR = (上涨日成交量 + 1/2平盘日成交量) / (下跌日成交量 + 1/2平盘日成交量) * 100
        up_sum = up_volume.rolling(period).sum() + 0.5 * flat_volume.rolling(period).sum()
        down_sum = down_volume.rolling(period).sum() + 0.5 * flat_volume.rolling(period).sum()
        
        vr = (up_sum / down_sum * 100).fillna(100)
        return vr
        
    def calculate_price_patterns(self, df: pd.DataFrame) -> pd.DataFrame:
        """计算价格形态"""
        high, low, close = df['high'].values, df['low'].values, df['close'].values
        open_price = df['open'].values
        
        # K线形态识别
        df['doji'] = talib.CDLDOJI(open_price, high, low, close)  # 十字星
        df['hammer'] = talib.CDLHAMMER(open_price, high, low, close)  # 锤子线
        df['engulfing'] = talib.CDLENGULFING(open_price, high, low, close)  # 吞噬形态
        df['morning_star'] = talib.CDLMORNINGSTAR(open_price, high, low, close)  # 晨星
        df['evening_star'] = talib.CDLEVENINGSTAR(open_price, high, low, close)  # 暮星
        
        # 缺口分析
        df['gap_up'] = (df['low'] > df['high'].shift(1)).astype(int)
        df['gap_down'] = (df['high'] < df['low'].shift(1)).astype(int)
        df['gap_size'] = np.where(df['gap_up'] == 1, df['low'] - df['high'].shift(1),
                                 np.where(df['gap_down'] == 1, df['high'] - df['low'].shift(1), 0))
        
        return df
        
    def calculate_custom_indicators(self, df: pd.DataFrame) -> pd.DataFrame:
        """计算自定义指标"""
        # 1. 多空力量对比
        df['bull_power'] = df['high'] - df['ema12']
        df['bear_power'] = df['low'] - df['ema12']
        
        # 2. 价量背离
        df['price_trend'] = (df['close'] / df['close'].shift(20) - 1) * 100
        df['volume_trend'] = (df['vol'] / df['vol'].rolling(20).mean() - 1) * 100
        df['pv_divergence'] = df['price_trend'] - df['volume_trend']
        
        # 3. 强弱度评分
        df['strength_score'] = self.calculate_strength_score(df)
        
        # 4. 支撑阻力位
        df['resistance'] = df['high'].rolling(20).max()
        df['support'] = df['low'].rolling(20).min()
        df['sr_ratio'] = (df['close'] - df['support']) / (df['resistance'] - df['support'])
        
        return df
        
    def calculate_strength_score(self, df: pd.DataFrame) -> pd.Series:
        """计算综合强弱度评分(0-100)"""
        scores = []
        
        # RSI评分(权重25%)
        rsi_score = np.where(df['rsi14'] > 70, 100, 
                    np.where(df['rsi14'] > 50, 60 + (df['rsi14'] - 50) * 2,
                    np.where(df['rsi14'] > 30, df['rsi14'],
                             0)))
        
        # MACD评分(权重25%)
        macd_score = np.where(df['macd'] > df['macd_signal'], 60, 40)
        macd_score += np.where(df['macd_hist'] > 0, 20, -20)
        macd_score = np.clip(macd_score, 0, 100)
        
        # 均线评分(权重25%)
        ma_score = 0
        ma_score += np.where(df['close'] > df['ma5'], 20, 0)
        ma_score += np.where(df['ma5'] > df['ma10'], 20, 0)
        ma_score += np.where(df['ma10'] > df['ma20'], 20, 0)
        ma_score += np.where(df['ma20'] > df['ma60'], 20, 0)
        ma_score += np.where(df['close'] > df['ma20'], 20, 0)
        
        # 量价评分(权重25%)
        vol_score = np.where(df['vol'] > df['vol'].rolling(20).mean(), 60, 40)
        
        # 综合评分
        total_score = (rsi_score * 0.25 + macd_score * 0.25 + ma_score * 0.25 + vol_score * 0.25)
        return pd.Series(total_score, index=df.index)
        
    def generate_signals(self, df: pd.DataFrame) -> pd.DataFrame:
        """生成交易信号"""
        signals = pd.DataFrame(index=df.index)
        
        # 1. 金叉死叉信号
        signals['ma_golden_cross'] = ((df['ma5'] > df['ma20']) & (df['ma5'].shift(1) <= df['ma20'].shift(1))).astype(int)
        signals['ma_death_cross'] = ((df['ma5'] < df['ma20']) & (df['ma5'].shift(1) >= df['ma20'].shift(1))).astype(int)
        
        # 2. MACD信号
        signals['macd_golden_cross'] = ((df['macd'] > df['macd_signal']) & (df['macd'].shift(1) <= df['macd_signal'].shift(1))).astype(int)
        signals['macd_death_cross'] = ((df['macd'] < df['macd_signal']) & (df['macd'].shift(1) >= df['macd_signal'].shift(1))).astype(int)
        
        # 3. RSI超买超卖
        signals['rsi_oversold'] = (df['rsi14'] < 30).astype(int)
        signals['rsi_overbought'] = (df['rsi14'] > 70).astype(int)
        
        # 4. KDJ信号
        signals['kdj_golden_cross'] = ((df['k'] > df['d']) & (df['k'].shift(1) <= df['d'].shift(1)) & (df['k'] < 80)).astype(int)
        signals['kdj_death_cross'] = ((df['k'] < df['d']) & (df['k'].shift(1) >= df['d'].shift(1)) & (df['k'] > 20)).astype(int)
        
        # 5. 布林带突破
        signals['bb_breakout_up'] = ((df['close'] > df['bb_upper']) & (df['close'].shift(1) <= df['bb_upper'].shift(1))).astype(int)
        signals['bb_breakout_down'] = ((df['close'] < df['bb_lower']) & (df['close'].shift(1) >= df['bb_lower'].shift(1))).astype(int)
        
        # 6. 综合买卖信号
        buy_conditions = [
            signals['ma_golden_cross'],
            signals['macd_golden_cross'], 
            signals['rsi_oversold'],
            signals['kdj_golden_cross'],
            (df['strength_score'] > 60).astype(int)
        ]
        
        sell_conditions = [
            signals['ma_death_cross'],
            signals['macd_death_cross'],
            signals['rsi_overbought'], 
            signals['kdj_death_cross'],
            (df['strength_score'] < 40).astype(int)
        ]
        
        signals['buy_signal'] = (sum(buy_conditions) >= 3).astype(int)
        signals['sell_signal'] = (sum(sell_conditions) >= 3).astype(int)
        signals['hold_signal'] = ((signals['buy_signal'] == 0) & (signals['sell_signal'] == 0)).astype(int)
        
        return signals
        
    def save_indicators_to_db(self, ts_code: str, df: pd.DataFrame) -> bool:
        """保存技术指标到数据库"""
        try:
            # 选择要保存的指标字段
            indicator_columns = [
                'trade_date', 'ts_code', 'ma5', 'ma10', 'ma20', 'ma60', 'ma120',
                'macd', 'macd_signal', 'macd_hist', 'rsi14', 'k', 'd', 'j',
                'bb_upper', 'bb_middle', 'bb_lower', 'wr10', 'cci14', 'obv',
                'atr14', 'adx', 'strength_score', 'resistance', 'support'
            ]
            
            # 准备数据
            indicator_df = df[indicator_columns].copy()
            indicator_df['ts_code'] = ts_code
            indicator_df = indicator_df.dropna()
            
            if len(indicator_df) == 0:
                return False
                
            # 批量插入数据库
            indicator_df.to_sql('technical_indicators', con=db.engine, if_exists='append', index=False, 
                              chunksize=config.CHUNK_SIZE, method='multi')
            
            logger.info(f"✅ {ts_code} 技术指标已保存({len(indicator_df)}条)")
            return True
            
        except Exception as e:
            logger.error(f"❌ 保存{ts_code}技术指标失败: {e}")
            return False

# 全局技术指标计算器实例
tech_indicator = TechnicalIndicators()

if __name__ == "__main__":
    # 测试技术指标计算
    test_code = '000001.SZ'
    df = db.query_stock_data(test_code, '20240101', '20241220')
    if not df.empty:
        df_with_indicators = tech_indicator.calculate_all_indicators(df)
        signals = tech_indicator.generate_signals(df_with_indicators)
        print(f"技术指标计算完成，数据量: {len(df_with_indicators)}")
        print(f"最新强弱度评分: {df_with_indicators['strength_score'].iloc[-1]:.2f}")
        print(f"最新交易信号: 买入={signals['buy_signal'].iloc[-1]}, 卖出={signals['sell_signal'].iloc[-1]}")
    else:
        print("未获取到测试数据") 