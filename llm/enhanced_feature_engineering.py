"""
增强的数据预处理和特征工程模块
支持深度学习和大语言模型的特征构建
"""
import pandas as pd
import numpy as np
import logging
from typing import Dict, List, Tuple, Optional, Any, Union
from datetime import datetime, timedelta
import sys, os
import warnings
warnings.filterwarnings('ignore')

# 添加项目路径
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from database.db_manager import DatabaseManager
from llm.feature_engineering import feature_engineer

# 可选的深度学习依赖
try:
    import torch
    import torch.nn as nn
    from sklearn.preprocessing import StandardScaler, MinMaxScaler, RobustScaler
    TORCH_AVAILABLE = True
except ImportError:
    TORCH_AVAILABLE = False

# 日志配置
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class EnhancedFeatureEngineer:
    """增强版特征工程器，支持深度学习和大模型"""
    
    def __init__(self):
        self.db_manager = DatabaseManager()
        self.base_engineer = feature_engineer
        self.feature_transformers = {}
        self.sequence_length = 30  # 时序数据长度
        self.feature_categories = {
            'price': [],
            'volume': [],
            'technical': [],
            'fundamental': [],
            'market': [],
            'sentiment': []
        }
        
    def create_sequence_features(self, df: pd.DataFrame, sequence_length: int = 30) -> np.ndarray:
        """创建时序特征，用于LSTM/Transformer模型"""
        try:
            if df.empty or len(df) < sequence_length:
                return np.array([])
            
            # 确保数据按时间排序
            df = df.sort_values('trade_date').copy()
            
            # 选择数值特征
            numeric_cols = df.select_dtypes(include=[np.number]).columns
            feature_cols = [col for col in numeric_cols 
                           if not col.startswith('future_') and 
                           col not in ['price_direction_1d', 'price_direction_5d']]
            
            # 标准化特征
            scaler = RobustScaler()
            scaled_data = scaler.fit_transform(df[feature_cols].fillna(0))
            
            # 保存转换器
            self.feature_transformers['sequence_scaler'] = scaler
            self.feature_transformers['sequence_features'] = feature_cols
            
            # 创建滑动窗口序列
            sequences = []
            for i in range(sequence_length, len(scaled_data)):
                sequence = scaled_data[i-sequence_length:i]
                sequences.append(sequence)
            
            return np.array(sequences)
            
        except Exception as e:
            logger.error(f"创建时序特征失败: {e}")
            return np.array([])
    
    def extract_statistical_features(self, df: pd.DataFrame, windows: List[int] = [5, 10, 20, 60]) -> pd.DataFrame:
        """提取统计特征"""
        try:
            if df.empty:
                return df
            
            df = df.copy()
            
            for window in windows:
                if len(df) >= window:
                    # 价格统计特征
                    df[f'close_mean_{window}'] = df['close'].rolling(window).mean()
                    df[f'close_std_{window}'] = df['close'].rolling(window).std()
                    df[f'close_skew_{window}'] = df['close'].rolling(window).skew()
                    df[f'close_kurt_{window}'] = df['close'].rolling(window).kurt()
                    
                    # 收益率统计特征
                    df[f'return_mean_{window}'] = df['pct_chg'].rolling(window).mean()
                    df[f'return_std_{window}'] = df['pct_chg'].rolling(window).std()
                    df[f'return_skew_{window}'] = df['pct_chg'].rolling(window).skew()
                    
                    # 成交量统计特征
                    df[f'volume_mean_{window}'] = df['vol'].rolling(window).mean()
                    df[f'volume_std_{window}'] = df['vol'].rolling(window).std()
                    
                    # 高低点特征
                    df[f'high_max_{window}'] = df['high'].rolling(window).max()
                    df[f'low_min_{window}'] = df['low'].rolling(window).min()
                    df[f'price_range_{window}'] = (df[f'high_max_{window}'] - df[f'low_min_{window}']) / df[f'low_min_{window}']
                    
                    # 分位数特征
                    df[f'close_quantile_25_{window}'] = df['close'].rolling(window).quantile(0.25)
                    df[f'close_quantile_75_{window}'] = df['close'].rolling(window).quantile(0.75)
                    
            return df
            
        except Exception as e:
            logger.error(f"提取统计特征失败: {e}")
            return df
    
    def create_cross_sectional_features(self, df: pd.DataFrame, all_stocks_data: pd.DataFrame = None) -> pd.DataFrame:
        """创建横截面特征（相对排名等）"""
        try:
            if df.empty:
                return df
                
            df = df.copy()
            
            # 如果没有提供全市场数据，获取同期其他股票数据进行对比
            if all_stocks_data is None:
                all_stocks_data = self._get_market_comparison_data(
                    df['trade_date'].min(), df['trade_date'].max()
                )
            
            if not all_stocks_data.empty:
                for date in df['trade_date'].unique():
                    date_mask = df['trade_date'] == date
                    market_date_mask = all_stocks_data['trade_date'] == date
                    
                    if market_date_mask.sum() > 10:  # 确保有足够的对比数据
                        market_day_data = all_stocks_data[market_date_mask]
                        
                        if date_mask.sum() > 0:
                            stock_return = df.loc[date_mask, 'pct_chg'].iloc[0]
                            stock_volume = df.loc[date_mask, 'vol'].iloc[0]
                            
                            # 收益率排名
                            return_rank = (market_day_data['pct_chg'] < stock_return).mean()
                            df.loc[date_mask, 'return_rank'] = return_rank
                            
                            # 成交量排名
                            volume_rank = (market_day_data['vol'] < stock_volume).mean()
                            df.loc[date_mask, 'volume_rank'] = volume_rank
                            
                            # 行业排名（如果有行业信息）
                            if 'industry' in df.columns and 'industry' in market_day_data.columns:
                                industry = df.loc[date_mask, 'industry'].iloc[0]
                                industry_data = market_day_data[market_day_data['industry'] == industry]
                                if len(industry_data) > 1:
                                    industry_return_rank = (industry_data['pct_chg'] < stock_return).mean()
                                    df.loc[date_mask, 'industry_return_rank'] = industry_return_rank
            
            return df
            
        except Exception as e:
            logger.error(f"创建横截面特征失败: {e}")
            return df
    
    def _get_market_comparison_data(self, start_date: str, end_date: str) -> pd.DataFrame:
        """获取市场对比数据"""
        try:
            # 获取同期活跃股票的数据用于排名对比
            query = f"""
                SELECT trade_date, ts_code, pct_chg, vol
                FROM stock_daily_202507
                WHERE trade_date BETWEEN '{start_date}' AND '{end_date}'
                    AND vol > 10000
                ORDER BY trade_date, ts_code
            """
            return self.db_manager.fetch_data(query)
            
        except Exception as e:
            logger.error(f"获取市场对比数据失败: {e}")
            return pd.DataFrame()
    
    def create_sentiment_features(self, df: pd.DataFrame) -> pd.DataFrame:
        """创建市场情绪特征"""
        try:
            if df.empty:
                return df
            
            df = df.copy()
            
            for date in df['trade_date'].unique():
                try:
                    # 龙虎榜情绪
                    dragon_tiger_data = self.db_manager.fetch_data(f"""
                        SELECT COUNT(*) as dragon_tiger_count,
                               AVG(pct_chg) as avg_dragon_tiger_return
                        FROM t_dragon_tiger_list
                        WHERE trade_date = '{date}'
                    """)
                    
                    if not dragon_tiger_data.empty:
                        df.loc[df['trade_date'] == date, 'dragon_tiger_count'] = dragon_tiger_data.iloc[0]['dragon_tiger_count']
                        df.loc[df['trade_date'] == date, 'avg_dragon_tiger_return'] = dragon_tiger_data.iloc[0]['avg_dragon_tiger_return'] or 0
                    
                    # 市场情绪指标（基于当日涨跌分布）
                    market_sentiment = self.db_manager.fetch_data(f"""
                        SELECT 
                            COUNT(CASE WHEN pct_chg > 0 THEN 1 END) as up_count,
                            COUNT(CASE WHEN pct_chg < 0 THEN 1 END) as down_count,
                            COUNT(*) as total_count,
                            AVG(pct_chg) as avg_return,
                            STDDEV(pct_chg) as market_volatility
                        FROM stock_daily_202507
                        WHERE trade_date = '{date}' AND vol > 10000
                    """)
                    
                    if not market_sentiment.empty:
                        sentiment_data = market_sentiment.iloc[0]
                        up_ratio = sentiment_data['up_count'] / sentiment_data['total_count'] if sentiment_data['total_count'] > 0 else 0.5
                        
                        df.loc[df['trade_date'] == date, 'market_up_ratio'] = up_ratio
                        df.loc[df['trade_date'] == date, 'market_avg_return'] = sentiment_data['avg_return'] or 0
                        df.loc[df['trade_date'] == date, 'market_volatility'] = sentiment_data['market_volatility'] or 0
                        
                except Exception as e:
                    logger.warning(f"处理日期{date}的情绪特征失败: {e}")
                    continue
            
            return df
            
        except Exception as e:
            logger.error(f"创建情绪特征失败: {e}")
            return df
    
    def create_momentum_features(self, df: pd.DataFrame) -> pd.DataFrame:
        """创建动量特征"""
        try:
            if df.empty:
                return df
            
            df = df.copy()
            
            # 价格动量
            for period in [5, 10, 20, 60]:
                if len(df) >= period:
                    df[f'price_momentum_{period}'] = df['close'] / df['close'].shift(period) - 1
                    df[f'volume_momentum_{period}'] = df['vol'] / df['vol'].shift(period) - 1
            
            # 加速度特征
            df['price_acceleration_5'] = df['price_momentum_5'] - df['price_momentum_10']
            df['price_acceleration_10'] = df['price_momentum_10'] - df['price_momentum_20']
            
            # 趋势一致性
            df['trend_consistency_5'] = df['close'].rolling(5).apply(
                lambda x: len([i for i in range(1, len(x)) if x.iloc[i] > x.iloc[i-1]]) / (len(x) - 1) if len(x) > 1 else 0.5
            )
            
            # 波动率调整动量
            for period in [10, 20]:
                if len(df) >= period:
                    volatility = df['close'].rolling(period).std()
                    df[f'volatility_adjusted_momentum_{period}'] = df[f'price_momentum_{period}'] / volatility
            
            return df
            
        except Exception as e:
            logger.error(f"创建动量特征失败: {e}")
            return df
    
    def create_regime_features(self, df: pd.DataFrame) -> pd.DataFrame:
        """创建市场状态/制度特征"""
        try:
            if df.empty:
                return df
            
            df = df.copy()
            
            # 波动率制度
            volatility_20 = df['close'].rolling(20).std()
            volatility_60 = df['close'].rolling(60).std()
            df['volatility_regime'] = np.where(
                volatility_20 > volatility_60 * 1.5, 1,  # 高波动
                np.where(volatility_20 < volatility_60 * 0.5, -1, 0)  # 低波动
            )
            
            # 趋势制度
            ma_5 = df['close'].rolling(5).mean()
            ma_20 = df['close'].rolling(20).mean() 
            ma_60 = df['close'].rolling(60).mean()
            
            df['trend_regime'] = np.where(
                (ma_5 > ma_20) & (ma_20 > ma_60), 1,  # 上升趋势
                np.where((ma_5 < ma_20) & (ma_20 < ma_60), -1, 0)  # 下降趋势
            )
            
            # 成交量制度
            volume_ma_20 = df['vol'].rolling(20).mean()
            df['volume_regime'] = np.where(
                df['vol'] > volume_ma_20 * 2, 1,  # 高成交量
                np.where(df['vol'] < volume_ma_20 * 0.5, -1, 0)  # 低成交量
            )
            
            return df
            
        except Exception as e:
            logger.error(f"创建制度特征失败: {e}")
            return df
    
    def create_target_variables_enhanced(self, df: pd.DataFrame, prediction_horizons: List[int] = [1, 3, 5, 10, 20]) -> pd.DataFrame:
        """创建增强的目标变量"""
        try:
            if df.empty:
                return df
            
            df = df.copy()
            df = df.sort_values('trade_date')
            
            for horizon in prediction_horizons:
                if horizon <= len(df):
                    # 未来收益率
                    df[f'future_return_{horizon}d'] = df['close'].shift(-horizon) / df['close'] - 1
                    
                    # 未来最高价收益率
                    df[f'future_high_return_{horizon}d'] = df['high'].shift(-horizon) / df['close'] - 1
                    
                    # 未来最低价收益率  
                    df[f'future_low_return_{horizon}d'] = df['low'].shift(-horizon) / df['close'] - 1
                    
                    # 未来波动率
                    if horizon >= 5:
                        future_returns = df['pct_chg'].shift(-horizon).rolling(horizon).std()
                        df[f'future_volatility_{horizon}d'] = future_returns
                    
                    # 分类目标 - 使用自适应阈值
                    returns = df[f'future_return_{horizon}d'].dropna()
                    if len(returns) > 50:
                        upper_threshold = returns.quantile(0.7)
                        lower_threshold = returns.quantile(0.3)
                    else:
                        upper_threshold = 0.02
                        lower_threshold = -0.02
                    
                    df[f'price_direction_{horizon}d'] = np.where(
                        df[f'future_return_{horizon}d'] > upper_threshold, 1,
                        np.where(df[f'future_return_{horizon}d'] < lower_threshold, -1, 0)
                    )
                    
                    # 风险调整收益目标
                    if f'future_volatility_{horizon}d' in df.columns:
                        df[f'risk_adjusted_return_{horizon}d'] = df[f'future_return_{horizon}d'] / (df[f'future_volatility_{horizon}d'] + 1e-6)
            
            # 组合目标：多期综合表现
            if all(f'future_return_{h}d' in df.columns for h in [1, 5, 10]):
                df['composite_future_return'] = (
                    0.2 * df['future_return_1d'] + 
                    0.5 * df['future_return_5d'] + 
                    0.3 * df['future_return_10d']
                )
                
                returns = df['composite_future_return'].dropna()
                if len(returns) > 50:
                    upper_threshold = returns.quantile(0.7)
                    lower_threshold = returns.quantile(0.3)
                    df['composite_direction'] = np.where(
                        df['composite_future_return'] > upper_threshold, 1,
                        np.where(df['composite_future_return'] < lower_threshold, -1, 0)
                    )
            
            return df
            
        except Exception as e:
            logger.error(f"创建增强目标变量失败: {e}")
            return df
    
    def create_feature_interactions(self, df: pd.DataFrame, important_features: List[str] = None) -> pd.DataFrame:
        """创建特征交互项"""
        try:
            if df.empty:
                return df
            
            df = df.copy()
            
            # 如果没有指定重要特征，选择一些常用的
            if important_features is None:
                important_features = ['ma5', 'ma20', 'rsi6', 'macd_dif', 'vol_ratio', 'pct_chg']
            
            # 过滤存在的特征
            available_features = [f for f in important_features if f in df.columns]
            
            # 创建两两交互特征
            for i, feat1 in enumerate(available_features):
                for feat2 in available_features[i+1:]:
                    # 乘积交互
                    df[f'{feat1}_x_{feat2}'] = df[feat1] * df[feat2]
                    
                    # 比率交互
                    with np.errstate(divide='ignore', invalid='ignore'):
                        ratio = df[feat1] / (df[feat2] + 1e-8)
                        df[f'{feat1}_div_{feat2}'] = np.where(np.isfinite(ratio), ratio, 0)
                    
                    # 差值交互
                    df[f'{feat1}_minus_{feat2}'] = df[feat1] - df[feat2]
            
            return df
            
        except Exception as e:
            logger.error(f"创建特征交互失败: {e}")
            return df
    
    def create_wavelet_features(self, df: pd.DataFrame) -> pd.DataFrame:
        """使用小波变换创建特征"""
        try:
            if df.empty or len(df) < 32:
                return df
                
            try:
                import pywt
                wavelet_available = True
            except ImportError:
                logger.warning("PyWavelets未安装，跳过小波特征")
                return df
            
            df = df.copy()
            
            # 对价格序列进行小波分解
            price_series = df['close'].fillna(method='ffill').fillna(method='bfill')
            
            # 使用Daubechies小波
            coeffs = pywt.wavedec(price_series, 'db4', level=3)
            
            # 提取近似和细节系数作为特征
            for i, coeff in enumerate(coeffs):
                # 重构到原始长度
                if len(coeff) < len(df):
                    # 上采样
                    coeff_resampled = np.interp(
                        np.linspace(0, 1, len(df)),
                        np.linspace(0, 1, len(coeff)),
                        coeff
                    )
                else:
                    coeff_resampled = coeff[:len(df)]
                
                if i == 0:
                    df[f'wavelet_approx'] = coeff_resampled
                else:
                    df[f'wavelet_detail_{i}'] = coeff_resampled
            
            # 小波能量特征
            for i in range(1, len(coeffs)):
                if len(coeffs[i]) > 0:
                    energy = np.sum(coeffs[i] ** 2)
                    df[f'wavelet_energy_{i}'] = energy
            
            return df
            
        except Exception as e:
            logger.error(f"创建小波特征失败: {e}")
            return df
    
    def create_fourier_features(self, df: pd.DataFrame) -> pd.DataFrame:
        """使用傅里叶变换创建频域特征"""
        try:
            if df.empty or len(df) < 32:
                return df
            
            df = df.copy()
            
            # 对价格序列进行FFT
            price_series = df['close'].fillna(method='ffill').fillna(method='bfill')
            fft_result = np.fft.fft(price_series)
            
            # 提取主要频率成分
            n_components = min(10, len(fft_result) // 4)
            
            # 幅度谱
            magnitude = np.abs(fft_result)
            for i in range(n_components):
                df[f'fft_magnitude_{i}'] = magnitude[i]
            
            # 相位谱
            phase = np.angle(fft_result)
            for i in range(n_components):
                df[f'fft_phase_{i}'] = phase[i]
            
            # 功率谱密度
            power = magnitude ** 2
            df['fft_total_power'] = np.sum(power)
            df['fft_dominant_freq'] = np.argmax(power[:len(power)//2])
            
            # 频域统计特征
            freqs = np.fft.fftfreq(len(price_series))
            df['fft_mean_freq'] = np.average(freqs[:len(freqs)//2], weights=power[:len(power)//2])
            df['fft_std_freq'] = np.sqrt(np.average((freqs[:len(freqs)//2] - df['fft_mean_freq'].iloc[0])**2, weights=power[:len(power)//2]))
            
            return df
            
        except Exception as e:
            logger.error(f"创建傅里叶特征失败: {e}")
            return df
    
    def create_fractal_features(self, df: pd.DataFrame) -> pd.DataFrame:
        """创建分形维数等复杂性特征"""
        try:
            if df.empty or len(df) < 50:
                return df
            
            df = df.copy()
            
            # Hurst指数计算
            def hurst_exponent(ts, max_lag=20):
                """计算Hurst指数"""
                try:
                    lags = range(2, min(max_lag, len(ts)//2))
                    tau = [np.sqrt(np.std(np.subtract(ts[lag:], ts[:-lag]))) for lag in lags]
                    tau = np.array(tau)
                    
                    # 过滤无效值
                    valid_idx = (tau > 0) & np.isfinite(tau)
                    if np.sum(valid_idx) < 3:
                        return 0.5
                    
                    # 线性回归计算斜率
                    poly = np.polyfit(np.log(np.array(lags)[valid_idx]), np.log(tau[valid_idx]), 1)
                    return poly[0] * 2.0
                except:
                    return 0.5
            
            # 计算不同窗口的Hurst指数
            for window in [20, 40, 60]:
                if len(df) >= window:
                    hurst_values = []
                    for i in range(window, len(df)):
                        price_window = df['close'].iloc[i-window:i].values
                        hurst = hurst_exponent(price_window)
                        hurst_values.append(hurst)
                    
                    # 填充前面的值
                    hurst_series = [0.5] * window + hurst_values
                    df[f'hurst_{window}'] = hurst_series[:len(df)]
            
            # 分形维数计算（简化版本）
            def box_counting_dimension(ts, max_box_size=10):
                """盒计数法计算分形维数"""
                try:
                    ts_norm = (ts - np.min(ts)) / (np.max(ts) - np.min(ts) + 1e-8)
                    box_sizes = np.logspace(0.1, np.log10(max_box_size), 10)
                    counts = []
                    
                    for box_size in box_sizes:
                        n_boxes = int(1.0 / box_size)
                        if n_boxes < 2:
                            continue
                        
                        grid = np.linspace(0, 1, n_boxes + 1)
                        count = 0
                        for i in range(n_boxes):
                            for j in range(len(ts_norm) - 1):
                                y1, y2 = ts_norm[j], ts_norm[j + 1]
                                if (grid[i] <= y1 <= grid[i + 1]) or (grid[i] <= y2 <= grid[i + 1]):
                                    count += 1
                                    break
                        counts.append(count)
                    
                    if len(counts) > 2:
                        valid_counts = np.array(counts)
                        valid_sizes = box_sizes[:len(counts)]
                        valid_idx = valid_counts > 0
                        
                        if np.sum(valid_idx) > 2:
                            poly = np.polyfit(np.log(valid_sizes[valid_idx]), np.log(valid_counts[valid_idx]), 1)
                            return -poly[0]
                    
                    return 1.5
                except:
                    return 1.5
            
            # 计算分形维数
            for window in [30, 60]:
                if len(df) >= window:
                    fractal_dims = []
                    for i in range(window, len(df)):
                        price_window = df['close'].iloc[i-window:i].values
                        fractal_dim = box_counting_dimension(price_window)
                        fractal_dims.append(fractal_dim)
                    
                    # 填充前面的值
                    fractal_series = [1.5] * window + fractal_dims
                    df[f'fractal_dim_{window}'] = fractal_series[:len(df)]
            
            return df
            
        except Exception as e:
            logger.error(f"创建分形特征失败: {e}")
            return df
    
    def create_network_features(self, df: pd.DataFrame, stock_code: str) -> pd.DataFrame:
        """创建网络/图特征（基于股票关联性）"""
        try:
            if df.empty:
                return df
            
            df = df.copy()
            
            # 获取同行业相关股票的信息
            industry_stocks = self._get_industry_stocks(stock_code)
            
            if industry_stocks and len(industry_stocks) > 1:
                # 计算与行业平均的相关性
                for date in df['trade_date'].unique():
                    try:
                        industry_return = self._get_industry_return(industry_stocks, date)
                        if industry_return is not None:
                            df.loc[df['trade_date'] == date, 'industry_correlation'] = industry_return
                    except:
                        continue
            
            # 市场网络中心性（简化版）
            # 基于成交量和收益率计算"影响力"
            df['volume_rank_normalized'] = df['vol'] / (df['vol'].rolling(20).mean() + 1e-8)
            df['return_volatility'] = df['pct_chg'].rolling(10).std()
            df['market_influence'] = df['volume_rank_normalized'] * np.abs(df['pct_chg'])
            
            return df
            
        except Exception as e:
            logger.error(f"创建网络特征失败: {e}")
            return df
    
    def _get_industry_stocks(self, stock_code: str) -> List[str]:
        """获取同行业股票列表"""
        try:
            query = """
                SELECT ts_code FROM stock_basic 
                WHERE industry = (
                    SELECT industry FROM stock_basic WHERE ts_code = %s
                ) AND ts_code != %s
                LIMIT 10
            """
            result = self.db_manager.fetch_data(query, (stock_code, stock_code))
            return result['ts_code'].tolist() if not result.empty else []
        except:
            return []
    
    def _get_industry_return(self, industry_stocks: List[str], date: str) -> Optional[float]:
        """获取行业平均收益率"""
        try:
            placeholders = ','.join(['%s'] * len(industry_stocks))
            query = f"""
                SELECT AVG(pct_chg) as avg_return 
                FROM stock_daily_202507 
                WHERE ts_code IN ({placeholders}) AND trade_date = %s
            """
            result = self.db_manager.fetch_data(query, industry_stocks + [date])
            return float(result.iloc[0]['avg_return']) if not result.empty and result.iloc[0]['avg_return'] is not None else None
        except:
            return None
    
    def prepare_multimodal_features(self, stock_list: List[str], start_date: str, end_date: str) -> Dict[str, Any]:
        """准备多模态特征（表格+时序+文本）"""
        logger.info(f"准备多模态特征: {len(stock_list)}只股票")
        
        try:
            all_tabular_features = []
            all_sequence_features = []
            all_text_features = []
            
            for ts_code in stock_list:
                # 获取基础数据
                df = self.base_engineer.fetch_raw_stock_data(ts_code, start_date, end_date)
                
                if df.empty or len(df) < self.sequence_length:
                    continue
                
                # 增强特征工程
                df = self.base_engineer.calculate_technical_features(df)
                df = self.base_engineer.calculate_fundamental_features(df)
                df = self.extract_statistical_features(df)
                df = self.create_momentum_features(df)
                df = self.create_regime_features(df)
                df = self.create_sentiment_features(df)
                df = self.create_target_variables_enhanced(df)
                
                # 表格特征
                numeric_cols = df.select_dtypes(include=[np.number]).columns
                feature_cols = [col for col in numeric_cols 
                               if not col.startswith('future_') and 
                               col not in ['price_direction_1d', 'price_direction_5d', 'price_direction_10d']]
                
                tabular_data = df[feature_cols].fillna(0)
                all_tabular_features.append(tabular_data)
                
                # 时序特征
                sequence_data = self.create_sequence_features(df, self.sequence_length)
                if sequence_data.size > 0:
                    all_sequence_features.append(sequence_data)
                
                # 文本特征（简化版，用于演示）
                text_features = self._create_text_features(df, ts_code)
                all_text_features.extend(text_features)
            
            # 合并数据
            if all_tabular_features:
                combined_tabular = pd.concat(all_tabular_features, ignore_index=True)
            else:
                combined_tabular = pd.DataFrame()
            
            if all_sequence_features:
                combined_sequences = np.concatenate(all_sequence_features, axis=0)
            else:
                combined_sequences = np.array([])
            
            result = {
                'tabular_features': combined_tabular,
                'sequence_features': combined_sequences,
                'text_features': all_text_features,
                'feature_categories': self._categorize_features(combined_tabular.columns if not combined_tabular.empty else [])
            }
            
            logger.info(f"多模态特征准备完成: 表格特征{combined_tabular.shape if not combined_tabular.empty else 'N/A'}, "
                       f"时序特征{combined_sequences.shape if combined_sequences.size > 0 else 'N/A'}, "
                       f"文本特征{len(all_text_features)}条")
            
            return result
            
        except Exception as e:
            logger.error(f"准备多模态特征失败: {e}")
            return {}
    
    def _create_text_features(self, df: pd.DataFrame, ts_code: str) -> List[str]:
        """创建文本特征（用于大模型）"""
        try:
            text_features = []
            
            # 获取股票基本信息
            if 'name' in df.columns and 'industry' in df.columns:
                name = df['name'].iloc[0] if not df.empty else ts_code
                industry = df['industry'].iloc[0] if 'industry' in df.columns else '未知'
                
                # 为每个交易日创建技术分析描述
                for _, row in df.tail(5).iterrows():  # 只处理最近5天
                    text = f"股票{name}({ts_code})属于{industry}行业。"
                    text += f"收盘价{row['close']:.2f}元，"
                    
                    if row.get('ma5', 0) > 0 and row.get('ma20', 0) > 0:
                        if row['close'] > row['ma5'] > row['ma20']:
                            text += "处于强势上升趋势，"
                        elif row['close'] < row['ma5'] < row['ma20']:
                            text += "处于弱势下降趋势，"
                        else:
                            text += "处于震荡整理状态，"
                    
                    if row.get('rsi6', 50) > 70:
                        text += "RSI显示超买状态，"
                    elif row.get('rsi6', 50) < 30:
                        text += "RSI显示超卖状态，"
                    
                    if row.get('vol_ratio', 1) > 2:
                        text += "成交量明显放大。"
                    elif row.get('vol_ratio', 1) < 0.5:
                        text += "成交量明显萎缩。"
                    else:
                        text += "成交量正常。"
                    
                    text_features.append(text)
            
            return text_features
            
        except Exception as e:
            logger.error(f"创建文本特征失败: {e}")
            return []
    
    def _categorize_features(self, feature_names: List[str]) -> Dict[str, List[str]]:
        """将特征分类"""
        categories = {
            'price': [],
            'volume': [],
            'technical': [],
            'fundamental': [],
            'market': [],
            'sentiment': [],
            'statistical': [],
            'momentum': [],
            'regime': []
        }
        
        for feature in feature_names:
            feature_lower = feature.lower()
            
            if any(keyword in feature_lower for keyword in ['close', 'high', 'low', 'open', 'price']):
                categories['price'].append(feature)
            elif any(keyword in feature_lower for keyword in ['vol', 'amount', 'turn']):
                categories['volume'].append(feature)
            elif any(keyword in feature_lower for keyword in ['ma', 'rsi', 'macd', 'kdj', 'boll']):
                categories['technical'].append(feature)
            elif any(keyword in feature_lower for keyword in ['industry', 'market', 'list']):
                categories['fundamental'].append(feature)
            elif any(keyword in feature_lower for keyword in ['dragon', 'sentiment', 'up_ratio']):
                categories['sentiment'].append(feature)
            elif any(keyword in feature_lower for keyword in ['mean', 'std', 'skew', 'kurt', 'quantile']):
                categories['statistical'].append(feature)
            elif any(keyword in feature_lower for keyword in ['momentum', 'acceleration']):
                categories['momentum'].append(feature)
            elif any(keyword in feature_lower for keyword in ['regime', 'volatility_regime', 'trend_regime']):
                categories['regime'].append(feature)
            else:
                categories['market'].append(feature)
        
        return categories

# 全局实例
enhanced_feature_engineer = EnhancedFeatureEngineer()

def main():
    """测试增强特征工程"""
    try:
        logger.info("开始测试增强特征工程...")
        
        # 获取少量股票进行测试
        stock_list = enhanced_feature_engineer.base_engineer.get_stock_list_for_training(10)
        
        if not stock_list:
            logger.error("无法获取测试股票")
            return
        
        # 设置日期范围
        end_date = datetime.now().strftime('%Y%m%d')
        start_date = (datetime.now() - timedelta(days=90)).strftime('%Y%m%d')
        
        # 准备多模态特征
        multimodal_data = enhanced_feature_engineer.prepare_multimodal_features(
            stock_list, start_date, end_date
        )
        
        print("\n增强特征工程测试结果:")
        print("=" * 50)
        
        if 'tabular_features' in multimodal_data and not multimodal_data['tabular_features'].empty:
            tabular = multimodal_data['tabular_features']
            print(f"表格特征: {tabular.shape}")
            print(f"特征数量: {len(tabular.columns)}")
        
        if 'sequence_features' in multimodal_data and multimodal_data['sequence_features'].size > 0:
            sequences = multimodal_data['sequence_features']
            print(f"时序特征: {sequences.shape}")
        
        if 'text_features' in multimodal_data:
            texts = multimodal_data['text_features']
            print(f"文本特征: {len(texts)}条")
            if texts:
                print(f"示例文本: {texts[0][:100]}...")
        
        if 'feature_categories' in multimodal_data:
            categories = multimodal_data['feature_categories']
            print("\n特征分类:")
            for category, features in categories.items():
                if features:
                    print(f"{category}: {len(features)}个特征")
        
        logger.info("增强特征工程测试完成")
        
    except Exception as e:
        logger.error(f"测试失败: {e}")

if __name__ == "__main__":
    main()