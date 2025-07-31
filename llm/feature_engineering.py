"""
股票推荐系统的数据预处理和特征工程模块
用于准备训练数据和构建机器学习特征
"""
import pandas as pd
import numpy as np
import logging
from typing import Dict, List, Tuple, Optional, Any
from datetime import datetime, timedelta
from sklearn.preprocessing import StandardScaler, LabelEncoder, MinMaxScaler
from sklearn.feature_selection import SelectKBest, f_classif
import sys, os

# 添加项目路径
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from database.db_manager import DatabaseManager

# 日志配置
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class StockFeatureEngineer:
    """股票数据特征工程器"""
    
    def __init__(self):
        self.db_manager = DatabaseManager()
        self.scalers = {}
        self.label_encoders = {}
        self.feature_names = []
        
    def fetch_raw_stock_data(self, ts_code: str, start_date: str, end_date: str) -> pd.DataFrame:
        """获取原始股票数据"""
        try:
            # 获取日线数据
            daily_data = self.db_manager.fetch_data(f"""
                SELECT 
                    trade_date, ts_code, open, high, low, close, 
                    vol, amount, pct_chg, pre_close
                FROM stock_daily_202507
                WHERE ts_code = '{ts_code}'
                    AND trade_date BETWEEN '{start_date}' AND '{end_date}'
                ORDER BY trade_date
            """)
            
            if daily_data.empty:
                return pd.DataFrame()
            
            # 获取技术指标
            technical_data = self.db_manager.fetch_data(f"""
                SELECT 
                    trade_date, ts_code,
                    ma5, ma10, ma20, ma60,
                    rsi6, rsi12, rsi24,
                    macd_dif, macd_dea, macd_macd,
                    kdj_k, kdj_d, kdj_j,
                    vol_ratio, turn_rate, swing
                FROM technical_indicators
                WHERE ts_code = '{ts_code}'
                    AND trade_date BETWEEN '{start_date}' AND '{end_date}'
                ORDER BY trade_date
            """)
            
            # 合并数据
            if not technical_data.empty:
                merged_data = pd.merge(daily_data, technical_data, 
                                     on=['trade_date', 'ts_code'], how='left')
            else:
                merged_data = daily_data
            
            # 获取基本面数据
            basic_data = self.db_manager.fetch_data(f"""
                SELECT ts_code, name, industry, market, list_date
                FROM stock_basic
                WHERE ts_code = '{ts_code}'
            """)
            
            if not basic_data.empty:
                for col in ['name', 'industry', 'market', 'list_date']:
                    merged_data[col] = basic_data.iloc[0][col]
            
            return merged_data
            
        except Exception as e:
            logger.error(f"获取股票数据失败 {ts_code}: {e}")
            return pd.DataFrame()
    
    def calculate_technical_features(self, df: pd.DataFrame) -> pd.DataFrame:
        """计算技术分析特征"""
        try:
            if df.empty or len(df) < 30:
                return df
            
            # 确保数据按日期排序
            df = df.sort_values('trade_date').copy()
            
            # 价格相关特征
            df['price_ma_ratio_5'] = df['close'] / df['ma5']
            df['price_ma_ratio_20'] = df['close'] / df['ma20']
            df['ma_ratio_5_20'] = df['ma5'] / df['ma20']
            
            # 价格位置特征
            df['price_position'] = (df['close'] - df['low']) / (df['high'] - df['low'])
            df['price_change_rate'] = df['close'].pct_change()
            
            # 成交量特征
            df['volume_ma5'] = df['vol'].rolling(window=5).mean()
            df['volume_ma20'] = df['vol'].rolling(window=20).mean()
            df['volume_ratio_ma5'] = df['vol'] / df['volume_ma5']
            df['volume_ratio_ma20'] = df['vol'] / df['volume_ma20']
            
            # 波动性特征
            df['volatility_5'] = df['close'].rolling(window=5).std()
            df['volatility_20'] = df['close'].rolling(window=20).std()
            df['high_low_ratio'] = df['high'] / df['low']
            
            # 趋势强度
            df['trend_strength'] = np.where(
                (df['ma5'] > df['ma20']) & (df['close'] > df['ma5']), 1,
                np.where((df['ma5'] < df['ma20']) & (df['close'] < df['ma5']), -1, 0)
            )
            
            # 支撑阻力位
            df['support_20'] = df['low'].rolling(window=20).min()
            df['resistance_20'] = df['high'].rolling(window=20).max()
            df['support_distance'] = (df['close'] - df['support_20']) / df['close']
            df['resistance_distance'] = (df['resistance_20'] - df['close']) / df['close']
            
            # MACD信号
            df['macd_signal'] = np.where(df['macd_dif'] > df['macd_dea'], 1, -1)
            df['macd_histogram'] = df['macd_dif'] - df['macd_dea']
            
            # KDJ信号
            df['kdj_signal'] = np.where(
                (df['kdj_k'] > df['kdj_d']) & (df['kdj_k'] < 80), 1,
                np.where((df['kdj_k'] < df['kdj_d']) & (df['kdj_k'] > 20), -1, 0)
            )
            
            # RSI特征
            df['rsi_signal'] = np.where(
                df['rsi6'] < 30, 1,  # 超卖
                np.where(df['rsi6'] > 70, -1, 0)  # 超买
            )
            
            return df
            
        except Exception as e:
            logger.error(f"计算技术特征失败: {e}")
            return df
    
    def calculate_fundamental_features(self, df: pd.DataFrame) -> pd.DataFrame:
        """计算基本面特征"""
        try:
            if df.empty:
                return df
            
            # 行业编码
            if 'industry' in df.columns:
                if 'industry' not in self.label_encoders:
                    self.label_encoders['industry'] = LabelEncoder()
                    
                # 处理缺失值
                df['industry'] = df['industry'].fillna('未知')
                df['industry_encoded'] = self.label_encoders['industry'].fit_transform(df['industry'])
            
            # 市场编码
            if 'market' in df.columns:
                if 'market' not in self.label_encoders:
                    self.label_encoders['market'] = LabelEncoder()
                
                df['market'] = df['market'].fillna('未知')
                df['market_encoded'] = self.label_encoders['market'].fit_transform(df['market'])
            
            # 上市时间特征
            if 'list_date' in df.columns:
                df['list_date'] = pd.to_datetime(df['list_date'], format='%Y%m%d', errors='coerce')
                df['days_since_listing'] = (pd.to_datetime(df['trade_date']) - df['list_date']).dt.days
                df['listing_years'] = df['days_since_listing'] / 365.25
            
            return df
            
        except Exception as e:
            logger.error(f"计算基本面特征失败: {e}")
            return df
    
    def calculate_market_features(self, df: pd.DataFrame, market_data: pd.DataFrame = None) -> pd.DataFrame:
        """计算市场相关特征"""
        try:
            if df.empty:
                return df
            
            # 如果没有市场数据，获取市场指数数据
            if market_data is None:
                market_data = self._get_market_index_data(
                    df['trade_date'].min(), df['trade_date'].max()
                )
            
            if not market_data.empty:
                # 合并市场数据
                df = pd.merge(df, market_data[['trade_date', 'market_return', 'market_volatility']], 
                            on='trade_date', how='left')
                
                # 相对市场表现
                df['relative_performance'] = df['pct_chg'] - df['market_return']
                
                # Beta系数（简化计算）
                if len(df) >= 30:
                    correlation = df['pct_chg'].rolling(window=30).corr(df['market_return'])
                    stock_std = df['pct_chg'].rolling(window=30).std()
                    market_std = df['market_return'].rolling(window=30).std()
                    df['beta'] = correlation * (stock_std / market_std)
                else:
                    df['beta'] = 1.0
            
            return df
            
        except Exception as e:
            logger.error(f"计算市场特征失败: {e}")
            return df
    
    def _get_market_index_data(self, start_date: str, end_date: str) -> pd.DataFrame:
        """获取市场指数数据"""
        try:
            # 使用A股整体表现作为市场指标
            market_data = self.db_manager.fetch_data(f"""
                SELECT 
                    trade_date,
                    AVG(pct_chg) as market_return,
                    STD(pct_chg) as market_volatility
                FROM stock_daily_202507
                WHERE trade_date BETWEEN '{start_date}' AND '{end_date}'
                GROUP BY trade_date
                ORDER BY trade_date
            """)
            
            return market_data
            
        except Exception as e:
            logger.error(f"获取市场指数数据失败: {e}")
            return pd.DataFrame()
    
    def create_target_variables(self, df: pd.DataFrame, prediction_days: int = 5) -> pd.DataFrame:
        """创建目标变量"""
        try:
            if df.empty or len(df) < prediction_days:
                return df
            
            # 确保按日期排序
            df = df.sort_values('trade_date').copy()
            
            # 未来收益率
            for days in [1, 3, 5, 10]:
                if days <= len(df):
                    df[f'future_return_{days}d'] = df['close'].shift(-days) / df['close'] - 1
            
            # 未来价格分类 - 使用更平衡的阈值
            df['price_direction_1d'] = np.where(df['future_return_1d'] > 0.01, 1,  # 上涨>1%
                                               np.where(df['future_return_1d'] < -0.01, -1, 0))  # 下跌>1%
            
            # 使用分位数创建更平衡的目标变量
            if len(df) > 30:
                upper_quantile = df['future_return_5d'].quantile(0.7)
                lower_quantile = df['future_return_5d'].quantile(0.3)
                df['price_direction_5d'] = np.where(df['future_return_5d'] > upper_quantile, 1,
                                                   np.where(df['future_return_5d'] < lower_quantile, -1, 0))
            else:
                df['price_direction_5d'] = np.where(df['future_return_5d'] > 0.02, 1,  # 2天上涨>2%
                                                   np.where(df['future_return_5d'] < -0.02, -1, 0))
            
            # 是否跑赢市场
            if 'market_return' in df.columns:
                df['outperform_market_1d'] = (df['future_return_1d'] > df['market_return'].shift(-1)).astype(int)
                df['outperform_market_5d'] = (df['future_return_5d'] > df['market_return'].shift(-5)).astype(int)
            
            return df
            
        except Exception as e:
            logger.error(f"创建目标变量失败: {e}")
            return df
    
    def select_features(self, df: pd.DataFrame, target_col: str = 'price_direction_5d', k: int = 20) -> List[str]:
        """特征选择"""
        try:
            # 获取数值型特征
            numeric_cols = df.select_dtypes(include=[np.number]).columns
            feature_cols = [col for col in numeric_cols 
                           if col not in ['future_return_1d', 'future_return_3d', 'future_return_5d', 'future_return_10d',
                                        'price_direction_1d', 'price_direction_5d', 'outperform_market_1d', 'outperform_market_5d']]
            
            # 移除缺失值过多的特征
            feature_cols = [col for col in feature_cols if df[col].isnull().sum() / len(df) < 0.3]
            
            if len(feature_cols) == 0 or target_col not in df.columns:
                logger.warning("没有可用的特征列或目标列")
                return []
            
            # 填充缺失值
            X = df[feature_cols].fillna(0)
            y = df[target_col].fillna(0)
            
            # 移除目标变量缺失的行
            valid_idx = ~y.isnull()
            X = X[valid_idx]
            y = y[valid_idx]
            
            if len(X) == 0:
                return []
            
            # 特征选择
            selector = SelectKBest(score_func=f_classif, k=min(k, len(feature_cols)))
            selector.fit(X, y)
            
            # 获取选中的特征
            selected_features = [feature_cols[i] for i in selector.get_support(indices=True)]
            
            logger.info(f"特征选择完成，从{len(feature_cols)}个特征中选择了{len(selected_features)}个")
            
            return selected_features
            
        except Exception as e:
            logger.error(f"特征选择失败: {e}")
            return []
    
    def preprocess_training_data(self, stock_list: List[str], 
                               start_date: str, end_date: str,
                               prediction_days: int = 5) -> Tuple[pd.DataFrame, List[str]]:
        """预处理训练数据"""
        logger.info(f"开始预处理训练数据: {len(stock_list)}只股票，时间范围: {start_date} - {end_date}")
        
        all_data = []
        processed_count = 0
        
        for ts_code in stock_list:
            try:
                # 获取原始数据
                df = self.fetch_raw_stock_data(ts_code, start_date, end_date)
                
                if df.empty or len(df) < 30:  # 数据太少跳过
                    continue
                
                # 特征工程
                df = self.calculate_technical_features(df)
                df = self.calculate_fundamental_features(df)
                df = self.calculate_market_features(df)
                df = self.create_target_variables(df, prediction_days)
                
                # 移除缺失目标变量的行
                df = df.dropna(subset=[f'future_return_{prediction_days}d', f'price_direction_{prediction_days}d'])
                
                if len(df) > 0:
                    all_data.append(df)
                    processed_count += 1
                
                if processed_count % 10 == 0:
                    logger.info(f"已处理 {processed_count}/{len(stock_list)} 只股票")
                    
            except Exception as e:
                logger.error(f"处理股票数据失败 {ts_code}: {e}")
                continue
        
        if not all_data:
            logger.error("没有成功处理的股票数据")
            return pd.DataFrame(), []
        
        # 合并所有数据
        combined_data = pd.concat(all_data, ignore_index=True)
        
        # 特征选择
        selected_features = self.select_features(combined_data, f'price_direction_{prediction_days}d')
        
        # 保存特征名称
        self.feature_names = selected_features
        
        logger.info(f"数据预处理完成: {len(combined_data)}条记录，{len(selected_features)}个特征")
        
        return combined_data, selected_features
    
    def prepare_features_for_prediction(self, df: pd.DataFrame, selected_features: List[str]) -> np.ndarray:
        """为预测准备特征"""
        try:
            if df.empty:
                return np.array([])
            
            # 获取特征数据
            feature_data = df[selected_features].fillna(0)
            
            # 标准化
            if 'feature_scaler' not in self.scalers:
                self.scalers['feature_scaler'] = StandardScaler()
                scaled_features = self.scalers['feature_scaler'].fit_transform(feature_data)
            else:
                scaled_features = self.scalers['feature_scaler'].transform(feature_data)
            
            return scaled_features
            
        except Exception as e:
            logger.error(f"准备预测特征失败: {e}")
            return np.array([])
    
    def get_stock_list_for_training(self, limit: int = 1000) -> List[str]:
        """获取用于训练的股票列表"""
        try:
            # 选择有足够数据的活跃股票
            stock_data = self.db_manager.fetch_data(f"""
                SELECT 
                    s.ts_code,
                    COUNT(d.trade_date) as data_count,
                    AVG(d.vol) as avg_volume
                FROM stock_basic s
                JOIN stock_daily_202507 d ON s.ts_code = d.ts_code
                WHERE d.trade_date >= DATE_SUB(CURDATE(), INTERVAL 180 DAY)
                GROUP BY s.ts_code
                HAVING data_count >= 100 AND avg_volume > 10000
                ORDER BY avg_volume DESC
                LIMIT {limit}
            """)
            
            if stock_data.empty:
                logger.warning("未找到合适的训练股票")
                return []
            
            return stock_data['ts_code'].tolist()
            
        except Exception as e:
            logger.error(f"获取训练股票列表失败: {e}")
            return []

# 全局实例
feature_engineer = StockFeatureEngineer()

def main():
    """主函数 - 用于测试特征工程"""
    try:
        # 获取训练股票列表
        stock_list = feature_engineer.get_stock_list_for_training(50)  # 测试用50只股票
        logger.info(f"获取训练股票: {len(stock_list)}只")
        
        if not stock_list:
            logger.error("没有可用的训练股票")
            return
        
        # 设置日期范围
        end_date = datetime.now().strftime('%Y%m%d')
        start_date = (datetime.now() - timedelta(days=180)).strftime('%Y%m%d')
        
        # 预处理训练数据
        training_data, selected_features = feature_engineer.preprocess_training_data(
            stock_list, start_date, end_date
        )
        
        if training_data.empty:
            logger.error("训练数据为空")
            return
        
        logger.info("特征工程测试完成")
        logger.info(f"训练数据形状: {training_data.shape}")
        logger.info(f"选择的特征数量: {len(selected_features)}")
        logger.info(f"选择的特征: {selected_features[:10]}...")  # 显示前10个特征
        
        # 显示数据统计
        print("\n数据统计:")
        print(f"总记录数: {len(training_data)}")
        print(f"股票数量: {training_data['ts_code'].nunique()}")
        print(f"时间范围: {training_data['trade_date'].min()} - {training_data['trade_date'].max()}")
        
        # 目标变量分布
        target_col = 'price_direction_5d'
        if target_col in training_data.columns:
            target_distribution = training_data[target_col].value_counts()
            print(f"\n目标变量分布:")
            print(target_distribution)
        
    except Exception as e:
        logger.error(f"特征工程测试失败: {e}")

if __name__ == "__main__":
    main()