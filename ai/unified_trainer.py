"""
综合模型训练框架
支持传统机器学习、深度学习和大语言模型的统一训练接口
"""
import pandas as pd
import numpy as np
import logging
import joblib
import json
import os
import pickle
from typing import Dict, List, Tuple, Optional, Any, Union
from datetime import datetime, timedelta
from sklearn.ensemble import RandomForestClassifier, GradientBoostingClassifier
from sklearn.linear_model import LogisticRegression
from sklearn.svm import SVC
from sklearn.neural_network import MLPClassifier
from sklearn.model_selection import train_test_split, cross_val_score, TimeSeriesSplit
from sklearn.metrics import classification_report, accuracy_score, precision_score, recall_score, f1_score
from sklearn.preprocessing import StandardScaler, LabelEncoder
import warnings
warnings.filterwarnings('ignore')

# 可选依赖检查
TORCH_AVAILABLE = False
TRANSFORMERS_AVAILABLE = False
XGBOOST_AVAILABLE = False
LIGHTGBM_AVAILABLE = False

try:
    import torch
    import torch.nn as nn
    import torch.optim as optim
    from torch.utils.data import Dataset, DataLoader
    TORCH_AVAILABLE = True
except ImportError:
    pass

try:
    from transformers import AutoTokenizer, AutoModel
    TRANSFORMERS_AVAILABLE = True
except ImportError:
    pass

try:
    import xgboost as xgb
    XGBOOST_AVAILABLE = True
except ImportError:
    XGBOOST_AVAILABLE = False
    xgb = None
except Exception as e:
    XGBOOST_AVAILABLE = False
    xgb = None
    logging.warning(f"XGBoost加载失败: {e}")

try:
    import lightgbm as lgb
    LIGHTGBM_AVAILABLE = True
except ImportError:
    LIGHTGBM_AVAILABLE = False
    lgb = None
except Exception as e:
    LIGHTGBM_AVAILABLE = False
    lgb = None
    logging.warning(f"LightGBM加载失败: {e}")

import sys
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from database.db_manager import DatabaseManager
from llm.enhanced_feature_engineering import enhanced_feature_engineer
from ai.ml_trainer import ml_trainer
from utils.m2_ultra_config import m2_ultra_config, get_optimal_config
from utils.performance_monitor import performance_monitor

# 日志配置
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# PyTorch相关类（仅在PyTorch可用时定义）
if TORCH_AVAILABLE:
    class StockDataset(Dataset):
        """PyTorch数据集"""
        def __init__(self, features: np.ndarray, targets: np.ndarray, sequence_features: np.ndarray = None):
            self.features = torch.FloatTensor(features)
            self.targets = torch.LongTensor(targets)
            self.sequence_features = torch.FloatTensor(sequence_features) if sequence_features is not None else None
        
        def __len__(self):
            return len(self.features)
        
        def __getitem__(self, idx):
            if self.sequence_features is not None:
                return {
                    'tabular': self.features[idx],
                    'sequence': self.sequence_features[idx],
                    'target': self.targets[idx]
                }
            else:
                return {
                    'tabular': self.features[idx],
                    'target': self.targets[idx]
                }

    class LSTMModel(nn.Module):
        """LSTM模型"""
        def __init__(self, sequence_length: int, feature_dim: int, hidden_dim: int = 64, num_layers: int = 2, num_classes: int = 3):
            super(LSTMModel, self).__init__()
            self.lstm = nn.LSTM(feature_dim, hidden_dim, num_layers, batch_first=True, dropout=0.2)
            self.classifier = nn.Sequential(
                nn.Linear(hidden_dim, hidden_dim // 2),
                nn.ReLU(),
                nn.Dropout(0.3),
                nn.Linear(hidden_dim // 2, num_classes)
            )
        
        def forward(self, x):
            lstm_out, (hidden, _) = self.lstm(x)
            # 使用最后一个时间步的输出
            output = self.classifier(hidden[-1])
            return output

    class MultiModalModel(nn.Module):
        """多模态融合模型"""
        def __init__(self, tabular_dim: int, sequence_length: int, sequence_feature_dim: int, 
                     hidden_dim: int = 128, num_classes: int = 3):
            super(MultiModalModel, self).__init__()
            
            # 表格特征处理
            self.tabular_net = nn.Sequential(
                nn.Linear(tabular_dim, hidden_dim),
                nn.ReLU(),
                nn.Dropout(0.3),
                nn.Linear(hidden_dim, hidden_dim // 2)
            )
            
            # 时序特征处理
            self.lstm = nn.LSTM(sequence_feature_dim, hidden_dim // 2, 2, batch_first=True, dropout=0.2)
            
            # 融合层
            self.fusion = nn.Sequential(
                nn.Linear(hidden_dim, hidden_dim),
                nn.ReLU(),
                nn.Dropout(0.3),
                nn.Linear(hidden_dim, num_classes)
            )
        
        def forward(self, tabular, sequence):
            # 处理表格特征
            tabular_features = self.tabular_net(tabular)
            
            # 处理时序特征
            lstm_out, (hidden, _) = self.lstm(sequence)
            sequence_features = hidden[-1]
            
            # 融合特征
            combined = torch.cat([tabular_features, sequence_features], dim=1)
            output = self.fusion(combined)
            
            return output
else:
    # 当PyTorch不可用时的占位类
    class StockDataset:
        def __init__(self, *args, **kwargs):
            raise RuntimeError("PyTorch不可用，无法使用StockDataset")
    
    class LSTMModel:
        def __init__(self, *args, **kwargs):
            raise RuntimeError("PyTorch不可用，无法使用LSTMModel")
    
    class MultiModalModel:
        def __init__(self, *args, **kwargs):
            raise RuntimeError("PyTorch不可用，无法使用MultiModalModel")

class UnifiedModelTrainer:
    """统一模型训练器"""
    
    def __init__(self):
        self.db_manager = DatabaseManager()
        self.feature_engineer = enhanced_feature_engineer
        self.ml_trainer = ml_trainer
        
        # M2 Ultra优化配置
        self.m2_config = m2_ultra_config
        self.is_m2_ultra = self.m2_config.is_m2_ultra
        
        # 模型存储
        self.models = {}
        self.model_configs = {}
        self.performance_history = {}
        
        # 根据硬件配置设备
        if TORCH_AVAILABLE:
            pytorch_config = self.m2_config.get_pytorch_config()
            device_type = pytorch_config.get('device', 'cpu')
            
            if device_type == 'mps' and torch.backends.mps.is_available():
                self.device = torch.device('mps')
                # 配置MPS优化 (安全的API调用)
                try:
                    if hasattr(torch.backends.mps, 'use_sync_allocator'):
                        torch.backends.mps.use_sync_allocator(pytorch_config.get('mps_sync_allocator', True))
                    if hasattr(torch.backends.mps, 'enable_metal_sync') and pytorch_config.get('mps_metal_sync', True):
                        torch.backends.mps.enable_metal_sync(True)
                except Exception as e:
                    logger.warning(f"MPS配置警告: {e}")
                logger.info("✅ 使用Apple Silicon GPU (MPS)")
            else:
                self.device = torch.device('cpu')
                logger.info("使用CPU计算")
        else:
            self.device = None
            
        self.model_save_path = "models"
        os.makedirs(self.model_save_path, exist_ok=True)
        
        # 打印系统信息
        if self.is_m2_ultra:
            logger.info("🚀 M2 Ultra优化模式已启用")
            self.m2_config.print_system_info()
        
        logger.info(f"统一训练器初始化完成，设备: {self.device if self.device else 'CPU only'}")
    
    def prepare_training_data(self, stock_limit: int = 500, days_back: int = 180, 
                             include_sequences: bool = True, include_text: bool = False) -> Dict[str, Any]:
        """准备训练数据"""
        logger.info(f"准备训练数据: {stock_limit}只股票, {days_back}天")
        
        # 获取股票列表
        stock_list = self.feature_engineer.base_engineer.get_stock_list_for_training(stock_limit)
        if not stock_list:
            raise ValueError("无法获取训练股票列表")
        
        # 设置日期范围
        end_date = datetime.now().strftime('%Y%m%d')
        start_date = (datetime.now() - timedelta(days=days_back)).strftime('%Y%m%d')
        
        if include_sequences or include_text:
            # 使用增强特征工程
            multimodal_data = self.feature_engineer.prepare_multimodal_features(
                stock_list, start_date, end_date
            )
            
            if not multimodal_data or multimodal_data['tabular_features'].empty:
                raise ValueError("多模态数据准备失败")
            
            return multimodal_data
        else:
            # 使用传统特征工程
            training_data, X, y = self.ml_trainer.prepare_training_data(days_back, stock_limit)
            return {
                'tabular_features': training_data,
                'X': X,
                'y': y,
                'feature_names': self.ml_trainer.feature_names
            }
    
    def train_traditional_models(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """训练传统机器学习模型"""
        logger.info("开始训练传统机器学习模型...")
        
        # 启动性能监控
        performance_monitor.start_monitoring()
        
        try:
            # 获取M2 Ultra优化配置
            ml_config = get_optimal_config('traditional_ml')
            logger.info(f"使用M2 Ultra优化配置: {ml_config}")
            
            if 'X' in data and 'y' in data:
                # 使用预处理好的数据
                X, y = data['X'], data['y']
            else:
                # 从表格特征准备数据
                tabular_features = data['tabular_features']
                target_col = 'price_direction_5d'
                
                if target_col not in tabular_features.columns:
                    target_col = 'composite_direction'
                
                if target_col not in tabular_features.columns:
                    raise ValueError("没有找到合适的目标变量")
                
                # 准备特征和目标
                feature_cols = [col for col in tabular_features.columns 
                               if not col.startswith('future_') and 
                               col not in ['price_direction_1d', 'price_direction_5d', 'price_direction_10d', 'composite_direction']]
                
                X = tabular_features[feature_cols].fillna(0).values
                y = tabular_features[target_col].fillna(0).values
                
                # 编码目标变量
                le = LabelEncoder()
                y = le.fit_transform(y)
                
                # 标准化特征
                scaler = StandardScaler()
                X = scaler.fit_transform(X)
                
                # 保存预处理器
                self.models['scaler'] = scaler
                self.models['label_encoder'] = le
                self.models['feature_names'] = feature_cols
            
            # 使用M2 Ultra优化的配置训练模型
            self._configure_ml_trainer_for_m2(ml_config)
            performance = self.ml_trainer.train_all_models(X, y)
            
            # 保存模型
            for model_name, model in self.ml_trainer.models.items():
                self.models[f'traditional_{model_name}'] = model
            
            self.performance_history['traditional'] = performance
            
            logger.info(f"传统模型训练完成: {len(performance)}个模型")
            return performance
            
        finally:
            performance_monitor.stop_monitoring()
    
    def _configure_ml_trainer_for_m2(self, ml_config: Dict[str, Any]):
        """为M2 Ultra配置ML训练器"""
        try:
            # 配置XGBoost
            if XGBOOST_AVAILABLE:
                xgb_config = self.m2_config.get_xgboost_config()
                # 这里可以更新ml_trainer的XGBoost配置
                logger.info(f"XGBoost配置: {xgb_config}")
            
            # 配置LightGBM
            if LIGHTGBM_AVAILABLE:
                lgb_config = self.m2_config.get_lightgbm_config()
                # 这里可以更新ml_trainer的LightGBM配置
                logger.info(f"LightGBM配置: {lgb_config}")
            
            # 配置sklearn并行度
            sklearn_jobs = ml_config.get('sklearn_jobs', -1)
            logger.info(f"scikit-learn并行度: {sklearn_jobs}")
            
        except Exception as e:
            logger.error(f"配置ML训练器失败: {e}")
    
    def train_deep_learning_models(self, data: Dict[str, Any], epochs: int = 50, batch_size: int = 64) -> Dict[str, Any]:
        """训练深度学习模型"""
        if not TORCH_AVAILABLE:
            logger.warning("PyTorch不可用，跳过深度学习模型训练")
            return {}
        
        logger.info("开始训练深度学习模型...")
        
        tabular_features = data['tabular_features']
        target_col = 'price_direction_5d'
        
        if target_col not in tabular_features.columns:
            target_col = 'composite_direction'
        
        if target_col not in tabular_features.columns:
            logger.error("没有找到合适的目标变量")
            return {}
        
        # 准备特征
        feature_cols = [col for col in tabular_features.columns 
                       if not col.startswith('future_') and 
                       col not in ['price_direction_1d', 'price_direction_5d', 'price_direction_10d', 'composite_direction']]
        
        X_tabular = tabular_features[feature_cols].fillna(0).values
        y = tabular_features[target_col].fillna(0).values
        
        # 编码目标变量
        le = LabelEncoder()
        y_encoded = le.fit_transform(y)
        
        # 标准化特征
        scaler = StandardScaler()
        X_tabular_scaled = scaler.fit_transform(X_tabular)
        
        # 数据分割
        X_train, X_test, y_train, y_test = train_test_split(
            X_tabular_scaled, y_encoded, test_size=0.2, random_state=42, stratify=y_encoded
        )
        
        performance = {}
        
        # 训练简单的全连接网络
        if self._train_mlp_model(X_train, X_test, y_train, y_test, epochs, batch_size):
            performance['mlp'] = self._evaluate_pytorch_model('mlp', X_test, y_test)
        
        # 如果有时序数据，训练LSTM
        if 'sequence_features' in data and data['sequence_features'].size > 0:
            if self._train_lstm_model(data['sequence_features'], y_encoded, epochs, batch_size):
                # 重新分割以匹配时序数据
                seq_len = data['sequence_features'].shape[0]
                y_seq = y_encoded[-seq_len:]  # 匹配序列长度
                X_seq_train, X_seq_test, y_seq_train, y_seq_test = train_test_split(
                    data['sequence_features'], y_seq, test_size=0.2, random_state=42
                )
                performance['lstm'] = self._evaluate_pytorch_model('lstm', X_seq_test, y_seq_test)
            
            # 训练多模态模型
            if self._train_multimodal_model(X_tabular_scaled, data['sequence_features'], y_encoded, epochs, batch_size):
                performance['multimodal'] = self._evaluate_multimodal_model(data, y_encoded)
        
        self.performance_history['deep_learning'] = performance
        
        logger.info(f"深度学习模型训练完成: {len(performance)}个模型")
        return performance
    
    def _train_mlp_model(self, X_train: np.ndarray, X_test: np.ndarray, 
                        y_train: np.ndarray, y_test: np.ndarray, 
                        epochs: int, batch_size: int) -> bool:
        """训练MLP模型"""
        try:
            input_dim = X_train.shape[1]
            num_classes = len(np.unique(y_train))
            
            model = nn.Sequential(
                nn.Linear(input_dim, 128),
                nn.ReLU(),
                nn.Dropout(0.3),
                nn.Linear(128, 64),
                nn.ReLU(),
                nn.Dropout(0.3),
                nn.Linear(64, num_classes)
            ).to(self.device)
            
            criterion = nn.CrossEntropyLoss()
            optimizer = optim.Adam(model.parameters(), lr=0.001)
            
            # 训练
            dataset = StockDataset(X_train, y_train)
            dataloader = DataLoader(dataset, batch_size=batch_size, shuffle=True)
            
            model.train()
            for epoch in range(epochs):
                total_loss = 0
                for batch in dataloader:
                    optimizer.zero_grad()
                    outputs = model(batch['tabular'].to(self.device))
                    loss = criterion(outputs, batch['target'].to(self.device))
                    loss.backward()
                    optimizer.step()
                    total_loss += loss.item()
                
                if (epoch + 1) % 10 == 0:
                    logger.info(f"MLP Epoch {epoch+1}/{epochs}, Loss: {total_loss/len(dataloader):.4f}")
            
            self.models['mlp'] = model
            return True
            
        except Exception as e:
            logger.error(f"MLP模型训练失败: {e}")
            return False
    
    def _train_lstm_model(self, sequence_data: np.ndarray, y: np.ndarray, 
                         epochs: int, batch_size: int) -> bool:
        """训练LSTM模型"""
        try:
            seq_len, feature_dim = sequence_data.shape[1], sequence_data.shape[2]
            num_classes = len(np.unique(y))
            
            # 匹配序列数据长度
            y_seq = y[-len(sequence_data):]
            
            model = LSTMModel(seq_len, feature_dim, num_classes=num_classes).to(self.device)
            criterion = nn.CrossEntropyLoss()
            optimizer = optim.Adam(model.parameters(), lr=0.001)
            
            # 数据分割
            X_train, X_test, y_train, y_test = train_test_split(
                sequence_data, y_seq, test_size=0.2, random_state=42
            )
            
            # 训练
            dataset = StockDataset(np.zeros((len(X_train), 1)), y_train, X_train)
            dataloader = DataLoader(dataset, batch_size=batch_size, shuffle=True)
            
            model.train()
            for epoch in range(epochs):
                total_loss = 0
                for batch in dataloader:
                    optimizer.zero_grad()
                    outputs = model(batch['sequence'].to(self.device))
                    loss = criterion(outputs, batch['target'].to(self.device))
                    loss.backward()
                    optimizer.step()
                    total_loss += loss.item()
                
                if (epoch + 1) % 10 == 0:
                    logger.info(f"LSTM Epoch {epoch+1}/{epochs}, Loss: {total_loss/len(dataloader):.4f}")
            
            self.models['lstm'] = model
            return True
            
        except Exception as e:
            logger.error(f"LSTM模型训练失败: {e}")
            return False
    
    def _train_multimodal_model(self, tabular_data: np.ndarray, sequence_data: np.ndarray, 
                               y: np.ndarray, epochs: int, batch_size: int) -> bool:
        """训练多模态模型"""
        try:
            # 确保数据长度匹配
            min_len = min(len(tabular_data), len(sequence_data))
            X_tabular = tabular_data[-min_len:]
            X_sequence = sequence_data[-min_len:]
            y_matched = y[-min_len:]
            
            tabular_dim = X_tabular.shape[1]
            seq_len, seq_feature_dim = X_sequence.shape[1], X_sequence.shape[2]
            num_classes = len(np.unique(y_matched))
            
            model = MultiModalModel(tabular_dim, seq_len, seq_feature_dim, num_classes=num_classes).to(self.device)
            criterion = nn.CrossEntropyLoss()
            optimizer = optim.Adam(model.parameters(), lr=0.001)
            
            # 数据分割
            indices = np.arange(len(X_tabular))
            train_idx, test_idx = train_test_split(indices, test_size=0.2, random_state=42)
            
            X_tab_train, X_seq_train, y_train = X_tabular[train_idx], X_sequence[train_idx], y_matched[train_idx]
            
            # 创建数据集
            dataset = StockDataset(X_tab_train, y_train, X_seq_train)
            dataloader = DataLoader(dataset, batch_size=batch_size, shuffle=True)
            
            model.train()
            for epoch in range(epochs):
                total_loss = 0
                for batch in dataloader:
                    optimizer.zero_grad()
                    outputs = model(batch['tabular'].to(self.device), batch['sequence'].to(self.device))
                    loss = criterion(outputs, batch['target'].to(self.device))
                    loss.backward()
                    optimizer.step()
                    total_loss += loss.item()
                
                if (epoch + 1) % 10 == 0:
                    logger.info(f"MultiModal Epoch {epoch+1}/{epochs}, Loss: {total_loss/len(dataloader):.4f}")
            
            self.models['multimodal'] = model
            return True
            
        except Exception as e:
            logger.error(f"多模态模型训练失败: {e}")
            return False
    
    def _evaluate_pytorch_model(self, model_name: str, X_test: np.ndarray, y_test: np.ndarray) -> Dict[str, float]:
        """评估PyTorch模型"""
        try:
            model = self.models[model_name]
            model.eval()
            
            with torch.no_grad():
                if model_name == 'mlp':
                    X_tensor = torch.FloatTensor(X_test).to(self.device)
                    outputs = model(X_tensor)
                elif model_name == 'lstm':
                    X_tensor = torch.FloatTensor(X_test).to(self.device)
                    outputs = model(X_tensor)
                else:
                    return {}
                
                predictions = torch.argmax(outputs, dim=1).cpu().numpy()
            
            accuracy = accuracy_score(y_test, predictions)
            precision = precision_score(y_test, predictions, average='weighted')
            recall = recall_score(y_test, predictions, average='weighted')
            f1 = f1_score(y_test, predictions, average='weighted')
            
            return {
                'accuracy': accuracy,
                'precision': precision,
                'recall': recall,
                'f1_score': f1
            }
            
        except Exception as e:
            logger.error(f"评估{model_name}模型失败: {e}")
            return {}
    
    def _evaluate_multimodal_model(self, data: Dict[str, Any], y: np.ndarray) -> Dict[str, float]:
        """评估多模态模型"""
        try:
            model = self.models['multimodal']
            model.eval()
            
            tabular_features = data['tabular_features']
            sequence_features = data['sequence_features']
            
            # 准备测试数据
            feature_cols = [col for col in tabular_features.columns 
                           if not col.startswith('future_') and 
                           col not in ['price_direction_1d', 'price_direction_5d', 'price_direction_10d', 'composite_direction']]
            
            X_tabular = tabular_features[feature_cols].fillna(0).values
            
            # 确保数据长度匹配
            min_len = min(len(X_tabular), len(sequence_features))
            X_tabular = X_tabular[-min_len:]
            X_sequence = sequence_features[-min_len:]
            y_matched = y[-min_len:]
            
            # 标准化
            scaler = StandardScaler()
            X_tabular_scaled = scaler.fit_transform(X_tabular)
            
            # 数据分割
            indices = np.arange(len(X_tabular_scaled))
            _, test_idx = train_test_split(indices, test_size=0.2, random_state=42)
            
            X_tab_test = X_tabular_scaled[test_idx]
            X_seq_test = X_sequence[test_idx]
            y_test = y_matched[test_idx]
            
            with torch.no_grad():
                X_tab_tensor = torch.FloatTensor(X_tab_test).to(self.device)
                X_seq_tensor = torch.FloatTensor(X_seq_test).to(self.device)
                outputs = model(X_tab_tensor, X_seq_tensor)
                predictions = torch.argmax(outputs, dim=1).cpu().numpy()
            
            accuracy = accuracy_score(y_test, predictions)
            precision = precision_score(y_test, predictions, average='weighted')
            recall = recall_score(y_test, predictions, average='weighted')
            f1 = f1_score(y_test, predictions, average='weighted')
            
            return {
                'accuracy': accuracy,
                'precision': precision,
                'recall': recall,
                'f1_score': f1
            }
            
        except Exception as e:
            logger.error(f"评估多模态模型失败: {e}")
            return {}
    
    def train_all_models(self, stock_limit: int = 500, days_back: int = 180, 
                        train_traditional: bool = True, train_deep: bool = True,
                        epochs: int = 50) -> Dict[str, Any]:
        """训练所有模型"""
        logger.info("🚀 开始训练所有模型...")
        
        # 准备数据
        data = self.prepare_training_data(stock_limit, days_back, 
                                         include_sequences=train_deep, 
                                         include_text=False)
        
        all_performance = {}
        
        # 训练传统模型
        if train_traditional:
            try:
                traditional_perf = self.train_traditional_models(data)
                all_performance.update(traditional_perf)
            except Exception as e:
                logger.error(f"传统模型训练失败: {e}")
        
        # 训练深度学习模型
        if train_deep and TORCH_AVAILABLE:
            try:
                deep_perf = self.train_deep_learning_models(data, epochs)
                all_performance.update(deep_perf)
            except Exception as e:
                logger.error(f"深度学习模型训练失败: {e}")
        
        # 保存所有模型
        self.save_all_models()
        
        # 找到最佳模型
        if all_performance:
            best_model = max(all_performance.keys(), key=lambda x: all_performance[x].get('accuracy', 0))
            logger.info(f"🏆 最佳模型: {best_model}, 准确率: {all_performance[best_model].get('accuracy', 0):.4f}")
        
        return all_performance
    
    def save_all_models(self) -> bool:
        """保存所有模型"""
        try:
            # 保存传统模型
            for name, model in self.models.items():
                if name.startswith('traditional_'):
                    model_path = os.path.join(self.model_save_path, f"{name}.joblib")
                    joblib.dump(model, model_path)
                elif name in ['scaler', 'label_encoder', 'feature_names']:
                    model_path = os.path.join(self.model_save_path, f"{name}.joblib")
                    joblib.dump(model, model_path)
                elif TORCH_AVAILABLE and isinstance(model, nn.Module):
                    model_path = os.path.join(self.model_save_path, f"{name}.pth")
                    torch.save(model.state_dict(), model_path)
            
            # 保存性能历史
            performance_path = os.path.join(self.model_save_path, "performance_history.json")
            with open(performance_path, 'w', encoding='utf-8') as f:
                json.dump(self.performance_history, f, ensure_ascii=False, indent=2)
            
            logger.info(f"✅ 所有模型已保存到: {self.model_save_path}")
            return True
            
        except Exception as e:
            logger.error(f"保存模型失败: {e}")
            return False
    
    def predict_stock(self, ts_code: str, model_name: str = None) -> Dict[str, Any]:
        """预测单只股票"""
        try:
            if not self.models:
                raise ValueError("没有训练好的模型")
            
            # 选择最佳模型
            if model_name is None:
                if self.performance_history:
                    all_models = {}
                    for category, models in self.performance_history.items():
                        all_models.update(models)
                    model_name = max(all_models.keys(), key=lambda x: all_models[x].get('accuracy', 0))
                else:
                    model_name = list(self.models.keys())[0]
            
            # 获取股票数据并进行预测
            end_date = datetime.now().strftime('%Y%m%d')
            start_date = (datetime.now() - timedelta(days=90)).strftime('%Y%m%d')
            
            df = self.feature_engineer.base_engineer.fetch_raw_stock_data(ts_code, start_date, end_date)
            if df.empty:
                raise ValueError(f"无法获取股票{ts_code}的数据")
            
            # 特征工程
            df = self.feature_engineer.base_engineer.calculate_technical_features(df)
            df = self.feature_engineer.base_engineer.calculate_fundamental_features(df)
            df = self.feature_engineer.extract_statistical_features(df)
            df = self.feature_engineer.create_momentum_features(df)
            
            # 根据模型类型进行预测
            if model_name.startswith('traditional_'):
                return self._predict_traditional(df, model_name, ts_code)
            elif model_name in ['mlp', 'lstm', 'multimodal'] and TORCH_AVAILABLE:
                return self._predict_deep_learning(df, model_name, ts_code)
            else:
                raise ValueError(f"不支持的模型类型: {model_name}")
            
        except Exception as e:
            logger.error(f"预测失败: {e}")
            return {'error': str(e)}
    
    def _predict_traditional(self, df: pd.DataFrame, model_name: str, ts_code: str) -> Dict[str, Any]:
        """传统模型预测"""
        feature_names = self.models.get('feature_names', [])
        if not feature_names:
            raise ValueError("缺少特征名称")
        
        # 准备特征
        latest_data = df.iloc[-1:][feature_names].fillna(0)
        
        # 标准化
        if 'scaler' in self.models:
            X_scaled = self.models['scaler'].transform(latest_data)
        else:
            X_scaled = latest_data.values
        
        # 预测
        model = self.models[model_name]
        prediction = model.predict(X_scaled)[0]
        probability = model.predict_proba(X_scaled)[0] if hasattr(model, 'predict_proba') else None
        
        # 解码
        if 'label_encoder' in self.models:
            prediction_label = self.models['label_encoder'].inverse_transform([prediction])[0]
        else:
            prediction_label = prediction
        
        return {
            'ts_code': ts_code,
            'model_name': model_name,
            'prediction': int(prediction_label),
            'prediction_text': self._prediction_to_text(prediction_label),
            'probability': probability.tolist() if probability is not None else None,
            'confidence': float(np.max(probability)) if probability is not None else 0.5,
            'timestamp': datetime.now().isoformat()
        }
    
    def _predict_deep_learning(self, df: pd.DataFrame, model_name: str, ts_code: str) -> Dict[str, Any]:
        """深度学习模型预测"""
        model = self.models[model_name]
        model.eval()
        
        with torch.no_grad():
            if model_name == 'mlp':
                # 准备表格特征
                feature_cols = [col for col in df.columns if col in self.models.get('feature_names', [])]
                X = df.iloc[-1:][feature_cols].fillna(0).values
                
                # 标准化
                if 'scaler' in self.models:
                    X = self.models['scaler'].transform(X)
                
                X_tensor = torch.FloatTensor(X).to(self.device)
                outputs = model(X_tensor)
                
            elif model_name == 'lstm':
                # 准备序列特征
                sequence_data = self.feature_engineer.create_sequence_features(df)
                if sequence_data.size == 0:
                    raise ValueError("无法创建序列特征")
                
                X_tensor = torch.FloatTensor(sequence_data[-1:]).to(self.device)
                outputs = model(X_tensor)
                
            else:
                raise ValueError(f"不支持的深度学习模型: {model_name}")
            
            probabilities = torch.softmax(outputs, dim=1)
            prediction = torch.argmax(outputs, dim=1).cpu().item()
            confidence = torch.max(probabilities).cpu().item()
        
        # 解码预测结果
        if 'label_encoder' in self.models:
            prediction_label = self.models['label_encoder'].inverse_transform([prediction])[0]
        else:
            prediction_label = prediction - 1  # 假设 0,1,2 对应 -1,0,1
        
        return {
            'ts_code': ts_code,
            'model_name': model_name,
            'prediction': int(prediction_label),
            'prediction_text': self._prediction_to_text(prediction_label),
            'probability': probabilities.cpu().numpy().tolist()[0],
            'confidence': float(confidence),
            'timestamp': datetime.now().isoformat()
        }
    
    def _prediction_to_text(self, prediction: int) -> str:
        """预测结果转文本"""
        mapping = {-1: "卖出", 0: "持有", 1: "买入"}
        return mapping.get(prediction, "未知")

# 全局实例
unified_trainer = UnifiedModelTrainer()

def main():
    """主函数 - 训练和测试所有模型"""
    try:
        logger.info("🚀 开始综合模型训练系统测试...")
        
        # 训练所有模型
        performance = unified_trainer.train_all_models(
            stock_limit=50,  # 测试用少量股票
            days_back=120,
            train_traditional=True,
            train_deep=TORCH_AVAILABLE,
            epochs=20  # 测试用少量epoch
        )
        
        # 显示结果
        print("\n📊 综合模型性能对比:")
        print("=" * 80)
        for model_name, perf in performance.items():
            if isinstance(perf, dict) and 'accuracy' in perf:
                print(f"{model_name:20} | 准确率: {perf['accuracy']:.4f} | F1: {perf.get('f1_score', 0):.4f}")
        
        # 测试预测
        test_stock = "000001.SZ"
        try:
            prediction = unified_trainer.predict_stock(test_stock)
            print(f"\n🎯 测试预测 {test_stock}:")
            print(f"模型: {prediction.get('model_name', 'N/A')}")
            print(f"预测: {prediction.get('prediction_text', 'N/A')}")
            print(f"置信度: {prediction.get('confidence', 0):.4f}")
        except Exception as e:
            print(f"测试预测失败: {e}")
        
        print("\n✅ 综合模型训练系统测试完成!")
        
    except Exception as e:
        logger.error(f"主程序运行失败: {e}")

if __name__ == "__main__":
    main()