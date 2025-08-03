"""
深度学习训练框架
支持多种深度学习模型的训练，包括MLP、LSTM、CNN、Transformer等
"""
import os
import sys
import logging
import json
import pickle
from pathlib import Path
from typing import Dict, List, Tuple, Optional, Any, Union
from datetime import datetime, timedelta
import numpy as np
import pandas as pd
import warnings
warnings.filterwarnings('ignore')

# 添加项目路径
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

# 深度学习依赖
try:
    import torch
    import torch.nn as nn
    import torch.optim as optim
    from torch.utils.data import Dataset, DataLoader, TensorDataset
    from torch.optim.lr_scheduler import StepLR, CosineAnnealingLR, ReduceLROnPlateau
    import torch.nn.functional as F
    TORCH_AVAILABLE = True
except ImportError:
    TORCH_AVAILABLE = False
    logging.warning("PyTorch未安装，深度学习功能不可用")

# 机器学习依赖
try:
    from sklearn.model_selection import train_test_split, StratifiedKFold, TimeSeriesSplit
    from sklearn.preprocessing import StandardScaler, LabelEncoder, MinMaxScaler
    from sklearn.metrics import accuracy_score, precision_score, recall_score, f1_score, roc_auc_score
    from sklearn.ensemble import RandomForestClassifier, GradientBoostingClassifier
    from sklearn.linear_model import LogisticRegression
    from sklearn.svm import SVC
    SKLEARN_AVAILABLE = True
except ImportError:
    SKLEARN_AVAILABLE = False
    logging.warning("scikit-learn未安装，传统机器学习功能不可用")

# 优化依赖
try:
    import optuna
    OPTUNA_AVAILABLE = True
except ImportError:
    OPTUNA_AVAILABLE = False
    logging.warning("Optuna未安装，超参数优化功能不可用")

from database.db_manager import DatabaseManager
from llm.enhanced_feature_engineering import enhanced_feature_engineer

# 日志配置
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# PyTorch相关类（仅在PyTorch可用时定义）
if TORCH_AVAILABLE:
    class StockDataset(Dataset):
        """股票数据集类"""
        
        def __init__(self, features: np.ndarray, targets: np.ndarray, sequence_features: np.ndarray = None):
            self.features = torch.FloatTensor(features) if isinstance(features, np.ndarray) else features
            self.targets = torch.LongTensor(targets) if isinstance(targets, np.ndarray) else targets
            self.sequence_features = torch.FloatTensor(sequence_features) if sequence_features is not None else None
            
        def __len__(self):
            return len(self.features)
        
        def __getitem__(self, idx):
            if self.sequence_features is not None:
                return self.features[idx], self.sequence_features[idx], self.targets[idx]
            else:
                return self.features[idx], self.targets[idx]

    class MLPModel(nn.Module):
        """多层感知机模型"""
        
        def __init__(self, input_size: int, hidden_sizes: List[int], num_classes: int, dropout_rate: float = 0.3):
            super(MLPModel, self).__init__()
            
            layers = []
            prev_size = input_size
            
            for hidden_size in hidden_sizes:
                layers.extend([
                    nn.Linear(prev_size, hidden_size),
                    nn.BatchNorm1d(hidden_size),
                    nn.ReLU(),
                    nn.Dropout(dropout_rate)
                ])
                prev_size = hidden_size
            
            layers.append(nn.Linear(prev_size, num_classes))
            self.network = nn.Sequential(*layers)
            
        def forward(self, x):
            return self.network(x)

    class LSTMModel(nn.Module):
        """LSTM模型"""
        
        def __init__(self, input_size: int, hidden_size: int, num_layers: int, num_classes: int, 
                     dropout_rate: float = 0.3, bidirectional: bool = True):
            super(LSTMModel, self).__init__()
            
            self.hidden_size = hidden_size
            self.num_layers = num_layers
            self.bidirectional = bidirectional
            
            self.lstm = nn.LSTM(
                input_size, hidden_size, num_layers,
                batch_first=True, dropout=dropout_rate if num_layers > 1 else 0,
                bidirectional=bidirectional
            )
            
            lstm_output_size = hidden_size * 2 if bidirectional else hidden_size
            self.dropout = nn.Dropout(dropout_rate)
            self.fc = nn.Linear(lstm_output_size, num_classes)
            
        def forward(self, x):
            # x shape: (batch_size, sequence_length, input_size)
            lstm_out, (h_n, c_n) = self.lstm(x)
            
            # 使用最后一个时间步的输出
            if self.bidirectional:
                # 拼接前向和后向的最后一个隐藏状态
                output = torch.cat((h_n[-2,:,:], h_n[-1,:,:]), dim=1)
            else:
                output = h_n[-1,:,:]
            
            output = self.dropout(output)
            output = self.fc(output)
            
            return output

    class CNNModel(nn.Module):
        """一维卷积神经网络模型"""
        
        def __init__(self, input_size: int, sequence_length: int, num_classes: int, 
                     num_filters: int = 64, kernel_sizes: List[int] = [3, 5, 7], dropout_rate: float = 0.3):
            super(CNNModel, self).__init__()
            
            self.convs = nn.ModuleList([
                nn.Conv1d(input_size, num_filters, kernel_size=k)
                for k in kernel_sizes
            ])
            
            # 计算池化后的特征维度
            conv_output_size = len(kernel_sizes) * num_filters
            
            self.dropout = nn.Dropout(dropout_rate)
            self.fc1 = nn.Linear(conv_output_size, 128)
            self.fc2 = nn.Linear(128, num_classes)
            
        def forward(self, x):
            # x shape: (batch_size, sequence_length, input_size)
            # 转换为 (batch_size, input_size, sequence_length) for Conv1d
            x = x.transpose(1, 2)
            
            conv_outputs = []
            for conv in self.convs:
                conv_out = F.relu(conv(x))
                # 全局最大池化
                pooled = F.max_pool1d(conv_out, conv_out.size(2))
                conv_outputs.append(pooled.squeeze(2))
            
            # 拼接所有卷积输出
            x = torch.cat(conv_outputs, dim=1)
            x = self.dropout(x)
            x = F.relu(self.fc1(x))
            x = self.dropout(x)
            x = self.fc2(x)
            
            return x

    class TransformerModel(nn.Module):
        """Transformer模型"""
        
        def __init__(self, input_size: int, d_model: int, nhead: int, num_layers: int, 
                     num_classes: int, dropout_rate: float = 0.1, max_seq_length: int = 100):
            super(TransformerModel, self).__init__()
            
            self.d_model = d_model
            self.input_projection = nn.Linear(input_size, d_model)
            
            # 位置编码
            self.pos_encoding = self._create_position_encoding(max_seq_length, d_model)
            
            # Transformer编码器
            encoder_layer = nn.TransformerEncoderLayer(
                d_model=d_model, nhead=nhead, dropout=dropout_rate, batch_first=True
            )
            self.transformer = nn.TransformerEncoder(encoder_layer, num_layers=num_layers)
            
            self.dropout = nn.Dropout(dropout_rate)
            self.classifier = nn.Linear(d_model, num_classes)
            
        def _create_position_encoding(self, max_len: int, d_model: int):
            """创建位置编码"""
            pe = torch.zeros(max_len, d_model)
            position = torch.arange(0, max_len, dtype=torch.float).unsqueeze(1)
            
            div_term = torch.exp(torch.arange(0, d_model, 2).float() * 
                               (-np.log(10000.0) / d_model))
            
            pe[:, 0::2] = torch.sin(position * div_term)
            pe[:, 1::2] = torch.cos(position * div_term)
            
            return pe.unsqueeze(0)  # (1, max_len, d_model)
            
        def forward(self, x):
            # x shape: (batch_size, sequence_length, input_size)
            batch_size, seq_len, _ = x.shape
            
            # 投影到模型维度
            x = self.input_projection(x)
            
            # 添加位置编码
            if seq_len <= self.pos_encoding.size(1):
                pos_enc = self.pos_encoding[:, :seq_len, :].to(x.device)
                x = x + pos_enc
            
            # Transformer编码
            x = self.transformer(x)
            
            # 使用CLS token（第一个位置）或平均池化
            x = x.mean(dim=1)  # 平均池化
            
            x = self.dropout(x)
            x = self.classifier(x)
            
            return x

    class MultiModalModel(nn.Module):
        """多模态融合模型"""
        
        def __init__(self, tabular_input_size: int, sequence_input_size: int, sequence_length: int,
                     hidden_size: int, num_classes: int, dropout_rate: float = 0.3):
            super(MultiModalModel, self).__init__()
            
            # 表格数据分支
            self.tabular_branch = nn.Sequential(
                nn.Linear(tabular_input_size, hidden_size),
                nn.BatchNorm1d(hidden_size),
                nn.ReLU(),
                nn.Dropout(dropout_rate),
                nn.Linear(hidden_size, hidden_size // 2)
            )
            
            # 序列数据分支 (LSTM)
            self.sequence_branch = nn.LSTM(
                sequence_input_size, hidden_size // 2, 2,
                batch_first=True, dropout=dropout_rate, bidirectional=True
            )
            
            # 融合层
            fusion_input_size = hidden_size // 2 + hidden_size  # tabular + sequence
            self.fusion = nn.Sequential(
                nn.Linear(fusion_input_size, hidden_size),
                nn.BatchNorm1d(hidden_size),
                nn.ReLU(),
                nn.Dropout(dropout_rate),
                nn.Linear(hidden_size, num_classes)
            )
            
        def forward(self, tabular_features, sequence_features):
            # 表格特征处理
            tabular_out = self.tabular_branch(tabular_features)
            
            # 序列特征处理
            seq_out, (h_n, c_n) = self.sequence_branch(sequence_features)
            # 使用双向LSTM的最后隐藏状态
            seq_out = torch.cat((h_n[-2,:,:], h_n[-1,:,:]), dim=1)
            
            # 特征融合
            fused = torch.cat([tabular_out, seq_out], dim=1)
            output = self.fusion(fused)
            
            return output
else:
    # 当PyTorch不可用时的占位类
    class StockDataset:
        def __init__(self, *args, **kwargs):
            raise RuntimeError("PyTorch不可用，无法使用StockDataset")
    
    class MLPModel:
        def __init__(self, *args, **kwargs):
            raise RuntimeError("PyTorch不可用，无法使用MLPModel")
    
    class LSTMModel:
        def __init__(self, *args, **kwargs):
            raise RuntimeError("PyTorch不可用，无法使用LSTMModel")
    
    class CNNModel:
        def __init__(self, *args, **kwargs):
            raise RuntimeError("PyTorch不可用，无法使用CNNModel")
    
    class TransformerModel:
        def __init__(self, *args, **kwargs):
            raise RuntimeError("PyTorch不可用，无法使用TransformerModel")
    
    class MultiModalModel:
        def __init__(self, *args, **kwargs):
            raise RuntimeError("PyTorch不可用，无法使用MultiModalModel")

class DeepLearningTrainer:
    """深度学习训练器"""
    
    def __init__(self):
        self.db_manager = DatabaseManager()
        self.models = {}
        self.scalers = {}
        self.encoders = {}
        
        # 只有在PyTorch可用时才设置设备
        if TORCH_AVAILABLE:
            self.device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
        else:
            self.device = None
            
        self.model_save_path = Path('models/deep_learning')
        self.model_save_path.mkdir(parents=True, exist_ok=True)
        
        if TORCH_AVAILABLE:
            logger.info(f"深度学习训练器初始化完成，使用设备: {self.device}")
        else:
            logger.info("深度学习训练器初始化完成，PyTorch不可用")
    
    def prepare_data(self, stock_list: List[str], start_date: str, end_date: str, 
                    target_column: str = 'price_direction_5d') -> Dict[str, Any]:
        """准备训练数据"""
        logger.info(f"准备训练数据: {len(stock_list)}只股票")
        
        try:
            # 使用增强特征工程准备多模态数据
            multimodal_data = enhanced_feature_engineer.prepare_multimodal_features(
                stock_list, start_date, end_date
            )
            
            if not multimodal_data or multimodal_data.get('tabular_features', pd.DataFrame()).empty:
                logger.error("没有获取到有效的特征数据")
                return {}
            
            tabular_features = multimodal_data['tabular_features']
            sequence_features = multimodal_data.get('sequence_features', np.array([]))
            
            # 准备目标变量
            if target_column not in tabular_features.columns:
                logger.error(f"目标列 {target_column} 不存在")
                return {}
            
            # 过滤有效样本
            valid_mask = tabular_features[target_column].notna()
            tabular_features = tabular_features[valid_mask]
            
            if sequence_features.size > 0:
                sequence_features = sequence_features[valid_mask.values]
            
            if len(tabular_features) == 0:
                logger.error("没有有效的训练样本")
                return {}
            
            # 分离特征和目标
            target = tabular_features[target_column].values
            feature_columns = [col for col in tabular_features.columns 
                             if not col.startswith('future_') and 
                             col not in ['price_direction_1d', 'price_direction_5d', 'price_direction_10d']]
            
            features = tabular_features[feature_columns].fillna(0).values
            
            # 标签编码
            if target_column.startswith('price_direction'):
                # 分类任务：-1, 0, 1 -> 0, 1, 2
                unique_labels = np.unique(target)
                label_encoder = LabelEncoder()
                target_encoded = label_encoder.fit_transform(target)
                num_classes = len(unique_labels)
                task_type = 'classification'
            else:
                # 回归任务
                target_encoded = target.astype(np.float32)
                num_classes = 1
                task_type = 'regression'
                label_encoder = None
            
            # 特征标准化
            scaler = StandardScaler()
            features_scaled = scaler.fit_transform(features)
            
            # 保存预处理器
            self.scalers['feature_scaler'] = scaler
            self.encoders['label_encoder'] = label_encoder
            
            # 序列特征标准化
            sequence_scaler = None
            if sequence_features.size > 0:
                # 对序列特征进行标准化
                batch_size, seq_len, feature_dim = sequence_features.shape
                sequence_features_reshaped = sequence_features.reshape(-1, feature_dim)
                sequence_scaler = StandardScaler()
                sequence_features_scaled = sequence_scaler.fit_transform(sequence_features_reshaped)
                sequence_features = sequence_features_scaled.reshape(batch_size, seq_len, feature_dim)
                self.scalers['sequence_scaler'] = sequence_scaler
            
            # 数据分割
            if sequence_features.size > 0:
                X_train, X_test, X_seq_train, X_seq_test, y_train, y_test = train_test_split(
                    features_scaled, sequence_features, target_encoded, 
                    test_size=0.2, random_state=42, stratify=target_encoded if task_type == 'classification' else None
                )
            else:
                X_train, X_test, y_train, y_test = train_test_split(
                    features_scaled, target_encoded, 
                    test_size=0.2, random_state=42, stratify=target_encoded if task_type == 'classification' else None
                )
                X_seq_train = X_seq_test = None
            
            result = {
                'X_train': X_train,
                'X_test': X_test,
                'X_seq_train': X_seq_train,
                'X_seq_test': X_seq_test,
                'y_train': y_train,
                'y_test': y_test,
                'feature_columns': feature_columns,
                'num_classes': num_classes,
                'task_type': task_type,
                'feature_dim': len(feature_columns),
                'sequence_dim': sequence_features.shape[-1] if sequence_features.size > 0 else 0,
                'sequence_length': sequence_features.shape[1] if sequence_features.size > 0 else 0
            }
            
            logger.info(f"数据准备完成: 训练集{len(X_train)}样本, 测试集{len(X_test)}样本, "
                       f"特征维度{result['feature_dim']}, 类别数{num_classes}")
            
            return result
            
        except Exception as e:
            logger.error(f"数据准备失败: {e}")
            return {}
    
    def train_mlp(self, data: Dict[str, Any], config: Dict[str, Any] = None) -> Dict[str, Any]:
        """训练MLP模型"""
        if not TORCH_AVAILABLE:
            return {'error': 'PyTorch不可用'}
        
        logger.info("开始训练MLP模型")
        
        # 默认配置
        default_config = {
            'hidden_sizes': [256, 128, 64],
            'dropout_rate': 0.3,
            'learning_rate': 0.001,
            'batch_size': 128,
            'epochs': 100,
            'patience': 10
        }
        config = {**default_config, **(config or {})}
        
        try:
            # 创建数据加载器
            train_dataset = StockDataset(data['X_train'], data['y_train'])
            test_dataset = StockDataset(data['X_test'], data['y_test'])
            
            train_loader = DataLoader(train_dataset, batch_size=config['batch_size'], shuffle=True)
            test_loader = DataLoader(test_dataset, batch_size=config['batch_size'], shuffle=False)
            
            # 创建模型
            model = MLPModel(
                input_size=data['feature_dim'],
                hidden_sizes=config['hidden_sizes'],
                num_classes=data['num_classes'],
                dropout_rate=config['dropout_rate']
            ).to(self.device)
            
            # 优化器和损失函数
            optimizer = optim.Adam(model.parameters(), lr=config['learning_rate'])
            if data['task_type'] == 'classification':
                criterion = nn.CrossEntropyLoss()
            else:
                criterion = nn.MSELoss()
            
            scheduler = ReduceLROnPlateau(optimizer, 'min', patience=5, factor=0.5)
            
            # 训练
            best_loss = float('inf')
            patience_counter = 0
            train_losses = []
            val_losses = []
            
            for epoch in range(config['epochs']):
                # 训练阶段
                model.train()
                train_loss = 0
                for batch_features, batch_targets in train_loader:
                    batch_features = batch_features.to(self.device)
                    batch_targets = batch_targets.to(self.device)
                    
                    optimizer.zero_grad()
                    outputs = model(batch_features)
                    loss = criterion(outputs, batch_targets)
                    loss.backward()
                    optimizer.step()
                    
                    train_loss += loss.item()
                
                # 验证阶段
                model.eval()
                val_loss = 0
                with torch.no_grad():
                    for batch_features, batch_targets in test_loader:
                        batch_features = batch_features.to(self.device)
                        batch_targets = batch_targets.to(self.device)
                        
                        outputs = model(batch_features)
                        loss = criterion(outputs, batch_targets)
                        val_loss += loss.item()
                
                train_loss /= len(train_loader)
                val_loss /= len(test_loader)
                
                train_losses.append(train_loss)
                val_losses.append(val_loss)
                
                scheduler.step(val_loss)
                
                # 早停
                if val_loss < best_loss:
                    best_loss = val_loss
                    patience_counter = 0
                    # 保存最佳模型
                    torch.save(model.state_dict(), self.model_save_path / 'mlp_best.pth')
                else:
                    patience_counter += 1
                
                if epoch % 10 == 0:
                    logger.info(f"Epoch {epoch}: Train Loss = {train_loss:.4f}, Val Loss = {val_loss:.4f}")
                
                if patience_counter >= config['patience']:
                    logger.info(f"早停于第{epoch}轮")
                    break
            
            # 加载最佳模型
            model.load_state_dict(torch.load(self.model_save_path / 'mlp_best.pth'))
            
            # 评估模型
            performance = self._evaluate_model(model, test_loader, data['task_type'])
            
            # 保存模型
            self.models['mlp'] = model
            
            result = {
                'model': model,
                'performance': performance,
                'train_losses': train_losses,
                'val_losses': val_losses,
                'best_epoch': len(train_losses) - patience_counter,
                'config': config
            }
            
            logger.info(f"MLP训练完成，性能: {performance}")
            return result
            
        except Exception as e:
            logger.error(f"MLP训练失败: {e}")
            return {'error': str(e)}
    
    def train_lstm(self, data: Dict[str, Any], config: Dict[str, Any] = None) -> Dict[str, Any]:
        """训练LSTM模型"""
        if not TORCH_AVAILABLE:
            return {'error': 'PyTorch不可用'}
        
        if data.get('X_seq_train') is None:
            return {'error': '没有序列数据'}
        
        logger.info("开始训练LSTM模型")
        
        # 默认配置
        default_config = {
            'hidden_size': 128,
            'num_layers': 2,
            'dropout_rate': 0.3,
            'bidirectional': True,
            'learning_rate': 0.001,
            'batch_size': 64,
            'epochs': 100,
            'patience': 10
        }
        config = {**default_config, **(config or {})}
        
        try:
            # 创建数据加载器
            train_dataset = StockDataset(data['X_seq_train'], data['y_train'])
            test_dataset = StockDataset(data['X_seq_test'], data['y_test'])
            
            train_loader = DataLoader(train_dataset, batch_size=config['batch_size'], shuffle=True)
            test_loader = DataLoader(test_dataset, batch_size=config['batch_size'], shuffle=False)
            
            # 创建模型
            model = LSTMModel(
                input_size=data['sequence_dim'],
                hidden_size=config['hidden_size'],
                num_layers=config['num_layers'],
                num_classes=data['num_classes'],
                dropout_rate=config['dropout_rate'],
                bidirectional=config['bidirectional']
            ).to(self.device)
            
            # 优化器和损失函数
            optimizer = optim.Adam(model.parameters(), lr=config['learning_rate'])
            if data['task_type'] == 'classification':
                criterion = nn.CrossEntropyLoss()
            else:
                criterion = nn.MSELoss()
            
            scheduler = ReduceLROnPlateau(optimizer, 'min', patience=5, factor=0.5)
            
            # 训练过程
            best_loss = float('inf')
            patience_counter = 0
            train_losses = []
            val_losses = []
            
            for epoch in range(config['epochs']):
                # 训练阶段
                model.train()
                train_loss = 0
                for batch_sequences, batch_targets in train_loader:
                    batch_sequences = batch_sequences.to(self.device)
                    batch_targets = batch_targets.to(self.device)
                    
                    optimizer.zero_grad()
                    outputs = model(batch_sequences)
                    loss = criterion(outputs, batch_targets)
                    loss.backward()
                    
                    # 梯度裁剪
                    torch.nn.utils.clip_grad_norm_(model.parameters(), max_norm=1.0)
                    
                    optimizer.step()
                    train_loss += loss.item()
                
                # 验证阶段
                model.eval()
                val_loss = 0
                with torch.no_grad():
                    for batch_sequences, batch_targets in test_loader:
                        batch_sequences = batch_sequences.to(self.device)
                        batch_targets = batch_targets.to(self.device)
                        
                        outputs = model(batch_sequences)
                        loss = criterion(outputs, batch_targets)
                        val_loss += loss.item()
                
                train_loss /= len(train_loader)
                val_loss /= len(test_loader)
                
                train_losses.append(train_loss)
                val_losses.append(val_loss)
                
                scheduler.step(val_loss)
                
                # 早停
                if val_loss < best_loss:
                    best_loss = val_loss
                    patience_counter = 0
                    torch.save(model.state_dict(), self.model_save_path / 'lstm_best.pth')
                else:
                    patience_counter += 1
                
                if epoch % 10 == 0:
                    logger.info(f"Epoch {epoch}: Train Loss = {train_loss:.4f}, Val Loss = {val_loss:.4f}")
                
                if patience_counter >= config['patience']:
                    logger.info(f"早停于第{epoch}轮")
                    break
            
            # 加载最佳模型
            model.load_state_dict(torch.load(self.model_save_path / 'lstm_best.pth'))
            
            # 评估模型
            performance = self._evaluate_model(model, test_loader, data['task_type'])
            
            # 保存模型
            self.models['lstm'] = model
            
            result = {
                'model': model,
                'performance': performance,
                'train_losses': train_losses,
                'val_losses': val_losses,
                'best_epoch': len(train_losses) - patience_counter,
                'config': config
            }
            
            logger.info(f"LSTM训练完成，性能: {performance}")
            return result
            
        except Exception as e:
            logger.error(f"LSTM训练失败: {e}")
            return {'error': str(e)}
    
    def train_multimodal(self, data: Dict[str, Any], config: Dict[str, Any] = None) -> Dict[str, Any]:
        """训练多模态融合模型"""
        if not TORCH_AVAILABLE:
            return {'error': 'PyTorch不可用'}
        
        if data.get('X_seq_train') is None:
            return {'error': '没有序列数据用于多模态训练'}
        
        logger.info("开始训练多模态融合模型")
        
        # 默认配置
        default_config = {
            'hidden_size': 256,
            'dropout_rate': 0.3,
            'learning_rate': 0.001,
            'batch_size': 64,
            'epochs': 100,
            'patience': 10
        }
        config = {**default_config, **(config or {})}
        
        try:
            # 创建数据加载器
            train_dataset = StockDataset(data['X_train'], data['y_train'], data['X_seq_train'])
            test_dataset = StockDataset(data['X_test'], data['y_test'], data['X_seq_test'])
            
            train_loader = DataLoader(train_dataset, batch_size=config['batch_size'], shuffle=True)
            test_loader = DataLoader(test_dataset, batch_size=config['batch_size'], shuffle=False)
            
            # 创建多模态模型
            model = MultiModalModel(
                tabular_input_size=data['feature_dim'],
                sequence_input_size=data['sequence_dim'],
                sequence_length=data['sequence_length'],
                hidden_size=config['hidden_size'],
                num_classes=data['num_classes'],
                dropout_rate=config['dropout_rate']
            ).to(self.device)
            
            # 优化器和损失函数
            optimizer = optim.Adam(model.parameters(), lr=config['learning_rate'])
            if data['task_type'] == 'classification':
                criterion = nn.CrossEntropyLoss()
            else:
                criterion = nn.MSELoss()
            
            scheduler = ReduceLROnPlateau(optimizer, 'min', patience=5, factor=0.5)
            
            # 训练过程
            best_loss = float('inf')
            patience_counter = 0
            train_losses = []
            val_losses = []
            
            for epoch in range(config['epochs']):
                # 训练阶段
                model.train()
                train_loss = 0
                for batch_tabular, batch_sequence, batch_targets in train_loader:
                    batch_tabular = batch_tabular.to(self.device)
                    batch_sequence = batch_sequence.to(self.device)
                    batch_targets = batch_targets.to(self.device)
                    
                    optimizer.zero_grad()
                    outputs = model(batch_tabular, batch_sequence)
                    loss = criterion(outputs, batch_targets)
                    loss.backward()
                    
                    # 梯度裁剪
                    torch.nn.utils.clip_grad_norm_(model.parameters(), max_norm=1.0)
                    
                    optimizer.step()
                    train_loss += loss.item()
                
                # 验证阶段
                model.eval()
                val_loss = 0
                with torch.no_grad():
                    for batch_tabular, batch_sequence, batch_targets in test_loader:
                        batch_tabular = batch_tabular.to(self.device)
                        batch_sequence = batch_sequence.to(self.device)
                        batch_targets = batch_targets.to(self.device)
                        
                        outputs = model(batch_tabular, batch_sequence)
                        loss = criterion(outputs, batch_targets)
                        val_loss += loss.item()
                
                train_loss /= len(train_loader)
                val_loss /= len(test_loader)
                
                train_losses.append(train_loss)
                val_losses.append(val_loss)
                
                scheduler.step(val_loss)
                
                # 早停
                if val_loss < best_loss:
                    best_loss = val_loss
                    patience_counter = 0
                    torch.save(model.state_dict(), self.model_save_path / 'multimodal_best.pth')
                else:
                    patience_counter += 1
                
                if epoch % 10 == 0:
                    logger.info(f"Epoch {epoch}: Train Loss = {train_loss:.4f}, Val Loss = {val_loss:.4f}")
                
                if patience_counter >= config['patience']:
                    logger.info(f"早停于第{epoch}轮")
                    break
            
            # 加载最佳模型
            model.load_state_dict(torch.load(self.model_save_path / 'multimodal_best.pth'))
            
            # 评估模型
            performance = self._evaluate_model(model, test_loader, data['task_type'], multimodal=True)
            
            # 保存模型
            self.models['multimodal'] = model
            
            result = {
                'model': model,
                'performance': performance,
                'train_losses': train_losses,
                'val_losses': val_losses,
                'best_epoch': len(train_losses) - patience_counter,
                'config': config
            }
            
            logger.info(f"多模态模型训练完成，性能: {performance}")
            return result
            
        except Exception as e:
            logger.error(f"多模态模型训练失败: {e}")
            return {'error': str(e)}
    
    def _evaluate_model(self, model, test_loader, task_type: str, multimodal: bool = False) -> Dict[str, float]:
        """评估模型性能"""
        model.eval()
        all_predictions = []
        all_targets = []
        
        with torch.no_grad():
            for batch in test_loader:
                if multimodal:
                    batch_tabular, batch_sequence, batch_targets = batch
                    batch_tabular = batch_tabular.to(self.device)
                    batch_sequence = batch_sequence.to(self.device)
                    outputs = model(batch_tabular, batch_sequence)
                else:
                    if len(batch) == 3:  # sequence data
                        batch_features, batch_sequence, batch_targets = batch
                        batch_sequence = batch_sequence.to(self.device)
                        outputs = model(batch_sequence)
                    else:  # tabular data
                        batch_features, batch_targets = batch
                        batch_features = batch_features.to(self.device)
                        outputs = model(batch_features)
                
                batch_targets = batch_targets.to(self.device)
                
                if task_type == 'classification':
                    predictions = torch.argmax(outputs, dim=1)
                    all_predictions.extend(predictions.cpu().numpy())
                    all_targets.extend(batch_targets.cpu().numpy())
                else:
                    all_predictions.extend(outputs.squeeze().cpu().numpy())
                    all_targets.extend(batch_targets.cpu().numpy())
        
        # 计算评估指标
        if task_type == 'classification':
            metrics = {
                'accuracy': accuracy_score(all_targets, all_predictions),
                'precision': precision_score(all_targets, all_predictions, average='weighted', zero_division=0),
                'recall': recall_score(all_targets, all_predictions, average='weighted', zero_division=0),
                'f1_score': f1_score(all_targets, all_predictions, average='weighted', zero_division=0)
            }
            
            # 如果是二分类或三分类，计算AUC
            try:
                if len(np.unique(all_targets)) <= 3:
                    with torch.no_grad():
                        # 重新获取概率预测
                        all_probs = []
                        for batch in test_loader:
                            if multimodal:
                                batch_tabular, batch_sequence, batch_targets = batch
                                batch_tabular = batch_tabular.to(self.device)
                                batch_sequence = batch_sequence.to(self.device)
                                outputs = model(batch_tabular, batch_sequence)
                            else:
                                if len(batch) == 3:
                                    batch_features, batch_sequence, batch_targets = batch
                                    batch_sequence = batch_sequence.to(self.device)
                                    outputs = model(batch_sequence)
                                else:
                                    batch_features, batch_targets = batch
                                    batch_features = batch_features.to(self.device)
                                    outputs = model(batch_features)
                            
                            probs = F.softmax(outputs, dim=1)
                            all_probs.extend(probs.cpu().numpy())
                        
                        if len(np.unique(all_targets)) == 2:
                            metrics['auc'] = roc_auc_score(all_targets, np.array(all_probs)[:, 1])
                        else:
                            metrics['auc'] = roc_auc_score(all_targets, all_probs, multi_class='ovr', average='weighted')
            except:
                pass
        else:
            from sklearn.metrics import mean_squared_error, mean_absolute_error, r2_score
            metrics = {
                'mse': mean_squared_error(all_targets, all_predictions),
                'mae': mean_absolute_error(all_targets, all_predictions),
                'r2': r2_score(all_targets, all_predictions)
            }
        
        return metrics
    
    def train_all_models(self, stock_list: List[str], start_date: str, end_date: str,
                        target_column: str = 'price_direction_5d') -> Dict[str, Any]:
        """训练所有深度学习模型"""
        logger.info(f"开始训练所有深度学习模型")
        
        # 准备数据
        data = self.prepare_data(stock_list, start_date, end_date, target_column)
        if not data:
            return {'error': '数据准备失败'}
        
        results = {}
        
        # 训练MLP
        logger.info("训练MLP模型...")
        mlp_result = self.train_mlp(data)
        if 'error' not in mlp_result:
            results['mlp'] = mlp_result
        
        # 训练LSTM（如果有序列数据）
        if data.get('X_seq_train') is not None:
            logger.info("训练LSTM模型...")
            lstm_result = self.train_lstm(data)
            if 'error' not in lstm_result:
                results['lstm'] = lstm_result
            
            # 训练多模态模型
            logger.info("训练多模态融合模型...")
            multimodal_result = self.train_multimodal(data)
            if 'error' not in multimodal_result:
                results['multimodal'] = multimodal_result
        
        # 保存训练信息
        training_info = {
            'data_info': {
                'stock_count': len(stock_list),
                'train_samples': len(data['X_train']),
                'test_samples': len(data['X_test']),
                'feature_dim': data['feature_dim'],
                'sequence_dim': data.get('sequence_dim', 0),
                'num_classes': data['num_classes'],
                'task_type': data['task_type']
            },
            'training_date': datetime.now().isoformat(),
            'results_summary': {
                model_name: result.get('performance', {})
                for model_name, result in results.items()
            }
        }
        
        # 保存训练信息
        with open(self.model_save_path / 'training_info.json', 'w', encoding='utf-8') as f:
            json.dump(training_info, f, ensure_ascii=False, indent=2)
        
        logger.info(f"深度学习模型训练完成，成功训练{len(results)}个模型")
        
        return {
            'results': results,
            'training_info': training_info,
            'data_info': data
        }

# 全局实例
deep_learning_trainer = DeepLearningTrainer()

def main():
    """测试深度学习训练"""
    try:
        if not TORCH_AVAILABLE:
            logger.error("PyTorch不可用，无法进行深度学习测试")
            return
        
        logger.info("开始深度学习训练测试...")
        
        # 获取测试股票
        stock_list = enhanced_feature_engineer.base_engineer.get_stock_list_for_training(20)
        
        if not stock_list:
            logger.error("无法获取测试股票")
            return
        
        # 设置日期范围
        end_date = datetime.now().strftime('%Y%m%d')
        start_date = (datetime.now() - timedelta(days=120)).strftime('%Y%m%d')
        
        # 训练所有模型
        results = deep_learning_trainer.train_all_models(
            stock_list, start_date, end_date, 'price_direction_5d'
        )
        
        if 'error' in results:
            logger.error(f"训练失败: {results['error']}")
            return
        
        print("\n深度学习模型训练结果:")
        print("=" * 60)
        
        for model_name, result in results['results'].items():
            print(f"\n{model_name.upper()} 模型:")
            print("-" * 30)
            
            performance = result.get('performance', {})
            for metric, value in performance.items():
                print(f"{metric}: {value:.4f}")
            
            config = result.get('config', {})
            print(f"最佳轮次: {result.get('best_epoch', 'N/A')}")
            print(f"训练轮次: {len(result.get('train_losses', []))}")
        
        print(f"\n训练信息:")
        data_info = results['training_info']['data_info']
        print(f"股票数量: {data_info['stock_count']}")
        print(f"训练样本: {data_info['train_samples']}")
        print(f"测试样本: {data_info['test_samples']}")
        print(f"特征维度: {data_info['feature_dim']}")
        print(f"序列维度: {data_info['sequence_dim']}")
        print(f"类别数: {data_info['num_classes']}")
        
        logger.info("深度学习训练测试完成")
        
    except Exception as e:
        logger.error(f"测试失败: {e}")

if __name__ == "__main__":
    main()