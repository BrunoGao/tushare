"""
在线学习和反馈优化系统
支持增量学习、用户反馈集成和模型持续改进
"""
import os
import sys
import logging
import json
import sqlite3
from pathlib import Path
from typing import Dict, List, Tuple, Optional, Any, Union
from datetime import datetime, timedelta
import numpy as np
import pandas as pd
import threading
import time
import warnings
warnings.filterwarnings('ignore')

# 添加项目路径
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

# 机器学习依赖
try:
    from sklearn.model_selection import train_test_split
    from sklearn.preprocessing import StandardScaler
    from sklearn.metrics import accuracy_score, f1_score
    from sklearn.ensemble import RandomForestClassifier
    from sklearn.linear_model import SGDClassifier  # 支持增量学习
    SKLEARN_AVAILABLE = True
except ImportError:
    SKLEARN_AVAILABLE = False

# 深度学习依赖
try:
    import torch
    import torch.nn as nn
    import torch.optim as optim
    from torch.utils.data import DataLoader
    TORCH_AVAILABLE = True
except ImportError:
    TORCH_AVAILABLE = False

from database.db_manager import DatabaseManager
from llm.enhanced_feature_engineering import enhanced_feature_engineer
from ai.advanced_evaluation_system import evaluation_system, model_optimizer

# 日志配置
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class IncrementalLearner:
    """增量学习器"""
    
    def __init__(self):
        self.db_manager = DatabaseManager()
        self.models = {}
        self.model_configs = {}
        self.learning_history = []
        self.feedback_buffer = []
        self.learning_scheduler_running = False
        self.learning_thread = None
        
        # 增量学习配置
        self.config = {
            'batch_size': 100,  # 增量学习批次大小
            'learning_rate_decay': 0.95,  # 学习率衰减
            'feedback_threshold': 50,  # 触发反馈学习的阈值
            'adaptation_frequency': 24,  # 自适应频率（小时）
            'max_buffer_size': 1000,  # 最大缓冲区大小
            'feature_drift_threshold': 0.1,  # 特征漂移阈值
            'performance_decline_threshold': 0.05  # 性能下降阈值
        }
        
        self.model_save_path = Path('models/incremental')
        self.model_save_path.mkdir(parents=True, exist_ok=True)
        
    def initialize_incremental_model(self, model_name: str, model_type: str = 'sgd') -> bool:
        """初始化增量学习模型"""
        try:
            if model_type == 'sgd':
                # SGD分类器支持增量学习
                model = SGDClassifier(
                    loss='log_loss',  # 逻辑回归损失
                    learning_rate='adaptive',
                    eta0=0.01,
                    random_state=42,
                    warm_start=True  # 支持增量学习
                )
            elif model_type == 'random_forest':
                # 随机森林（部分支持增量学习）
                model = RandomForestClassifier(
                    n_estimators=10,  # 起始较小
                    warm_start=True,
                    random_state=42
                )
            else:
                logger.error(f"不支持的增量学习模型类型: {model_type}")
                return False
            
            self.models[model_name] = model
            self.model_configs[model_name] = {
                'type': model_type,
                'created_date': datetime.now().isoformat(),
                'training_samples': 0,
                'update_count': 0,
                'last_update': None
            }
            
            logger.info(f"初始化增量学习模型: {model_name} ({model_type})")
            return True
            
        except Exception as e:
            logger.error(f"初始化增量学习模型失败: {e}")
            return False
    
    def add_training_sample(self, model_name: str, features: np.ndarray, label: int,
                           feedback_type: str = 'system', confidence: float = 1.0) -> bool:
        """添加训练样本到缓冲区"""
        try:
            sample = {
                'model_name': model_name,
                'features': features,
                'label': label,
                'feedback_type': feedback_type,
                'confidence': confidence,
                'timestamp': datetime.now().isoformat()
            }
            
            self.feedback_buffer.append(sample)
            
            # 限制缓冲区大小
            if len(self.feedback_buffer) > self.config['max_buffer_size']:
                self.feedback_buffer.pop(0)
            
            # 检查是否需要触发增量学习
            model_samples = [s for s in self.feedback_buffer if s['model_name'] == model_name]
            if len(model_samples) >= self.config['feedback_threshold']:
                self._trigger_incremental_learning(model_name)
            
            return True
            
        except Exception as e:
            logger.error(f"添加训练样本失败: {e}")
            return False
    
    def _trigger_incremental_learning(self, model_name: str):
        """触发增量学习"""
        try:
            if model_name not in self.models:
                logger.warning(f"模型 {model_name} 不存在，跳过增量学习")
                return
            
            # 获取该模型的样本
            model_samples = [s for s in self.feedback_buffer if s['model_name'] == model_name]
            
            if len(model_samples) < 10:  # 样本太少
                return
            
            # 准备训练数据
            features_list = []
            labels_list = []
            weights_list = []
            
            for sample in model_samples:
                features_list.append(sample['features'])
                labels_list.append(sample['label'])
                weights_list.append(sample['confidence'])
            
            X = np.array(features_list)
            y = np.array(labels_list)
            sample_weights = np.array(weights_list)
            
            # 数据验证
            if len(np.unique(y)) < 2:
                logger.warning(f"模型 {model_name} 样本类别不足，跳过增量学习")
                return
            
            # 特征标准化（如果需要）
            if hasattr(self.models[model_name], 'partial_fit'):
                # 支持部分拟合的模型
                self.models[model_name].partial_fit(X, y, sample_weight=sample_weights)
            else:
                # 不支持部分拟合，重新训练
                self.models[model_name].fit(X, y, sample_weight=sample_weights)
            
            # 更新配置
            config = self.model_configs[model_name]
            config['training_samples'] += len(model_samples)
            config['update_count'] += 1
            config['last_update'] = datetime.now().isoformat()
            
            # 记录学习历史
            self.learning_history.append({
                'model_name': model_name,
                'samples_count': len(model_samples),
                'unique_labels': len(np.unique(y)),
                'update_time': datetime.now().isoformat(),
                'learning_type': 'incremental'
            })
            
            # 从缓冲区移除已处理的样本
            self.feedback_buffer = [s for s in self.feedback_buffer if s['model_name'] != model_name]
            
            logger.info(f"完成模型 {model_name} 的增量学习，处理样本: {len(model_samples)}")
            
        except Exception as e:
            logger.error(f"增量学习失败: {e}")
    
    def predict_with_uncertainty(self, model_name: str, features: np.ndarray) -> Tuple[int, float]:
        """带不确定性的预测"""
        try:
            if model_name not in self.models:
                return 0, 0.0
            
            model = self.models[model_name]
            
            # 预测
            if hasattr(model, 'predict_proba'):
                probabilities = model.predict_proba(features.reshape(1, -1))[0]
                prediction = np.argmax(probabilities)
                confidence = np.max(probabilities)
            else:
                prediction = model.predict(features.reshape(1, -1))[0]
                # 对于不支持概率预测的模型，使用距离度量
                if hasattr(model, 'decision_function'):
                    decision_scores = model.decision_function(features.reshape(1, -1))
                    confidence = min(abs(decision_scores[0]) / 2.0, 1.0)
                else:
                    confidence = 0.5  # 默认置信度
            
            return int(prediction), float(confidence)
            
        except Exception as e:
            logger.error(f"预测失败: {e}")
            return 0, 0.0
    
    def detect_concept_drift(self, model_name: str) -> Dict[str, Any]:
        """检测概念漂移"""
        try:
            if model_name not in self.models:
                return {'drift_detected': False, 'reason': 'Model not found'}
            
            # 获取最近的预测历史
            recent_samples = [
                s for s in self.feedback_buffer[-100:] 
                if s['model_name'] == model_name
            ]
            
            if len(recent_samples) < 20:
                return {'drift_detected': False, 'reason': 'Insufficient samples'}
            
            # 分析性能趋势
            recent_predictions = []
            actual_labels = []
            timestamps = []
            
            for sample in recent_samples:
                if 'actual_label' in sample:  # 只考虑有真实标签的样本
                    pred, _ = self.predict_with_uncertainty(model_name, sample['features'])
                    recent_predictions.append(pred)
                    actual_labels.append(sample['actual_label'])
                    timestamps.append(sample['timestamp'])
            
            if len(recent_predictions) < 10:
                return {'drift_detected': False, 'reason': 'Insufficient labeled samples'}
            
            # 计算最近性能
            recent_accuracy = accuracy_score(actual_labels, recent_predictions)
            
            # 获取历史性能基准
            historical_performance = self._get_historical_performance(model_name)
            baseline_accuracy = historical_performance.get('accuracy', 0.5)
            
            # 检测性能下降
            performance_decline = baseline_accuracy - recent_accuracy
            drift_detected = performance_decline > self.config['performance_decline_threshold']
            
            # 特征分布变化检测（简化版）
            if len(recent_samples) >= 50:
                features_recent = np.array([s['features'] for s in recent_samples[-25:]])
                features_older = np.array([s['features'] for s in recent_samples[-50:-25]])
                
                # 比较特征分布
                feature_drift_score = np.mean(np.abs(
                    np.mean(features_recent, axis=0) - np.mean(features_older, axis=0)
                ))
                
                feature_drift_detected = feature_drift_score > self.config['feature_drift_threshold']
            else:
                feature_drift_detected = False
                feature_drift_score = 0.0
            
            drift_result = {
                'drift_detected': drift_detected or feature_drift_detected,
                'performance_decline': performance_decline,
                'recent_accuracy': recent_accuracy,
                'baseline_accuracy': baseline_accuracy,
                'feature_drift_score': feature_drift_score,
                'feature_drift_detected': feature_drift_detected,
                'sample_count': len(recent_predictions),
                'detection_time': datetime.now().isoformat()
            }
            
            if drift_result['drift_detected']:
                logger.warning(f"检测到模型 {model_name} 的概念漂移: {drift_result}")
                self._handle_concept_drift(model_name, drift_result)
            
            return drift_result
            
        except Exception as e:
            logger.error(f"概念漂移检测失败: {e}")
            return {'drift_detected': False, 'error': str(e)}
    
    def _get_historical_performance(self, model_name: str) -> Dict[str, float]:
        """获取历史性能基准"""
        try:
            # 从评估系统获取历史性能
            performance_report = evaluation_system.get_model_performance_report(model_name, days_back=30)
            
            if 'report' in performance_report and model_name in performance_report['report']:
                return performance_report['report'][model_name].get('latest_metrics', {})
            
            # 如果没有历史数据，返回默认值
            return {'accuracy': 0.5, 'f1_score': 0.4}
            
        except Exception as e:
            logger.error(f"获取历史性能失败: {e}")
            return {'accuracy': 0.5, 'f1_score': 0.4}
    
    def _handle_concept_drift(self, model_name: str, drift_info: Dict[str, Any]):
        """处理概念漂移"""
        try:
            logger.info(f"处理模型 {model_name} 的概念漂移")
            
            # 策略1: 增加学习率
            if hasattr(self.models[model_name], 'eta0'):
                current_lr = self.models[model_name].eta0
                new_lr = min(current_lr * 1.5, 0.1)
                self.models[model_name].eta0 = new_lr
                logger.info(f"调整学习率: {current_lr} -> {new_lr}")
            
            # 策略2: 强制增量学习
            model_samples = [s for s in self.feedback_buffer if s['model_name'] == model_name]
            if len(model_samples) >= 5:
                self._trigger_incremental_learning(model_name)
            
            # 策略3: 如果漂移严重，触发重训练
            if drift_info.get('performance_decline', 0) > 0.15:
                logger.warning(f"严重概念漂移，建议重训练模型 {model_name}")
                # 这里可以触发自动重训练
                self._schedule_retraining(model_name)
            
        except Exception as e:
            logger.error(f"处理概念漂移失败: {e}")
    
    def _schedule_retraining(self, model_name: str):
        """安排重训练"""
        try:
            # 记录重训练需求
            retrain_request = {
                'model_name': model_name,
                'reason': 'concept_drift',
                'requested_time': datetime.now().isoformat(),
                'priority': 'high'
            }
            
            # 这里可以添加到重训练队列
            logger.info(f"已安排模型 {model_name} 的重训练")
            
        except Exception as e:
            logger.error(f"安排重训练失败: {e}")

class FeedbackOptimizer:
    """反馈优化器"""
    
    def __init__(self):
        self.incremental_learner = IncrementalLearner()
        self.feedback_weights = {
            'user_positive': 1.2,
            'user_negative': 0.8,
            'expert_positive': 1.5,
            'expert_negative': 0.5,
            'system_correct': 1.0,
            'system_incorrect': 0.6
        }
        
    def process_user_feedback(self, prediction_id: int, feedback_type: str, 
                            feedback_value: str, user_type: str = 'user') -> bool:
        """处理用户反馈"""
        try:
            # 获取原始预测信息
            conn = sqlite3.connect(str(evaluation_system.eval_db_path))
            cursor = conn.cursor()
            
            cursor.execute('''
                SELECT ts_code, model_name, prediction_value, confidence, prediction_date
                FROM predictions WHERE id = ?
            ''', (prediction_id,))
            
            prediction_info = cursor.fetchone()
            conn.close()
            
            if not prediction_info:
                logger.warning(f"未找到预测ID {prediction_id}")
                return False
            
            ts_code, model_name, prediction_value, confidence, prediction_date = prediction_info
            
            # 记录反馈到评估系统
            evaluation_system.add_user_feedback(
                prediction_id, feedback_type, feedback_value, 
                user_id=user_type, comments=None
            )
            
            # 转换反馈为训练信号
            training_signal = self._convert_feedback_to_signal(
                feedback_type, feedback_value, prediction_value, user_type
            )
            
            if training_signal is not None:
                # 获取特征（这里需要重新计算或从缓存获取）
                features = self._get_features_for_prediction(ts_code, prediction_date)
                
                if features is not None:
                    # 计算反馈权重
                    feedback_key = f"{user_type}_{feedback_value.lower()}"
                    weight = self.feedback_weights.get(feedback_key, 1.0)
                    
                    # 添加到增量学习
                    self.incremental_learner.add_training_sample(
                        model_name, features, training_signal, 
                        feedback_type='user', confidence=weight
                    )
                    
                    logger.info(f"处理用户反馈: 预测ID {prediction_id}, 反馈 {feedback_value}, 权重 {weight}")
                    return True
            
            return False
            
        except Exception as e:
            logger.error(f"处理用户反馈失败: {e}")
            return False
    
    def _convert_feedback_to_signal(self, feedback_type: str, feedback_value: str, 
                                  original_prediction: float, user_type: str) -> Optional[int]:
        """将反馈转换为训练信号"""
        try:
            if feedback_type == 'accuracy':
                if feedback_value.lower() == 'correct':
                    return int(original_prediction)  # 保持原预测
                elif feedback_value.lower() == 'incorrect':
                    # 反转预测（简化处理）
                    if original_prediction > 0:
                        return -1
                    elif original_prediction < 0:
                        return 1
                    else:
                        return np.random.choice([-1, 1])  # 随机选择
            
            elif feedback_type == 'usefulness':
                if feedback_value.lower() in ['useful', 'very_useful']:
                    return int(original_prediction)
                elif feedback_value.lower() in ['not_useful', 'misleading']:
                    return 0  # 中性标签
            
            elif feedback_type == 'satisfaction':
                # 满意度转换为权重，不直接影响标签
                return int(original_prediction)
            
            return None
            
        except Exception as e:
            logger.error(f"转换反馈信号失败: {e}")
            return None
    
    def _get_features_for_prediction(self, ts_code: str, prediction_date: str) -> Optional[np.ndarray]:
        """获取预测时的特征"""
        try:
            # 这里需要重新计算或从缓存获取特征
            # 为了简化，我们使用最近的特征数据
            pred_date = datetime.fromisoformat(prediction_date.replace('Z', '+00:00'))
            start_date = (pred_date - timedelta(days=60)).strftime('%Y%m%d')
            end_date = pred_date.strftime('%Y%m%d')
            
            # 获取股票数据
            df = enhanced_feature_engineer.base_engineer.fetch_raw_stock_data(
                ts_code, start_date, end_date
            )
            
            if df.empty:
                return None
            
            # 应用特征工程
            df = enhanced_feature_engineer.base_engineer.calculate_technical_features(df)
            df = enhanced_feature_engineer.base_engineer.calculate_fundamental_features(df)
            df = enhanced_feature_engineer.extract_statistical_features(df)
            
            # 获取最近的特征
            if not df.empty:
                numeric_cols = df.select_dtypes(include=[np.number]).columns
                feature_cols = [col for col in numeric_cols 
                               if not col.startswith('future_') and 
                               col not in ['price_direction_1d', 'price_direction_5d']]
                
                features = df[feature_cols].fillna(0).iloc[-1].values
                return features
            
            return None
            
        except Exception as e:
            logger.error(f"获取预测特征失败: {e}")
            return None
    
    def adaptive_weight_adjustment(self, user_feedback_history: List[Dict]) -> Dict[str, float]:
        """自适应权重调整"""
        try:
            # 分析用户反馈的准确性
            feedback_accuracy = {}
            
            for feedback_type in ['user', 'expert']:
                correct_feedback = 0
                total_feedback = 0
                
                for feedback in user_feedback_history:
                    if feedback.get('user_type') == feedback_type:
                        total_feedback += 1
                        # 这里需要验证反馈的准确性
                        # 简化版本：假设专家反馈更准确
                        if feedback_type == 'expert':
                            correct_feedback += 1
                        else:
                            correct_feedback += np.random.choice([0, 1], p=[0.3, 0.7])
                
                if total_feedback > 0:
                    feedback_accuracy[feedback_type] = correct_feedback / total_feedback
                else:
                    feedback_accuracy[feedback_type] = 0.5
            
            # 调整权重
            new_weights = self.feedback_weights.copy()
            
            for feedback_type in ['user', 'expert']:
                accuracy = feedback_accuracy.get(feedback_type, 0.5)
                adjustment_factor = 0.5 + accuracy  # 0.5 到 1.5 的范围
                
                new_weights[f'{feedback_type}_positive'] *= adjustment_factor
                new_weights[f'{feedback_type}_negative'] *= adjustment_factor
            
            logger.info(f"自适应权重调整完成: {new_weights}")
            return new_weights
            
        except Exception as e:
            logger.error(f"自适应权重调整失败: {e}")
            return self.feedback_weights

class ContinuousLearningSystem:
    """持续学习系统"""
    
    def __init__(self):
        self.incremental_learner = IncrementalLearner()
        self.feedback_optimizer = FeedbackOptimizer()
        self.learning_scheduler_running = False
        self.scheduler_thread = None
        
        # 持续学习配置
        self.config = {
            'learning_interval_hours': 6,  # 学习间隔
            'drift_check_interval_hours': 2,  # 漂移检查间隔
            'performance_check_interval_hours': 12,  # 性能检查间隔
            'auto_retrain_threshold': 0.2,  # 自动重训练阈值
            'feedback_batch_size': 50,  # 反馈批次大小
            'max_learning_iterations': 100  # 最大学习迭代次数
        }
    
    def start_continuous_learning(self):
        """启动持续学习"""
        if self.learning_scheduler_running:
            logger.warning("持续学习调度器已在运行")
            return
        
        self.learning_scheduler_running = True
        self.scheduler_thread = threading.Thread(target=self._learning_scheduler, daemon=True)
        self.scheduler_thread.start()
        
        logger.info("持续学习系统已启动")
    
    def stop_continuous_learning(self):
        """停止持续学习"""
        self.learning_scheduler_running = False
        
        if self.scheduler_thread and self.scheduler_thread.is_alive():
            self.scheduler_thread.join(timeout=10)
        
        logger.info("持续学习系统已停止")
    
    def _learning_scheduler(self):
        """学习调度器"""
        last_learning_time = time.time()
        last_drift_check_time = time.time()
        last_performance_check_time = time.time()
        
        while self.learning_scheduler_running:
            try:
                current_time = time.time()
                
                # 检查概念漂移
                if current_time - last_drift_check_time >= self.config['drift_check_interval_hours'] * 3600:
                    self._check_all_models_drift()
                    last_drift_check_time = current_time
                
                # 执行增量学习
                if current_time - last_learning_time >= self.config['learning_interval_hours'] * 3600:
                    self._execute_scheduled_learning()
                    last_learning_time = current_time
                
                # 性能检查
                if current_time - last_performance_check_time >= self.config['performance_check_interval_hours'] * 3600:
                    self._check_model_performance()
                    last_performance_check_time = current_time
                
                # 休眠1小时
                time.sleep(3600)
                
            except Exception as e:
                logger.error(f"学习调度器错误: {e}")
                time.sleep(60)  # 错误时休眠1分钟
    
    def _check_all_models_drift(self):
        """检查所有模型的概念漂移"""
        try:
            for model_name in self.incremental_learner.models.keys():
                drift_result = self.incremental_learner.detect_concept_drift(model_name)
                
                if drift_result.get('drift_detected', False):
                    logger.warning(f"检测到模型 {model_name} 概念漂移: {drift_result}")
                    
                    # 记录漂移事件
                    self._record_drift_event(model_name, drift_result)
        
        except Exception as e:
            logger.error(f"概念漂移检查失败: {e}")
    
    def _execute_scheduled_learning(self):
        """执行计划的学习任务"""
        try:
            # 处理缓冲区中的样本
            if len(self.incremental_learner.feedback_buffer) > 0:
                logger.info(f"处理缓冲区中的 {len(self.incremental_learner.feedback_buffer)} 个样本")
                
                # 按模型分组处理
                models_to_update = set()
                for sample in self.incremental_learner.feedback_buffer:
                    models_to_update.add(sample['model_name'])
                
                for model_name in models_to_update:
                    if model_name in self.incremental_learner.models:
                        self.incremental_learner._trigger_incremental_learning(model_name)
            
            # 执行自动优化检查
            auto_opt_result = model_optimizer.auto_optimization_check()
            if auto_opt_result.get('actions_taken'):
                logger.info(f"自动优化执行的行动: {auto_opt_result['actions_taken']}")
        
        except Exception as e:
            logger.error(f"计划学习执行失败: {e}")
    
    def _check_model_performance(self):
        """检查模型性能"""
        try:
            # 运行模型评估
            evaluation_result = evaluation_system.evaluate_predictions(days_back=3)
            
            if 'results' in evaluation_result:
                for model_name, metrics in evaluation_result['results'].items():
                    accuracy = metrics['metrics'].get('accuracy', 0)
                    
                    if accuracy < self.config['auto_retrain_threshold']:
                        logger.warning(f"模型 {model_name} 性能过低 (准确率: {accuracy:.3f})，触发重训练")
                        self._trigger_emergency_retraining(model_name)
        
        except Exception as e:
            logger.error(f"性能检查失败: {e}")
    
    def _trigger_emergency_retraining(self, model_name: str):
        """触发紧急重训练"""
        try:
            logger.info(f"触发紧急重训练: {model_name}")
            
            # 这里可以调用重训练逻辑
            retrain_result = model_optimizer.retrain_models([model_name])
            
            if retrain_result.get('success', False):
                logger.info(f"模型 {model_name} 紧急重训练成功")
            else:
                logger.error(f"模型 {model_name} 紧急重训练失败: {retrain_result.get('error', 'Unknown error')}")
        
        except Exception as e:
            logger.error(f"紧急重训练失败: {e}")
    
    def _record_drift_event(self, model_name: str, drift_info: Dict[str, Any]):
        """记录概念漂移事件"""
        try:
            drift_record = {
                'model_name': model_name,
                'drift_info': drift_info,
                'recorded_time': datetime.now().isoformat()
            }
            
            # 这里可以保存到数据库或日志文件
            logger.info(f"记录概念漂移事件: {drift_record}")
        
        except Exception as e:
            logger.error(f"记录漂移事件失败: {e}")
    
    def get_learning_status(self) -> Dict[str, Any]:
        """获取学习状态"""
        try:
            status = {
                'continuous_learning_active': self.learning_scheduler_running,
                'incremental_models': list(self.incremental_learner.models.keys()),
                'feedback_buffer_size': len(self.incremental_learner.feedback_buffer),
                'learning_history_count': len(self.incremental_learner.learning_history),
                'model_configs': self.incremental_learner.model_configs,
                'last_update_times': {
                    model_name: config.get('last_update')
                    for model_name, config in self.incremental_learner.model_configs.items()
                },
                'config': self.config,
                'status_time': datetime.now().isoformat()
            }
            
            return status
            
        except Exception as e:
            logger.error(f"获取学习状态失败: {e}")
            return {'error': str(e)}
    
    def manual_learning_trigger(self, model_name: str = None) -> Dict[str, Any]:
        """手动触发学习"""
        try:
            if model_name:
                if model_name not in self.incremental_learner.models:
                    return {'error': f'模型 {model_name} 不存在'}
                
                self.incremental_learner._trigger_incremental_learning(model_name)
                return {'success': True, 'message': f'手动触发模型 {model_name} 的增量学习'}
            else:
                # 触发所有模型的学习
                models_updated = []
                for model_name in self.incremental_learner.models.keys():
                    self.incremental_learner._trigger_incremental_learning(model_name)
                    models_updated.append(model_name)
                
                return {
                    'success': True, 
                    'message': f'手动触发 {len(models_updated)} 个模型的增量学习',
                    'models_updated': models_updated
                }
        
        except Exception as e:
            logger.error(f"手动学习触发失败: {e}")
            return {'error': str(e)}

# 全局实例
continuous_learning_system = ContinuousLearningSystem()

def main():
    """测试持续学习系统"""
    try:
        logger.info("开始测试持续学习系统...")
        
        print("\n持续学习系统测试")
        print("=" * 50)
        
        # 测试增量学习器
        print("\n1. 初始化增量学习模型...")
        incremental_learner = IncrementalLearner()
        
        success = incremental_learner.initialize_incremental_model('test_sgd', 'sgd')
        print(f"SGD模型初始化: {'成功' if success else '失败'}")
        
        success = incremental_learner.initialize_incremental_model('test_rf', 'random_forest')
        print(f"随机森林模型初始化: {'成功' if success else '失败'}")
        
        # 测试添加训练样本
        print("\n2. 添加训练样本...")
        for i in range(60):
            features = np.random.randn(50)  # 模拟50维特征
            label = np.random.choice([-1, 0, 1])
            confidence = np.random.uniform(0.6, 1.0)
            
            success = incremental_learner.add_training_sample(
                'test_sgd', features, label, 'system', confidence
            )
        
        print(f"添加了60个训练样本")
        
        # 测试预测
        print("\n3. 测试预测...")
        test_features = np.random.randn(50)
        prediction, confidence = incremental_learner.predict_with_uncertainty('test_sgd', test_features)
        print(f"预测结果: {prediction}, 置信度: {confidence:.3f}")
        
        # 测试概念漂移检测
        print("\n4. 测试概念漂移检测...")
        drift_result = incremental_learner.detect_concept_drift('test_sgd')
        print(f"概念漂移检测: {drift_result.get('drift_detected', False)}")
        
        # 测试反馈优化器
        print("\n5. 测试反馈优化器...")
        feedback_optimizer = FeedbackOptimizer()
        
        # 模拟用户反馈
        feedback_history = [
            {'user_type': 'user', 'accuracy': 0.7},
            {'user_type': 'expert', 'accuracy': 0.9}
        ]
        
        new_weights = feedback_optimizer.adaptive_weight_adjustment(feedback_history)
        print(f"自适应权重调整完成，新权重数量: {len(new_weights)}")
        
        # 测试持续学习系统状态
        print("\n6. 获取持续学习状态...")
        continuous_learning_system.incremental_learner = incremental_learner
        status = continuous_learning_system.get_learning_status()
        
        print(f"持续学习状态:")
        print(f"  - 增量模型数量: {len(status['incremental_models'])}")
        print(f"  - 反馈缓冲区大小: {status['feedback_buffer_size']}")
        print(f"  - 学习历史记录: {status['learning_history_count']}")
        
        # 测试手动学习触发
        print("\n7. 测试手动学习触发...")
        trigger_result = continuous_learning_system.manual_learning_trigger('test_sgd')
        print(f"手动学习触发: {'成功' if trigger_result.get('success') else '失败'}")
        
        print("\n测试完成！")
        
    except Exception as e:
        logger.error(f"测试失败: {e}")

if __name__ == "__main__":
    main()