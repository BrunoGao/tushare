"""
模型优化和重训练机制
基于性能评估结果自动优化模型参数和重新训练
"""
import pandas as pd
import numpy as np
import logging
import json
import os
import schedule
import time
from typing import Dict, List, Tuple, Optional, Any, Union
from datetime import datetime, timedelta
from threading import Thread
import asyncio
import warnings
warnings.filterwarnings('ignore')

import sys
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from database.db_manager import DatabaseManager
from ai.unified_trainer import unified_trainer
from ai.model_evaluation import evaluation_system
from llm.enhanced_feature_engineering import enhanced_feature_engineer

# 日志配置
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class ModelOptimizer:
    """模型优化器"""
    
    def __init__(self):
        self.db_manager = DatabaseManager()
        self.unified_trainer = unified_trainer
        self.evaluation_system = evaluation_system
        self.feature_engineer = enhanced_feature_engineer
        
        self.optimization_config = {
            'min_accuracy_threshold': 0.6,  # 最低准确率阈值
            'min_samples_for_retrain': 100,  # 重训练最少样本数
            'retrain_frequency_days': 7,     # 重训练频率（天）
            'evaluation_window_days': 5,    # 评估窗口（天）
            'auto_retrain_enabled': False,  # 是否启用自动重训练
            'optimization_enabled': True,   # 是否启用优化
        }
        
        self.optimization_history = []
        self.scheduler_running = False
        
    def analyze_model_performance(self, days_back: int = 30) -> Dict[str, Any]:
        """分析模型性能"""
        try:
            # 获取性能报告
            report = self.evaluation_system.get_model_performance_report(None, days_back)
            
            if 'error' in report:
                return {'error': report['error']}
            
            analysis = {
                'timestamp': datetime.now().isoformat(),
                'analysis_period': days_back,
                'models_analyzed': {},
                'recommendations': [],
                'needs_optimization': False,
                'needs_retraining': False
            }
            
            # 分析各模型表现
            predictions_summary = report.get('predictions_summary', {})
            by_model = predictions_summary.get('by_model', {})
            
            for model_name, stats in by_model.items():
                model_analysis = {
                    'accuracy': stats.get('accuracy', 0),
                    'confidence': stats.get('avg_confidence', 0),
                    'sample_count': stats.get('evaluated', 0),
                    'performance_score': 0,
                    'issues': [],
                    'recommendations': []
                }
                
                # 计算综合性能分数
                accuracy = model_analysis['accuracy']
                confidence = model_analysis['confidence']
                sample_count = model_analysis['sample_count']
                
                # 性能分数 = 准确率 * 0.6 + 置信度 * 0.3 + 样本充足度 * 0.1
                sample_score = min(sample_count / 100, 1.0)  # 100个样本为满分
                model_analysis['performance_score'] = (
                    accuracy * 0.6 + confidence * 0.3 + sample_score * 0.1
                )
                
                # 识别问题
                if accuracy < self.optimization_config['min_accuracy_threshold']:
                    model_analysis['issues'].append('accuracy_low')
                    model_analysis['recommendations'].append('重新训练或调整参数')
                    analysis['needs_retraining'] = True
                
                if confidence < 0.6:
                    model_analysis['issues'].append('confidence_low')
                    model_analysis['recommendations'].append('增加训练数据或调整模型结构')
                    analysis['needs_optimization'] = True
                
                if sample_count < 50:
                    model_analysis['issues'].append('insufficient_samples')
                    model_analysis['recommendations'].append('积累更多评估样本')
                
                analysis['models_analyzed'][model_name] = model_analysis
            
            # 生成整体建议
            if analysis['needs_retraining']:
                analysis['recommendations'].append('建议对表现差的模型进行重新训练')
            
            if analysis['needs_optimization']:
                analysis['recommendations'].append('建议优化模型参数或特征工程')
            
            # 检查是否需要自动优化
            overall_accuracy = predictions_summary.get('overall_accuracy', 0)
            if overall_accuracy < self.optimization_config['min_accuracy_threshold']:
                analysis['recommendations'].append('系统整体准确率偏低，建议立即优化')
                analysis['priority'] = 'high'
            elif analysis['needs_optimization']:
                analysis['priority'] = 'medium'
            else:
                analysis['priority'] = 'low'
            
            return analysis
            
        except Exception as e:
            logger.error(f"分析模型性能失败: {e}")
            return {'error': str(e)}
    
    def optimize_model_parameters(self, model_name: str, optimization_type: str = 'hyperparameter') -> Dict[str, Any]:
        """优化模型参数"""
        try:
            logger.info(f"开始优化模型 {model_name}，优化类型: {optimization_type}")
            
            optimization_result = {
                'model_name': model_name,
                'optimization_type': optimization_type,
                'timestamp': datetime.now().isoformat(),
                'success': False,
                'improvements': {},
                'new_performance': {}
            }
            
            if optimization_type == 'hyperparameter':
                # 超参数调优
                result = self._optimize_hyperparameters(model_name)
                optimization_result.update(result)
                
            elif optimization_type == 'feature_selection':
                # 特征选择优化
                result = self._optimize_feature_selection(model_name)
                optimization_result.update(result)
                
            elif optimization_type == 'ensemble':
                # 集成学习优化
                result = self._optimize_ensemble_weights(model_name)
                optimization_result.update(result)
                
            else:
                optimization_result['error'] = f"不支持的优化类型: {optimization_type}"
            
            # 记录优化历史
            self.optimization_history.append(optimization_result)
            
            return optimization_result
            
        except Exception as e:
            logger.error(f"优化模型参数失败: {e}")
            return {'error': str(e)}
    
    def _optimize_hyperparameters(self, model_name: str) -> Dict[str, Any]:
        """超参数优化"""
        try:
            # 获取最新数据进行优化
            training_data, X, y = self.unified_trainer.prepare_training_data(
                stock_limit=300, days_back=120, include_sequences=False
            )
            
            if len(X) == 0:
                return {'error': '没有足够的训练数据'}
            
            # 对指定模型进行超参数调优
            if model_name in self.unified_trainer.ml_trainer.available_models:
                tuning_result = self.unified_trainer.ml_trainer.hyperparameter_tuning(model_name, X, y)
                
                if tuning_result:
                    return {
                        'success': True,
                        'best_params': tuning_result.get('best_params', {}),
                        'best_score': tuning_result.get('best_score', 0),
                        'improvement': tuning_result.get('best_score', 0) - 0.5  # 假设基准分数
                    }
                else:
                    return {'error': '超参数调优失败'}
            else:
                return {'error': f'模型 {model_name} 不支持超参数调优'}
            
        except Exception as e:
            logger.error(f"超参数优化失败: {e}")
            return {'error': str(e)}
    
    def _optimize_feature_selection(self, model_name: str) -> Dict[str, Any]:
        """特征选择优化"""
        try:
            # 重新进行特征工程和选择
            stock_list = self.feature_engineer.base_engineer.get_stock_list_for_training(200)
            
            if not stock_list:
                return {'error': '无法获取训练股票列表'}
            
            end_date = datetime.now().strftime('%Y%m%d')
            start_date = (datetime.now() - timedelta(days=120)).strftime('%Y%m%d')
            
            # 使用增强特征工程
            multimodal_data = self.feature_engineer.prepare_multimodal_features(
                stock_list, start_date, end_date
            )
            
            if not multimodal_data or multimodal_data['tabular_features'].empty:
                return {'error': '特征工程失败'}
            
            # 分析特征重要性
            tabular_features = multimodal_data['tabular_features']
            feature_categories = multimodal_data.get('feature_categories', {})
            
            # 计算各类特征的表现
            category_performance = {}
            for category, features in feature_categories.items():
                if features and len(features) > 0:
                    # 简化的特征评估（实际应该用更复杂的方法）
                    category_score = len(features) / max(len(tabular_features.columns), 1)
                    category_performance[category] = {
                        'feature_count': len(features),
                        'relative_importance': category_score,
                        'features': features[:5]  # 只保存前5个特征名
                    }
            
            return {
                'success': True,
                'feature_analysis': category_performance,
                'total_features': len(tabular_features.columns),
                'recommendations': ['使用增强特征工程提升模型性能']
            }
            
        except Exception as e:
            logger.error(f"特征选择优化失败: {e}")
            return {'error': str(e)}
    
    def _optimize_ensemble_weights(self, model_name: str) -> Dict[str, Any]:
        """集成学习权重优化"""
        try:
            # 获取所有可用模型的性能
            performance_report = self.evaluation_system.get_model_performance_report(None, 30)
            
            if 'error' in performance_report:
                return {'error': performance_report['error']}
            
            models_performance = performance_report.get('predictions_summary', {}).get('by_model', {})
            
            if not models_performance:
                return {'error': '没有足够的模型性能数据'}
            
            # 计算最优权重（基于准确率）
            total_accuracy = sum(perf.get('accuracy', 0) for perf in models_performance.values())
            
            if total_accuracy == 0:
                return {'error': '所有模型准确率为0'}
            
            optimal_weights = {}
            for model, perf in models_performance.items():
                weight = perf.get('accuracy', 0) / total_accuracy
                optimal_weights[model] = round(weight, 3)
            
            return {
                'success': True,
                'optimal_weights': optimal_weights,
                'weight_strategy': 'accuracy_based',
                'expected_improvement': 0.05  # 预期改进
            }
            
        except Exception as e:
            logger.error(f"集成学习优化失败: {e}")
            return {'error': str(e)}
    
    def retrain_models(self, model_names: List[str] = None, 
                      retrain_config: Dict[str, Any] = None) -> Dict[str, Any]:
        """重新训练模型"""
        try:
            logger.info(f"开始重新训练模型: {model_names or 'ALL'}")
            
            config = retrain_config or {
                'stock_limit': 400,
                'days_back': 150,
                'epochs': 40,
                'train_traditional': True,
                'train_deep': True
            }
            
            # 运行训练
            performance = self.unified_trainer.train_all_models(
                stock_limit=config['stock_limit'],
                days_back=config['days_back'],
                train_traditional=config['train_traditional'],
                train_deep=config['train_deep'],
                epochs=config['epochs']
            )
            
            if not performance:
                return {'error': '重新训练失败，没有成功训练的模型'}
            
            # 与之前性能对比
            retrain_result = {
                'timestamp': datetime.now().isoformat(),
                'retrained_models': list(performance.keys()),
                'performance': performance,
                'config_used': config,
                'improvements': {}
            }
            
            # 计算改进情况（简化版）
            for model_name, perf in performance.items():
                retrain_result['improvements'][model_name] = {
                    'new_accuracy': perf.get('accuracy', 0),
                    'new_f1_score': perf.get('f1_score', 0),
                    'training_time': perf.get('training_time', 0)
                }
            
            # 保存重训练历史
            self._save_retrain_history(retrain_result)
            
            return retrain_result
            
        except Exception as e:
            logger.error(f"重新训练模型失败: {e}")
            return {'error': str(e)}
    
    def _save_retrain_history(self, retrain_result: Dict[str, Any]):
        """保存重训练历史"""
        try:
            history_file = "models/retrain_history.json"
            os.makedirs(os.path.dirname(history_file), exist_ok=True)
            
            history = []
            if os.path.exists(history_file):
                with open(history_file, 'r', encoding='utf-8') as f:
                    history = json.load(f)
            
            history.append(retrain_result)
            
            # 只保留最近50次记录
            if len(history) > 50:
                history = history[-50:]
            
            with open(history_file, 'w', encoding='utf-8') as f:
                json.dump(history, f, ensure_ascii=False, indent=2)
                
        except Exception as e:
            logger.warning(f"保存重训练历史失败: {e}")
    
    def auto_optimization_check(self) -> Dict[str, Any]:
        """自动优化检查"""
        try:
            logger.info("运行自动优化检查...")
            
            # 分析当前性能
            performance_analysis = self.analyze_model_performance(7)  # 最近7天
            
            if 'error' in performance_analysis:
                return performance_analysis
            
            optimization_actions = {
                'timestamp': datetime.now().isoformat(),
                'analysis': performance_analysis,
                'actions_taken': [],
                'recommendations': performance_analysis.get('recommendations', [])
            }
            
            # 检查是否需要自动重训练
            if (self.optimization_config['auto_retrain_enabled'] and 
                performance_analysis.get('needs_retraining')):
                
                # 检查距离上次重训练的时间
                if self._should_retrain():
                    logger.info("触发自动重训练...")
                    retrain_result = self.retrain_models()
                    
                    if 'error' not in retrain_result:
                        optimization_actions['actions_taken'].append({
                            'action': 'auto_retrain',
                            'result': 'success',
                            'models_count': len(retrain_result.get('retrained_models', []))
                        })
                    else:
                        optimization_actions['actions_taken'].append({
                            'action': 'auto_retrain',
                            'result': 'failed',
                            'error': retrain_result['error']
                        })
            
            # 检查是否需要参数优化
            if (self.optimization_config['optimization_enabled'] and 
                performance_analysis.get('needs_optimization')):
                
                # 找出需要优化的模型
                models_to_optimize = []
                for model_name, analysis in performance_analysis.get('models_analyzed', {}).items():
                    if 'confidence_low' in analysis.get('issues', []):
                        models_to_optimize.append(model_name)
                
                if models_to_optimize:
                    logger.info(f"优化模型参数: {models_to_optimize}")
                    for model_name in models_to_optimize[:3]:  # 最多优化3个模型
                        opt_result = self.optimize_model_parameters(model_name, 'hyperparameter')
                        optimization_actions['actions_taken'].append({
                            'action': f'optimize_{model_name}',
                            'result': 'success' if opt_result.get('success') else 'failed',
                            'details': opt_result
                        })
            
            return optimization_actions
            
        except Exception as e:
            logger.error(f"自动优化检查失败: {e}")
            return {'error': str(e)}
    
    def _should_retrain(self) -> bool:
        """检查是否应该重新训练"""
        try:
            history_file = "models/retrain_history.json"
            
            if not os.path.exists(history_file):
                return True  # 没有历史记录，应该训练
            
            with open(history_file, 'r', encoding='utf-8') as f:
                history = json.load(f)
            
            if not history:
                return True
            
            # 检查最后一次重训练时间
            last_retrain = history[-1]
            last_timestamp = datetime.fromisoformat(last_retrain['timestamp'])
            days_since_retrain = (datetime.now() - last_timestamp).days
            
            return days_since_retrain >= self.optimization_config['retrain_frequency_days']
            
        except Exception as e:
            logger.warning(f"检查重训练时间失败: {e}")
            return True
    
    def start_scheduler(self):
        """启动定时优化任务"""
        if self.scheduler_running:
            logger.warning("调度器已经在运行")
            return
        
        logger.info("启动模型优化调度器...")
        
        # 设置定时任务
        schedule.every().day.at("02:00").do(self._daily_optimization_job)
        schedule.every().sunday.at("01:00").do(self._weekly_evaluation_job)
        
        self.scheduler_running = True
        
        # 在后台线程运行调度器
        def run_scheduler():
            while self.scheduler_running:
                schedule.run_pending()
                time.sleep(60)  # 每分钟检查一次
        
        scheduler_thread = Thread(target=run_scheduler, daemon=True)
        scheduler_thread.start()
        
        logger.info("模型优化调度器已启动")
    
    def stop_scheduler(self):
        """停止定时优化任务"""
        self.scheduler_running = False
        schedule.clear()
        logger.info("模型优化调度器已停止")
    
    def _daily_optimization_job(self):
        """每日优化任务"""
        logger.info("执行每日优化检查...")
        result = self.auto_optimization_check()
        
        # 记录任务执行结果
        task_result = {
            'task_type': 'daily_optimization',
            'timestamp': datetime.now().isoformat(),
            'result': result
        }
        
        self._save_task_log(task_result)
    
    def _weekly_evaluation_job(self):
        """每周评估任务"""
        logger.info("执行每周模型评估...")
        
        # 运行全面评估
        evaluation_result = self.evaluation_system.evaluate_predictions(5)
        
        # 记录任务执行结果
        task_result = {
            'task_type': 'weekly_evaluation',
            'timestamp': datetime.now().isoformat(),
            'result': evaluation_result
        }
        
        self._save_task_log(task_result)
    
    def _save_task_log(self, task_result: Dict[str, Any]):
        """保存任务日志"""
        try:
            log_file = "models/scheduler_log.json"
            os.makedirs(os.path.dirname(log_file), exist_ok=True)
            
            logs = []
            if os.path.exists(log_file):
                with open(log_file, 'r', encoding='utf-8') as f:
                    logs = json.load(f)
            
            logs.append(task_result)
            
            # 只保留最近100条记录
            if len(logs) > 100:
                logs = logs[-100:]
            
            with open(log_file, 'w', encoding='utf-8') as f:
                json.dump(logs, f, ensure_ascii=False, indent=2)
                
        except Exception as e:
            logger.warning(f"保存任务日志失败: {e}")
    
    def get_optimization_status(self) -> Dict[str, Any]:
        """获取优化状态"""
        try:
            status = {
                'scheduler_running': self.scheduler_running,
                'config': self.optimization_config,
                'optimization_history': self.optimization_history[-10:],  # 最近10次
                'last_optimization': None,
                'next_scheduled': None
            }
            
            # 获取最近的优化记录
            if self.optimization_history:
                status['last_optimization'] = self.optimization_history[-1]
            
            # 获取下次计划执行时间
            if self.scheduler_running:
                jobs = schedule.jobs
                if jobs:
                    next_run = min(job.next_run for job in jobs)
                    status['next_scheduled'] = next_run.isoformat()
            
            return status
            
        except Exception as e:
            logger.error(f"获取优化状态失败: {e}")
            return {'error': str(e)}

# 全局实例
model_optimizer = ModelOptimizer()

def main():
    """测试模型优化器"""
    try:
        logger.info("开始测试模型优化器...")
        
        # 测试性能分析
        analysis = model_optimizer.analyze_model_performance(days_back=7)
        print(f"性能分析结果: {json.dumps(analysis, indent=2, ensure_ascii=False)}")
        
        # 测试自动优化检查
        optimization_check = model_optimizer.auto_optimization_check()
        print(f"自动优化检查: {json.dumps(optimization_check, indent=2, ensure_ascii=False)}")
        
        # 获取优化状态
        status = model_optimizer.get_optimization_status()
        print(f"优化状态: {json.dumps(status, indent=2, ensure_ascii=False)}")
        
        print("✅ 模型优化器测试完成")
        
    except Exception as e:
        logger.error(f"测试失败: {e}")

if __name__ == "__main__":
    main()