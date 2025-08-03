#!/usr/bin/env python3
"""
大模型股票预测系统 - 主训练脚本
端到端训练验证，自动化训练流程，生成完整的模型和评估报告
"""
import os
import sys
import logging
import json
import time
from pathlib import Path
from datetime import datetime, timedelta
import argparse
import warnings
warnings.filterwarnings('ignore')

# 添加项目路径
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

# 核心模块导入
from database.db_manager import DatabaseManager
from llm.enhanced_feature_engineering import enhanced_feature_engineer

# AI训练模块
try:
    from ai.unified_trainer import unified_trainer
    UNIFIED_TRAINER_AVAILABLE = True
except ImportError:
    UNIFIED_TRAINER_AVAILABLE = False

try:
    from ai.deep_learning_trainer import deep_learning_trainer
    DEEP_LEARNING_AVAILABLE = True
except ImportError:
    DEEP_LEARNING_AVAILABLE = False

try:
    from ai.advanced_evaluation_system import evaluation_system, model_optimizer
    EVALUATION_AVAILABLE = True
except ImportError:
    EVALUATION_AVAILABLE = False

try:
    from ai.continuous_learning_system import continuous_learning_system
    CONTINUOUS_LEARNING_AVAILABLE = True
except ImportError:
    CONTINUOUS_LEARNING_AVAILABLE = False

# 日志配置
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler('logs/ai_training.log'),
        logging.StreamHandler()
    ]
)
logger = logging.getLogger(__name__)

class AITrainingPipeline:
    """AI训练管道"""
    
    def __init__(self, config: dict = None):
        self.db_manager = DatabaseManager()
        self.config = self._load_config(config)
        self.results = {}
        self.training_start_time = None
        self.training_end_time = None
        
        # 确保必要目录存在
        self._ensure_directories()
        
    def _load_config(self, config: dict = None) -> dict:
        """加载配置"""
        default_config = {
            # 数据配置
            'stock_limit': 200,
            'days_back': 150,
            'target_columns': ['price_direction_5d', 'price_direction_10d'],
            
            # 训练配置
            'train_traditional': True,
            'train_deep_learning': True,
            'train_multimodal': True,
            
            # 深度学习配置
            'dl_epochs': 50,
            'dl_batch_size': 64,
            'dl_learning_rate': 0.001,
            
            # 评估配置
            'enable_evaluation': True,
            'evaluation_days': 5,
            
            # 优化配置
            'enable_optimization': True,
            'optimization_trials': 30,
            
            # 持续学习配置
            'enable_continuous_learning': True,
            'start_continuous_learning': False,
            
            # 输出配置
            'save_models': True,
            'generate_report': True,
            'report_format': 'html',
            
            # 其他配置
            'random_seed': 42,
            'verbose': True
        }
        
        if config:
            default_config.update(config)
            
        return default_config
    
    def _ensure_directories(self):
        """确保必要目录存在"""
        directories = [
            'models',
            'models/traditional',
            'models/deep_learning', 
            'models/incremental',
            'models/evaluation',
            'logs',
            'reports',
            'reports/training'
        ]
        
        for directory in directories:
            Path(directory).mkdir(parents=True, exist_ok=True)
    
    def run_full_pipeline(self) -> dict:
        """运行完整的训练管道"""
        logger.info("🚀 开始运行AI大模型训练管道")
        self.training_start_time = datetime.now()
        
        try:
            # 阶段1: 环境检查和数据准备
            logger.info("📋 阶段1: 环境检查和数据准备")
            env_check = self._check_environment()
            self.results['environment_check'] = env_check
            
            if not env_check['success']:
                return self._finalize_results(success=False, error="环境检查失败")
            
            # 准备训练数据
            data_prep = self._prepare_training_data()
            self.results['data_preparation'] = data_prep
            
            if not data_prep['success']:
                return self._finalize_results(success=False, error="数据准备失败")
            
            # 阶段2: 传统机器学习模型训练
            if self.config['train_traditional'] and UNIFIED_TRAINER_AVAILABLE:
                logger.info("🎯 阶段2: 传统机器学习模型训练")
                traditional_results = self._train_traditional_models()
                self.results['traditional_models'] = traditional_results
            
            # 阶段3: 深度学习模型训练
            if self.config['train_deep_learning'] and DEEP_LEARNING_AVAILABLE:
                logger.info("🧠 阶段3: 深度学习模型训练")
                dl_results = self._train_deep_learning_models()
                self.results['deep_learning_models'] = dl_results
            
            # 阶段4: 模型评估
            if self.config['enable_evaluation'] and EVALUATION_AVAILABLE:
                logger.info("📊 阶段4: 模型评估")
                eval_results = self._evaluate_models()
                self.results['evaluation'] = eval_results
            
            # 阶段5: 模型优化
            if self.config['enable_optimization'] and EVALUATION_AVAILABLE:
                logger.info("⚡ 阶段5: 模型优化")
                opt_results = self._optimize_models()
                self.results['optimization'] = opt_results
            
            # 阶段6: 持续学习系统初始化
            if self.config['enable_continuous_learning'] and CONTINUOUS_LEARNING_AVAILABLE:
                logger.info("🔄 阶段6: 持续学习系统初始化")
                cl_results = self._setup_continuous_learning()
                self.results['continuous_learning'] = cl_results
            
            # 阶段7: 生成报告
            if self.config['generate_report']:
                logger.info("📝 阶段7: 生成训练报告")
                report_results = self._generate_comprehensive_report()
                self.results['report'] = report_results
            
            return self._finalize_results(success=True)
            
        except Exception as e:
            logger.error(f"训练管道执行失败: {e}")
            return self._finalize_results(success=False, error=str(e))
    
    def _check_environment(self) -> dict:
        """检查环境和依赖"""
        logger.info("检查环境和依赖...")
        
        try:
            env_status = {
                'unified_trainer': UNIFIED_TRAINER_AVAILABLE,
                'deep_learning': DEEP_LEARNING_AVAILABLE,
                'evaluation_system': EVALUATION_AVAILABLE,
                'continuous_learning': CONTINUOUS_LEARNING_AVAILABLE,
                'database_connection': False,
                'feature_engineering': False
            }
            
            # 检查数据库连接
            try:
                test_query = self.db_manager.fetch_data("SELECT COUNT(*) as count FROM stock_basic LIMIT 1")
                env_status['database_connection'] = not test_query.empty
                stock_count = int(test_query.iloc[0]['count']) if not test_query.empty else 0
            except Exception as e:
                logger.error(f"数据库连接检查失败: {e}")
                stock_count = 0
            
            # 检查特征工程
            try:
                test_stocks = enhanced_feature_engineer.base_engineer.get_stock_list_for_training(5)
                env_status['feature_engineering'] = len(test_stocks) > 0
            except Exception as e:
                logger.error(f"特征工程检查失败: {e}")
            
            # 评估整体状态
            critical_components = ['database_connection', 'feature_engineering']
            success = all(env_status[comp] for comp in critical_components)
            
            available_features = sum(1 for available in env_status.values() if available)
            
            result = {
                'success': success,
                'environment_status': env_status,
                'stock_count': stock_count,
                'available_features': available_features,
                'total_features': len(env_status),
                'check_time': datetime.now().isoformat()
            }
            
            if success:
                logger.info(f"✅ 环境检查通过 ({available_features}/{len(env_status)} 功能可用)")
            else:
                logger.error(f"❌ 环境检查失败，关键组件不可用")
            
            return result
            
        except Exception as e:
            logger.error(f"环境检查失败: {e}")
            return {'success': False, 'error': str(e)}
    
    def _prepare_training_data(self) -> dict:
        """准备训练数据"""
        logger.info(f"准备训练数据: {self.config['stock_limit']}只股票, {self.config['days_back']}天历史数据")
        
        try:
            # 获取股票列表
            stock_list = enhanced_feature_engineer.base_engineer.get_stock_list_for_training(
                self.config['stock_limit']
            )
            
            if not stock_list:
                return {'success': False, 'error': '无法获取股票列表'}
            
            # 设置日期范围
            end_date = datetime.now().strftime('%Y%m%d')
            start_date = (datetime.now() - timedelta(days=self.config['days_back'])).strftime('%Y%m%d')
            
            # 准备多模态特征
            logger.info("正在构建多模态特征...")
            multimodal_data = enhanced_feature_engineer.prepare_multimodal_features(
                stock_list, start_date, end_date
            )
            
            if not multimodal_data:
                return {'success': False, 'error': '多模态特征准备失败'}
            
            # 统计信息
            tabular_features = multimodal_data.get('tabular_features', pd.DataFrame())
            sequence_features = multimodal_data.get('sequence_features', np.array([]))
            text_features = multimodal_data.get('text_features', [])
            
            data_stats = {
                'stock_count': len(stock_list),
                'date_range': f"{start_date} - {end_date}",
                'tabular_samples': len(tabular_features) if not tabular_features.empty else 0,
                'tabular_features': len(tabular_features.columns) if not tabular_features.empty else 0,
                'sequence_samples': sequence_features.shape[0] if sequence_features.size > 0 else 0,
                'sequence_length': sequence_features.shape[1] if sequence_features.size > 0 else 0,
                'sequence_features': sequence_features.shape[2] if sequence_features.size > 0 else 0,
                'text_samples': len(text_features),
                'feature_categories': multimodal_data.get('feature_categories', {}),
                'valid_targets': 0
            }
            
            # 检查目标变量
            valid_targets = 0
            for target_col in self.config['target_columns']:
                if target_col in tabular_features.columns:
                    valid_count = tabular_features[target_col].notna().sum()
                    valid_targets += valid_count
                    data_stats[f'{target_col}_valid_samples'] = valid_count
            
            data_stats['valid_targets'] = valid_targets
            
            result = {
                'success': valid_targets > 100,  # 至少需要100个有效目标
                'stock_list': stock_list,
                'multimodal_data': multimodal_data,
                'data_statistics': data_stats,
                'preparation_time': datetime.now().isoformat()
            }
            
            if result['success']:
                logger.info(f"✅ 数据准备完成: {data_stats['tabular_samples']}个样本, "
                           f"{data_stats['tabular_features']}维特征")
            else:
                logger.error(f"❌ 数据准备失败: 有效目标样本不足({valid_targets})")
            
            return result
            
        except Exception as e:
            logger.error(f"数据准备失败: {e}")
            return {'success': False, 'error': str(e)}
    
    def _train_traditional_models(self) -> dict:
        """训练传统机器学习模型"""
        logger.info("开始训练传统机器学习模型...")
        
        try:
            training_results = unified_trainer.train_all_models(
                stock_limit=self.config['stock_limit'],
                days_back=self.config['days_back'],
                train_traditional=True,
                train_deep=False,
                epochs=30
            )
            
            if not training_results:
                return {'success': False, 'error': '传统模型训练失败'}
            
            # 统计训练结果
            trained_models = []
            performance_summary = {}
            
            for model_name, performance in training_results.items():
                if isinstance(performance, dict) and 'accuracy' in performance:
                    trained_models.append(model_name)
                    performance_summary[model_name] = {
                        'accuracy': performance.get('accuracy', 0),
                        'precision': performance.get('precision', 0),
                        'recall': performance.get('recall', 0),
                        'f1_score': performance.get('f1_score', 0)
                    }
            
            result = {
                'success': len(trained_models) > 0,
                'trained_models': trained_models,
                'model_count': len(trained_models),
                'performance_summary': performance_summary,
                'best_model': max(performance_summary.keys(), 
                                key=lambda x: performance_summary[x]['accuracy']) if performance_summary else None,
                'training_time': datetime.now().isoformat()
            }
            
            if result['success']:
                best_acc = max(p['accuracy'] for p in performance_summary.values()) if performance_summary else 0
                logger.info(f"✅ 传统模型训练完成: {len(trained_models)}个模型, 最佳准确率: {best_acc:.4f}")
            else:
                logger.error("❌ 传统模型训练失败")
            
            return result
            
        except Exception as e:
            logger.error(f"传统模型训练失败: {e}")
            return {'success': False, 'error': str(e)}
    
    def _train_deep_learning_models(self) -> dict:
        """训练深度学习模型"""
        logger.info("开始训练深度学习模型...")
        
        try:
            # 获取数据
            data_prep = self.results.get('data_preparation', {})
            if not data_prep.get('success', False):
                return {'success': False, 'error': '缺少训练数据'}
            
            stock_list = data_prep['stock_list']
            
            # 设置日期范围
            end_date = datetime.now().strftime('%Y%m%d')
            start_date = (datetime.now() - timedelta(days=self.config['days_back'])).strftime('%Y%m%d')
            
            # 训练深度学习模型
            dl_results = deep_learning_trainer.train_all_models(
                stock_list, start_date, end_date, 'price_direction_5d'
            )
            
            if 'error' in dl_results:
                return {'success': False, 'error': dl_results['error']}
            
            # 处理结果
            trained_models = []
            performance_summary = {}
            
            for model_name, result in dl_results.get('results', {}).items():
                performance = result.get('performance', {})
                if performance:
                    trained_models.append(model_name)
                    performance_summary[model_name] = performance
            
            result = {
                'success': len(trained_models) > 0,
                'trained_models': trained_models,
                'model_count': len(trained_models),
                'performance_summary': performance_summary,
                'training_info': dl_results.get('training_info', {}),
                'best_model': max(performance_summary.keys(),
                                key=lambda x: performance_summary[x].get('accuracy', 0)) if performance_summary else None,
                'training_time': datetime.now().isoformat()
            }
            
            if result['success']:
                best_acc = max(p.get('accuracy', 0) for p in performance_summary.values()) if performance_summary else 0
                logger.info(f"✅ 深度学习模型训练完成: {len(trained_models)}个模型, 最佳准确率: {best_acc:.4f}")
            else:
                logger.error("❌ 深度学习模型训练失败")
            
            return result
            
        except Exception as e:
            logger.error(f"深度学习模型训练失败: {e}")
            return {'success': False, 'error': str(e)}
    
    def _evaluate_models(self) -> dict:
        """评估模型"""
        logger.info("开始评估模型...")
        
        try:
            # 运行预测评估
            evaluation_result = evaluation_system.evaluate_predictions(
                days_back=self.config['evaluation_days']
            )
            
            if 'error' in evaluation_result:
                return {'success': False, 'error': evaluation_result['error']}
            
            # 获取性能报告
            performance_report = evaluation_system.get_model_performance_report(days_back=30)
            
            # 获取反馈统计
            feedback_stats = evaluation_system.get_feedback_statistics(days_back=30)
            
            result = {
                'success': True,
                'evaluation_result': evaluation_result,
                'performance_report': performance_report,
                'feedback_statistics': feedback_stats,
                'evaluated_predictions': evaluation_result.get('evaluated_count', 0),
                'models_evaluated': len(evaluation_result.get('results', {})),
                'evaluation_time': datetime.now().isoformat()
            }
            
            logger.info(f"✅ 模型评估完成: 评估了{result['evaluated_predictions']}个预测, "
                       f"{result['models_evaluated']}个模型")
            
            return result
            
        except Exception as e:
            logger.error(f"模型评估失败: {e}")
            return {'success': False, 'error': str(e)}
    
    def _optimize_models(self) -> dict:
        """优化模型"""
        logger.info("开始优化模型...")
        
        try:
            # 分析模型性能
            analysis_result = model_optimizer.analyze_model_performance(days_back=30)
            
            if 'error' in analysis_result:
                return {'success': False, 'error': analysis_result['error']}
            
            optimization_actions = []
            optimization_results = {}
            
            # 获取需要优化的模型
            models_analyzed = analysis_result.get('models_analyzed', {})
            high_priority_models = [
                model_name for model_name, data in models_analyzed.items()
                if data.get('priority') == 'high'
            ]
            
            # 对高优先级模型进行优化
            for model_name in high_priority_models[:3]:  # 限制优化数量
                try:
                    opt_result = model_optimizer.optimize_model_parameters(
                        model_name, 'hyperparameter'
                    )
                    
                    if opt_result.get('success', False):
                        optimization_actions.append(f"优化了模型 {model_name} 的超参数")
                        optimization_results[model_name] = opt_result
                    
                except Exception as e:
                    logger.warning(f"优化模型 {model_name} 失败: {e}")
            
            # 执行自动优化检查
            auto_opt_result = model_optimizer.auto_optimization_check()
            optimization_actions.extend(auto_opt_result.get('actions_taken', []))
            
            result = {
                'success': True,
                'analysis_result': analysis_result,
                'optimization_actions': optimization_actions,
                'optimization_results': optimization_results,
                'auto_optimization': auto_opt_result,
                'models_optimized': len(optimization_results),
                'optimization_time': datetime.now().isoformat()
            }
            
            logger.info(f"✅ 模型优化完成: 执行了{len(optimization_actions)}个优化动作")
            
            return result
            
        except Exception as e:
            logger.error(f"模型优化失败: {e}")
            return {'success': False, 'error': str(e)}
    
    def _setup_continuous_learning(self) -> dict:
        """设置持续学习系统"""
        logger.info("初始化持续学习系统...")
        
        try:
            # 初始化增量学习模型
            setup_results = []
            
            # 为主要模型类型设置增量学习
            model_types = ['sgd', 'random_forest']
            for i, model_type in enumerate(model_types):
                model_name = f"incremental_{model_type}"
                success = continuous_learning_system.incremental_learner.initialize_incremental_model(
                    model_name, model_type
                )
                
                if success:
                    setup_results.append(f"初始化增量学习模型: {model_name}")
                
            # 获取持续学习状态
            learning_status = continuous_learning_system.get_learning_status()
            
            # 如果配置要求，启动持续学习
            if self.config.get('start_continuous_learning', False):
                continuous_learning_system.start_continuous_learning()
                setup_results.append("启动了持续学习调度器")
            
            result = {
                'success': len(setup_results) > 0,
                'setup_actions': setup_results,
                'learning_status': learning_status,
                'continuous_learning_active': continuous_learning_system.learning_scheduler_running,
                'setup_time': datetime.now().isoformat()
            }
            
            logger.info(f"✅ 持续学习系统初始化完成: {len(setup_results)}个动作")
            
            return result
            
        except Exception as e:
            logger.error(f"持续学习系统初始化失败: {e}")
            return {'success': False, 'error': str(e)}
    
    def _generate_comprehensive_report(self) -> dict:
        """生成综合训练报告"""
        logger.info("生成综合训练报告...")
        
        try:
            # 收集所有结果
            report_data = {
                'training_summary': self._create_training_summary(),
                'environment_info': self.results.get('environment_check', {}),
                'data_preparation': self.results.get('data_preparation', {}),
                'traditional_models': self.results.get('traditional_models', {}),
                'deep_learning_models': self.results.get('deep_learning_models', {}),
                'evaluation_results': self.results.get('evaluation', {}),
                'optimization_results': self.results.get('optimization', {}),
                'continuous_learning': self.results.get('continuous_learning', {}),
                'generation_time': datetime.now().isoformat()
            }
            
            # 生成报告文件
            report_filename = f"ai_training_report_{datetime.now().strftime('%Y%m%d_%H%M%S')}"
            
            # JSON格式报告
            json_path = Path(f"reports/training/{report_filename}.json")
            with open(json_path, 'w', encoding='utf-8') as f:
                json.dump(report_data, f, ensure_ascii=False, indent=2, default=str)
            
            # HTML格式报告
            if self.config['report_format'] == 'html':
                html_path = Path(f"reports/training/{report_filename}.html")
                html_content = self._generate_html_report(report_data)
                with open(html_path, 'w', encoding='utf-8') as f:
                    f.write(html_content)
            
            result = {
                'success': True,
                'report_files': {
                    'json': str(json_path),
                    'html': str(html_path) if self.config['report_format'] == 'html' else None
                },
                'report_data': report_data,
                'generation_time': datetime.now().isoformat()
            }
            
            logger.info(f"✅ 综合报告生成完成: {json_path}")
            
            return result
            
        except Exception as e:
            logger.error(f"报告生成失败: {e}")
            return {'success': False, 'error': str(e)}
    
    def _create_training_summary(self) -> dict:
        """创建训练摘要"""
        try:
            summary = {
                'training_duration': None,
                'total_models_trained': 0,
                'best_overall_accuracy': 0,
                'best_model': None,
                'data_samples': 0,
                'feature_dimensions': 0,
                'evaluation_performed': False,
                'optimization_performed': False,
                'continuous_learning_enabled': False
            }
            
            if self.training_start_time and self.training_end_time:
                duration = self.training_end_time - self.training_start_time
                summary['training_duration'] = str(duration)
            
            # 统计传统模型
            traditional = self.results.get('traditional_models', {})
            if traditional.get('success'):
                summary['total_models_trained'] += traditional.get('model_count', 0)
                
                # 更新最佳准确率
                for model_name, perf in traditional.get('performance_summary', {}).items():
                    accuracy = perf.get('accuracy', 0)
                    if accuracy > summary['best_overall_accuracy']:
                        summary['best_overall_accuracy'] = accuracy
                        summary['best_model'] = f"traditional_{model_name}"
            
            # 统计深度学习模型
            deep_learning = self.results.get('deep_learning_models', {})
            if deep_learning.get('success'):
                summary['total_models_trained'] += deep_learning.get('model_count', 0)
                
                # 更新最佳准确率
                for model_name, perf in deep_learning.get('performance_summary', {}).items():
                    accuracy = perf.get('accuracy', 0)
                    if accuracy > summary['best_overall_accuracy']:
                        summary['best_overall_accuracy'] = accuracy
                        summary['best_model'] = f"deep_learning_{model_name}"
            
            # 数据统计
            data_prep = self.results.get('data_preparation', {})
            if data_prep.get('success'):
                data_stats = data_prep.get('data_statistics', {})
                summary['data_samples'] = data_stats.get('tabular_samples', 0)
                summary['feature_dimensions'] = data_stats.get('tabular_features', 0)
            
            # 其他状态
            summary['evaluation_performed'] = self.results.get('evaluation', {}).get('success', False)
            summary['optimization_performed'] = self.results.get('optimization', {}).get('success', False)
            summary['continuous_learning_enabled'] = self.results.get('continuous_learning', {}).get('success', False)
            
            return summary
            
        except Exception as e:
            logger.error(f"创建训练摘要失败: {e}")
            return {}
    
    def _generate_html_report(self, report_data: dict) -> str:
        """生成HTML格式报告"""
        try:
            training_summary = report_data.get('training_summary', {})
            
            html_content = f"""
<!DOCTYPE html>
<html lang="zh-CN">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>AI大模型训练报告</title>
    <style>
        body {{ font-family: 'Microsoft YaHei', Arial, sans-serif; margin: 40px; background: #f5f5f5; }}
        .container {{ max-width: 1200px; margin: 0 auto; background: white; padding: 30px; border-radius: 10px; box-shadow: 0 2px 10px rgba(0,0,0,0.1); }}
        .header {{ text-align: center; border-bottom: 3px solid #007bff; padding-bottom: 20px; margin-bottom: 30px; }}
        .section {{ margin: 30px 0; padding: 20px; border-left: 4px solid #007bff; background: #f8f9fa; }}
        .metrics {{ display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 20px; margin: 20px 0; }}
        .metric {{ background: white; padding: 20px; border-radius: 8px; text-align: center; box-shadow: 0 2px 5px rgba(0,0,0,0.1); }}
        .metric-value {{ font-size: 2em; font-weight: bold; color: #007bff; }}
        .metric-label {{ color: #666; margin-top: 5px; }}
        .success {{ color: #28a745; }}
        .warning {{ color: #ffc107; }}
        .error {{ color: #dc3545; }}
        table {{ width: 100%; border-collapse: collapse; margin: 20px 0; }}
        th, td {{ padding: 12px; text-align: left; border-bottom: 1px solid #ddd; }}
        th {{ background-color: #007bff; color: white; }}
        .status-badge {{ padding: 4px 8px; border-radius: 4px; color: white; font-size: 0.8em; }}
        .status-success {{ background-color: #28a745; }}
        .status-error {{ background-color: #dc3545; }}
    </style>
</head>
<body>
    <div class="container">
        <div class="header">
            <h1>🚀 AI大模型股票预测系统</h1>
            <h2>训练报告</h2>
            <p>生成时间: {report_data['generation_time']}</p>
        </div>
        
        <div class="section">
            <h3>📊 训练摘要</h3>
            <div class="metrics">
                <div class="metric">
                    <div class="metric-value">{training_summary.get('total_models_trained', 0)}</div>
                    <div class="metric-label">训练模型数量</div>
                </div>
                <div class="metric">
                    <div class="metric-value">{training_summary.get('best_overall_accuracy', 0):.4f}</div>
                    <div class="metric-label">最佳准确率</div>
                </div>
                <div class="metric">
                    <div class="metric-value">{training_summary.get('data_samples', 0):,}</div>
                    <div class="metric-label">训练样本数</div>
                </div>
                <div class="metric">
                    <div class="metric-value">{training_summary.get('feature_dimensions', 0)}</div>
                    <div class="metric-label">特征维度</div>
                </div>
            </div>
            <p><strong>最佳模型:</strong> {training_summary.get('best_model', 'N/A')}</p>
            <p><strong>训练时长:</strong> {training_summary.get('training_duration', 'N/A')}</p>
        </div>
        
        <div class="section">
            <h3>🔧 环境状态</h3>
            {self._format_environment_status(report_data.get('environment_info', {}))}
        </div>
        
        <div class="section">
            <h3>🎯 传统机器学习模型</h3>
            {self._format_model_results(report_data.get('traditional_models', {}))}
        </div>
        
        <div class="section">
            <h3>🧠 深度学习模型</h3>
            {self._format_model_results(report_data.get('deep_learning_models', {}))}
        </div>
        
        <div class="section">
            <h3>📈 模型评估结果</h3>
            {self._format_evaluation_results(report_data.get('evaluation_results', {}))}
        </div>
        
        <div class="section">
            <h3>⚡ 模型优化结果</h3>
            {self._format_optimization_results(report_data.get('optimization_results', {}))}
        </div>
        
        <div class="section">
            <h3>🔄 持续学习系统</h3>
            {self._format_continuous_learning(report_data.get('continuous_learning', {}))}
        </div>
        
        <div class="section">
            <h3>💡 建议和下一步</h3>
            {self._format_recommendations(report_data)}
        </div>
    </div>
</body>
</html>
"""
            return html_content
            
        except Exception as e:
            logger.error(f"生成HTML报告失败: {e}")
            return f"<html><body><h1>报告生成失败: {e}</h1></body></html>"
    
    def _format_environment_status(self, env_info: dict) -> str:
        """格式化环境状态"""
        if not env_info.get('success'):
            return '<p class="error">环境检查失败</p>'
        
        env_status = env_info.get('environment_status', {})
        status_items = []
        
        for component, available in env_status.items():
            status_class = 'status-success' if available else 'status-error'
            status_text = '可用' if available else '不可用'
            status_items.append(f'<span class="status-badge {status_class}">{component}: {status_text}</span>')
        
        return f'<p>{" ".join(status_items)}</p>'
    
    def _format_model_results(self, model_results: dict) -> str:
        """格式化模型结果"""
        if not model_results.get('success'):
            return '<p class="error">模型训练失败</p>'
        
        performance = model_results.get('performance_summary', {})
        if not performance:
            return '<p>无性能数据</p>'
        
        table_rows = []
        for model_name, metrics in performance.items():
            accuracy = metrics.get('accuracy', 0)
            precision = metrics.get('precision', 0)
            recall = metrics.get('recall', 0)
            f1_score = metrics.get('f1_score', 0)
            
            table_rows.append(f"""
                <tr>
                    <td>{model_name}</td>
                    <td>{accuracy:.4f}</td>
                    <td>{precision:.4f}</td>
                    <td>{recall:.4f}</td>
                    <td>{f1_score:.4f}</td>
                </tr>
            """)
        
        return f"""
            <table>
                <thead>
                    <tr><th>模型</th><th>准确率</th><th>精确率</th><th>召回率</th><th>F1分数</th></tr>
                </thead>
                <tbody>{''.join(table_rows)}</tbody>
            </table>
        """
    
    def _format_evaluation_results(self, eval_results: dict) -> str:
        """格式化评估结果"""
        if not eval_results.get('success'):
            return '<p class="error">模型评估失败</p>'
        
        evaluated_count = eval_results.get('evaluated_predictions', 0)
        models_count = eval_results.get('models_evaluated', 0)
        
        return f"""
            <p>评估了 <strong>{evaluated_count}</strong> 个预测结果</p>
            <p>涉及 <strong>{models_count}</strong> 个模型</p>
        """
    
    def _format_optimization_results(self, opt_results: dict) -> str:
        """格式化优化结果"""
        if not opt_results.get('success'):
            return '<p class="error">模型优化失败</p>'
        
        actions = opt_results.get('optimization_actions', [])
        if not actions:
            return '<p>未执行优化动作</p>'
        
        action_list = '</li><li>'.join(actions)
        return f'<ul><li>{action_list}</li></ul>'
    
    def _format_continuous_learning(self, cl_results: dict) -> str:
        """格式化持续学习结果"""
        if not cl_results.get('success'):
            return '<p class="error">持续学习系统初始化失败</p>'
        
        setup_actions = cl_results.get('setup_actions', [])
        is_active = cl_results.get('continuous_learning_active', False)
        
        status_text = '运行中' if is_active else '已初始化但未启动'
        status_class = 'success' if is_active else 'warning'
        
        action_list = '</li><li>'.join(setup_actions) if setup_actions else '无'
        
        return f"""
            <p class="{status_class}">状态: {status_text}</p>
            <p>初始化动作:</p>
            <ul><li>{action_list}</li></ul>
        """
    
    def _format_recommendations(self, report_data: dict) -> str:
        """格式化建议"""
        recommendations = []
        
        # 基于训练结果的建议
        training_summary = report_data.get('training_summary', {})
        best_accuracy = training_summary.get('best_overall_accuracy', 0)
        
        if best_accuracy < 0.6:
            recommendations.append("模型准确率偏低，建议增加训练数据或优化特征工程")
        
        if training_summary.get('total_models_trained', 0) < 3:
            recommendations.append("训练的模型数量较少，建议启用更多模型类型")
        
        if not training_summary.get('continuous_learning_enabled', False):
            recommendations.append("建议启用持续学习系统以保持模型性能")
        
        # 基于评估结果的建议
        eval_results = report_data.get('evaluation_results', {})
        if eval_results.get('evaluated_predictions', 0) < 100:
            recommendations.append("评估样本较少，建议积累更多预测结果后再评估")
        
        if not recommendations:
            recommendations.append("系统运行良好，建议定期监控模型性能")
        
        rec_list = '</li><li>'.join(recommendations)
        return f'<ul><li>{rec_list}</li></ul>'
    
    def _finalize_results(self, success: bool, error: str = None) -> dict:
        """完成并返回最终结果"""
        self.training_end_time = datetime.now()
        
        if self.training_start_time:
            duration = self.training_end_time - self.training_start_time
        else:
            duration = timedelta(0)
        
        final_results = {
            'success': success,
            'error': error,
            'training_duration': str(duration),
            'start_time': self.training_start_time.isoformat() if self.training_start_time else None,
            'end_time': self.training_end_time.isoformat(),
            'config_used': self.config,
            'detailed_results': self.results
        }
        
        if success:
            logger.info(f"🎉 AI训练管道执行完成！总耗时: {duration}")
        else:
            logger.error(f"❌ AI训练管道执行失败: {error}")
        
        return final_results

def main():
    """主函数"""
    parser = argparse.ArgumentParser(description='AI大模型股票预测系统训练')
    parser.add_argument('--stock-limit', type=int, default=200, help='训练股票数量')
    parser.add_argument('--days-back', type=int, default=150, help='历史数据天数')
    parser.add_argument('--no-traditional', action='store_true', help='跳过传统模型训练')
    parser.add_argument('--no-deep', action='store_true', help='跳过深度学习模型训练')
    parser.add_argument('--no-eval', action='store_true', help='跳过模型评估')
    parser.add_argument('--no-opt', action='store_true', help='跳过模型优化')
    parser.add_argument('--start-continuous', action='store_true', help='启动持续学习')
    parser.add_argument('--config', type=str, help='配置文件路径')
    
    args = parser.parse_args()
    
    # 从命令行参数构建配置
    config = {
        'stock_limit': args.stock_limit,
        'days_back': args.days_back,
        'train_traditional': not args.no_traditional,
        'train_deep_learning': not args.no_deep,
        'enable_evaluation': not args.no_eval,
        'enable_optimization': not args.no_opt,
        'start_continuous_learning': args.start_continuous,
    }
    
    # 如果提供了配置文件，加载并合并
    if args.config and os.path.exists(args.config):
        try:
            with open(args.config, 'r', encoding='utf-8') as f:
                file_config = json.load(f)
            config.update(file_config)
            logger.info(f"加载配置文件: {args.config}")
        except Exception as e:
            logger.error(f"加载配置文件失败: {e}")
    
    # 创建并运行训练管道
    logger.info("启动AI大模型训练系统...")
    pipeline = AITrainingPipeline(config)
    
    try:
        results = pipeline.run_full_pipeline()
        
        # 输出简要结果
        print("\n" + "="*60)
        print("🎯 AI大模型训练完成！")
        print("="*60)
        
        if results['success']:
            print("✅ 训练状态: 成功")
            print(f"⏱️  总耗时: {results['training_duration']}")
            
            # 显示训练摘要
            detailed_results = results.get('detailed_results', {})
            
            # 传统模型
            traditional = detailed_results.get('traditional_models', {})
            if traditional.get('success'):
                print(f"🎯 传统模型: {traditional.get('model_count', 0)}个")
            
            # 深度学习模型
            deep_learning = detailed_results.get('deep_learning_models', {})
            if deep_learning.get('success'):
                print(f"🧠 深度学习模型: {deep_learning.get('model_count', 0)}个")
            
            # 评估结果
            evaluation = detailed_results.get('evaluation', {})
            if evaluation.get('success'):
                print(f"📊 评估预测: {evaluation.get('evaluated_predictions', 0)}个")
            
            # 优化结果
            optimization = detailed_results.get('optimization', {})
            if optimization.get('success'):
                opt_actions = len(optimization.get('optimization_actions', []))
                print(f"⚡ 优化动作: {opt_actions}个")
            
            # 报告文件
            report = detailed_results.get('report', {})
            if report.get('success'):
                json_file = report.get('report_files', {}).get('json')
                html_file = report.get('report_files', {}).get('html')
                print(f"📝 JSON报告: {json_file}")
                if html_file:
                    print(f"📄 HTML报告: {html_file}")
        else:
            print("❌ 训练状态: 失败")
            print(f"💥 错误信息: {results['error']}")
        
        print("="*60)
        
        return 0 if results['success'] else 1
        
    except KeyboardInterrupt:
        logger.info("用户中断训练")
        return 130
    except Exception as e:
        logger.error(f"训练过程发生异常: {e}")
        return 1

if __name__ == "__main__":
    exit(main())