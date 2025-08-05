#!/usr/bin/env python3
"""
超参数优化器
集成Optuna进行系统化的超参数调优
"""

import os
import json
import logging
import numpy as np
from datetime import datetime
from typing import Dict, List, Optional, Any, Callable, Tuple
from pathlib import Path

# 尝试导入Optuna，如果不可用则使用模拟实现
try:
    import optuna
    from optuna import Trial, Study
    from optuna.storages import InMemoryStorage
    from optuna.samplers import TPESampler, RandomSampler
    from optuna.pruners import MedianPruner, HyperbandPruner
    OPTUNA_AVAILABLE = True
except ImportError:
    OPTUNA_AVAILABLE = False
    print("Optuna未安装，使用模拟超参数优化器")

from .experiment_manager import get_experiment_manager

class HyperparameterOptimizer:
    """超参数优化器"""
    
    def __init__(self, 
                 study_name: str = "ljwx-stock-hyperopt",
                 direction: str = "maximize",
                 sampler: str = "tpe",
                 pruner: str = "median",
                 storage_path: str = None):
        """
        初始化超参数优化器
        
        Args:
            study_name: 研究名称
            direction: 优化方向 ("maximize" 或 "minimize")
            sampler: 采样器类型 ("tpe", "random")
            pruner: 剪枝器类型 ("median", "hyperband")
            storage_path: 存储路径
        """
        self.logger = logging.getLogger(__name__)
        self.study_name = study_name
        self.direction = direction
        
        # 初始化存储路径
        if storage_path:
            self.storage_path = Path(storage_path)
        else:
            self.storage_path = Path("hyperopt_storage")
        self.storage_path.mkdir(exist_ok=True)
        
        # 获取实验管理器
        self.experiment_manager = get_experiment_manager()
        
        if OPTUNA_AVAILABLE:
            self._init_optuna(sampler, pruner)
        else:
            self._init_mock_optuna()
    
    def _init_optuna(self, sampler: str, pruner: str):
        """初始化Optuna"""
        try:
            # 配置采样器
            if sampler == "tpe":
                sampler_obj = TPESampler()
            elif sampler == "random":
                sampler_obj = RandomSampler()
            else:
                sampler_obj = TPESampler()
            
            # 配置剪枝器
            if pruner == "median":
                pruner_obj = MedianPruner()
            elif pruner == "hyperband":
                pruner_obj = HyperbandPruner()
            else:
                pruner_obj = MedianPruner()
            
            # 创建或加载研究
            storage_file = self.storage_path / f"{self.study_name}.db"
            storage_url = f"sqlite:///{storage_file}"
            
            self.study = optuna.create_study(
                study_name=self.study_name,
                direction=self.direction,
                sampler=sampler_obj,
                pruner=pruner_obj,
                storage=storage_url,
                load_if_exists=True
            )
            
            self.optuna_enabled = True
            self.logger.info(f"Optuna超参数优化器初始化成功: {self.study_name}")
            
        except Exception as e:
            self.logger.error(f"Optuna初始化失败: {e}")
            self._init_mock_optuna()
    
    def _init_mock_optuna(self):
        """初始化模拟Optuna实现"""
        self.optuna_enabled = False
        self.study = None
        self.trial_results = []
        self.best_params = {}
        self.best_value = None
        
        # 创建模拟存储文件
        self.mock_storage_file = self.storage_path / f"{self.study_name}_mock.json"
        
        self.logger.warning("使用模拟超参数优化器")
    
    def suggest_params(self, param_space: Dict[str, Dict]) -> Dict[str, Any]:
        """
        建议超参数
        
        Args:
            param_space: 参数空间定义
            
        Returns:
            建议的参数组合
        """
        if self.optuna_enabled:
            # 这个方法在真实的objective函数中使用
            raise NotImplementedError("请在objective函数中使用trial.suggest_*方法")
        else:
            # 模拟实现：随机采样
            suggested_params = {}
            for param_name, param_config in param_space.items():
                param_type = param_config.get("type")
                
                if param_type == "float":
                    low = param_config.get("low", 0.0)
                    high = param_config.get("high", 1.0)
                    log = param_config.get("log", False)
                    
                    if log:
                        suggested_params[param_name] = np.exp(np.random.uniform(np.log(low), np.log(high)))
                    else:
                        suggested_params[param_name] = np.random.uniform(low, high)
                
                elif param_type == "int":
                    low = param_config.get("low", 0)
                    high = param_config.get("high", 100)
                    suggested_params[param_name] = np.random.randint(low, high + 1)
                
                elif param_type == "categorical":
                    choices = param_config.get("choices", [])
                    if choices:
                        suggested_params[param_name] = np.random.choice(choices)
                
                elif param_type == "bool":
                    suggested_params[param_name] = np.random.choice([True, False])
            
            return suggested_params
    
    def optimize(self, 
                objective: Callable,
                param_space: Dict[str, Dict],
                n_trials: int = 100,
                timeout: int = None,
                callbacks: List[Callable] = None) -> Dict:
        """
        执行超参数优化
        
        Args:
            objective: 目标函数
            param_space: 参数空间定义
            n_trials: 试验次数
            timeout: 超时时间（秒）
            callbacks: 回调函数列表
            
        Returns:
            优化结果
        """
        try:
            if self.optuna_enabled:
                # 使用Optuna优化
                def optuna_objective(trial):
                    # 根据参数空间建议参数
                    params = {}
                    for param_name, param_config in param_space.items():
                        param_type = param_config.get("type")
                        
                        if param_type == "float":
                            low = param_config.get("low", 0.0)
                            high = param_config.get("high", 1.0)
                            log = param_config.get("log", False)
                            params[param_name] = trial.suggest_float(param_name, low, high, log=log)
                        
                        elif param_type == "int":
                            low = param_config.get("low", 0)
                            high = param_config.get("high", 100)
                            params[param_name] = trial.suggest_int(param_name, low, high)
                        
                        elif param_type == "categorical":
                            choices = param_config.get("choices", [])
                            params[param_name] = trial.suggest_categorical(param_name, choices)
                        
                        elif param_type == "bool":
                            params[param_name] = trial.suggest_categorical(param_name, [True, False])
                    
                    # 启动MLflow运行
                    run_name = f"trial_{trial.number}"
                    run_id = self.experiment_manager.start_run(
                        run_name=run_name,
                        tags={
                            "optuna_study": self.study_name,
                            "trial_number": str(trial.number)
                        }
                    )
                    
                    try:
                        # 记录参数
                        self.experiment_manager.log_params(params, run_id)
                        
                        # 调用目标函数
                        result = objective(params, trial)
                        
                        # 记录结果
                        if isinstance(result, dict):
                            metrics = result
                            objective_value = result.get("objective", 0.0)
                        else:
                            objective_value = float(result)
                            metrics = {"objective": objective_value}
                        
                        self.experiment_manager.log_metrics(metrics, run_id=run_id)
                        
                        # 结束运行
                        self.experiment_manager.end_run(run_id, "FINISHED")
                        
                        return objective_value
                        
                    except Exception as e:
                        self.experiment_manager.end_run(run_id, "FAILED")
                        self.logger.error(f"目标函数执行失败: {e}")
                        raise e
                
                # 执行优化
                self.study.optimize(
                    optuna_objective,
                    n_trials=n_trials,
                    timeout=timeout,
                    callbacks=callbacks
                )
                
                # 获取最佳结果
                best_trial = self.study.best_trial
                
                result = {
                    "best_params": best_trial.params,
                    "best_value": best_trial.value,
                    "best_trial_number": best_trial.number,
                    "n_trials": len(self.study.trials),
                    "study_name": self.study_name,
                    "direction": self.direction
                }
                
            else:
                # 使用模拟实现
                result = self._mock_optimize(objective, param_space, n_trials)
            
            self.logger.info(f"超参数优化完成: {result['best_value']}")
            return result
            
        except Exception as e:
            self.logger.error(f"超参数优化失败: {e}")
            raise e
    
    def _mock_optimize(self, objective: Callable, param_space: Dict[str, Dict], n_trials: int) -> Dict:
        """模拟超参数优化"""
        best_value = None
        best_params = None
        best_trial_number = 0
        
        for trial_number in range(n_trials):
            try:
                # 生成参数
                params = self.suggest_params(param_space)
                
                # 启动MLflow运行
                run_name = f"mock_trial_{trial_number}"
                run_id = self.experiment_manager.start_run(
                    run_name=run_name,
                    tags={
                        "mock_study": self.study_name,
                        "trial_number": str(trial_number)
                    }
                )
                
                # 记录参数
                self.experiment_manager.log_params(params, run_id)
                
                # 调用目标函数
                result = objective(params, None)
                
                # 处理结果
                if isinstance(result, dict):
                    objective_value = result.get("objective", 0.0)
                    metrics = result
                else:
                    objective_value = float(result)
                    metrics = {"objective": objective_value}
                
                # 记录指标
                self.experiment_manager.log_metrics(metrics, run_id=run_id)
                
                # 更新最佳结果
                if best_value is None:
                    best_value = objective_value
                    best_params = params
                    best_trial_number = trial_number
                else:
                    if self.direction == "maximize" and objective_value > best_value:
                        best_value = objective_value
                        best_params = params
                        best_trial_number = trial_number
                    elif self.direction == "minimize" and objective_value < best_value:
                        best_value = objective_value
                        best_params = params
                        best_trial_number = trial_number
                
                # 结束运行
                self.experiment_manager.end_run(run_id, "FINISHED")
                
                # 保存试验结果
                self.trial_results.append({
                    "trial_number": trial_number,
                    "params": params,
                    "value": objective_value,
                    "metrics": metrics
                })
                
            except Exception as e:
                self.logger.error(f"试验 {trial_number} 失败: {e}")
                self.experiment_manager.end_run(run_id, "FAILED")
                continue
        
        # 保存模拟结果
        mock_result = {
            "best_params": best_params,
            "best_value": best_value,
            "best_trial_number": best_trial_number,
            "trials": self.trial_results,
            "n_trials": len(self.trial_results)
        }
        
        with open(self.mock_storage_file, 'w', encoding='utf-8') as f:
            json.dump(mock_result, f, ensure_ascii=False, indent=2)
        
        return {
            "best_params": best_params or {},
            "best_value": best_value or 0.0,
            "best_trial_number": best_trial_number,
            "n_trials": len(self.trial_results),
            "study_name": self.study_name,
            "direction": self.direction
        }
    
    def get_trials(self) -> List[Dict]:
        """
        获取所有试验结果
        
        Returns:
            试验结果列表
        """
        try:
            if self.optuna_enabled:
                trials = []
                for trial in self.study.trials:
                    trial_data = {
                        "trial_number": trial.number,
                        "params": trial.params,
                        "value": trial.value,
                        "state": trial.state.name,
                        "datetime_start": trial.datetime_start.isoformat() if trial.datetime_start else None,
                        "datetime_complete": trial.datetime_complete.isoformat() if trial.datetime_complete else None,
                        "duration": (trial.datetime_complete - trial.datetime_start).total_seconds() if trial.datetime_complete and trial.datetime_start else None
                    }
                    trials.append(trial_data)
                return trials
            else:
                # 模拟实现
                return self.trial_results
                
        except Exception as e:
            self.logger.error(f"获取试验结果失败: {e}")
            return []
    
    def get_best_params(self) -> Dict[str, Any]:
        """
        获取最佳参数
        
        Returns:
            最佳参数组合
        """
        try:
            if self.optuna_enabled:
                return self.study.best_params
            else:
                return self.best_params or {}
                
        except Exception as e:
            self.logger.error(f"获取最佳参数失败: {e}")
            return {}
    
    def get_best_value(self) -> float:
        """
        获取最佳目标值
        
        Returns:
            最佳目标值
        """
        try:
            if self.optuna_enabled:
                return self.study.best_value
            else:
                return self.best_value or 0.0
                
        except Exception as e:
            self.logger.error(f"获取最佳值失败: {e}")
            return 0.0
    
    def get_study_statistics(self) -> Dict:
        """
        获取研究统计信息
        
        Returns:
            统计信息
        """
        try:
            trials = self.get_trials()
            
            if not trials:
                return {}
            
            values = [t.get("value") for t in trials if t.get("value") is not None]
            
            stats = {
                "n_trials": len(trials),
                "n_complete_trials": len(values),
                "best_value": self.get_best_value(),
                "best_params": self.get_best_params()
            }
            
            if values:
                stats.update({
                    "mean_value": np.mean(values),
                    "std_value": np.std(values),
                    "min_value": np.min(values),
                    "max_value": np.max(values),
                    "median_value": np.median(values)
                })
            
            return stats
            
        except Exception as e:
            self.logger.error(f"获取统计信息失败: {e}")
            return {}
    
    def plot_optimization_history(self, save_path: str = None) -> str:
        """
        绘制优化历史
        
        Args:
            save_path: 保存路径
            
        Returns:
            图表文件路径
        """
        try:
            if not self.optuna_enabled:
                # 模拟实现：生成简单的文本报告
                if not save_path:
                    save_path = self.storage_path / f"{self.study_name}_history.txt"
                
                trials = self.get_trials()
                with open(save_path, 'w', encoding='utf-8') as f:
                    f.write(f"优化历史报告 - {self.study_name}\n")
                    f.write("=" * 50 + "\n\n")
                    
                    for trial in trials:
                        f.write(f"试验 {trial.get('trial_number', 'N/A')}: ")
                        f.write(f"值={trial.get('value', 'N/A'):.4f}, ")
                        f.write(f"参数={trial.get('params', {})}\n")
                
                return str(save_path)
            
            # 使用Optuna绘图功能
            try:
                import optuna.visualization as vis
                import plotly.graph_objects as go
                
                fig = vis.plot_optimization_history(self.study)
                
                if not save_path:
                    save_path = self.storage_path / f"{self.study_name}_history.html"
                
                fig.write_html(save_path)
                return str(save_path)
                
            except ImportError:
                # 如果没有plotly，创建简单的文本报告
                if not save_path:
                    save_path = self.storage_path / f"{self.study_name}_history.txt"
                
                with open(save_path, 'w', encoding='utf-8') as f:
                    f.write(f"优化历史报告 - {self.study_name}\n")
                    f.write("=" * 50 + "\n\n")
                    f.write(f"最佳值: {self.get_best_value()}\n")
                    f.write(f"最佳参数: {self.get_best_params()}\n\n")
                    
                    stats = self.get_study_statistics()
                    for key, value in stats.items():
                        f.write(f"{key}: {value}\n")
                
                return str(save_path)
                
        except Exception as e:
            self.logger.error(f"绘制优化历史失败: {e}")
            return ""
    
    def save_study(self, save_path: str = None) -> str:
        """
        保存研究结果
        
        Args:
            save_path: 保存路径
            
        Returns:
            保存的文件路径
        """
        try:
            if not save_path:
                save_path = self.storage_path / f"{self.study_name}_results.json"
            
            results = {
                "study_name": self.study_name,
                "direction": self.direction,
                "best_params": self.get_best_params(),
                "best_value": self.get_best_value(),
                "statistics": self.get_study_statistics(),
                "trials": self.get_trials(),
                "saved_at": datetime.now().isoformat()
            }
            
            with open(save_path, 'w', encoding='utf-8') as f:
                json.dump(results, f, ensure_ascii=False, indent=2)
            
            self.logger.info(f"研究结果已保存: {save_path}")
            return str(save_path)
            
        except Exception as e:
            self.logger.error(f"保存研究结果失败: {e}")
            return ""

def create_param_space_for_sklearn_model(model_type: str = "random_forest") -> Dict[str, Dict]:
    """
    为sklearn模型创建参数空间
    
    Args:
        model_type: 模型类型
        
    Returns:
        参数空间定义
    """
    if model_type == "random_forest":
        return {
            "n_estimators": {"type": "int", "low": 10, "high": 200},
            "max_depth": {"type": "int", "low": 3, "high": 20},
            "min_samples_split": {"type": "int", "low": 2, "high": 20},
            "min_samples_leaf": {"type": "int", "low": 1, "high": 10},
            "max_features": {"type": "categorical", "choices": ["sqrt", "log2", None]},
            "bootstrap": {"type": "bool"}
        }
    elif model_type == "svm":
        return {
            "C": {"type": "float", "low": 0.001, "high": 100, "log": True},
            "gamma": {"type": "categorical", "choices": ["scale", "auto"]},
            "kernel": {"type": "categorical", "choices": ["rbf", "poly", "sigmoid"]}
        }
    elif model_type == "logistic_regression":
        return {
            "C": {"type": "float", "low": 0.001, "high": 100, "log": True},
            "penalty": {"type": "categorical", "choices": ["l1", "l2", "elasticnet", None]},
            "solver": {"type": "categorical", "choices": ["liblinear", "saga", "lbfgs"]},
            "max_iter": {"type": "int", "low": 100, "high": 1000}
        }
    else:
        # 通用参数空间
        return {
            "learning_rate": {"type": "float", "low": 0.001, "high": 0.3, "log": True},
            "batch_size": {"type": "categorical", "choices": [16, 32, 64, 128]},
            "epochs": {"type": "int", "low": 10, "high": 100},
            "dropout": {"type": "float", "low": 0.1, "high": 0.5}
        }

def create_param_space_for_deep_learning() -> Dict[str, Dict]:
    """
    为深度学习模型创建参数空间
    
    Returns:
        参数空间定义
    """
    return {
        "learning_rate": {"type": "float", "low": 1e-5, "high": 1e-1, "log": True},
        "batch_size": {"type": "categorical", "choices": [16, 32, 64, 128, 256]},
        "epochs": {"type": "int", "low": 10, "high": 200},
        "hidden_size": {"type": "int", "low": 32, "high": 512},
        "num_layers": {"type": "int", "low": 1, "high": 5},
        "dropout": {"type": "float", "low": 0.0, "high": 0.5},
        "weight_decay": {"type": "float", "low": 1e-6, "high": 1e-2, "log": True},
        "optimizer": {"type": "categorical", "choices": ["adam", "sgd", "rmsprop"]},
        "activation": {"type": "categorical", "choices": ["relu", "tanh", "sigmoid", "gelu"]}
    }