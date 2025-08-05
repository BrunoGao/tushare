#!/usr/bin/env python3
"""
Walk-forward分析器
实现时间序列数据的前向验证分析
"""

import logging
import numpy as np
import pandas as pd
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Any, Tuple, Callable
from dataclasses import dataclass
from pathlib import Path

# 尝试导入sklearn，如果不可用则使用简化实现
try:
    from sklearn.model_selection import TimeSeriesSplit
    from sklearn.metrics import mean_squared_error, mean_absolute_error, r2_score
    SKLEARN_AVAILABLE = True
except ImportError:
    SKLEARN_AVAILABLE = False
    print("scikit-learn未安装，使用简化Walk-forward分析")

@dataclass
class WalkForwardResult:
    """Walk-forward分析结果"""
    fold_results: List[Dict]
    aggregate_metrics: Dict[str, float]
    stability_metrics: Dict[str, float]
    time_series_metrics: Dict[str, List[float]]
    fold_count: int
    total_samples: int
    start_date: str
    end_date: str
    analysis_timestamp: str

class WalkForwardAnalysis:
    """Walk-forward分析器"""
    
    def __init__(self, 
                 n_splits: int = 5,
                 test_size: Optional[int] = None,
                 gap: int = 0,
                 max_train_size: Optional[int] = None):
        """
        初始化Walk-forward分析器
        
        Args:
            n_splits: 交叉验证折数
            test_size: 测试集大小（样本数或时间窗口）
            gap: 训练集和测试集之间的间隔
            max_train_size: 最大训练集大小
        """
        self.logger = logging.getLogger(__name__)
        self.n_splits = n_splits
        self.test_size = test_size
        self.gap = gap
        self.max_train_size = max_train_size
        
        if SKLEARN_AVAILABLE:
            self.tscv = TimeSeriesSplit(
                n_splits=n_splits,
                test_size=test_size,
                gap=gap,
                max_train_size=max_train_size
            )
        else:
            self.tscv = None
        
        self.logger.info(f"Walk-forward分析器初始化完成: {n_splits} 折")
    
    def validate(self, 
                 model: Any,
                 X: pd.DataFrame,
                 y: pd.Series,
                 dates: pd.Series = None,
                 scoring_functions: Dict[str, Callable] = None,
                 fit_params: Dict = None,
                 prediction_params: Dict = None) -> WalkForwardResult:
        """
        执行Walk-forward验证
        
        Args:
            model: 模型对象（需要有fit和predict方法）
            X: 特征数据
            y: 目标变量
            dates: 日期序列
            scoring_functions: 评分函数字典
            fit_params: 模型训练参数
            prediction_params: 预测参数
            
        Returns:
            Walk-forward分析结果
        """
        try:
            self.logger.info("开始执行Walk-forward验证")
            
            # 参数初始化
            if scoring_functions is None:
                scoring_functions = self._get_default_scoring_functions()
            if fit_params is None:
                fit_params = {}
            if prediction_params is None:
                prediction_params = {}
            
            # 确保数据索引对齐
            if dates is None:
                dates = pd.Series(range(len(X)), index=X.index)
            
            fold_results = []
            all_predictions = []
            all_actuals = []
            
            # 执行时间序列交叉验证
            splits = self._generate_splits(X, y) if not SKLEARN_AVAILABLE else self.tscv.split(X)
            
            for fold, (train_idx, test_idx) in enumerate(splits):
                fold_start_time = datetime.now()
                
                # 准备训练和测试数据
                X_train, X_test = X.iloc[train_idx], X.iloc[test_idx]
                y_train, y_test = y.iloc[train_idx], y.iloc[test_idx]
                
                # 获取日期信息
                train_dates = dates.iloc[train_idx]
                test_dates = dates.iloc[test_idx]
                
                try:
                    # 训练模型
                    if hasattr(model, 'fit'):
                        model.fit(X_train, y_train, **fit_params)
                    else:
                        # 对于不支持fit方法的模型，尝试其他训练方法
                        if hasattr(model, 'train'):
                            model.train(X_train, y_train, **fit_params)
                        else:
                            self.logger.warning(f"模型 {type(model)} 不支持训练方法")
                    
                    # 预测
                    if hasattr(model, 'predict'):
                        y_pred = model.predict(X_test, **prediction_params)
                    else:
                        self.logger.error(f"模型 {type(model)} 不支持预测方法")
                        continue
                    
                    # 计算指标
                    fold_metrics = {}
                    for metric_name, metric_func in scoring_functions.items():
                        try:
                            fold_metrics[metric_name] = metric_func(y_test, y_pred)
                        except Exception as e:
                            self.logger.warning(f"计算指标 {metric_name} 失败: {e}")
                            fold_metrics[metric_name] = np.nan
                    
                    # 记录fold结果
                    fold_result = {
                        'fold': fold,
                        'train_start': train_dates.iloc[0] if len(train_dates) > 0 else 'N/A',
                        'train_end': train_dates.iloc[-1] if len(train_dates) > 0 else 'N/A',
                        'test_start': test_dates.iloc[0] if len(test_dates) > 0 else 'N/A',
                        'test_end': test_dates.iloc[-1] if len(test_dates) > 0 else 'N/A',
                        'train_samples': len(train_idx),
                        'test_samples': len(test_idx),
                        'metrics': fold_metrics,
                        'execution_time': (datetime.now() - fold_start_time).total_seconds(),
                        'predictions_sample': y_pred[:5].tolist() if len(y_pred) > 0 else [],
                        'actuals_sample': y_test.iloc[:5].tolist() if len(y_test) > 0 else []
                    }
                    
                    fold_results.append(fold_result)
                    
                    # 收集所有预测和实际值用于总体分析
                    all_predictions.extend(y_pred)
                    all_actuals.extend(y_test.values)
                    
                    self.logger.info(f"Fold {fold} 完成: 训练 {len(train_idx)} 样本, 测试 {len(test_idx)} 样本")
                    
                except Exception as e:
                    self.logger.error(f"Fold {fold} 执行失败: {e}")
                    continue
            
            # 计算聚合指标
            aggregate_metrics = self._calculate_aggregate_metrics(fold_results, scoring_functions)
            
            # 计算稳定性指标
            stability_metrics = self._calculate_stability_metrics(fold_results)
            
            # 计算时间序列指标
            time_series_metrics = self._calculate_time_series_metrics(fold_results)
            
            # 创建结果对象
            result = WalkForwardResult(
                fold_results=fold_results,
                aggregate_metrics=aggregate_metrics,
                stability_metrics=stability_metrics,
                time_series_metrics=time_series_metrics,
                fold_count=len(fold_results),
                total_samples=len(X),
                start_date=str(dates.iloc[0]) if len(dates) > 0 else 'N/A',
                end_date=str(dates.iloc[-1]) if len(dates) > 0 else 'N/A',
                analysis_timestamp=datetime.now().isoformat()
            )
            
            self.logger.info(f"Walk-forward验证完成: {len(fold_results)} 折")
            return result
            
        except Exception as e:
            self.logger.error(f"Walk-forward验证失败: {e}")
            raise e
    
    def _generate_splits(self, X: pd.DataFrame, y: pd.Series) -> List[Tuple[List[int], List[int]]]:
        """
        为不支持sklearn的情况生成时间序列分割
        
        Args:
            X: 特征数据
            y: 目标变量
            
        Returns:
            分割索引列表
        """
        n_samples = len(X)
        test_size = self.test_size or max(1, n_samples // (self.n_splits + 1))
        
        splits = []
        for i in range(self.n_splits):
            # 计算测试集结束位置
            test_end = n_samples - i * test_size
            test_start = max(0, test_end - test_size)
            
            # 计算训练集结束位置（考虑gap）
            train_end = max(0, test_start - self.gap)
            
            # 计算训练集开始位置
            if self.max_train_size:
                train_start = max(0, train_end - self.max_train_size)
            else:
                train_start = 0
            
            # 确保有效的分割
            if train_start < train_end and test_start < test_end:
                train_idx = list(range(train_start, train_end))
                test_idx = list(range(test_start, test_end))
                splits.append((train_idx, test_idx))
        
        return list(reversed(splits))  # 按时间顺序返回
    
    def _get_default_scoring_functions(self) -> Dict[str, Callable]:
        """获取默认评分函数"""
        scoring_functions = {
            'mae': lambda y_true, y_pred: np.mean(np.abs(y_true - y_pred)),
            'mse': lambda y_true, y_pred: np.mean((y_true - y_pred) ** 2),
            'rmse': lambda y_true, y_pred: np.sqrt(np.mean((y_true - y_pred) ** 2)),
            'mape': lambda y_true, y_pred: np.mean(np.abs((y_true - y_pred) / (y_true + 1e-8))) * 100,
            'r2': lambda y_true, y_pred: 1 - np.sum((y_true - y_pred) ** 2) / np.sum((y_true - np.mean(y_true)) ** 2)
        }
        
        # 如果有sklearn，使用其实现
        if SKLEARN_AVAILABLE:
            scoring_functions.update({
                'sklearn_mae': mean_absolute_error,
                'sklearn_mse': mean_squared_error,
                'sklearn_r2': r2_score
            })
        
        return scoring_functions
    
    def _calculate_aggregate_metrics(self, fold_results: List[Dict], scoring_functions: Dict) -> Dict[str, float]:
        """计算聚合指标"""
        if not fold_results:
            return {}
        
        aggregate_metrics = {}
        metric_names = list(scoring_functions.keys())
        
        for metric_name in metric_names:
            metric_values = []
            for fold_result in fold_results:
                metric_value = fold_result['metrics'].get(metric_name)
                if metric_value is not None and not np.isnan(metric_value):
                    metric_values.append(metric_value)
            
            if metric_values:
                aggregate_metrics[f'{metric_name}_mean'] = np.mean(metric_values)
                aggregate_metrics[f'{metric_name}_std'] = np.std(metric_values)
                aggregate_metrics[f'{metric_name}_min'] = np.min(metric_values)
                aggregate_metrics[f'{metric_name}_max'] = np.max(metric_values)
                aggregate_metrics[f'{metric_name}_median'] = np.median(metric_values)
        
        return aggregate_metrics
    
    def _calculate_stability_metrics(self, fold_results: List[Dict]) -> Dict[str, float]:
        """计算模型稳定性指标"""
        if len(fold_results) < 2:
            return {}
        
        stability_metrics = {}
        
        # 计算各指标的稳定性
        metric_names = set()
        for fold_result in fold_results:
            metric_names.update(fold_result['metrics'].keys())
        
        for metric_name in metric_names:
            values = []
            for fold_result in fold_results:
                value = fold_result['metrics'].get(metric_name)
                if value is not None and not np.isnan(value):
                    values.append(value)
            
            if len(values) >= 2:
                # 变异系数（CV）- 相对稳定性指标
                mean_val = np.mean(values)
                std_val = np.std(values)
                if mean_val != 0:
                    stability_metrics[f'{metric_name}_cv'] = std_val / abs(mean_val)
                
                # 最大最小比值
                min_val = np.min(values)
                max_val = np.max(values)
                if min_val != 0:
                    stability_metrics[f'{metric_name}_max_min_ratio'] = max_val / min_val
        
        # 整体稳定性评分
        if fold_results:
            execution_times = [fr['execution_time'] for fr in fold_results if 'execution_time' in fr]
            if execution_times:
                stability_metrics['execution_time_mean'] = np.mean(execution_times)
                stability_metrics['execution_time_std'] = np.std(execution_times)
        
        return stability_metrics
    
    def _calculate_time_series_metrics(self, fold_results: List[Dict]) -> Dict[str, List[float]]:
        """计算时间序列特定指标"""
        time_series_metrics = {}
        
        if not fold_results:
            return time_series_metrics
        
        # 按时间顺序的指标变化
        metric_names = set()
        for fold_result in fold_results:
            metric_names.update(fold_result['metrics'].keys())
        
        for metric_name in metric_names:
            metric_values = []
            for fold_result in fold_results:
                value = fold_result['metrics'].get(metric_name)
                if value is not None and not np.isnan(value):
                    metric_values.append(value)
                else:
                    metric_values.append(None)
            time_series_metrics[metric_name] = metric_values
        
        # 训练集和测试集大小变化
        train_sizes = [fr['train_samples'] for fr in fold_results]
        test_sizes = [fr['test_samples'] for fr in fold_results]
        
        time_series_metrics['train_sizes'] = train_sizes
        time_series_metrics['test_sizes'] = test_sizes
        
        return time_series_metrics
    
    def analyze_model_degradation(self, result: WalkForwardResult) -> Dict[str, Any]:
        """
        分析模型在时间序列上的性能退化
        
        Args:
            result: Walk-forward分析结果
            
        Returns:
            性能退化分析结果
        """
        try:
            degradation_analysis = {}
            
            # 分析主要指标的时间趋势
            for metric_name, values in result.time_series_metrics.items():
                if isinstance(values, list) and len(values) > 1:
                    clean_values = [v for v in values if v is not None and not (isinstance(v, float) and np.isnan(v))]
                    
                    if len(clean_values) >= 2:
                        # 计算趋势
                        x = np.arange(len(clean_values))
                        trend = np.polyfit(x, clean_values, 1)[0]  # 线性趋势斜率
                        
                        # 计算性能变化
                        first_half = clean_values[:len(clean_values)//2]
                        second_half = clean_values[len(clean_values)//2:]
                        
                        if first_half and second_half:
                            first_half_mean = np.mean(first_half)
                            second_half_mean = np.mean(second_half)
                            
                            degradation_analysis[metric_name] = {
                                'trend_slope': trend,
                                'first_half_mean': first_half_mean,
                                'second_half_mean': second_half_mean,
                                'performance_change': second_half_mean - first_half_mean,
                                'relative_change': (second_half_mean - first_half_mean) / first_half_mean if first_half_mean != 0 else 0
                            }
            
            return degradation_analysis
            
        except Exception as e:
            self.logger.error(f"性能退化分析失败: {e}")
            return {}
    
    def generate_report(self, result: WalkForwardResult, save_path: str = None) -> str:
        """
        生成Walk-forward分析报告
        
        Args:
            result: 分析结果
            save_path: 报告保存路径
            
        Returns:
            报告文件路径
        """
        try:
            if save_path is None:
                save_path = f"walk_forward_report_{datetime.now().strftime('%Y%m%d_%H%M%S')}.txt"
            
            # 生成性能退化分析
            degradation_analysis = self.analyze_model_degradation(result)
            
            report_lines = []
            report_lines.append("=" * 80)
            report_lines.append("Walk-Forward 分析报告")
            report_lines.append("=" * 80)
            report_lines.append(f"分析时间: {result.analysis_timestamp}")
            report_lines.append(f"数据范围: {result.start_date} ~ {result.end_date}")
            report_lines.append(f"总样本数: {result.total_samples}")
            report_lines.append(f"交叉验证折数: {result.fold_count}")
            report_lines.append("")
            
            # 聚合指标
            report_lines.append("聚合性能指标:")
            report_lines.append("-" * 40)
            for metric_name, value in result.aggregate_metrics.items():
                report_lines.append(f"{metric_name}: {value:.6f}")
            report_lines.append("")
            
            # 稳定性指标
            if result.stability_metrics:
                report_lines.append("模型稳定性指标:")
                report_lines.append("-" * 40)
                for metric_name, value in result.stability_metrics.items():
                    report_lines.append(f"{metric_name}: {value:.6f}")
                report_lines.append("")
            
            # 各折详细结果
            report_lines.append("各折详细结果:")
            report_lines.append("-" * 40)
            for fold_result in result.fold_results:
                report_lines.append(f"Fold {fold_result['fold']}:")
                report_lines.append(f"  训练期: {fold_result['train_start']} ~ {fold_result['train_end']} ({fold_result['train_samples']} 样本)")
                report_lines.append(f"  测试期: {fold_result['test_start']} ~ {fold_result['test_end']} ({fold_result['test_samples']} 样本)")
                report_lines.append(f"  执行时间: {fold_result['execution_time']:.2f} 秒")
                report_lines.append("  指标:")
                for metric_name, metric_value in fold_result['metrics'].items():
                    report_lines.append(f"    {metric_name}: {metric_value:.6f}")
                report_lines.append("")
            
            # 性能退化分析
            if degradation_analysis:
                report_lines.append("性能退化分析:")
                report_lines.append("-" * 40)
                for metric_name, analysis in degradation_analysis.items():
                    report_lines.append(f"{metric_name}:")
                    report_lines.append(f"  趋势斜率: {analysis['trend_slope']:.6f}")
                    report_lines.append(f"  前半段均值: {analysis['first_half_mean']:.6f}")
                    report_lines.append(f"  后半段均值: {analysis['second_half_mean']:.6f}")
                    report_lines.append(f"  性能变化: {analysis['performance_change']:.6f}")
                    report_lines.append(f"  相对变化: {analysis['relative_change']:.2%}")
                    report_lines.append("")
            
            # 写入文件
            with open(save_path, 'w', encoding='utf-8') as f:
                f.write('\n'.join(report_lines))
            
            self.logger.info(f"Walk-forward分析报告已生成: {save_path}")
            return save_path
            
        except Exception as e:
            self.logger.error(f"生成报告失败: {e}")
            return ""
    
    def compare_models(self, 
                      models: Dict[str, Any],
                      X: pd.DataFrame,
                      y: pd.Series,
                      dates: pd.Series = None) -> Dict[str, WalkForwardResult]:
        """
        比较多个模型的Walk-forward性能
        
        Args:
            models: 模型字典 {模型名称: 模型对象}
            X: 特征数据
            y: 目标变量
            dates: 日期序列
            
        Returns:
            各模型的分析结果
        """
        try:
            results = {}
            
            for model_name, model in models.items():
                self.logger.info(f"开始分析模型: {model_name}")
                try:
                    result = self.validate(model, X, y, dates)
                    results[model_name] = result
                    self.logger.info(f"模型 {model_name} 分析完成")
                except Exception as e:
                    self.logger.error(f"模型 {model_name} 分析失败: {e}")
                    continue
            
            return results
            
        except Exception as e:
            self.logger.error(f"模型比较失败: {e}")
            return {}

def create_sample_model():
    """创建示例模型用于测试"""
    class SimpleLinearModel:
        def __init__(self):
            self.coef_ = None
            self.intercept_ = None
        
        def fit(self, X, y):
            # 简单线性回归
            X_mean = X.mean()
            y_mean = y.mean()
            
            numerator = ((X - X_mean) * (y - y_mean)).sum().sum()
            denominator = ((X - X_mean) ** 2).sum().sum()
            
            if denominator != 0:
                self.coef_ = numerator / denominator
                self.intercept_ = y_mean - self.coef_ * X_mean.mean()
            else:
                self.coef_ = 0
                self.intercept_ = y_mean
        
        def predict(self, X):
            if self.coef_ is None:
                return np.zeros(len(X))
            return self.coef_ * X.mean(axis=1) + self.intercept_
    
    return SimpleLinearModel()