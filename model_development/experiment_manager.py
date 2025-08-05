#!/usr/bin/env python3
"""
MLflow实验管理器
负责机器学习实验的跟踪、管理和模型版本控制
"""

import os
import json
import logging
import mlflow
import mlflow.sklearn
import mlflow.pytorch
from datetime import datetime
from typing import Dict, List, Optional, Any, Tuple
from pathlib import Path

# 尝试导入MLflow，如果不可用则使用模拟实现
try:
    from mlflow.tracking import MlflowClient
    from mlflow.entities import Run, Experiment
    MLFLOW_AVAILABLE = True
except ImportError:
    MLFLOW_AVAILABLE = False
    print("MLflow未安装，使用模拟实验管理器")

class ExperimentManager:
    """MLflow实验管理器"""
    
    def __init__(self, tracking_uri: str = None, experiment_name: str = "ljwx-stock-experiments"):
        """
        初始化实验管理器
        
        Args:
            tracking_uri: MLflow跟踪服务器URI，默认使用本地文件存储
            experiment_name: 实验名称
        """
        self.logger = logging.getLogger(__name__)
        self.experiment_name = experiment_name
        
        if MLFLOW_AVAILABLE:
            self._init_mlflow(tracking_uri)
        else:
            self._init_mock_mlflow()
    
    def _init_mlflow(self, tracking_uri: str = None):
        """初始化MLflow"""
        try:
            # 设置跟踪URI
            if tracking_uri:
                mlflow.set_tracking_uri(tracking_uri)
            else:
                # 使用本地文件存储
                tracking_dir = Path("mlruns").absolute()
                tracking_dir.mkdir(exist_ok=True)
                mlflow.set_tracking_uri(f"file://{tracking_dir}")
            
            # 初始化客户端
            self.client = MlflowClient()
            
            # 创建或获取实验
            try:
                self.experiment = self.client.get_experiment_by_name(self.experiment_name)
                if self.experiment is None:
                    experiment_id = self.client.create_experiment(
                        name=self.experiment_name,
                        tags={
                            "project": "ljwx-stock",
                            "version": "1.0",
                            "created_by": "ljwx-stock-system"
                        }
                    )
                    self.experiment = self.client.get_experiment(experiment_id)
            except Exception as e:
                self.logger.warning(f"获取实验失败，创建新实验: {e}")
                experiment_id = self.client.create_experiment(self.experiment_name)
                self.experiment = self.client.get_experiment(experiment_id)
            
            self.experiment_id = self.experiment.experiment_id
            mlflow.set_experiment(experiment_id=self.experiment_id)
            
            self.logger.info(f"MLflow实验管理器初始化成功: {self.experiment_name}")
            self.mlflow_enabled = True
            
        except Exception as e:
            self.logger.error(f"MLflow初始化失败: {e}")
            self._init_mock_mlflow()
    
    def _init_mock_mlflow(self):
        """初始化模拟MLflow实现"""
        self.mlflow_enabled = False
        self.experiment_id = "mock_experiment"
        self.client = None
        self.experiment = None
        
        # 创建本地存储目录
        self.mock_storage_dir = Path("mock_mlruns")
        self.mock_storage_dir.mkdir(exist_ok=True)
        
        self.logger.warning("使用模拟MLflow实验管理器")
    
    def create_experiment(self, name: str, description: str = "", tags: Dict[str, str] = None) -> str:
        """
        创建新实验
        
        Args:
            name: 实验名称
            description: 实验描述
            tags: 实验标签
            
        Returns:
            实验ID
        """
        try:
            if self.mlflow_enabled:
                # 使用MLflow创建实验
                try:
                    experiment_id = mlflow.create_experiment(
                        name=name,
                        artifact_location=None,
                        tags=tags or {}
                    )
                    self.logger.info(f"MLflow实验创建成功: {name} (ID: {experiment_id})")
                    return experiment_id
                except mlflow.exceptions.MlflowException as e:
                    if "already exists" in str(e):
                        # 如果实验已存在，获取现有实验ID
                        experiment = mlflow.get_experiment_by_name(name)
                        self.logger.info(f"实验已存在: {name} (ID: {experiment.experiment_id})")
                        return experiment.experiment_id
                    else:
                        raise e
            else:
                # 使用模拟实现
                experiment_id = f"mock_exp_{int(datetime.now().timestamp())}"
                experiment_data = {
                    "experiment_id": experiment_id,
                    "name": name,
                    "description": description,
                    "tags": tags or {},
                    "created_at": datetime.now().isoformat(),
                    "artifact_location": str(self.mock_storage_dir / experiment_id)
                }
                
                # 保存实验数据
                experiment_file = self.mock_storage_dir / f"experiment_{experiment_id}.json"
                with open(experiment_file, 'w', encoding='utf-8') as f:
                    json.dump(experiment_data, f, ensure_ascii=False, indent=2)
                
                self.logger.info(f"模拟实验创建成功: {name} (ID: {experiment_id})")
                return experiment_id
                
        except Exception as e:
            self.logger.error(f"创建实验失败: {e}")
            raise e
    
    def start_run(self, run_name: str = None, tags: Dict[str, str] = None) -> str:
        """
        开始一个新的实验运行
        
        Args:
            run_name: 运行名称
            tags: 运行标签
            
        Returns:
            运行ID
        """
        try:
            if self.mlflow_enabled:
                # 使用MLflow
                run = mlflow.start_run(
                    experiment_id=self.experiment_id,
                    run_name=run_name,
                    tags=tags or {}
                )
                run_id = run.info.run_id
                self.logger.info(f"MLflow运行已启动: {run_id}")
                return run_id
            else:
                # 使用模拟实现
                run_id = f"mock_run_{int(datetime.now().timestamp())}"
                run_data = {
                    "run_id": run_id,
                    "run_name": run_name,
                    "tags": tags or {},
                    "start_time": datetime.now().isoformat(),
                    "status": "RUNNING",
                    "params": {},
                    "metrics": {},
                    "artifacts": []
                }
                
                # 保存到本地文件
                run_file = self.mock_storage_dir / f"{run_id}.json"
                with open(run_file, 'w', encoding='utf-8') as f:
                    json.dump(run_data, f, ensure_ascii=False, indent=2)
                
                self.logger.info(f"模拟运行已启动: {run_id}")
                return run_id
                
        except Exception as e:
            self.logger.error(f"启动运行失败: {e}")
            raise e
    
    def log_params(self, params: Dict[str, Any], run_id: str = None):
        """
        记录实验参数
        
        Args:
            params: 参数字典
            run_id: 运行ID，如果为None则使用当前活跃运行
        """
        try:
            if self.mlflow_enabled:
                # 使用MLflow
                if run_id:
                    with mlflow.start_run(run_id=run_id):
                        mlflow.log_params(params)
                else:
                    mlflow.log_params(params)
            else:
                # 使用模拟实现
                if run_id:
                    run_file = self.mock_storage_dir / f"{run_id}.json"
                    if run_file.exists():
                        with open(run_file, 'r', encoding='utf-8') as f:
                            run_data = json.load(f)
                        
                        run_data["params"].update(params)
                        
                        with open(run_file, 'w', encoding='utf-8') as f:
                            json.dump(run_data, f, ensure_ascii=False, indent=2)
            
            self.logger.debug(f"参数已记录: {list(params.keys())}")
            
        except Exception as e:
            self.logger.error(f"记录参数失败: {e}")
    
    def log_metrics(self, metrics: Dict[str, float], step: int = None, run_id: str = None):
        """
        记录实验指标
        
        Args:
            metrics: 指标字典
            step: 步数（用于时间序列指标）
            run_id: 运行ID
        """
        try:
            if self.mlflow_enabled:
                # 使用MLflow
                if run_id:
                    with mlflow.start_run(run_id=run_id):
                        for key, value in metrics.items():
                            mlflow.log_metric(key, value, step=step)
                else:
                    for key, value in metrics.items():
                        mlflow.log_metric(key, value, step=step)
            else:
                # 使用模拟实现
                if run_id:
                    run_file = self.mock_storage_dir / f"{run_id}.json"
                    if run_file.exists():
                        with open(run_file, 'r', encoding='utf-8') as f:
                            run_data = json.load(f)
                        
                        for key, value in metrics.items():
                            if key not in run_data["metrics"]:
                                run_data["metrics"][key] = []
                            run_data["metrics"][key].append({
                                "value": value,
                                "step": step,
                                "timestamp": datetime.now().isoformat()
                            })
                        
                        with open(run_file, 'w', encoding='utf-8') as f:
                            json.dump(run_data, f, ensure_ascii=False, indent=2)
            
            self.logger.debug(f"指标已记录: {list(metrics.keys())}")
            
        except Exception as e:
            self.logger.error(f"记录指标失败: {e}")
    
    def log_model(self, model: Any, model_name: str, 
                  signature=None, input_example=None, run_id: str = None) -> str:
        """
        记录训练好的模型
        
        Args:
            model: 训练好的模型对象
            model_name: 模型名称
            signature: 模型签名
            input_example: 输入示例
            run_id: 运行ID
            
        Returns:
            模型URI
        """
        try:
            if self.mlflow_enabled:
                # 使用MLflow
                if run_id:
                    with mlflow.start_run(run_id=run_id):
                        # 根据模型类型选择合适的记录方法
                        if hasattr(model, 'fit'):  # sklearn类型模型
                            model_info = mlflow.sklearn.log_model(
                                model, 
                                model_name,
                                signature=signature,
                                input_example=input_example
                            )
                        else:
                            # 使用通用模型记录
                            model_info = mlflow.pyfunc.log_model(
                                model_name,
                                python_model=model,
                                signature=signature,
                                input_example=input_example
                            )
                        return model_info.model_uri
                else:
                    if hasattr(model, 'fit'):
                        model_info = mlflow.sklearn.log_model(model, model_name)
                    else:
                        model_info = mlflow.pyfunc.log_model(model_name, python_model=model)
                    return model_info.model_uri
            else:
                # 使用模拟实现
                model_uri = f"mock://models/{model_name}/{run_id or 'current'}"
                
                if run_id:
                    run_file = self.mock_storage_dir / f"{run_id}.json"
                    if run_file.exists():
                        with open(run_file, 'r', encoding='utf-8') as f:
                            run_data = json.load(f)
                        
                        run_data["artifacts"].append({
                            "name": model_name,
                            "type": "model",
                            "uri": model_uri,
                            "logged_at": datetime.now().isoformat()
                        })
                        
                        with open(run_file, 'w', encoding='utf-8') as f:
                            json.dump(run_data, f, ensure_ascii=False, indent=2)
                
                return model_uri
            
        except Exception as e:
            self.logger.error(f"记录模型失败: {e}")
            return f"error://model_logging_failed/{model_name}"
    
    def log_artifact(self, artifact_path: str, artifact_name: str = None, run_id: str = None):
        """
        记录工件（文件、图表等）
        
        Args:
            artifact_path: 工件文件路径
            artifact_name: 工件名称
            run_id: 运行ID
        """
        try:
            if self.mlflow_enabled:
                if run_id:
                    with mlflow.start_run(run_id=run_id):
                        mlflow.log_artifact(artifact_path, artifact_name)
                else:
                    mlflow.log_artifact(artifact_path, artifact_name)
            else:
                # 模拟实现：复制文件到mock存储目录
                if os.path.exists(artifact_path):
                    import shutil
                    dest_name = artifact_name or os.path.basename(artifact_path)
                    dest_path = self.mock_storage_dir / "artifacts" / dest_name
                    dest_path.parent.mkdir(exist_ok=True)
                    shutil.copy2(artifact_path, dest_path)
            
            self.logger.debug(f"工件已记录: {artifact_path}")
            
        except Exception as e:
            self.logger.error(f"记录工件失败: {e}")
    
    def end_run(self, run_id: str = None, status: str = "FINISHED"):
        """
        结束实验运行
        
        Args:
            run_id: 运行ID
            status: 运行状态
        """
        try:
            if self.mlflow_enabled:
                if run_id:
                    # 结束指定运行
                    self.client.set_terminated(run_id, status)
                else:
                    # 结束当前活跃运行
                    mlflow.end_run(status=status)
            else:
                # 模拟实现
                if run_id:
                    run_file = self.mock_storage_dir / f"{run_id}.json"
                    if run_file.exists():
                        with open(run_file, 'r', encoding='utf-8') as f:
                            run_data = json.load(f)
                        
                        run_data["status"] = status
                        run_data["end_time"] = datetime.now().isoformat()
                        
                        with open(run_file, 'w', encoding='utf-8') as f:
                            json.dump(run_data, f, ensure_ascii=False, indent=2)
            
            self.logger.info(f"运行已结束: {run_id or 'current'}")
            
        except Exception as e:
            self.logger.error(f"结束运行失败: {e}")
    
    def get_runs(self, limit: int = 100) -> List[Dict]:
        """
        获取实验运行列表
        
        Args:
            limit: 返回数量限制
            
        Returns:
            运行列表
        """
        try:
            if self.mlflow_enabled:
                runs = self.client.search_runs(
                    experiment_ids=[self.experiment_id],
                    max_results=limit
                )
                
                run_list = []
                for run in runs:
                    run_info = {
                        "run_id": run.info.run_id,
                        "run_name": run.data.tags.get("mlflow.runName", ""),
                        "status": run.info.status,
                        "start_time": run.info.start_time,
                        "end_time": run.info.end_time,
                        "params": run.data.params,
                        "metrics": run.data.metrics,
                        "tags": run.data.tags
                    }
                    run_list.append(run_info)
                
                return run_list
            else:
                # 模拟实现
                run_list = []
                for run_file in self.mock_storage_dir.glob("mock_run_*.json"):
                    try:
                        with open(run_file, 'r', encoding='utf-8') as f:
                            run_data = json.load(f)
                        run_list.append(run_data)
                    except:
                        continue
                
                # 按开始时间排序
                run_list.sort(key=lambda x: x.get("start_time", ""), reverse=True)
                return run_list[:limit]
                
        except Exception as e:
            self.logger.error(f"获取运行列表失败: {e}")
            return []
    
    def get_run(self, run_id: str) -> Optional[Dict]:
        """
        获取特定运行的详细信息
        
        Args:
            run_id: 运行ID
            
        Returns:
            运行详细信息
        """
        try:
            if self.mlflow_enabled:
                run = self.client.get_run(run_id)
                return {
                    "run_id": run.info.run_id,
                    "run_name": run.data.tags.get("mlflow.runName", ""),
                    "status": run.info.status,
                    "start_time": run.info.start_time,
                    "end_time": run.info.end_time,
                    "params": run.data.params,
                    "metrics": run.data.metrics,
                    "tags": run.data.tags,
                    "artifact_uri": run.info.artifact_uri
                }
            else:
                # 模拟实现
                run_file = self.mock_storage_dir / f"{run_id}.json"
                if run_file.exists():
                    with open(run_file, 'r', encoding='utf-8') as f:
                        return json.load(f)
                return None
                
        except Exception as e:
            self.logger.error(f"获取运行详情失败: {e}")
            return None
    
    def delete_run(self, run_id: str) -> bool:
        """
        删除实验运行
        
        Args:
            run_id: 运行ID
            
        Returns:
            是否成功删除
        """
        try:
            if self.mlflow_enabled:
                self.client.delete_run(run_id)
            else:
                # 模拟实现
                run_file = self.mock_storage_dir / f"{run_id}.json"
                if run_file.exists():
                    run_file.unlink()
                else:
                    return False
            
            self.logger.info(f"运行已删除: {run_id}")
            return True
            
        except Exception as e:
            self.logger.error(f"删除运行失败: {e}")
            return False
    
    def compare_runs(self, run_ids: List[str]) -> Dict:
        """
        比较多个实验运行
        
        Args:
            run_ids: 运行ID列表
            
        Returns:
            比较结果
        """
        try:
            comparison = {
                "runs": [],
                "metric_comparison": {}
            }
            
            all_metrics = set()
            for run_id in run_ids:
                run_data = self.get_run(run_id)
                if run_data:
                    comparison["runs"].append(run_data)
                    if "metrics" in run_data:
                        all_metrics.update(run_data["metrics"].keys())
            
            # 构建指标比较表
            for metric in all_metrics:
                comparison["metric_comparison"][metric] = {}
                for run_data in comparison["runs"]:
                    run_id = run_data["run_id"]
                    metric_value = run_data.get("metrics", {}).get(metric)
                    if isinstance(metric_value, list):
                        # 模拟实现中的指标格式
                        metric_value = metric_value[-1]["value"] if metric_value else None
                    comparison["metric_comparison"][metric][run_id] = metric_value
            
            return comparison
            
        except Exception as e:
            self.logger.error(f"比较运行失败: {e}")
            return {"runs": [], "metric_comparison": {}}
    
    def get_best_run(self, metric_name: str, ascending: bool = False) -> Optional[Dict]:
        """
        获取指定指标的最佳运行
        
        Args:
            metric_name: 指标名称
            ascending: 是否升序排列（True表示越小越好）
            
        Returns:
            最佳运行信息
        """
        try:
            runs = self.get_runs()
            best_run = None
            best_value = None
            
            for run in runs:
                metrics = run.get("metrics", {})
                if metric_name in metrics:
                    value = metrics[metric_name]
                    if isinstance(value, list):
                        # 模拟实现中的指标格式
                        value = value[-1]["value"] if value else None
                    
                    if value is not None:
                        if best_value is None:
                            best_value = value
                            best_run = run
                        elif (ascending and value < best_value) or (not ascending and value > best_value):
                            best_value = value
                            best_run = run
            
            return best_run
            
        except Exception as e:
            self.logger.error(f"获取最佳运行失败: {e}")
            return None
    
    def cleanup(self):
        """清理资源"""
        try:
            if self.mlflow_enabled:
                # MLflow不需要特殊清理
                pass
            else:
                # 清理模拟存储（可选）
                pass
            
            self.logger.info("实验管理器已清理")
            
        except Exception as e:
            self.logger.error(f"清理失败: {e}")

# 全局实验管理器实例
_experiment_manager = None

def get_experiment_manager() -> ExperimentManager:
    """获取全局实验管理器实例"""
    global _experiment_manager
    if _experiment_manager is None:
        _experiment_manager = ExperimentManager()
    return _experiment_manager