#!/usr/bin/env python3
"""
模型注册表
负责模型版本控制、元数据管理和生产部署管理
"""

import os
import json
import logging
import hashlib
import shutil
import pickle
from datetime import datetime
from typing import Dict, List, Optional, Any, Tuple
from pathlib import Path
from dataclasses import dataclass, asdict
from enum import Enum

class ModelStage(Enum):
    """模型阶段枚举"""
    STAGING = "staging"           # 暂存
    PRODUCTION = "production"     # 生产
    ARCHIVED = "archived"         # 归档
    DEPRECATED = "deprecated"     # 废弃

@dataclass
class ModelMetadata:
    """模型元数据"""
    name: str
    version: str
    stage: ModelStage
    created_at: str
    created_by: str
    description: str
    model_type: str
    framework: str
    metrics: Dict[str, float]
    parameters: Dict[str, Any]
    dataset_hash: str
    model_size: int
    tags: Dict[str, str]
    artifacts: List[str]
    parent_run_id: Optional[str] = None
    updated_at: Optional[str] = None
    promoted_at: Optional[str] = None
    promoted_by: Optional[str] = None

class ModelRegistry:
    """模型注册表"""
    
    def __init__(self, registry_path: str = "model_registry"):
        """
        初始化模型注册表
        
        Args:
            registry_path: 注册表存储路径
        """
        self.logger = logging.getLogger(__name__)
        self.registry_path = Path(registry_path)
        self.models_path = self.registry_path / "models"
        self.metadata_path = self.registry_path / "metadata"
        
        # 创建必要的目录
        self.registry_path.mkdir(exist_ok=True)
        self.models_path.mkdir(exist_ok=True)
        self.metadata_path.mkdir(exist_ok=True)
        
        # 初始化注册表索引
        self.index_file = self.registry_path / "index.json"
        self._init_index()
        
        self.logger.info(f"模型注册表初始化完成: {self.registry_path}")
    
    def _init_index(self):
        """初始化注册表索引"""
        if not self.index_file.exists():
            index_data = {
                "models": {},
                "created_at": datetime.now().isoformat(),
                "last_updated": datetime.now().isoformat()
            }
            with open(self.index_file, 'w', encoding='utf-8') as f:
                json.dump(index_data, f, ensure_ascii=False, indent=2)
    
    def _load_index(self) -> Dict:
        """加载注册表索引"""
        try:
            with open(self.index_file, 'r', encoding='utf-8') as f:
                return json.load(f)
        except Exception as e:
            self.logger.error(f"加载索引失败: {e}")
            return {"models": {}, "created_at": datetime.now().isoformat()}
    
    def _save_index(self, index_data: Dict):
        """保存注册表索引"""
        try:
            index_data["last_updated"] = datetime.now().isoformat()
            with open(self.index_file, 'w', encoding='utf-8') as f:
                json.dump(index_data, f, ensure_ascii=False, indent=2)
        except Exception as e:
            self.logger.error(f"保存索引失败: {e}")
    
    def _calculate_model_hash(self, model_data: bytes) -> str:
        """计算模型哈希值"""
        return hashlib.sha256(model_data).hexdigest()
    
    def _generate_version(self, model_name: str) -> str:
        """生成模型版本号"""
        index_data = self._load_index()
        models = index_data.get("models", {})
        
        if model_name not in models:
            return "v1.0.0"
        
        # 获取最新版本号并递增
        versions = [v["version"] for v in models[model_name]]
        if not versions:
            return "v1.0.0"
        
        # 简单的版本递增逻辑
        latest_version = max(versions)
        try:
            # 解析版本号 "v1.2.3"
            version_parts = latest_version[1:].split('.')
            major, minor, patch = map(int, version_parts)
            # 递增patch版本
            return f"v{major}.{minor}.{patch + 1}"
        except:
            # 如果解析失败，则使用时间戳
            timestamp = int(datetime.now().timestamp())
            return f"v1.0.{timestamp % 1000}"
    
    def register_model(self, 
                      model: Any, 
                      name: str,
                      description: str = "",
                      model_type: str = "sklearn",
                      framework: str = "scikit-learn",
                      metrics: Dict[str, float] = None,
                      parameters: Dict[str, Any] = None,
                      dataset_hash: str = "",
                      tags: Dict[str, str] = None,
                      created_by: str = "system",
                      parent_run_id: str = None) -> str:
        """
        注册新模型
        
        Args:
            model: 模型对象
            name: 模型名称
            description: 模型描述
            model_type: 模型类型
            framework: 框架名称
            metrics: 性能指标
            parameters: 模型参数
            dataset_hash: 数据集哈希
            tags: 标签
            created_by: 创建者
            parent_run_id: 父运行ID
            
        Returns:
            模型版本
        """
        try:
            # 生成版本号
            version = self._generate_version(name)
            
            # 序列化模型
            model_filename = f"{name}_{version}.pkl"
            model_path = self.models_path / model_filename
            
            with open(model_path, 'wb') as f:
                pickle.dump(model, f)
            
            # 计算模型大小和哈希
            model_size = model_path.stat().st_size
            with open(model_path, 'rb') as f:
                model_hash = self._calculate_model_hash(f.read())
            
            # 创建元数据
            metadata = ModelMetadata(
                name=name,
                version=version,
                stage=ModelStage.STAGING,
                created_at=datetime.now().isoformat(),
                created_by=created_by,
                description=description,
                model_type=model_type,
                framework=framework,
                metrics=metrics or {},
                parameters=parameters or {},
                dataset_hash=dataset_hash,
                model_size=model_size,
                tags=tags or {},
                artifacts=[model_filename],
                parent_run_id=parent_run_id
            )
            
            # 保存元数据
            metadata_filename = f"{name}_{version}_metadata.json"
            metadata_path = self.metadata_path / metadata_filename
            
            with open(metadata_path, 'w', encoding='utf-8') as f:
                json.dump(asdict(metadata), f, ensure_ascii=False, indent=2)
            
            # 更新索引
            index_data = self._load_index()
            if name not in index_data["models"]:
                index_data["models"][name] = []
            
            index_data["models"][name].append({
                "version": version,
                "stage": metadata.stage.value,
                "created_at": metadata.created_at,
                "model_hash": model_hash,
                "metadata_file": metadata_filename
            })
            
            self._save_index(index_data)
            
            self.logger.info(f"模型已注册: {name} {version}")
            return version
            
        except Exception as e:
            self.logger.error(f"注册模型失败: {e}")
            raise e
    
    def get_model(self, name: str, version: str = None, stage: str = None) -> Tuple[Any, ModelMetadata]:
        """
        获取模型
        
        Args:
            name: 模型名称
            version: 模型版本，如果为None则获取最新版本
            stage: 模型阶段，如果指定则获取该阶段的模型
            
        Returns:
            (模型对象, 元数据)
        """
        try:
            metadata = self.get_model_metadata(name, version, stage)
            if not metadata:
                raise ValueError(f"模型不存在: {name}")
            
            # 加载模型
            model_filename = metadata.artifacts[0]  # 假设第一个工件是模型文件
            model_path = self.models_path / model_filename
            
            with open(model_path, 'rb') as f:
                model = pickle.load(f)
            
            return model, metadata
            
        except Exception as e:
            self.logger.error(f"获取模型失败: {e}")
            raise e
    
    def get_model_metadata(self, name: str, version: str = None, stage: str = None) -> Optional[ModelMetadata]:
        """
        获取模型元数据
        
        Args:
            name: 模型名称
            version: 模型版本
            stage: 模型阶段
            
        Returns:
            模型元数据
        """
        try:
            index_data = self._load_index()
            models = index_data.get("models", {})
            
            if name not in models:
                return None
            
            model_versions = models[name]
            
            # 根据条件筛选
            if stage:
                model_versions = [v for v in model_versions if v["stage"] == stage]
            
            if version:
                model_versions = [v for v in model_versions if v["version"] == version]
            
            if not model_versions:
                return None
            
            # 获取最新的版本
            latest_version = max(model_versions, key=lambda x: x["created_at"])
            
            # 加载元数据
            metadata_file = latest_version["metadata_file"]
            metadata_path = self.metadata_path / metadata_file
            
            with open(metadata_path, 'r', encoding='utf-8') as f:
                metadata_dict = json.load(f)
            
            # 转换stage为枚举
            metadata_dict["stage"] = ModelStage(metadata_dict["stage"])
            
            return ModelMetadata(**metadata_dict)
            
        except Exception as e:
            self.logger.error(f"获取模型元数据失败: {e}")
            return None
    
    def list_models(self) -> List[Dict]:
        """
        列出所有模型
        
        Returns:
            模型列表
        """
        try:
            index_data = self._load_index()
            models = index_data.get("models", {})
            
            model_list = []
            for name, versions in models.items():
                latest_version = max(versions, key=lambda x: x["created_at"])
                
                # 获取元数据
                metadata = self.get_model_metadata(name, latest_version["version"])
                
                model_info = {
                    "name": name,
                    "latest_version": latest_version["version"],
                    "stage": latest_version["stage"],
                    "created_at": latest_version["created_at"],
                    "total_versions": len(versions),
                    "description": metadata.description if metadata else "",
                    "model_type": metadata.model_type if metadata else "",
                    "metrics": metadata.metrics if metadata else {}
                }
                model_list.append(model_info)
            
            return sorted(model_list, key=lambda x: x["created_at"], reverse=True)
            
        except Exception as e:
            self.logger.error(f"列出模型失败: {e}")
            return []
    
    def list_model_versions(self, name: str) -> List[Dict]:
        """
        列出模型的所有版本
        
        Args:
            name: 模型名称
            
        Returns:
            版本列表
        """
        try:
            index_data = self._load_index()
            models = index_data.get("models", {})
            
            if name not in models:
                return []
            
            versions = []
            for version_info in models[name]:
                metadata = self.get_model_metadata(name, version_info["version"])
                version_data = {
                    "version": version_info["version"],
                    "stage": version_info["stage"],
                    "created_at": version_info["created_at"],
                    "model_hash": version_info["model_hash"],
                    "description": metadata.description if metadata else "",
                    "metrics": metadata.metrics if metadata else {},
                    "model_size": metadata.model_size if metadata else 0
                }
                versions.append(version_data)
            
            return sorted(versions, key=lambda x: x["created_at"], reverse=True)
            
        except Exception as e:
            self.logger.error(f"列出模型版本失败: {e}")
            return []
    
    def promote_model(self, name: str, version: str, stage: ModelStage, promoted_by: str = "system") -> bool:
        """
        提升模型阶段
        
        Args:
            name: 模型名称
            version: 模型版本
            stage: 目标阶段
            promoted_by: 提升者
            
        Returns:
            是否成功
        """
        try:
            # 更新元数据
            metadata = self.get_model_metadata(name, version)
            if not metadata:
                return False
            
            metadata.stage = stage
            metadata.updated_at = datetime.now().isoformat()
            metadata.promoted_at = datetime.now().isoformat()
            metadata.promoted_by = promoted_by
            
            # 保存元数据
            metadata_filename = f"{name}_{version}_metadata.json"
            metadata_path = self.metadata_path / metadata_filename
            
            with open(metadata_path, 'w', encoding='utf-8') as f:
                json.dump(asdict(metadata), f, ensure_ascii=False, indent=2)
            
            # 更新索引
            index_data = self._load_index()
            for version_info in index_data["models"][name]:
                if version_info["version"] == version:
                    version_info["stage"] = stage.value
                    break
            
            self._save_index(index_data)
            
            self.logger.info(f"模型已提升: {name} {version} -> {stage.value}")
            return True
            
        except Exception as e:
            self.logger.error(f"提升模型失败: {e}")
            return False
    
    def delete_model(self, name: str, version: str = None) -> bool:
        """
        删除模型
        
        Args:
            name: 模型名称
            version: 模型版本，如果为None则删除所有版本
            
        Returns:
            是否成功
        """
        try:
            index_data = self._load_index()
            models = index_data.get("models", {})
            
            if name not in models:
                return False
            
            if version:
                # 删除特定版本
                model_versions = models[name]
                version_to_delete = None
                
                for i, v in enumerate(model_versions):
                    if v["version"] == version:
                        version_to_delete = i
                        break
                
                if version_to_delete is not None:
                    # 删除文件
                    metadata_file = model_versions[version_to_delete]["metadata_file"]
                    metadata = self.get_model_metadata(name, version)
                    
                    if metadata:
                        # 删除模型文件
                        for artifact in metadata.artifacts:
                            artifact_path = self.models_path / artifact
                            if artifact_path.exists():
                                artifact_path.unlink()
                        
                        # 删除元数据文件
                        metadata_path = self.metadata_path / metadata_file
                        if metadata_path.exists():
                            metadata_path.unlink()
                    
                    # 从索引中移除
                    del model_versions[version_to_delete]
                    
                    # 如果没有更多版本，删除整个模型条目
                    if not model_versions:
                        del models[name]
                else:
                    return False
            else:
                # 删除所有版本
                for version_info in models[name]:
                    metadata = self.get_model_metadata(name, version_info["version"])
                    if metadata:
                        # 删除模型文件
                        for artifact in metadata.artifacts:
                            artifact_path = self.models_path / artifact
                            if artifact_path.exists():
                                artifact_path.unlink()
                        
                        # 删除元数据文件
                        metadata_path = self.metadata_path / version_info["metadata_file"]
                        if metadata_path.exists():
                            metadata_path.unlink()
                
                # 从索引中移除
                del models[name]
            
            self._save_index(index_data)
            
            self.logger.info(f"模型已删除: {name} {version or '(所有版本)'}")
            return True
            
        except Exception as e:
            self.logger.error(f"删除模型失败: {e}")
            return False
    
    def search_models(self, query: str = "", stage: str = "", tags: Dict[str, str] = None) -> List[Dict]:
        """
        搜索模型
        
        Args:
            query: 搜索查询
            stage: 模型阶段
            tags: 标签过滤
            
        Returns:
            匹配的模型列表
        """
        try:
            all_models = self.list_models()
            filtered_models = []
            
            for model in all_models:
                # 名称查询过滤
                if query and query.lower() not in model["name"].lower():
                    continue
                
                # 阶段过滤
                if stage and model["stage"] != stage:
                    continue
                
                # 标签过滤
                if tags:
                    metadata = self.get_model_metadata(model["name"])
                    if not metadata:
                        continue
                    
                    model_tags = metadata.tags
                    match_tags = True
                    for key, value in tags.items():
                        if key not in model_tags or model_tags[key] != value:
                            match_tags = False
                            break
                    
                    if not match_tags:
                        continue
                
                filtered_models.append(model)
            
            return filtered_models
            
        except Exception as e:
            self.logger.error(f"搜索模型失败: {e}")
            return []
    
    def get_production_model(self, name: str) -> Tuple[Any, ModelMetadata]:
        """
        获取生产环境模型
        
        Args:
            name: 模型名称
            
        Returns:
            (模型对象, 元数据)
        """
        return self.get_model(name, stage=ModelStage.PRODUCTION.value)
    
    def compare_models(self, name: str, versions: List[str]) -> Dict:
        """
        比较模型版本
        
        Args:
            name: 模型名称
            versions: 版本列表
            
        Returns:
            比较结果
        """
        try:
            comparison = {
                "model_name": name,
                "versions": {},
                "metrics_comparison": {}
            }
            
            all_metrics = set()
            
            # 收集所有版本的信息
            for version in versions:
                metadata = self.get_model_metadata(name, version)
                if metadata:
                    version_info = {
                        "version": version,
                        "stage": metadata.stage.value,
                        "created_at": metadata.created_at,
                        "metrics": metadata.metrics,
                        "parameters": metadata.parameters,
                        "model_size": metadata.model_size,
                        "description": metadata.description
                    }
                    comparison["versions"][version] = version_info
                    all_metrics.update(metadata.metrics.keys())
            
            # 构建指标比较
            for metric in all_metrics:
                comparison["metrics_comparison"][metric] = {}
                for version in versions:
                    if version in comparison["versions"]:
                        metrics = comparison["versions"][version]["metrics"]
                        comparison["metrics_comparison"][metric][version] = metrics.get(metric)
            
            return comparison
            
        except Exception as e:
            self.logger.error(f"比较模型失败: {e}")
            return {}
    
    def export_model(self, name: str, version: str, export_path: str) -> bool:
        """
        导出模型
        
        Args:
            name: 模型名称
            version: 模型版本
            export_path: 导出路径
            
        Returns:
            是否成功
        """
        try:
            model, metadata = self.get_model(name, version)
            
            export_dir = Path(export_path)
            export_dir.mkdir(parents=True, exist_ok=True)
            
            # 导出模型文件
            model_export_path = export_dir / f"{name}_{version}.pkl"
            with open(model_export_path, 'wb') as f:
                pickle.dump(model, f)
            
            # 导出元数据
            metadata_export_path = export_dir / f"{name}_{version}_metadata.json"
            with open(metadata_export_path, 'w', encoding='utf-8') as f:
                json.dump(asdict(metadata), f, ensure_ascii=False, indent=2)
            
            self.logger.info(f"模型已导出: {name} {version} -> {export_path}")
            return True
            
        except Exception as e:
            self.logger.error(f"导出模型失败: {e}")
            return False
    
    def cleanup_old_versions(self, name: str, keep_count: int = 5) -> int:
        """
        清理旧版本
        
        Args:
            name: 模型名称
            keep_count: 保留版本数量
            
        Returns:
            清理的版本数量
        """
        try:
            versions = self.list_model_versions(name)
            if len(versions) <= keep_count:
                return 0
            
            # 按创建时间排序，保留最新的版本
            sorted_versions = sorted(versions, key=lambda x: x["created_at"], reverse=True)
            versions_to_delete = sorted_versions[keep_count:]
            
            deleted_count = 0
            for version_info in versions_to_delete:
                # 不删除生产环境模型
                if version_info["stage"] != ModelStage.PRODUCTION.value:
                    if self.delete_model(name, version_info["version"]):
                        deleted_count += 1
            
            self.logger.info(f"已清理旧版本: {name}, 删除 {deleted_count} 个版本")
            return deleted_count
            
        except Exception as e:
            self.logger.error(f"清理旧版本失败: {e}")
            return 0

# 全局模型注册表实例
_model_registry = None

def get_model_registry() -> ModelRegistry:
    """获取全局模型注册表实例"""
    global _model_registry
    if _model_registry is None:
        _model_registry = ModelRegistry()
    return _model_registry