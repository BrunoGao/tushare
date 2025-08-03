"""
M2 Ultra 硬件优化配置管理器
针对Apple M2 Ultra芯片的专项优化配置
"""

import os
import psutil
import platform
import subprocess
from typing import Dict, Any, Optional
import logging

logger = logging.getLogger(__name__)

class M2UltraConfig:
    """M2 Ultra优化配置管理器"""
    
    # M2 Ultra硬件规格
    CPU_CORES = 24
    GPU_CORES = 76
    MEMORY_GB = 192
    ARCHITECTURE = "arm64"
    
    def __init__(self):
        self.is_m2_ultra = self._detect_m2_ultra()
        self.available_memory = self._get_available_memory_gb()
        self.cpu_count = os.cpu_count()
        
        if self.is_m2_ultra:
            logger.info(f"✅ 检测到M2 Ultra芯片，启用优化配置")
            self._setup_environment()
        else:
            logger.warning(f"⚠️ 未检测到M2 Ultra，使用通用配置")
    
    def _detect_m2_ultra(self) -> bool:
        """检测是否为M2 Ultra芯片"""
        try:
            if platform.system() != "Darwin":
                return False
                
            result = subprocess.run(
                ["system_profiler", "SPHardwareDataType"], 
                capture_output=True, text=True
            )
            
            output = result.stdout
            return "M2 Ultra" in output and "192 GB" in output
            
        except Exception as e:
            logger.error(f"硬件检测失败: {e}")
            return False
    
    def _get_available_memory_gb(self) -> float:
        """获取可用内存（GB）"""
        try:
            memory = psutil.virtual_memory()
            return memory.available / (1024**3)
        except Exception:
            return 16.0  # 默认值
    
    def _setup_environment(self):
        """设置M2 Ultra优化环境变量"""
        env_vars = {
            'OMP_NUM_THREADS': str(self.CPU_CORES),
            'MKL_NUM_THREADS': str(self.CPU_CORES),
            'NUMEXPR_NUM_THREADS': str(self.CPU_CORES),
            'VECLIB_MAXIMUM_THREADS': str(self.CPU_CORES),
            'ACCELERATE_NEW_LAPACK': '1',  # 使用Apple Accelerate
            'PYTORCH_ENABLE_MPS_FALLBACK': '1',  # MPS回退支持
        }
        
        for key, value in env_vars.items():
            os.environ[key] = value
            logger.info(f"设置环境变量: {key}={value}")
    
    def get_optimal_config(self, task_type: str) -> Dict[str, Any]:
        """根据任务类型获取最优配置"""
        
        base_config = {
            'cpu_cores': self.cpu_count,
            'memory_gb': self.available_memory,
            'is_m2_ultra': self.is_m2_ultra
        }
        
        if not self.is_m2_ultra:
            return self._get_fallback_config(task_type, base_config)
        
        configs = {
            'data_processing': {
                'max_workers': min(20, self.CPU_CORES - 4),  # 保留4核给系统
                'chunk_size': 50000,  # 大内存允许更大chunk
                'memory_limit_gb': min(64, self.available_memory * 0.4),
                'use_multiprocessing': True,
                'prefetch_factor': 4,
                'pin_memory': True,
                **base_config
            },
            
            'traditional_ml': {
                'xgboost_threads': self.CPU_CORES,
                'lightgbm_threads': self.CPU_CORES,
                'sklearn_jobs': self.CPU_CORES,
                'memory_limit_gb': min(128, self.available_memory * 0.7),
                'tree_method': 'hist',  # 适合大内存
                'max_bin': 1024,  # 利用大内存提高精度
                'cache_enabled': True,
                'cache_size_gb': min(32, self.available_memory * 0.2),
                **base_config
            },
            
            'deep_learning': {
                'device': 'mps' if self._check_mps_available() else 'cpu',
                'batch_size_multiplier': 8 if self.available_memory > 100 else 4,
                'num_workers': 16,
                'pin_memory': True,
                'persistent_workers': True,
                'prefetch_factor': 4,
                'memory_limit_gb': min(96, self.available_memory * 0.5),
                'use_mixed_precision': True,  # MPS支持混合精度
                **base_config
            },
            
            'model_optimization': {
                'optuna_n_jobs': min(16, self.CPU_CORES - 8),
                'n_trials': 200,  # 大内存允许更多trials
                'memory_limit_gb': min(80, self.available_memory * 0.4),
                'parallel_backend': 'multiprocessing',
                **base_config
            }
        }
        
        return configs.get(task_type, base_config)
    
    def _check_mps_available(self) -> bool:
        """检查MPS是否可用"""
        try:
            import torch
            return torch.backends.mps.is_available()
        except ImportError:
            return False
    
    def _get_fallback_config(self, task_type: str, base_config: Dict) -> Dict[str, Any]:
        """非M2 Ultra的回退配置"""
        fallback_configs = {
            'data_processing': {
                'max_workers': min(8, self.cpu_count - 2),
                'chunk_size': 10000,
                'memory_limit_gb': min(16, self.available_memory * 0.5),
                **base_config
            },
            'traditional_ml': {
                'xgboost_threads': self.cpu_count,
                'lightgbm_threads': self.cpu_count,
                'sklearn_jobs': self.cpu_count,
                'memory_limit_gb': min(32, self.available_memory * 0.6),
                'tree_method': 'approx',
                'max_bin': 256,
                **base_config
            },
            'deep_learning': {
                'device': 'cpu',
                'batch_size_multiplier': 2,
                'num_workers': 4,
                'pin_memory': False,
                'memory_limit_gb': min(24, self.available_memory * 0.4),
                **base_config
            }
        }
        
        return fallback_configs.get(task_type, base_config)
    
    def get_xgboost_config(self) -> Dict[str, Any]:
        """获取XGBoost优化配置"""
        if not self.is_m2_ultra:
            return {
                'nthread': self.cpu_count,
                'tree_method': 'approx',
                'max_bin': 256
            }
        
        return {
            'nthread': self.CPU_CORES,
            'tree_method': 'hist',  # M2 Ultra大内存优化
            'max_bin': 1024,  # 更高精度
            'subsample': 0.8,
            'colsample_bytree': 0.8,
            'objective': 'multi:softprob',
            'eval_metric': 'mlogloss',
            'verbosity': 1,
            'use_label_encoder': False,
            # M2 Ultra特定优化
            'max_delta_step': 1,
            'scale_pos_weight': 1,
            'reg_alpha': 0.1,
            'reg_lambda': 1.0
        }
    
    def get_lightgbm_config(self) -> Dict[str, Any]:
        """获取LightGBM优化配置"""
        if not self.is_m2_ultra:
            return {
                'num_threads': self.cpu_count,
                'max_bin': 255,
                'device_type': 'cpu'
            }
        
        return {
            'num_threads': self.CPU_CORES,
            'device_type': 'cpu',  # Apple Silicon优化
            'max_bin': 1024,  # M2 Ultra大内存优化
            'bagging_fraction': 0.8,
            'feature_fraction': 0.8,
            'objective': 'multiclass',
            'metric': 'multi_logloss',
            'verbose': 1,
            'force_col_wise': True,  # 内存优化
            'histogram_pool_size': 512,  # 大内存优化
            'max_depth': -1,
            'learning_rate': 0.1,
            'num_leaves': 31,
            'min_data_in_leaf': 20,
            'lambda_l1': 0.1,
            'lambda_l2': 0.1
        }
    
    def get_pytorch_config(self) -> Dict[str, Any]:
        """获取PyTorch优化配置"""
        config = {
            'device': 'cpu',
            'num_workers': 4,
            'batch_size_base': 32,
            'pin_memory': False,
            'persistent_workers': False
        }
        
        if self.is_m2_ultra and self._check_mps_available():
            config.update({
                'device': 'mps',
                'num_workers': 16,
                'batch_size_base': 256,  # M2 Ultra大内存优化
                'pin_memory': True,
                'persistent_workers': True,
                'prefetch_factor': 4,
                'enable_mixed_precision': True,
                # MPS特定配置
                'mps_sync_allocator': True,
                'mps_metal_sync': True
            })
        
        return config
    
    def get_memory_config(self) -> Dict[str, Any]:
        """获取内存管理配置"""
        if not self.is_m2_ultra:
            return {
                'cache_size_gb': min(4, self.available_memory * 0.2),
                'max_memory_usage': 0.6,
                'enable_memory_mapping': False
            }
        
        return {
            'cache_size_gb': min(32, self.available_memory * 0.2),  # 192GB的20%
            'max_memory_usage': 0.8,  # M2 Ultra可以使用更多内存
            'enable_memory_mapping': True,  # 大文件内存映射
            'prefetch_size_gb': 8,  # 预读取大小
            'compress_cache': False,  # 大内存不需要压缩
            'memory_pool_size_gb': 16,  # 内存池大小
        }
    
    def get_parallel_config(self) -> Dict[str, Any]:
        """获取并行处理配置"""
        if not self.is_m2_ultra:
            return {
                'max_workers': min(4, self.cpu_count - 1),
                'backend': 'threading',
                'chunk_size': 1000
            }
        
        return {
            'max_workers': min(20, self.CPU_CORES - 4),  # 保留4核给系统
            'backend': 'multiprocessing',  # M2 Ultra多进程性能更好
            'chunk_size': 10000,  # 大内存允许更大chunk
            'maxtasksperchild': 1000,  # 防止内存泄漏
            'context': 'spawn',  # macOS推荐
        }
    
    def print_system_info(self):
        """打印系统信息"""
        print("\n" + "="*60)
        print("🖥️  M2 Ultra 系统信息")
        print("="*60)
        print(f"芯片类型: {'M2 Ultra' if self.is_m2_ultra else '其他'}")
        print(f"CPU核心数: {self.cpu_count}")
        print(f"可用内存: {self.available_memory:.1f} GB")
        print(f"系统架构: {platform.machine()}")
        
        if self._check_mps_available():
            print(f"MPS支持: ✅ 可用")
        else:
            print(f"MPS支持: ❌ 不可用")
        
        print("="*60)

# 全局配置实例
m2_ultra_config = M2UltraConfig()

def get_optimal_config(task_type: str) -> Dict[str, Any]:
    """获取任务类型的最优配置"""
    return m2_ultra_config.get_optimal_config(task_type)

def is_m2_ultra() -> bool:
    """检查是否为M2 Ultra"""
    return m2_ultra_config.is_m2_ultra

if __name__ == "__main__":
    # 测试配置
    config = M2UltraConfig()
    config.print_system_info()
    
    print("\n📊 各任务类型优化配置:")
    for task in ['data_processing', 'traditional_ml', 'deep_learning']:
        print(f"\n{task}:")
        task_config = config.get_optimal_config(task)
        for key, value in task_config.items():
            print(f"  {key}: {value}")