#!/usr/bin/env python3
"""
LJWX-Stock 统一配置管理器
整合所有配置项到单一管理系统中
"""

import os
import json
import logging
from typing import Dict, Any, Optional, List
from pathlib import Path
from dataclasses import dataclass, field
from datetime import datetime
import platform
import psutil
from dotenv import load_dotenv

# 加载环境变量
load_dotenv()

logger = logging.getLogger(__name__)

@dataclass
class TuShareConfig:
    """TuShare配置"""
    token: str = ""
    rate_limit: int = 500  # 每分钟请求限制
    batch_size: int = 100
    thread_count: int = 5
    use_pro_api: bool = True
    fallback_to_free: bool = True

@dataclass  
class DatabaseConfig:
    """数据库配置"""
    type: str = "mysql"
    host: str = "127.0.0.1"
    port: int = 3306
    user: str = "root"
    password: str = ""
    database: str = "ljwx_stock"
    charset: str = "utf8mb4"
    # SQLite配置（备用）
    sqlite_path: str = "data/ljwx_stock.db"

@dataclass
class RedisConfig:
    """Redis配置"""
    enabled: bool = False
    host: str = "localhost"
    port: int = 6379
    db: int = 0
    password: Optional[str] = None
    default_ttl: int = 3600

@dataclass
class OllamaConfig:
    """Ollama LLM配置"""
    api_url: str = "http://14.127.218.229:11434"
    model: str = "lingjingwanxiang:70b"
    timeout: int = 30
    temperature: float = 0.7
    top_p: float = 0.9
    top_k: int = 40
    repeat_penalty: float = 1.1
    num_predict: int = 1024
    # OpenAI兼容API配置
    openai_api_key: str = "sk-fake-key"
    openai_base_url: str = "http://14.127.218.229:11434/v1"

@dataclass
class APIConfig:
    """API服务配置"""
    host: str = "0.0.0.0"
    port: int = 5005
    debug: bool = False
    cors_enabled: bool = True
    cors_origins: List[str] = field(default_factory=lambda: ["*"])
    max_connections: int = 100

@dataclass
class WebSocketConfig:
    """WebSocket配置"""
    host: str = "0.0.0.0"
    port: int = 8765
    realtime_update_interval: int = 10
    max_connections: int = 100
    ping_timeout: int = 30
    ping_interval: int = 10

@dataclass
class LoggingConfig:
    """日志配置"""
    level: str = "INFO"
    format: str = "%(asctime)s - %(name)s - %(levelname)s - %(message)s"
    file_rotation: bool = True
    max_log_files: int = 10
    log_dir: str = "logs"

@dataclass
class SchedulerConfig:
    """调度器配置"""
    enabled: bool = True
    timezone: str = "Asia/Shanghai"
    daily_update_enabled: bool = True
    daily_update_times: List[str] = field(default_factory=lambda: ["09:35", "15:05", "21:00"])
    weekly_sync_enabled: bool = True
    weekly_sync_day: int = 6  # 星期六
    weekly_sync_time: str = "20:00"
    market_hours: Dict[str, str] = field(default_factory=lambda: {
        "morning_start": "09:30",
        "morning_end": "11:30", 
        "afternoon_start": "13:00",
        "afternoon_end": "15:00"
    })

@dataclass
class TrainingConfig:
    """模型训练配置"""
    base_model: str = "ljwx-stock"
    training_data_dir: str = "data/llm_training"
    models_dir: str = "data/models"
    max_training_examples_per_batch: int = 200
    stock_batch_size: int = 30
    days_back: int = 90
    incremental_days: int = 30
    max_models_to_keep: int = 5
    min_success_rate: float = 0.8
    daily_training_time: str = "02:00"
    weekly_training_days: List[str] = field(default_factory=lambda: ["monday", "friday"])

@dataclass
class HardwareConfig:
    """硬件优化配置"""
    cpu_cores: int = 0
    memory_gb: float = 0.0
    is_m2_ultra: bool = False
    max_workers: int = 4
    chunk_size: int = 10000
    memory_limit_gb: float = 16.0
    enable_gpu_acceleration: bool = False
    gpu_device: str = "cpu"

@dataclass
class SecurityConfig:
    """安全配置"""
    secret_key: str = "ljwx-stock-unified-2025"
    jwt_secret_key: str = ""
    jwt_expiration_hours: int = 24
    api_rate_limit: int = 1000  # 每小时请求限制
    enable_api_key_auth: bool = False
    allowed_ips: List[str] = field(default_factory=list)

@dataclass  
class SystemConfig:
    """系统配置"""
    max_concurrent_tasks: int = 5
    data_retention_days: int = 90
    cache_enabled: bool = True
    cache_size_gb: float = 4.0
    monitoring_enabled: bool = True
    max_history: int = 1000
    backup_enabled: bool = True
    backup_dir: str = "backups"

class ConfigManager:
    """统一配置管理器"""
    
    def __init__(self, config_file: Optional[str] = None):
        self.config_file = config_file or "unified_config.json"
        self.config_dir = Path("config")
        self.config_dir.mkdir(exist_ok=True)
        
        # 初始化所有配置
        self.tushare = TuShareConfig()
        self.database = DatabaseConfig()
        self.redis = RedisConfig()
        self.ollama = OllamaConfig()
        self.api = APIConfig()
        self.websocket = WebSocketConfig()
        self.logging = LoggingConfig()
        self.scheduler = SchedulerConfig()
        self.training = TrainingConfig()
        self.hardware = HardwareConfig()
        self.security = SecurityConfig()
        self.system = SystemConfig()
        
        # 加载配置
        self._load_configurations()
        
        # 检测并设置硬件配置
        self._detect_hardware()
        
        logger.info("✅ 统一配置管理器初始化完成")
    
    def _load_configurations(self):
        """加载所有配置"""
        # 1. 从环境变量加载
        self._load_from_env()
        
        # 2. 从JSON配置文件加载
        self._load_from_json_files()
        
        # 3. 从统一配置文件加载（如果存在）
        self._load_from_unified_config()
        
        # 4. 验证必需配置
        self._validate_config()
    
    def _load_from_env(self):
        """从环境变量加载配置"""
        # TuShare配置
        self.tushare.token = os.getenv('TS_TOKEN', self.tushare.token)
        
        # 数据库配置
        self.database.host = os.getenv('DB_HOST', self.database.host)
        self.database.port = int(os.getenv('DB_PORT', self.database.port))
        self.database.user = os.getenv('DB_USER', self.database.user)
        self.database.password = os.getenv('DB_PASSWORD', self.database.password)
        self.database.database = os.getenv('DB_NAME', self.database.database)
        
        # Redis配置
        self.redis.host = os.getenv('REDIS_HOST', self.redis.host)
        self.redis.port = int(os.getenv('REDIS_PORT', self.redis.port))
        
        # Ollama配置
        self.ollama.api_url = os.getenv('LLM_API_URL', self.ollama.api_url)
        self.ollama.model = os.getenv('LLM_MODEL', self.ollama.model)
        self.ollama.openai_api_key = os.getenv('OPENAI_API_KEY', self.ollama.openai_api_key)
        self.ollama.openai_base_url = os.getenv('OPENAI_BASE_URL', self.ollama.openai_base_url)
        
        # API配置
        self.api.host = os.getenv('API_HOST', self.api.host)
        self.api.port = int(os.getenv('API_PORT', self.api.port))
        self.api.debug = os.getenv('API_DEBUG', 'False').lower() == 'true'
        
        # WebSocket配置
        self.websocket.host = os.getenv('WS_HOST', self.websocket.host)
        self.websocket.port = int(os.getenv('WS_PORT', self.websocket.port))
        self.websocket.realtime_update_interval = int(os.getenv('REALTIME_UPDATE_INTERVAL', self.websocket.realtime_update_interval))
        
        # 日志配置
        self.logging.level = os.getenv('LOG_LEVEL', self.logging.level)
    
    def _load_from_json_files(self):
        """从现有JSON配置文件加载"""
        try:
            # 加载数据库配置
            db_config_file = Path("database_config.json")
            if db_config_file.exists():
                with open(db_config_file, 'r', encoding='utf-8') as f:
                    db_config = json.load(f)
                    if 'database' in db_config:
                        db = db_config['database']
                        self.database.type = db.get('type', self.database.type)
                        self.database.host = db.get('host', self.database.host)
                        self.database.port = db.get('port', self.database.port)
                        self.database.user = db.get('user', self.database.user)
                        self.database.password = db.get('password', self.database.password)
                        self.database.database = db.get('database', self.database.database)
            
            # 加载调度器配置
            scheduler_config_file = Path("config/scheduler_config.json")
            if scheduler_config_file.exists():
                with open(scheduler_config_file, 'r', encoding='utf-8') as f:
                    scheduler_config = json.load(f)
                    if 'daily_update' in scheduler_config:
                        daily = scheduler_config['daily_update']
                        self.scheduler.daily_update_enabled = daily.get('enabled', True)
                        self.scheduler.daily_update_times = daily.get('times', self.scheduler.daily_update_times)
                    
                    if 'weekly_full_sync' in scheduler_config:
                        weekly = scheduler_config['weekly_full_sync']
                        self.scheduler.weekly_sync_enabled = weekly.get('enabled', True)
                        self.scheduler.weekly_sync_day = weekly.get('day', self.scheduler.weekly_sync_day)
                        self.scheduler.weekly_sync_time = weekly.get('time', self.scheduler.weekly_sync_time)
                    
                    if 'market_hours' in scheduler_config:
                        self.scheduler.market_hours = scheduler_config['market_hours']
            
            # 加载训练配置
            training_config_file = Path("training_config.json")
            if training_config_file.exists():
                with open(training_config_file, 'r', encoding='utf-8') as f:
                    training_config = json.load(f)
                    if 'continuous_training' in training_config:
                        ct = training_config['continuous_training']
                        self.training.base_model = ct.get('base_model', self.training.base_model)
                        self.training.training_data_dir = ct.get('training_data_dir', self.training.training_data_dir)
                        self.training.models_dir = ct.get('models_dir', self.training.models_dir)
                        self.training.max_training_examples_per_batch = ct.get('max_training_examples_per_batch', self.training.max_training_examples_per_batch)
                        self.training.stock_batch_size = ct.get('stock_batch_size', self.training.stock_batch_size)
                        
                    if 'model_settings' in training_config:
                        ms = training_config['model_settings']
                        self.ollama.temperature = ms.get('temperature', self.ollama.temperature)
                        self.ollama.top_p = ms.get('top_p', self.ollama.top_p)
                        self.ollama.top_k = ms.get('top_k', self.ollama.top_k)
                        self.ollama.repeat_penalty = ms.get('repeat_penalty', self.ollama.repeat_penalty)
                        self.ollama.num_predict = ms.get('num_predict', self.ollama.num_predict)
                        
        except Exception as e:
            logger.warning(f"加载JSON配置文件失败: {e}")
    
    def _load_from_unified_config(self):
        """从统一配置文件加载"""
        unified_config_path = Path(self.config_file)
        if unified_config_path.exists():
            try:
                with open(unified_config_path, 'r', encoding='utf-8') as f:
                    config = json.load(f)
                    self._update_from_dict(config)
                logger.info(f"✅ 从统一配置文件加载: {self.config_file}")
            except Exception as e:
                logger.error(f"加载统一配置文件失败: {e}")
    
    def _update_from_dict(self, config: Dict[str, Any]):
        """从字典更新配置"""
        for section_name, section_config in config.items():
            if hasattr(self, section_name) and isinstance(section_config, dict):
                section = getattr(self, section_name)
                for key, value in section_config.items():
                    if hasattr(section, key):
                        setattr(section, key, value)
    
    def _detect_hardware(self):
        """检测硬件配置"""
        try:
            # CPU信息
            self.hardware.cpu_cores = os.cpu_count() or 4
            
            # 内存信息
            memory = psutil.virtual_memory()
            self.hardware.memory_gb = memory.total / (1024**3)
            
            # 检测M2 Ultra
            if platform.system() == "Darwin":
                try:
                    import subprocess
                    result = subprocess.run(
                        ["system_profiler", "SPHardwareDataType"], 
                        capture_output=True, text=True
                    )
                    output = result.stdout
                    self.hardware.is_m2_ultra = "M2 Ultra" in output and "192 GB" in output
                except Exception:
                    pass
            
            # 设置优化参数
            if self.hardware.is_m2_ultra:
                self.hardware.max_workers = min(20, self.hardware.cpu_cores - 4)
                self.hardware.chunk_size = 50000
                self.hardware.memory_limit_gb = min(64, self.hardware.memory_gb * 0.4)
                self.hardware.enable_gpu_acceleration = True
                self.hardware.gpu_device = "mps"
            else:
                self.hardware.max_workers = min(8, self.hardware.cpu_cores - 2)
                self.hardware.chunk_size = 10000
                self.hardware.memory_limit_gb = min(16, self.hardware.memory_gb * 0.5)
            
            # 检测GPU支持
            try:
                import torch
                if torch.backends.mps.is_available():
                    self.hardware.enable_gpu_acceleration = True
                    self.hardware.gpu_device = "mps"
            except ImportError:
                pass
                
        except Exception as e:
            logger.warning(f"硬件检测失败: {e}")
    
    def _validate_config(self):
        """验证必需配置"""
        errors = []
        
        # 检查TuShare token
        if not self.tushare.token or self.tushare.token == "your_tushare_token":
            errors.append("TuShare token未配置或使用默认值")
        
        # 检查数据库配置
        if not self.database.password:
            logger.warning("数据库密码为空")
        
        # 检查必需目录
        required_dirs = [
            self.logging.log_dir,
            self.training.training_data_dir,
            self.training.models_dir,
            "data",
            "temp",
            "cache"
        ]
        
        for dir_path in required_dirs:
            Path(dir_path).mkdir(parents=True, exist_ok=True)
        
        if errors:
            logger.warning(f"配置验证警告: {', '.join(errors)}")
    
    def save_unified_config(self):
        """保存统一配置到文件"""
        config_data = {}
        
        for attr_name in dir(self):
            if not attr_name.startswith('_') and not callable(getattr(self, attr_name)):
                attr_value = getattr(self, attr_name)
                if hasattr(attr_value, '__dict__'):
                    config_data[attr_name] = attr_value.__dict__
        
        unified_config_path = Path(self.config_file)
        try:
            with open(unified_config_path, 'w', encoding='utf-8') as f:
                json.dump(config_data, f, indent=2, ensure_ascii=False, default=str)
            logger.info(f"✅ 统一配置已保存到: {self.config_file}")
        except Exception as e:
            logger.error(f"保存统一配置失败: {e}")
    
    def get_database_url(self) -> str:
        """获取数据库连接URL"""
        if self.database.type == "mysql":
            return f"mysql+pymysql://{self.database.user}:{self.database.password}@{self.database.host}:{self.database.port}/{self.database.database}?charset={self.database.charset}"
        elif self.database.type == "sqlite":
            return f"sqlite:///{self.database.sqlite_path}"
        else:
            raise ValueError(f"不支持的数据库类型: {self.database.type}")
    
    def get_redis_url(self) -> str:
        """获取Redis连接URL"""
        if self.redis.password:
            return f"redis://:{self.redis.password}@{self.redis.host}:{self.redis.port}/{self.redis.db}"
        else:
            return f"redis://{self.redis.host}:{self.redis.port}/{self.redis.db}"
    
    def get_ollama_config(self) -> Dict[str, Any]:
        """获取Ollama配置字典"""
        return {
            'api_url': self.ollama.api_url,
            'model': self.ollama.model,
            'timeout': self.ollama.timeout,
            'temperature': self.ollama.temperature,
            'top_p': self.ollama.top_p,
            'top_k': self.ollama.top_k,
            'repeat_penalty': self.ollama.repeat_penalty,
            'num_predict': self.ollama.num_predict,
            'openai_api_key': self.ollama.openai_api_key,
            'openai_base_url': self.ollama.openai_base_url
        }
    
    def get_hardware_config(self, task_type: str = "general") -> Dict[str, Any]:
        """获取特定任务的硬件配置"""
        base_config = {
            'cpu_cores': self.hardware.cpu_cores,
            'memory_gb': self.hardware.memory_gb,
            'is_m2_ultra': self.hardware.is_m2_ultra,
            'max_workers': self.hardware.max_workers,
            'chunk_size': self.hardware.chunk_size,
            'memory_limit_gb': self.hardware.memory_limit_gb,
            'enable_gpu_acceleration': self.hardware.enable_gpu_acceleration,
            'gpu_device': self.hardware.gpu_device
        }
        
        # 根据任务类型调整配置
        if task_type == "data_processing" and self.hardware.is_m2_ultra:
            base_config.update({
                'max_workers': min(20, self.hardware.cpu_cores - 4),
                'chunk_size': 50000,
                'memory_limit_gb': min(64, self.hardware.memory_gb * 0.4)
            })
        elif task_type == "ml_training" and self.hardware.is_m2_ultra:
            base_config.update({
                'max_workers': min(16, self.hardware.cpu_cores - 8),
                'memory_limit_gb': min(96, self.hardware.memory_gb * 0.5)
            })
        
        return base_config
    
    def print_config_summary(self):
        """打印配置摘要"""
        print("\n" + "="*80)
        print("🔧 LJWX-Stock 统一配置摘要")
        print("="*80)
        
        print(f"📊 TuShare: {'✅ 已配置' if self.tushare.token else '❌ 未配置'}")
        print(f"🗄️ 数据库: {self.database.type}://{self.database.host}:{self.database.port}/{self.database.database}")
        print(f"🧠 Ollama: {self.ollama.model} @ {self.ollama.api_url}")
        print(f"🌐 API服务: {self.api.host}:{self.api.port}")
        print(f"🔌 WebSocket: {self.websocket.host}:{self.websocket.port}")
        print(f"🖥️ 硬件: {'M2 Ultra' if self.hardware.is_m2_ultra else 'Standard'} - {self.hardware.cpu_cores} cores, {self.hardware.memory_gb:.1f}GB")
        print(f"🚀 GPU加速: {'✅ MPS' if self.hardware.enable_gpu_acceleration else '❌ CPU only'}")
        print(f"📝 日志级别: {self.logging.level}")
        print(f"⏰ 调度器: {'✅ 启用' if self.scheduler.enabled else '❌ 禁用'}")
        print(f"🔒 安全: {'✅ 启用' if self.security.enable_api_key_auth else '❌ 禁用'}")
        
        print("="*80)

# 全局配置实例
config = ConfigManager()

def get_config() -> ConfigManager:
    """获取全局配置实例"""
    return config

def reload_config():
    """重新加载配置"""
    global config
    config = ConfigManager()
    return config

if __name__ == "__main__":
    # 测试配置管理器
    config_mgr = ConfigManager()
    config_mgr.print_config_summary()
    
    # 保存统一配置
    config_mgr.save_unified_config()
    
    print(f"\n✅ 配置管理器测试完成")
    print(f"📁 统一配置已保存到: {config_mgr.config_file}")