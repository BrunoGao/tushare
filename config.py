import os
from dotenv import load_dotenv
from config_manager import get_config

# 加载环境变量和统一配置
load_dotenv()
_config = get_config()

# TuShare配置 - 从统一配置获取
TS_TOKEN = _config.tushare.token
TS_RATE_LIMIT = _config.tushare.rate_limit
TS_BATCH_SIZE = _config.tushare.batch_size
TS_THREAD_COUNT = _config.tushare.thread_count

# 数据库配置 - 从统一配置获取
DB_HOST = _config.database.host
DB_PORT = _config.database.port
DB_USER = _config.database.user
DB_PASSWORD = _config.database.password
DB_NAME = _config.database.database
DB_CHARSET = _config.database.charset

# 数据源配置
START_DATE = '20100101'  # 历史数据开始日期
BATCH_YEARS = 3  # 每批次处理年数
CHUNK_SIZE = 1000  # 数据库批量插入大小

# API配置 - 从统一配置获取
API_HOST = _config.api.host
API_PORT = _config.api.port
API_DEBUG = _config.api.debug

# 缓存配置 - 从统一配置获取
REDIS_HOST = _config.redis.host
REDIS_PORT = _config.redis.port
CACHE_TTL = _config.redis.default_ttl

# 推荐算法配置
MA_SHORT = 5  # 短期均线
MA_LONG = 20  # 长期均线
SCORE_THRESHOLD = 0.6  # 推荐分数阈值
MAX_RECOMMEND = 50  # 最大推荐数量

# 日志配置 - 从统一配置获取
LOG_LEVEL = _config.logging.level
LOG_FORMAT = _config.logging.format

# LLM配置 - 从统一配置获取
OPENAI_API_KEY = _config.ollama.openai_api_key
OPENAI_BASE_URL = _config.ollama.openai_base_url
LLM_MODEL = _config.ollama.model
LLM_API_URL = _config.ollama.api_url
LLM_TIMEOUT = _config.ollama.timeout

# WebSocket配置 - 从统一配置获取
WS_HOST = _config.websocket.host
WS_PORT = _config.websocket.port
REALTIME_UPDATE_INTERVAL = _config.websocket.realtime_update_interval
MAX_CONNECTIONS = _config.websocket.max_connections

# 数据清洗配置
DATA_CLEAN_RULES = {
    'REQUIRED_DATE_COLS': ['ann_date'],  # 必需的日期字段
    'FILTER_NULL_ANN_DATE': True,       # 过滤ann_date为空的记录
    'MAX_RETRY_COUNT': 3,                # 最大重试次数
    'RETRY_DELAY': 2                     # 重试延迟(秒)
}

# 错误处理配置  
ERROR_HANDLING = {
    'IGNORE_EMPTY_DATA': True,           # 忽略空数据错误
    'LOG_FILTERED_RECORDS': True,        # 记录被过滤的记录数
    'FALLBACK_INDUSTRY_API': True        # 使用备用行业分类API
}

# 行业板块配置
INDUSTRY_MAPPING = {  # 行业板块映射
    '银行': 'banking', '保险': 'insurance', '证券': 'securities',
    '房地产': 'real_estate', '建筑': 'construction', '钢铁': 'steel',
    '煤炭': 'coal', '石油': 'oil', '化工': 'chemical', '电力': 'power',
    '医药': 'pharmaceutical', '食品': 'food', '汽车': 'automotive',
    '电子': 'electronics', '计算机': 'computer', '通信': 'telecom',
    '传媒': 'media', '军工': 'defense', '环保': 'environmental'
}

class Config:  # 统一配置类 - 使用配置管理器
    # 提供向后兼容的配置类
    _config = _config
    
    # TuShare配置
    TS_TOKEN = TS_TOKEN
    TS_RATE_LIMIT = TS_RATE_LIMIT
    TS_BATCH_SIZE = TS_BATCH_SIZE
    TS_THREAD_COUNT = TS_THREAD_COUNT
    
    # 数据库配置
    DB_HOST = DB_HOST
    DB_PORT = DB_PORT
    DB_USER = DB_USER
    DB_PASSWORD = DB_PASSWORD
    DB_NAME = DB_NAME
    DB_CHARSET = DB_CHARSET
    
    # API配置
    API_HOST = API_HOST
    API_PORT = API_PORT
    API_DEBUG = API_DEBUG
    
    # Redis配置
    REDIS_HOST = REDIS_HOST
    REDIS_PORT = REDIS_PORT
    CACHE_TTL = CACHE_TTL
    
    # LLM配置
    OPENAI_API_KEY = OPENAI_API_KEY
    OPENAI_BASE_URL = OPENAI_BASE_URL
    LLM_MODEL = LLM_MODEL
    LLM_API_URL = LLM_API_URL
    LLM_TIMEOUT = LLM_TIMEOUT
    
    # WebSocket配置
    WS_HOST = WS_HOST
    WS_PORT = WS_PORT
    REALTIME_UPDATE_INTERVAL = REALTIME_UPDATE_INTERVAL
    MAX_CONNECTIONS = MAX_CONNECTIONS
    
    # 日志配置
    LOG_LEVEL = LOG_LEVEL
    LOG_FORMAT = LOG_FORMAT
    
    @classmethod
    def get_config_manager(cls):
        """获取配置管理器实例"""
        return cls._config 