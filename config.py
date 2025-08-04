import os
from dotenv import load_dotenv
load_dotenv()

# TuShare配置
TS_TOKEN = os.getenv('TS_TOKEN', 'e43b6eab95ac0d2d9de22f6ca3b1b4ef3483650893794569337dc973')  # TuShare Token
TS_RATE_LIMIT = 500  # 每分钟请求限制
TS_BATCH_SIZE = 100  # 批处理大小
TS_THREAD_COUNT = 5  # 并发线程数

# 数据库配置 - 使用ljwx_stock数据库
DB_HOST = os.getenv('DB_HOST', '127.0.0.1')
DB_PORT = int(os.getenv('DB_PORT', 3306))
DB_USER = os.getenv('DB_USER', 'root')
DB_PASSWORD = os.getenv('DB_PASSWORD', '123456')
DB_NAME = os.getenv('DB_NAME', 'ljwx_stock')  # 统一使用ljwx_stock数据库
DB_CHARSET = 'utf8mb4'

# 数据源配置
START_DATE = '20100101'  # 历史数据开始日期
BATCH_YEARS = 3  # 每批次处理年数
CHUNK_SIZE = 1000  # 数据库批量插入大小

# API配置
API_HOST = os.getenv('API_HOST', '0.0.0.0')
API_PORT = int(os.getenv('API_PORT', 5005))
API_DEBUG = bool(os.getenv('API_DEBUG', True))

# 缓存配置
REDIS_HOST = os.getenv('REDIS_HOST', 'localhost')
REDIS_PORT = int(os.getenv('REDIS_PORT', 6379))
CACHE_TTL = 3600  # 缓存过期时间(秒)

# 推荐算法配置
MA_SHORT = 5  # 短期均线
MA_LONG = 20  # 长期均线
SCORE_THRESHOLD = 0.6  # 推荐分数阈值
MAX_RECOMMEND = 50  # 最大推荐数量

# 日志配置
LOG_LEVEL = os.getenv('LOG_LEVEL', 'INFO')
LOG_FORMAT = '%(asctime)s - %(name)s - %(levelname)s - %(message)s'

# LLM配置 - 优化OpenAI兼容配置
OPENAI_API_KEY = os.getenv('OPENAI_API_KEY', 'sk-fake-key')
OPENAI_BASE_URL = os.getenv('OPENAI_BASE_URL', 'http://14.127.218.229:11434/v1')
LLM_MODEL = os.getenv('LLM_MODEL', 'lingjingwanxiang:70b')
LLM_API_URL = os.getenv('LLM_API_URL', 'http://14.127.218.229:11434')
LLM_TIMEOUT = 30

# WebSocket配置
WS_HOST = os.getenv('WS_HOST', '0.0.0.0')
WS_PORT = int(os.getenv('WS_PORT', 8765))
REALTIME_UPDATE_INTERVAL = int(os.getenv('REALTIME_UPDATE_INTERVAL', 10))  # 实时更新间隔(秒)
MAX_CONNECTIONS = int(os.getenv('MAX_CONNECTIONS', 100))  # 最大连接数

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

class Config:  # 统一配置类
    TS_TOKEN = TS_TOKEN
    DB_HOST = DB_HOST
    DB_PORT = DB_PORT
    DB_USER = DB_USER
    DB_PASSWORD = DB_PASSWORD
    DB_NAME = DB_NAME
    OPENAI_API_KEY = OPENAI_API_KEY
    OPENAI_BASE_URL = OPENAI_BASE_URL
    LLM_MODEL = LLM_MODEL 