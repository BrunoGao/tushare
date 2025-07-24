import os
from dotenv import load_dotenv
load_dotenv()

# TuShare配置
TS_TOKEN = os.getenv('TS_TOKEN', 'f4b30b0b2f4ea3f5ae7daedb1c280f0d4e380599ef16647df5e80ac8')  # TuShare Token
TS_RATE_LIMIT = 500  # 每分钟请求限制
TS_BATCH_SIZE = 100  # 批处理大小
TS_THREAD_COUNT = 5  # 并发线程数

# 数据库配置
DB_HOST = os.getenv('DB_HOST', '127.0.0.1')
DB_PORT = int(os.getenv('DB_PORT', 3306))
DB_USER = os.getenv('DB_USER', 'root')
DB_PASSWORD = os.getenv('DB_PASSWORD', '123456')
DB_NAME = os.getenv('DB_NAME', 'ljwx_stock')
DB_CHARSET = 'utf8mb4'

# 数据源配置
START_DATE = '20100101'  # 历史数据开始日期
BATCH_YEARS = 3  # 每批次处理年数
CHUNK_SIZE = 1000  # 数据库批量插入大小

# API配置
API_HOST = os.getenv('API_HOST', '0.0.0.0')
API_PORT = int(os.getenv('API_PORT', 5000))
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

# LLM配置
LLM_MODEL = os.getenv('LLM_MODEL', 'lingjingwanxiang:70b')
LLM_API_URL = os.getenv('LLM_API_URL', 'http://localhost:11434')
LLM_TIMEOUT = 30 