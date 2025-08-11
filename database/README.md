# 投资策略数据库优化系统

基于投资策略回测需求构建的完整数据库优化解决方案，包含数据获取、存储、计算、缓存和监控的全套功能。

## 🎯 系统特性

### ✨ 核心功能

- **🗄️ 数据库优化**: 完整的MySQL/SQLite数据库设计，包含分区、索引和性能优化
- **📊 TuShare数据获取**: 全面的TuShare API集成，支持股票、财务、市场数据同步
- **🔍 数据验证清洗**: 专业的数据质量检查、异常值处理和清洗流程
- **📈 技术指标预计算**: 高性能批量计算技术指标，支持并行处理
- **⏰ 任务调度**: 基于APScheduler的自动化数据同步和计算任务
- **⚡ 缓存系统**: 内存缓存+Redis缓存，大幅提升查询性能
- **📊 性能监控**: 实时系统性能监控和告警

### 🏗️ 系统架构

```
投资策略数据库优化系统
├── 数据层 (Data Layer)
│   ├── MySQL/SQLite 数据库
│   ├── 分区表设计
│   └── 索引优化
├── 数据获取层 (Data Acquisition)
│   ├── TuShare API集成
│   ├── 数据验证清洗
│   └── 增量同步
├── 计算层 (Calculation Layer)
│   ├── 技术指标计算
│   ├── 并行处理
│   └── 结果缓存
├── 调度层 (Scheduler Layer)
│   ├── 定时任务
│   ├── 依赖管理
│   └── 错误处理
├── 缓存层 (Cache Layer)
│   ├── 内存缓存
│   ├── Redis缓存
│   └── 智能失效
└── 监控层 (Monitoring Layer)
    ├── 性能监控
    ├── 告警系统
    └── 统计分析
```

## 🚀 快速开始

### 1. 环境要求

- Python 3.8+
- MySQL 8.0+ (可选，支持SQLite)
- Redis 6.0+ (可选)

### 2. 安装依赖

```bash
# 安装基础依赖
pip install pandas numpy mysql-connector-python tushare

# 安装技术指标库
pip install TA-Lib

# 安装调度器
pip install APScheduler

# 安装缓存依赖(可选)
pip install redis

# 安装监控依赖
pip install psutil

# 安装加速库(可选)
pip install numba
```

### 3. 配置设置

#### 3.1 环境变量

```bash
export TUSHARE_TOKEN="your_tushare_token_here"
export DB_TYPE="mysql"  # 或 "sqlite"
export DB_HOST="localhost"
export DB_PORT="3306"
export DB_USER="root"
export DB_PASSWORD="your_password"
export DB_NAME="stock_analysis"
```

#### 3.2 配置文件 (config.json)

```json
{
  "database": {
    "type": "mysql",
    "host": "localhost",
    "port": 3306,
    "user": "root", 
    "password": "your_password",
    "database": "stock_analysis"
  },
  "tushare_token": "your_tushare_token",
  "cache": {
    "memory_cache": {
      "enabled": true,
      "max_size": 1000,
      "default_ttl": 3600
    },
    "redis_cache": {
      "enabled": true,
      "host": "localhost",
      "port": 6379,
      "db": 0,
      "default_ttl": 3600
    }
  },
  "scheduler": {
    "timezone": "Asia/Shanghai",
    "enabled": true
  },
  "monitoring": {
    "enabled": true,
    "max_history": 1000
  }
}
```

### 4. 系统初始化

```bash
# 初始化系统并创建数据库结构
python database/database_system.py --command init --create-db

# 使用配置文件初始化
python database/database_system.py -c config.json --command init --create-db
```

## 📖 使用指南

### 1. 数据同步

#### 同步股票基础信息
```bash
python database/database_system.py --command sync-basic
```

#### 同步日线数据
```bash
# 增量同步最新数据
python database/database_system.py --command sync-daily

# 同步指定日期数据
python database/database_system.py --command sync-daily --date 2025-01-15
```

#### 计算技术指标
```bash
# 计算所有股票的技术指标
python database/database_system.py --command calc-indicators

# 计算指定股票的技术指标
python database/database_system.py --command calc-indicators --stocks 000001 000002 600000
```

### 2. 启动调度器

```bash
# 启动自动化任务调度器
python database/database_system.py --command start-scheduler
```

调度器包含以下定时任务：
- **每日8:30**: 同步股票基础信息
- **交易日17:30**: 同步日线数据
- **交易日18:00**: 同步市场数据
- **交易日19:00**: 计算技术指标
- **每月1/15日9:00**: 同步财务数据
- **每日20:00**: 数据质量验证
- **每周日2:00**: 系统维护

### 3. 系统监控

```bash
# 查看系统状态
python database/database_system.py --command status

# 执行系统维护
python database/database_system.py --command maintenance

# 数据质量验证
python database/database_system.py --command validate
```

## 📊 数据库设计

### 核心数据表

#### 1. 股票基础信息表 (stock_basic)
```sql
CREATE TABLE stock_basic (
    stock_code VARCHAR(10) PRIMARY KEY,
    stock_name VARCHAR(50) NOT NULL,
    industry VARCHAR(50),
    sector VARCHAR(50),
    market VARCHAR(10),
    list_date DATE,
    is_active BOOLEAN DEFAULT TRUE,
    -- 索引
    INDEX idx_industry (industry),
    INDEX idx_market (market)
);
```

#### 2. 日线行情表 (stock_daily) - 分区表
```sql
CREATE TABLE stock_daily (
    stock_code VARCHAR(10) NOT NULL,
    trade_date DATE NOT NULL,
    open_price DECIMAL(10,3),
    high_price DECIMAL(10,3),
    low_price DECIMAL(10,3),
    close_price DECIMAL(10,3),
    volume BIGINT,
    amount DECIMAL(15,2),
    UNIQUE KEY uk_stock_date (stock_code, trade_date)
) PARTITION BY RANGE (YEAR(trade_date)) (
    PARTITION p2024 VALUES LESS THAN (2025),
    PARTITION p2025 VALUES LESS THAN (2026)
);
```

#### 3. 技术指标缓存表 (technical_indicators)
```sql
CREATE TABLE technical_indicators (
    stock_code VARCHAR(10) NOT NULL,
    trade_date DATE NOT NULL,
    -- 移动平均线
    ma5 DECIMAL(10,3), ma10 DECIMAL(10,3), ma20 DECIMAL(10,3),
    -- RSI指标  
    rsi6 DECIMAL(6,2), rsi12 DECIMAL(6,2), rsi24 DECIMAL(6,2),
    -- MACD指标
    macd_dif DECIMAL(8,4), macd_dea DECIMAL(8,4), macd_macd DECIMAL(8,4),
    -- KDJ指标
    kdj_k DECIMAL(6,2), kdj_d DECIMAL(6,2), kdj_j DECIMAL(6,2),
    UNIQUE KEY uk_stock_date (stock_code, trade_date)
);
```

### 索引优化策略

```sql
-- 复合索引
CREATE INDEX idx_stock_date_range ON stock_daily (stock_code, trade_date, close_price);
CREATE INDEX idx_change_volume ON stock_daily (trade_date, change_pct, volume);
CREATE INDEX idx_tech_combo ON technical_indicators (stock_code, trade_date, rsi6, kdj_k);
```

## 🔧 API使用示例

### Python API使用

```python
from database.database_system import DatabaseOptimizationSystem

# 创建系统实例
system = DatabaseOptimizationSystem('config.json')

# 初始化系统
if system.initialize_system():
    # 同步数据
    system.sync_stock_basic_data()
    system.sync_daily_data()
    
    # 计算技术指标
    system.calculate_technical_indicators(['000001', '000002'])
    
    # 获取系统状态
    status = system.get_system_status()
    print(f"系统状态: {status}")
```

### 缓存使用

```python
from database.cache_and_monitor import CacheManager, cache_result

# 创建缓存管理器
cache_manager = CacheManager(config['cache'])

# 使用缓存装饰器
@cache_result(cache_manager, ttl=1800)  # 30分钟缓存
def get_stock_data(stock_code, start_date, end_date):
    # 耗时的数据库查询
    return query_database(stock_code, start_date, end_date)

# 第一次调用会查询数据库并缓存
data = get_stock_data('000001', '2025-01-01', '2025-01-15')

# 第二次调用直接从缓存获取
data = get_stock_data('000001', '2025-01-01', '2025-01-15')  # 缓存命中
```

### 技术指标计算

```python
from database.technical_indicator_calculator import TechnicalIndicatorCalculator

# 创建计算器
calculator = TechnicalIndicatorCalculator(db_config)

# 批量计算指标
results = calculator.calculate_all_indicators_batch(
    stock_codes=['000001.SZ', '000002.SZ'],
    indicators=['ma', 'rsi', 'macd', 'kdj'],
    parallel=True
)

for result in results:
    if result.success:
        print(f"{result.stock_code}: {result.calculated_indicators}")
```

## ⚡ 性能优化

### 1. 数据库优化

- **分区策略**: 按年份对大表进行分区
- **索引优化**: 针对查询模式设计复合索引
- **查询优化**: 使用EXPLAIN分析和优化慢查询
- **连接池**: 使用连接池减少连接开销

### 2. 计算优化

- **并行处理**: 使用多进程并行计算技术指标
- **向量化计算**: 基于NumPy/Pandas的向量化操作
- **JIT编译**: 使用Numba加速关键计算函数
- **批量处理**: 批量插入减少数据库交互

### 3. 缓存策略

- **多级缓存**: 内存缓存 + Redis分布式缓存
- **智能失效**: 基于TTL和LRU的缓存失效策略
- **缓存预热**: 预计算常用数据到缓存
- **缓存穿透保护**: 避免无效查询击穿缓存

## 📊 监控告警

### 性能指标监控

- CPU使用率
- 内存使用率  
- 磁盘使用率
- 数据库连接数
- 缓存命中率
- 平均响应时间

### 告警规则

```python
# CPU使用率超过80%
if cpu_usage > 80:
    send_alert("CPU使用率过高")

# 内存使用率超过85%
if memory_usage > 85:
    send_alert("内存使用率过高")

# 缓存命中率低于50%
if cache_hit_rate < 0.5:
    send_alert("缓存命中率过低")
```

## 🔒 数据质量保证

### 验证规则

- **格式验证**: 股票代码、日期格式
- **范围验证**: 价格、成交量合理性
- **关系验证**: OHLC价格关系
- **完整性验证**: 必要字段完整性
- **一致性验证**: 数据前后一致性

### 清洗策略

- **异常值处理**: 基于统计学的异常值检测和处理
- **缺失值填充**: 前向填充、线性插值
- **重复数据删除**: 基于业务键的重复数据处理
- **数据标准化**: 统一数据格式和精度

## 🐛 故障排查

### 常见问题

#### 1. TuShare API限制
```bash
# 错误: API调用频率超限
解决: 系统已内置限流机制，会自动等待和重试
```

#### 2. 数据库连接失败
```bash
# 检查数据库配置
python database/database_system.py --command status

# 检查连接参数
mysql -h localhost -u root -p stock_analysis
```

#### 3. 内存不足
```bash
# 调整批处理大小
# 在配置中减小batch_size参数
# 启用Redis缓存减少内存使用
```

#### 4. 计算性能问题
```bash
# 启用并行计算
# 安装numba加速库
pip install numba

# 减少计算股票数量
python database/database_system.py --command calc-indicators --stocks 000001 000002
```

### 日志查看

```bash
# 查看系统日志
tail -f database_system.log

# 查看特定组件日志
grep "TuShareDataManager" database_system.log
grep "ERROR" database_system.log
```

## 📈 扩展功能

### 1. 添加新的技术指标

```python
# 在TechnicalIndicatorCalculator中添加新指标
def _calculate_custom_indicator(self, data, params):
    # 实现自定义指标计算
    pass

# 在supported_indicators中注册
self.supported_indicators['custom_indicator'] = {'period': 14}
```

### 2. 集成新的数据源

```python
# 创建新的数据管理器
class NewDataManager:
    def sync_data(self):
        # 实现数据同步逻辑
        pass

# 在调度器中添加新任务
def _execute_sync_new_data(self, task_id: str):
    # 执行新数据源同步
    pass
```

### 3. 自定义验证规则

```python
# 在DataValidator中添加新规则
def validate_custom_data(self, df):
    # 实现自定义验证逻辑
    pass
```

## 🤝 贡献指南

1. Fork 项目
2. 创建特性分支 (`git checkout -b feature/amazing-feature`)
3. 提交更改 (`git commit -m 'Add amazing feature'`)
4. 推送到分支 (`git push origin feature/amazing-feature`)
5. 创建 Pull Request

## 📄 许可证

本项目采用 MIT 许可证 - 查看 [LICENSE](LICENSE) 文件了解详情。

## 🙏 致谢

- [TuShare](http://tushare.org/) - 提供优质的金融数据API
- [TA-Lib](https://ta-lib.org/) - 技术分析指标库
- [APScheduler](https://apscheduler.readthedocs.io/) - Python任务调度库
- [Redis](https://redis.io/) - 高性能缓存数据库

## 📞 支持

如有问题或建议，请：

1. 查看文档和FAQ
2. 搜索已有Issue
3. 创建新Issue描述问题
4. 加入讨论群组

---

**投资策略数据库优化系统** - 让您的投资策略回测更加高效可靠！ 🚀📊