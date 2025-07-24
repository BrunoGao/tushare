# 🚀 A股智能推荐系统

基于TuShare数据源，结合技术分析和机器学习的A股智能推荐系统。支持历史数据获取、实时分析、策略推荐，并预留大模型接入接口。

## ✨ 主要特性

- 🔄 **自动化数据获取**: 基于TuShare Pro的多线程高并发数据抓取
- 📊 **技术分析策略**: 均线交叉、动量分析等多种推荐策略  
- 🎯 **智能推荐**: 基于技术指标的股票评分和排名系统
- ⏰ **定时任务**: 每日自动更新数据和重新计算推荐
- 🔗 **API服务**: RESTful API + 简易Web界面
- 🤖 **LLM集成**: 预留Ollama等本地大模型接入接口
- 🐳 **容器化部署**: Docker + Docker Compose一键部署

## 🏗️ 系统架构

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   TuShare Pro   │────│  数据获取层     │────│   MySQL存储     │
└─────────────────┘    └─────────────────┘    └─────────────────┘
                                │
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   技术分析      │────│   分析推荐层    │────│   Redis缓存     │
└─────────────────┘    └─────────────────┘    └─────────────────┘
                                │
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│  Flask API      │────│   服务接口层    │────│   LLM接口       │
└─────────────────┘    └─────────────────┘    └─────────────────┘
```

## 📋 数据库设计

### 核心表结构

- **stock_basic**: 股票基本信息(5000+只A股)
- **stock_daily_YYYYMM**: 按月分表的历史行情数据
- **recommend_result**: 推荐结果和评分
- **technical_indicators**: 技术指标计算结果
- **system_log**: 系统操作日志

## 🚀 快速开始

### 环境要求

- Mac Studio M2 Ultra (或类似配置)
- Python 3.11+
- MySQL 8.0+
- Docker & Docker Compose
- TuShare Pro账号

### 1. 克隆项目

```bash
git clone <repository>
cd tushare
```

### 2. 配置环境

```bash
# 复制环境变量配置
cp env.example .env

# 编辑配置文件，设置TuShare Token等
vim .env
```

### 3. Docker部署(推荐)

```bash
# 一键启动所有服务
docker-compose up -d

# 查看服务状态  
docker-compose ps

# 查看日志
docker-compose logs -f stock_app
```

### 4. 本地开发部署

```bash
# 安装依赖
pip install -r requirements.txt

# 初始化数据库
mysql -u root -p < sql/init_tables.sql

# 获取股票基本信息
python scripts/fetch_stock_basic.py

# 启动API服务
python api/app.py

# 启动定时任务(另一个终端)
python scheduler/daily_tasks.py
```

## 📊 数据获取流程

### 3天历史数据获取计划

```bash
# 执行完整的3天获取计划
python scripts/fetch_stock_daily.py plan

# 或分批执行
python scripts/fetch_stock_daily.py range 20100101 20151231  # 第1天
python scripts/fetch_stock_daily.py range 20160101 20201231  # 第2天  
python scripts/fetch_stock_daily.py range 20210101 20241201  # 第3天
```

### 每日增量更新

```bash
# 手动执行每日更新
python scripts/fetch_stock_daily.py update

# 或通过定时任务自动执行(交易日17:00)
python scheduler/daily_tasks.py
```

## 🎯 推荐策略使用

### 生成推荐

```bash
# 均线交叉策略
python analysis/recommender.py ma_crossover 50

# 动量策略
python analysis/recommender.py momentum 30
```

### API调用

```bash
# 获取推荐结果
curl "http://localhost:5000/api/recommend?strategy=ma_crossover&limit=20"

# 生成新推荐
curl "http://localhost:5000/api/recommend/generate?strategy=momentum"

# 获取股票历史数据
curl "http://localhost:5000/api/stock/000001.SZ/history?days=30"
```

## 🤖 大模型接入

### Ollama本地部署

```bash
# 安装Ollama
curl -fsSL https://ollama.ai/install.sh | sh

# 拉取模型
ollama pull llama2

# 启动服务(默认11434端口)
ollama serve
```

### LLM功能测试

```bash
# 股票问答
python llm/interface.py ask "平安银行怎么样?" 000001.SZ

# 市场趋势分析
python llm/interface.py trend 银行

# 投资建议
python llm/interface.py advice medium 50000

# 推荐解释
python llm/interface.py explain 000001.SZ ma_crossover
```

## 🔧 API接口文档

### 核心接口

| 接口 | 方法 | 说明 |
|------|------|------|
| `/api/health` | GET | 健康检查 |
| `/api/recommend` | GET | 获取推荐结果 |
| `/api/recommend/generate` | GET | 生成新推荐 |
| `/api/stock/{code}` | GET | 股票基本信息 |
| `/api/stock/{code}/history` | GET | 股票历史数据 |
| `/api/llm/ask` | POST | LLM问答 |
| `/api/llm/trend` | GET | 市场趋势分析 |
| `/api/llm/advice` | GET | 投资建议 |

### 请求示例

```bash
# 获取推荐
curl "http://localhost:5000/api/recommend?strategy=ma_crossover&limit=10"

# LLM问答
curl -X POST "http://localhost:5000/api/llm/ask" \
  -H "Content-Type: application/json" \
  -d '{"question": "今日推荐哪些银行股？"}'
```

## ⚙️ 配置说明

### 主要配置项

```python
# TuShare配置
TS_TOKEN = "your_token"       # TuShare Pro Token
TS_RATE_LIMIT = 500          # 每分钟请求限制
TS_THREAD_COUNT = 5          # 并发线程数

# 数据库配置
DB_HOST = "127.0.0.1"        # 数据库地址
DB_NAME = "ljwx_stock"       # 数据库名称

# 推荐策略配置
MA_SHORT = 5                 # 短期均线
MA_LONG = 20                 # 长期均线
SCORE_THRESHOLD = 0.6        # 推荐分数阈值

# LLM配置
LLM_API_URL = "http://localhost:11434"  # Ollama地址
LLM_MODEL = "llama2"                    # 使用模型
```

## 📈 性能优化

### 数据库优化

- **按月分表**: `stock_daily_YYYYMM`减少单表数据量
- **索引优化**: 为`ts_code + trade_date`建立联合索引
- **批量插入**: 使用`pandas.to_sql(chunksize=1000)`
- **连接池**: SQLAlchemy连接池管理

### 并发控制

- **频率控制**: TuShare API限频500次/分钟
- **多线程**: 5个并发线程获取数据
- **错误重试**: 失败请求自动重试机制

## 🛠️ 定时任务

### 任务调度

- **每日17:00**: 更新昨日股票数据
- **每日18:00**: 重新计算推荐结果  
- **每周日23:00**: 清理历史日志

### 手动执行

```bash
# 数据更新
python scheduler/daily_tasks.py update

# 推荐更新  
python scheduler/daily_tasks.py recommend

# 清理任务
python scheduler/daily_tasks.py cleanup
```

## 🐳 容器化部署

### 服务组件

- **mysql**: MySQL 8.0数据库
- **redis**: Redis缓存服务
- **stock_app**: Flask API应用
- **scheduler**: 定时任务调度器

### 常用命令

```bash
# 启动服务
docker-compose up -d

# 重启特定服务
docker-compose restart stock_app

# 查看日志
docker-compose logs -f scheduler

# 进入容器
docker-compose exec stock_app bash

# 停止服务
docker-compose down
```

## 📝 开发指南

### 添加新策略

1. 在`analysis/recommender.py`中添加策略方法
2. 在`generate_recommendations`中注册策略
3. 更新API文档和测试用例

### 扩展LLM功能

1. 在`llm/interface.py`中添加新方法
2. 在`api/app.py`中添加对应接口
3. 更新前端页面模板

### 数据库扩展

1. 在`sql/init_tables.sql`中添加表结构
2. 在`utils/db_helper.py`中添加操作方法
3. 更新相关业务逻辑

## ⚠️ 注意事项

1. **TuShare限制**: 请遵守API调用频率限制
2. **数据存储**: 历史数据量较大，建议定期清理
3. **服务器资源**: Mac Studio M2 Ultra配置足够支撑全量数据
4. **安全配置**: 生产环境请修改默认密码
5. **备份策略**: 重要数据请定期备份

## 🔮 后续规划

- [ ] 更多技术指标策略
- [ ] 机器学习模型集成
- [ ] 实时行情数据流
- [ ] 移动端APP接口
- [ ] 量化交易信号
- [ ] 风险控制模块

## 📞 技术支持

如有问题，请查看日志文件或提交Issue：

```bash
# 查看应用日志
tail -f logs/app.log

# 查看数据库日志
docker-compose logs mysql

# 系统监控
curl http://localhost:5000/api/stats
``` 