# 🚀 灵境万象股票分析系统 - 实时版本

> **WebSocket实时推送 | 专业技术分析 | AI智能问答 | 缓存优化的高性能股票分析平台**

## 🎯 核心功能特性

### 📊 **专业技术分析引擎**
- **30+技术指标**: MA、MACD、RSI、KDJ、布林带、ATR、ADX等专业指标
- **智能信号生成**: 多指标综合判断，生成买入/卖出/持有信号
- **价格形态识别**: 十字星、锤子线、吞噬形态等K线模式识别
- **支撑阻力位**: 自动计算并显示关键价位
- **强弱度评分**: 0-100分综合强弱度量化评估

### 🤖 **AI智能分析师**
- **自然语言理解**: 支持中文股票问答，智能解析用户意图
- **专业分析报告**: 基于技术指标生成专业级分析报告
- **市场趋势分析**: 行业对比、市场概览、投资建议
- **多股票对比**: 同时分析对比多只股票优劣势
- **风险评估**: 投资风险提示和建议

### 🔌 **WebSocket实时推送系统**
- **实时行情推送**: WebSocket推送最新价格、涨跌幅、成交量
- **技术指标实时更新**: MA、RSI、MACD等指标实时计算推送
- **实时图表渲染**: ECharts实时更新K线走势和技术指标
- **智能缓存机制**: 热门股票预加载，提升响应速度
- **连接状态监控**: 自动重连，保证数据连续性

### 📈 **实时交易信号监控**
- **订阅式监控**: 一键订阅/取消股票实时数据
- **多股票并发**: 同时监控多只股票实时信号
- **图表信号标注**: 在K线图上直观显示买卖点
- **强弱度实时显示**: 动态更新股票强弱度指标
- **10秒更新频率**: 超高频数据推送

### 💻 **专业级前端界面**
- **响应式设计**: 支持PC/平板/手机多端适配
- **实时数据面板**: 股价、涨跌幅、成交量实时更新
- **技术指标图表**: ECharts专业K线图和技术指标展示
- **AI问答界面**: 智能对话式股票分析助手
- **订阅管理**: 可视化股票订阅和监控管理

### **🗂️ 全维度数据管理系统** ⭐NEW⭐
- **基本面信息**: 股票基本信息、行业分类、指数成分、股本结构、公司高管
- **财务类信息**: 资产负债表、利润表、现金流量表、财务指标、业绩预告/快报  
- **资金流向**: 个股资金流、龙虎榜数据、主力资金监控
- **股东及股权**: 十大股东、流通股东、股东户数、限售解禁
- **公告类信息**: 分红送股、股票回购、重要公告
- **行情扩展**: 停复牌、涨跌停、概念板块分类
- **宏观经济**: 沪深港通、融资融券、CPI/PPI/GDP等宏观指标

### **⏰ 智能调度系统** ⭐NEW⭐ 
- **定时任务**: 基本信息周更新、财务季更新、资金流日更新
- **错误重试**: 失败任务自动重试，指数退避策略
- **健康监控**: 数据库连接、API状态、磁盘空间监控
- **手动执行**: 支持单个任务手动触发和全量/增量模式

## 🏗️ 系统架构

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│  WebSocket服务  │    │   技术分析引擎   │    │   AI分析引擎    │
│  实时数据推送    │◀──▶│   30+指标计算   │◀──▶│   LLM智能分析   │
│  订阅管理       │    │   信号生成      │    │   自然语言理解   │
└─────────────────┘    └─────────────────┘    └─────────────────┘
         │                       │                       │
         ▼                       ▼                       ▼
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   数据库层      │    │   Flask API层   │    │   前端展示层     │
│ ljwx_stock DB   │◀──▶│   RESTful API   │◀──▶│   专业Web界面   │
│ MySQL + Redis   │    │   CORS支持      │    │   实时图表      │
└─────────────────┘    └─────────────────┘    └─────────────────┘
```

## 🚀 快速开始

### 1. 环境准备
```bash
# 1. 克隆仓库
git clone <repository-url>
cd tushare

# 2. 创建虚拟环境
python3 -m venv venv
source venv/bin/activate  # Windows: venv\Scripts\activate

# 3. 安装依赖
pip install -r requirements.txt
```

### 2. 配置设置
```bash
# 1. 复制环境变量配置
cp env.example .env

# 2. 编辑配置文件
nano config.py

# 必须配置项:
# - TS_TOKEN: TuShare Pro Token
# - DB_PASSWORD: MySQL密码  
# - OPENAI_API_KEY: OpenAI API密钥 (或兼容接口)
# - OPENAI_BASE_URL: AI模型API地址 (如http://14.127.218.229:11434/v1)
```

### 3. 数据库初始化
```bash
# 1. 初始化数据库结构
python3 start_realtime_system.py init

# 2. 获取股票基本信息
python3 scripts/fetch_stock_basic.py

# 3. 获取历史数据 (后台运行)
python3 run.py fetch-10years &
```

### 4. 启动服务

#### 🚀 **一键启动实时系统** (推荐)
```bash
# 启动完整实时系统
python3 start_realtime_system.py

# 访问地址:
# - 前端界面: http://localhost:5005/frontend/realtime_dashboard.html
# - API文档: http://localhost:5005/api/stats
# - WebSocket: ws://localhost:8765
```

#### 📊 **单独启动服务**
```bash
# 仅启动API服务
python3 api/app.py

# 仅启动WebSocket服务
python3 api/websocket_server.py
```

## 🌟 新功能特性

### **实时股票监控界面**
1. **订阅管理**: 输入股票代码一键订阅实时数据
2. **实时价格面板**: 显示股价、涨跌幅、成交量等核心指标
3. **技术指标展示**: MA5/MA20、RSI、MACD等关键指标实时更新
4. **交易信号**: 智能显示买入🟢/卖出🔴/持有🟡信号
5. **行业板块**: 显示股票所属行业和板块信息

### **WebSocket实时推送**
- **10秒更新**: 超高频实时数据推送
- **智能缓存**: 30秒缓存机制，减少重复请求
- **断线重连**: 自动检测连接状态并重连
- **批量订阅**: 支持同时监控多只股票

### **AI智能问答系统**
```javascript
// 支持的问题类型：
"分析一下000001的走势"
"推荐几只好股票"  
"银行股怎么样"
"000001和000002哪个更好"
"当前市场趋势如何"
```

### **技术指标图表**
- **ECharts集成**: 专业级股票图表库
- **多指标展示**: K线+均线+成交量+技术指标
- **实时更新**: 图表数据10秒自动刷新
- **交互功能**: 支持缩放、数据点查看

## 📋 使用指南

### **基础操作**
1. **启动系统**: `python3 start_realtime_system.py`
2. **访问界面**: 浏览器打开 `http://localhost:5005/frontend/realtime_dashboard.html`
3. **订阅股票**: 输入股票代码(如: 000001.SZ)点击订阅
4. **查看数据**: 实时监控面板显示股价和技术指标
5. **AI咨询**: 在AI助手界面提问股票相关问题

### **高级功能**
1. **批量订阅**: 输入多个股票代码(逗号分隔)
2. **图表分析**: 选择股票查看详细技术指标图表
3. **信号监控**: 关注买入/卖出信号变化
4. **行业分析**: 查看同行业股票对比

### **全维度数据获取** ⭐NEW⭐
```bash
# 初始化全维度数据库表结构
python3 run.py init-schema

# 获取所有维度数据（首次全量获取）
python3 run.py fetch-comprehensive --mode full

# 增量更新特定类别数据
python3 run.py fetch-comprehensive --category financial --mode incremental
python3 run.py fetch-comprehensive --category money_flow --mode incremental

# 可用数据类别:
# basic - 基本面信息 | financial - 财务数据 | money_flow - 资金流向
# shareholder - 股东数据 | announcement - 公告数据 | market_ext - 行情扩展  
# macro - 宏观数据 | all - 全部数据
```

### **数据清洗和错误处理** ⭐NEW⭐
系统内置智能数据清洗机制，自动处理常见数据质量问题：

- **空值过滤**: 自动过滤`ann_date`等关键字段为空的记录
- **日期转换**: 智能转换各种日期格式，处理异常日期
- **重试机制**: API调用失败时自动重试，使用指数退避策略
- **备用接口**: 行业分类等接口失败时自动切换备用接口

配置项在`config.py`中的`DATA_CLEAN_RULES`和`ERROR_HANDLING`中管理。

### **智能调度管理** ⭐NEW⭐
```bash
# 启动定时调度器（后台运行）
python3 run.py scheduler --scheduler-cmd start

# 查看调度状态
python3 run.py scheduler --scheduler-cmd status

# 手动执行特定任务
python3 run.py scheduler --scheduler-cmd run --task money_flow --mode incremental

# 系统健康检查
python3 run.py health-check

# 可用任务:
# stock_basic - 基本信息 | financial_data - 财务数据 | money_flow - 资金流向
# shareholder_data - 股东数据 | announcement_data - 公告数据
# market_extension - 行情扩展 | macro_data - 宏观数据
```

## 🛠️ API接口文档

### **WebSocket消息格式**

#### 订阅股票
```json
{
  "type": "subscribe",
  "stock_codes": ["000001.SZ", "000002.SZ"]
}
```

#### 获取实时数据
```json
{
  "type": "get_realtime",
  "ts_code": "000001.SZ"
}
```

#### AI查询
```json
{
  "type": "ai_query", 
  "query": "分析一下000001的走势",
  "ts_code": "000001.SZ"
}
```

### **REST API接口**

#### 技术分析
```http
GET /api/technical/indicators/000001.SZ?days=60
```

#### 实时信号
```http
POST /api/signals/realtime
Content-Type: application/json
{
  "stock_codes": ["000001.SZ", "000002.SZ"]
}
```

#### AI分析
```http
POST /api/llm/intelligent
Content-Type: application/json
{
  "question": "分析一下银行股的投资机会"
}
```

## 📁 项目结构

```
tushare/
├── 🚀 start_realtime_system.py    # 一键启动脚本
├── 📊 utils/                      # 核心工具模块
│   ├── technical_indicators.py    # 技术指标计算
│   ├── llm_analyzer.py           # LLM智能分析
│   └── db_helper.py              # 数据库操作
├── 🌐 api/                       # API服务
│   ├── app.py                    # Flask REST API
│   └── websocket_server.py       # WebSocket实时服务
├── 🎨 frontend/                  # 前端界面
│   └── realtime_dashboard.html   # 实时分析界面
├── 🔧 scripts/                   # 数据脚本
│   ├── fetch_stock_basic.py      # 获取基本信息
│   └── fetch_stock_daily.py      # 获取历史数据
├── ⚙️ config.py                  # 统一配置
├── 📄 requirements.txt           # 依赖包
└── 📖 README.md                  # 项目文档
```

## 📅 更新日志

### **2025-08-12 - 策略驱动训练与验证系统** ⭐NEW⭐
- **🎯 策略训练系统**: 新增完整的策略驱动AI训练框架 (`strategy_training/`)
  - `dataset_generator.py`: 从投资策略生成高质量训练数据集
  - `unified_trainer.py`: 统一模型训练，支持多策略融合
  - `recommendation_scheduler.py`: 异步推荐生成调度系统
- **✅ 策略验证系统**: 新增高级策略回测验证 (`strategy_validation/`) 
  - `advanced_validator.py`: 专业级策略性能验证与报告生成
  - 支持夏普比率、最大回撤、年化收益等专业指标
- **🔌 新增API端点**: 
  - `api/strategy_training_api.py`: 策略训练管理API (端口5005)
  - `api/strategy_validation_api.py`: 策略验证API (端口5006)
- **📊 增强推荐系统**:
  - 集成策略训练生成的推荐到实时系统
  - WebSocket推送策略驱动的AI推荐
  - 个性化推荐面板实时更新
- **🎨 前端界面优化**:
  - `templates/strategy_validation.html`: 策略验证专业界面
  - `templates/user_dashboard.html`: 优化推荐展示体验
- **📚 完善文档**: `docs/strategy-validation-guide.md` 详细使用指南

### **主要技术特性**:
```bash
# 策略训练API使用示例
curl -X POST http://localhost:5005/api/strategy-training/dataset/generate-all \
  -H "Content-Type: application/json" \
  -d '{"num_stocks": 30, "time_range": 180}'

# 策略验证API使用示例  
curl -X POST http://localhost:5006/api/validate-strategy \
  -H "Content-Type: application/json" \
  -d '{"strategy_type": "rsi_mean_reversion", "symbol": "000001.SZ"}'
```

## 🔧 配置说明

### **数据库配置**
```python
DB_HOST = '127.0.0.1'            # MySQL地址
DB_PORT = 3306                   # MySQL端口  
DB_USER = 'root'                 # 数据库用户
DB_PASSWORD = 'your_password'    # 数据库密码
DB_NAME = 'ljwx_stock'           # 数据库名(固定)
```

### **WebSocket配置**
```python
WS_HOST = '0.0.0.0'              # WebSocket绑定地址
WS_PORT = 8765                   # WebSocket端口
REALTIME_UPDATE_INTERVAL = 10    # 实时更新间隔(秒)
MAX_CONNECTIONS = 100            # 最大连接数
```

### **AI配置**
```python
OPENAI_API_KEY = 'sk-xxx'        # OpenAI API密钥
OPENAI_BASE_URL = 'http://localhost:11434/v1'  # 本地Ollama地址
LLM_MODEL = 'lingjingwanxiang:70b'  # 使用的模型
```