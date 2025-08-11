# LJWX-Stock 智能股票分析系统

## 项目概述

ljwx-stock 是一个基于AI大模型的智能股票分析与推荐系统，提供实时数据推送、专业技术分析、AI智能问答等功能。

## 核心功能

### 🤖 AI智能分析
- 基于Ollama大语言模型的股票分析
- 自然语言问答式股票咨询
- 智能投资建议和风险评估
- 50,000+训练样本，覆盖5,000+股票

### 📊 技术分析引擎
- 30+专业技术指标：MA、MACD、RSI、KDJ、布林带、ATR、ADX等
- 智能信号生成：买入/卖出/持有信号
- K线形态识别：十字星、锤子线、吞噬形态
- 支撑阻力位自动计算

### 🔌 实时数据系统
- WebSocket实时推送股价和技术指标
- 10秒更新频率，超高频数据推送
- 智能缓存机制，热门股票预加载
- 自动重连，保证数据连续性

### 📈 多维度数据管理
- 基本面信息：股票基本信息、行业分类、财务数据
- 资金流向：个股资金流、龙虎榜、主力资金监控
- 公告信息：分红送股、股票回购、重要公告
- 宏观数据：沪深港通、融资融券、CPI/PPI/GDP

## 系统架构

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

## 技术栈

### 后端技术
- **Python 3.12+**: 主要开发语言
- **Flask**: Web框架和API服务
- **SQLAlchemy**: ORM数据库操作
- **MySQL**: 主数据库存储
- **Redis**: 缓存和会话管理
- **WebSocket**: 实时数据推送

### AI和机器学习
- **Ollama**: 大语言模型集成
- **TensorFlow**: 深度学习框架
- **scikit-learn**: 机器学习算法
- **TA-Lib**: 技术指标计算库
- **MLflow**: 模型管理和实验跟踪

### 数据源
- **TuShare Pro**: 主要股票数据API
- **实时行情**: WebSocket推送
- **历史数据**: 5年全市场数据

### 前端技术
- **HTML/CSS/JavaScript**: 基础前端技术
- **ECharts**: 专业股票图表库
- **Alpine.js**: 轻量级前端框架
- **WebSocket Client**: 实时数据接收

## 项目结构

```
ljwx-stock/
├── 🚀 start_realtime_system.py    # 一键启动脚本
├── 📊 unified_app.py               # 统一应用入口
├── ⚙️ config.py                   # 系统配置文件
├── 🤖 ai/                         # AI模型模块
│   ├── unified_trainer.py         # 统一训练器
│   ├── model_evaluation.py        # 模型评估
│   └── continuous_learning_system.py # 持续学习
├── 🧠 llm/                        # 大语言模型
│   ├── intelligent_recommender.py # 智能推荐
│   ├── stock_analyzer.py          # 股票分析器
│   └── tushare_data_extractor.py  # 数据提取
├── 📊 analysis/                   # 分析模块
│   ├── recommender.py             # 推荐分析
│   └── technical_indicators.py    # 技术指标
├── 🎯 strategy/                   # 策略模块
│   ├── strategy_engine.py         # 策略引擎
│   └── strategy_models.py         # 策略模型
├── 💾 database/                   # 数据库模块
│   ├── db_manager.py              # 数据库管理
│   ├── database_system.py         # 数据库系统
│   └── task_scheduler.py          # 任务调度
├── 🌐 api/                        # API接口
│   ├── app.py                     # Flask API
│   ├── unified_api.py             # 统一API
│   └── websocket_server.py        # WebSocket服务
├── 📈 frontend/                   # 前端模块
│   ├── realtime_dashboard.html    # 实时界面
│   └── chart_generator.py         # 图表生成
├── ⏰ scheduler/                  # 调度模块
│   ├── daily_tasks.py             # 定时任务
│   └── auto_scheduler.py          # 自动调度
├── 🔄 recommendation_backtest/    # 推荐回测
│   ├── recommendation_tracker.py  # 推荐跟踪
│   └── model_recommender.py       # 模型推荐器
├── 🛠️ utils/                      # 工具模块
├── 📝 templates/                  # 前端模板
├── 📊 static/                     # 静态资源
├── 📚 docs/                       # 文档目录
└── 🎯 harmonyos_client/           # HarmonyOS客户端
```

## 快速开始

### 环境要求
- Python 3.12+
- MySQL 8.0+
- Redis (可选)
- Apple Silicon M2 Ultra (推荐，已优化)

### 安装部署
```bash
# 1. 克隆项目
git clone <repository-url>
cd tushare

# 2. 创建虚拟环境
python3 -m venv venv
source venv/bin/activate  # Windows: venv\Scripts\activate

# 3. 安装依赖
pip install -r requirements.txt

# 4. 配置环境变量
cp env.example .env
# 编辑 .env 文件，配置数据库和API密钥

# 5. 初始化数据库
python3 start_realtime_system.py init

# 6. 启动服务
python3 start_realtime_system.py
```

### 配置说明
在 `config.py` 中配置以下关键参数：

```python
# TuShare配置
TS_TOKEN = 'your_tushare_token'

# 数据库配置
DB_HOST = '127.0.0.1'
DB_PORT = 3306
DB_USER = 'root'
DB_PASSWORD = 'your_password'
DB_NAME = 'ljwx_stock'

# AI模型配置
OPENAI_API_KEY = 'your_api_key'
OPENAI_BASE_URL = 'http://localhost:11434/v1'
LLM_MODEL = 'lingjingwanxiang:70b'
```

## 主要命令

### 启动服务
```bash
# 启动完整实时系统
python3 start_realtime_system.py

# 仅启动API服务
python3 api/app.py

# 仅启动WebSocket服务
python3 api/websocket_server.py
```

### 数据管理
```bash
# 获取股票基本信息
python3 scripts/fetch_stock_basic.py

# 获取历史数据
python3 run.py fetch-10years

# 全维度数据获取
python3 run.py fetch-comprehensive --mode full

# 启动定时调度器
python3 run.py scheduler --scheduler-cmd start
```

### AI模型训练
```bash
# 创建基础模型
python3 create_ljwx_stock_model.py

# 训练模型
python3 train_ljwx_stock.py

# 持续训练
python3 continuous_training.py

# 综合训练
python3 comprehensive_training.py
```

### 测试命令
```bash
# 系统集成测试
python3 test_integration.py

# AI系统测试
python3 test_ai_system.py

# 性能测试
python3 test_m2_ultra_performance.py

# WebSocket连接测试
python3 test_websocket_connection.py
```

## API接口

### REST API
- **基础接口**: `http://localhost:5005/api/`
- **技术分析**: `/api/technical/indicators/{ts_code}`
- **实时信号**: `/api/signals/realtime`
- **AI分析**: `/api/llm/intelligent`
- **股票搜索**: `/api/stocks/search`

### WebSocket
- **连接地址**: `ws://localhost:8765`
- **订阅股票**: `{"type": "subscribe", "stock_codes": ["000001.SZ"]}`
- **获取实时数据**: `{"type": "get_realtime", "ts_code": "000001.SZ"}`
- **AI查询**: `{"type": "ai_query", "query": "分析000001"}`

## 性能指标

- **推荐命中率**: 65%+ (历史平均)
- **数据处理**: 5000+股票，1000万+记录
- **API响应**: < 200ms
- **WebSocket更新**: 10秒频率
- **系统可用性**: 99.5%+
- **训练数据**: 50,000+样本

## 开发指南

### 代码规范
- 遵循PEP 8 Python编码规范
- 使用中文注释，详细说明功能
- 添加类型提示提高代码可读性
- 完善的异常处理和日志记录

### 测试规范
- 运行完整测试套件验证功能
- 新功能必须包含相应测试用例
- 集成测试确保模块间协作正常
- 性能测试验证系统响应能力

### 部署注意事项
- 确保MySQL和Redis服务正常运行
- 配置正确的TuShare Pro Token
- AI模型服务(Ollama)需要独立部署
- 生产环境建议使用Gunicorn+Nginx

## 故障排除

### 常见问题
1. **数据库连接失败**: 检查MySQL服务状态和配置
2. **TuShare API限制**: 检查Token有效性和请求频率
3. **WebSocket连接断开**: 检查网络连接和防火墙设置
4. **AI模型响应慢**: 检查Ollama服务状态和模型加载

### 日志查看
```bash
# 应用日志
tail -f logs/api_server.log

# 训练日志
tail -f logs/ai_training.log

# 错误日志
grep -i error logs/*.log
```

## 许可证

MIT License - 详见 [LICENSE](LICENSE) 文件

## 贡献指南

欢迎提交Pull Request和Issue。请确保：
1. 代码符合项目规范
2. 包含必要的测试用例
3. 更新相关文档
4. 通过所有自动化测试

---

**⚠️ 投资风险提示**: 
本系统提供的股票分析和投资建议仅供参考，不构成投资建议。投资有风险，决策需谨慎。用户应根据自身情况独立判断，系统开发者不承担任何投资损失责任。

---

*文档版本: v2.0.0*  
*最后更新: 2025-08-11*  
*维护团队: LJWX开发团队*