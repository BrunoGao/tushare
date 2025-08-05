# LJWX-Stock 智能股票分析系统文档中心

## 📚 文档导航

欢迎来到LJWX-Stock智能股票分析系统的文档中心。这里包含了系统的完整技术文档、用户指南和API说明。

---

### 🎯 核心文档

#### 📖 [技术规格书](ljwx-stock-technical-specification.md)
**完整的系统技术规格和架构说明**
- 系统架构设计
- 核心模块详解  
- AI模型技术栈
- 数据库设计
- 部署和运维指南
- 性能指标和优化

#### 👥 [用户使用手册](ljwx-stock-user-manual.md)
**面向投资者和分析师的使用指南**
- 快速入门指南
- 核心功能详解
- AI推荐系统使用
- 策略管理操作
- 常见问题解答
- 最佳实践建议

#### 🔌 [API接口文档](ljwx-stock-api-documentation.md)
**完整的REST API和WebSocket接口说明**
- API概览和认证
- 核心接口详解
- WebSocket实时接口
- 错误码说明
- SDK使用示例
- 请求限制和规范

---

### 🏗️ 系统概述

LJWX-Stock是一个基于人工智能大模型的股票分析与推荐系统，具备以下核心特性：

#### ✨ 主要功能
- **🤖 AI智能推荐** - 基于大语言模型的股票投资建议
- **📊 技术分析** - 50+项技术指标的专业分析
- **📈 策略管理** - 自定义交易策略和回测验证
- **🔄 实时数据** - 实时市场数据获取和分析
- **📱 多端支持** - Web端、移动端、API接口
- **🧪 回测验证** - 历史数据验证推荐效果

#### 🎯 技术亮点
- **Apple M2 Ultra优化** - 专门针对Apple Silicon优化
- **大规模训练** - 50,000+训练样本，覆盖5,000+只股票
- **5年历史数据** - 基于5年全市场历史数据训练
- **多模型支持** - 传统ML + 深度学习 + 大语言模型
- **企业级架构** - 分布式部署、高可用设计

#### 📊 性能指标
- **推荐命中率**: 65%+ (历史平均)
- **数据处理**: 5000+股票，1000万+记录
- **响应速度**: API响应 < 200ms
- **系统可用性**: 99.5%+

---

### 🚀 快速开始

#### 1. 系统要求
```
硬件要求:
- CPU: Apple Silicon M2 Ultra (推荐)
- 内存: 32GB+ 
- 存储: 100GB+ 可用空间

软件要求:
- macOS 13.0+ (Ventura)
- Python 3.12+
- MySQL 8.0+
- Ollama (AI模型服务)
```

#### 2. 安装部署
```bash
# 1. 克隆项目
git clone https://github.com/your-org/ljwx-stock.git
cd ljwx-stock

# 2. 创建虚拟环境
python3 -m venv venv_m2_native
source venv_m2_native/bin/activate

# 3. 安装依赖
pip install -r requirements.txt

# 4. 初始化数据库
python scripts/data_initialization.py

# 5. 启动服务
python unified_app.py
```

#### 3. 访问系统
- **Web界面**: http://localhost:5005
- **移动端API**: http://localhost:5005/api/mobile/
- **管理界面**: http://localhost:5005/admin

---

### 📁 项目结构

```
ljwx-stock/
├── 📄 unified_app.py          # 统一应用入口
├── ⚙️ config.py               # 系统配置
├── 🤖 ai/                     # AI模型模块
│   ├── unified_trainer.py     # 统一训练器
│   └── model_evaluation.py    # 模型评估
├── 🧠 llm/                    # 大语言模型
│   ├── intelligent_recommender.py  # 智能推荐
│   ├── stock_analyzer.py      # 股票分析器
│   └── tushare_data_extractor.py   # 数据提取
├── 📊 analysis/               # 分析模块
│   ├── recommender.py         # 推荐分析
│   └── technical_indicators.py     # 技术指标
├── 🎯 strategy/               # 策略模块
│   ├── strategy_engine.py     # 策略引擎
│   └── strategy_models.py     # 策略模型
├── 💾 database/               # 数据库模块
│   └── db_manager.py          # 数据库管理
├── 🌐 api/                    # API接口
│   └── unified_api.py         # 统一API
├── 📈 frontend/               # 前端模块
│   └── chart_generator.py     # 图表生成
├── ⏰ scheduler/              # 调度模块
│   └── daily_tasks.py         # 定时任务
├── 🔄 recommendation_backtest/ # 推荐回测
│   ├── recommendation_tracker.py   # 推荐跟踪
│   └── model_recommender.py   # 模型推荐器
├── 🛠️ utils/                  # 工具模块
├── 📝 templates/              # 前端模板
├── 📊 data/                   # 数据目录
├── 📋 logs/                   # 日志目录
└── 📚 docs/                   # 文档目录
    ├── README.md              # 文档索引
    ├── ljwx-stock-technical-specification.md  # 技术规格
    ├── ljwx-stock-user-manual.md              # 用户手册
    └── ljwx-stock-api-documentation.md        # API文档
```

---

### 🔧 核心模块说明

#### 🤖 AI智能推荐模块
- **智能推荐器** (`llm/intelligent_recommender.py`)
  - 基于Ollama的多模态分析
  - 市场环境智能感知
  - 综合评分和风险评估

- **股票分析器** (`llm/stock_analyzer.py`)  
  - 自然语言股票分析
  - 多维度分析报告
  - 智能问答交互

#### 📊 数据处理模块
- **数据提取器** (`llm/tushare_data_extractor.py`)
  - TuShare Pro API集成
  - 技术指标自动计算
  - 批量数据处理优化

- **数据库管理** (`database/db_manager.py`)
  - MySQL分表存储策略
  - 批量操作优化
  - 连接池管理

#### 🎯 策略引擎模块
- **策略引擎** (`strategy/strategy_engine.py`)
  - 多种技术指标支持
  - 信号生成和评分
  - 回测引擎集成

- **推荐分析** (`analysis/recommender.py`)
  - 多策略综合评分
  - 推荐排序算法
  - 实时推荐生成

#### 🌐 Web服务模块
- **统一应用** (`unified_app.py`)
  - Flask Web服务器
  - Socket.IO实时通信
  - 多端API支持

- **图表生成器** (`frontend/chart_generator.py`)
  - Plotly专业图表
  - K线和技术指标可视化
  - 交互式图表支持

---

### 📊 功能特性

#### 🎯 自定义AI策略
```python
# 策略定义示例
strategy = {
    "name": "MA双线策略",
    "buy_conditions": [
        "MA5 > MA20",           # 5日均线大于20日均线
        "RSI < 70",             # RSI不超买
        "Volume > AvgVolume*1.2" # 成交量放大
    ],
    "sell_conditions": [
        "MA5 < MA20",           # 均线死叉
        "Price < StopLoss"      # 止损
    ],
    "risk_management": {
        "stop_loss": 0.05,      # 5%止损
        "take_profit": 0.10     # 10%止盈
    }
}
```

#### 🤖 自动推荐流程
1. **数据获取** - 实时获取市场数据
2. **特征工程** - 提取50+项技术特征
3. **AI分析** - 大语言模型深度分析
4. **风险评估** - 多维度风险量化
5. **推荐生成** - 生成投资建议
6. **回测验证** - 历史数据验证效果

#### 📈 技术指标支持
- **趋势指标**: MA、EMA、MACD、ADX
- **动量指标**: RSI、KDJ、ROC、MFI  
- **波动指标**: 布林带、ATR、VIX
- **成交量指标**: OBV、VWAP、PVT
- **支撑阻力**: 斐波那契、关键价位

---

### 🛠️ 开发指南

#### 环境配置
```bash
# M2 Ultra优化配置
export ARCHFLAGS="-arch arm64"
export CPPFLAGS="-I/opt/homebrew/include"
export LDFLAGS="-L/opt/homebrew/lib"

# Python环境
python3 -m venv venv_m2_native
source venv_m2_native/bin/activate
pip install --upgrade pip
```

#### 代码规范
- **Python**: 遵循PEP 8标准
- **注释**: 中文注释，详细说明
- **类型提示**: 使用类型注解
- **错误处理**: 完善的异常处理
- **日志记录**: 结构化日志输出

#### 测试指南
```bash
# 运行单元测试
python -m pytest tests/ -v

# 集成测试
python test_integration.py

# 性能测试
python test_m2_ultra_performance.py

# AI系统测试
python test_ai_system.py
```

---

### 📞 技术支持

#### 📚 学习资源
- [系统架构文档](ai_training_architecture.md)
- [M2优化指南](m2_ultra_optimization_plan.md) 
- [WebSocket开发指南](../WEBSOCKET_GUIDE.md)
- [Web界面使用指南](../WEB_INTERFACE_GUIDE.md)

#### 🐛 问题反馈
- **GitHub Issues**: https://github.com/your-org/ljwx-stock/issues
- **技术支持**: tech-support@ljwx.com
- **功能建议**: feedback@ljwx.com

#### 💬 社区支持
- **开发者交流群**: 微信群二维码
- **用户社区**: 官方论坛链接
- **知识库**: 常见问题解答

---

### 📄 许可证

本项目采用 MIT 许可证。详见 [LICENSE](../LICENSE) 文件。

### 🤝 贡献指南

欢迎提交Pull Request和Issue。请阅读 [CONTRIBUTING.md](../CONTRIBUTING.md) 了解贡献指南。

### 📈 更新日志

查看 [CHANGELOG.md](../CHANGELOG.md) 了解版本更新历史。

---

### 📊 系统监控

#### 实时状态检查
```bash
# 检查系统状态
curl http://localhost:5005/api/status

# 检查健康状态  
python utils/system_monitor.py --health-check

# 查看性能指标
python utils/system_monitor.py --metrics
```

#### 日志监控
```bash
# 应用日志
tail -f logs/api_server.log

# 训练日志
tail -f logs/ai_training.log

# 错误日志
grep -i error logs/*.log
```

---

**⚠️ 免责声明**: 
本系统提供的股票分析和投资建议仅供参考，不构成投资建议。投资有风险，决策需谨慎。用户应根据自身情况独立判断，系统开发者不承担任何投资损失责任。

---

*文档最后更新: 2025-08-05*  
*系统版本: v2.0.0*  
*维护团队: LJWX开发团队*