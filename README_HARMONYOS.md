# 灵境万象股票分析系统

## 项目概述

灵境万象是一个全栈股票分析系统，集成了传统技术分析、AI智能推荐和实时数据推送功能。支持Web端、鸿蒙原生应用和小程序多平台客户端，提供专业的股票投资决策支持。

## 系统架构

```
灵境万象股票分析系统
├── 后端服务 (Python Flask)
│   ├── 数据获取 (TuShare Pro API)
│   ├── 技术分析引擎
│   ├── AI机器学习模型
│   ├── 智能推荐系统 (Ollama LLM)
│   └── 实时数据推送 (WebSocket)
├── 统一API接口层
│   ├── RESTful API (Flask)
│   ├── 认证与鉴权
│   ├── 请求限流
│   └── 跨平台数据同步
├── 客户端
│   ├── Web前端 (HTML5 + JavaScript)
│   ├── 鸿蒙原生应用 (ArkTS)
│   └── 小程序 (待开发)
└── 数据存储
    ├── MySQL (结构化数据)
    ├── Redis (缓存 + 实时数据)
    └── 本地存储 (模型文件)
```

## 核心功能

### 📊 技术分析
- **完整技术指标**: MA、RSI、MACD、KDJ、布林带等
- **多时间周期**: 日线、周线、月线分析
- **自动信号识别**: 金叉死叉、突破信号等
- **支撑阻力位**: 自动计算关键价位

### 🤖 AI智能分析
- **多模型集成**: RandomForest、GradientBoosting、神经网络
- **特征工程**: 15+维度特征提取
- **预测准确率**: 92%+ (GradientBoosting模型)
- **LLM增强**: Ollama本地大模型分析

### 📈 实时数据
- **WebSocket推送**: 毫秒级数据更新
- **多股票监控**: 同时跟踪多只股票
- **实时信号**: 即时买卖信号提醒
- **价格预警**: 自定义价格提醒

### 🔄 跨平台同步
- **统一数据**: 自选股、设置等云端同步
- **多端协同**: Web、鸿蒙、小程序数据一致
- **冲突解决**: 智能数据冲突处理
- **离线支持**: 本地缓存 + 增量同步

## 技术栈

### 后端技术
- **框架**: Flask + SQLAlchemy
- **数据库**: MySQL 8.0 + Redis 6.0
- **机器学习**: scikit-learn + pandas + numpy
- **大语言模型**: Ollama (本地部署)
- **实时通信**: WebSocket + asyncio
- **数据源**: TuShare Pro API

### 前端技术
- **Web端**: HTML5 + CSS3 + JavaScript (ES6+)
- **图表库**: ECharts 5.0
- **UI框架**: Bootstrap 5 + 自定义组件
- **实时通信**: WebSocket API

### 鸿蒙端技术
- **开发语言**: ArkTS (TypeScript)
- **UI框架**: ArkUI 声明式开发
- **网络请求**: @kit.NetworkKit
- **数据存储**: @kit.ArkData Preferences
- **状态管理**: 响应式数据绑定

## 快速开始

### 环境要求
- Python 3.8+
- Node.js 16+ (开发工具)
- MySQL 8.0
- Redis 6.0+
- DevEco Studio 4.0+ (鸿蒙开发)

### 安装步骤

#### 1. 克隆项目
```bash
git clone https://github.com/yourusername/tushare-analysis-system.git
cd tushare-analysis-system
```

#### 2. 后端设置
```bash
# 安装Python依赖
pip install -r requirements.txt
pip install -r requirements_ai.txt

# 配置数据库
mysql -u root -p < database/schema.sql

# 配置环境变量
cp .env.example .env
# 编辑 .env 文件，填入TuShare API Token等配置

# 启动后端服务
python api/app.py
```

#### 3. 前端设置
```bash
# Web端可直接通过浏览器访问
http://localhost:5005

# 或使用静态文件服务器
cd frontend
python -m http.server 8080
```

#### 4. 鸿蒙端设置
```bash
# 使用DevEco Studio打开 harmonyos_client 目录
# 配置签名和包名
# 编译安装到鸿蒙设备或模拟器
```

### 配置说明

#### 主要配置文件
- `config.py`: 系统主配置
- `.env`: 环境变量配置
- `harmonyos_client/src/main/resources/rawfile/config.json`: 鸿蒙端配置

#### 重要配置项
```python
# TuShare配置
TS_TOKEN = "your_tushare_token"

# 数据库配置
DB_HOST = "127.0.0.1"
DB_USER = "root"
DB_PASSWORD = "your_password"
DB_NAME = "ljwx_stock"

# LLM配置
LLM_API_URL = "http://localhost:11434"
LLM_MODEL = "lingjingwanxiang:70b"
```

## API文档

### 认证方式
所有API请求需要包含以下header：
```
X-API-Key: ljwx_harmonyos_client
Content-Type: application/json
```

### 主要API端点

#### 市场数据
```
GET /api/v1/market/overview              # 市场概览
GET /api/v1/market/stocks/hot            # 热门股票
GET /api/v1/stocks/{code}/basic          # 股票基本信息
GET /api/v1/stocks/{code}/kline          # K线数据
```

#### 技术分析
```
GET /api/v1/analysis/technical/{code}    # 技术分析
GET /api/v1/analysis/signals/{code}      # 交易信号
```

#### AI分析
```
GET /api/v1/ai/predict/{code}            # AI预测
GET /api/v1/ai/recommendations           # AI推荐
GET /api/v1/ai/models/performance        # 模型性能
```

#### 数据同步
```
POST /api/v1/sync/register               # 注册客户端
POST /api/v1/sync/sync                   # 同步数据
GET /api/v1/sync/data/{user_id}/{type}   # 获取同步数据
```

### 响应格式
```json
{
  "success": true,
  "code": 200,
  "message": "操作成功",
  "data": { ... },
  "timestamp": "2024-07-31T18:00:00.000Z"
}
```

## 部署指南

### Docker部署
```bash
# 构建镜像
docker build -t ljwx-stock-analysis .

# 运行容器
docker run -d \
  -p 5005:5005 \
  -e TS_TOKEN=your_token \
  -e DB_HOST=mysql_host \
  --name ljwx-stock \
  ljwx-stock-analysis
```

### 生产环境部署
```bash
# 使用Gunicorn
pip install gunicorn
gunicorn -w 4 -b 0.0.0.0:5005 api.app:app

# 使用Nginx反向代理
# 配置SSL证书
# 设置负载均衡
```

## 性能优化

### 数据库优化
- 建立适当索引
- 分区表设计
- 查询缓存优化
- 连接池配置

### 缓存策略
- Redis多级缓存
- 客户端本地缓存
- CDN静态资源缓存
- API响应缓存

### 并发处理
- 异步I/O操作
- 连接池复用
- 请求限流
- 负载均衡

## 监控与日志

### 系统监控
- API响应时间监控
- 数据库性能监控
- Redis缓存命中率
- WebSocket连接状态

### 日志管理
- 结构化日志记录
- 日志轮转策略
- 错误告警机制
- 性能指标追踪

## 开发指南

### 代码规范
- Python: PEP 8
- JavaScript: ES6+ 标准
- ArkTS: 鸿蒙官方规范
- Git提交规范: Conventional Commits

### 测试策略
```bash
# 单元测试
python -m pytest tests/

# API测试
python -m pytest tests/api/

# 集成测试
python -m pytest tests/integration/
```

### 贡献指南
1. Fork项目
2. 创建功能分支
3. 提交代码变更
4. 推送到分支
5. 创建Pull Request

## 故障排除

### 常见问题

#### 1. TuShare API调用失败
```bash
# 检查Token配置
echo $TS_TOKEN

# 检查网络连接
curl -I "https://tushare.pro"

# 检查API限制
# 确保没有超过调用频率限制
```

#### 2. AI模型训练失败
```bash
# 检查依赖库
python -c "import sklearn, pandas, numpy"

# 检查数据库连接
python -c "from database.db_manager import DatabaseManager; db = DatabaseManager(); print('DB连接正常')"

# 重新训练模型
curl -X POST http://localhost:5005/api/ai/models/train
```

#### 3. 鸿蒙端无法连接
- 检查网络权限配置
- 确认API地址正确
- 验证证书配置
- 查看DevEco Studio日志

### 日志文件位置
- 应用日志: `logs/app.log`
- 错误日志: `logs/error.log`
- 访问日志: `logs/access.log`
- 鸿蒙日志: DevEco Studio Console

## 更新日志

### v2.1.0 (2024-07-31)
- ✨ 新增鸿蒙原生客户端
- ✨ 实现跨平台数据同步
- 🐛 修复AI模型特征重要性显示问题
- ⚡ 优化M2 Ultra架构兼容性
- 📈 AI模型准确率提升至92%+

### v2.0.0 (2024-07-30)
- ✨ 集成Ollama大语言模型
- ✨ 新增智能推荐系统
- ✨ 实现实时数据推送
- 🔧 重构API架构
- 📊 增强技术分析功能

## 许可证

本项目采用 MIT 许可证 - 查看 [LICENSE](LICENSE) 文件了解详情。

## 联系方式

- 项目维护者: [您的姓名]
- 邮箱: your.email@example.com
- 项目主页: https://github.com/yourusername/tushare-analysis-system
- 文档站点: https://docs.your-domain.com

## 致谢

感谢以下开源项目的支持：
- [TuShare](https://tushare.pro) - 数据源
- [Flask](https://flask.palletsprojects.com) - Web框架
- [scikit-learn](https://scikit-learn.org) - 机器学习
- [Ollama](https://ollama.ai) - 本地大语言模型
- [ECharts](https://echarts.apache.org) - 图表库
- [HarmonyOS](https://developer.harmonyos.com) - 鸿蒙开发平台

---

**灵境万象** - 让数据洞察未来，让智能驱动投资 🚀