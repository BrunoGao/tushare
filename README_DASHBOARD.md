# 🎯 TuShare 市场数据仪表板

## 📊 项目概述

TuShare 市场数据仪表板是一个专业的股票市场数据可视化系统，基于5年历史数据，为投资者提供全面的技术指标分析、板块轮动监控、龙虎榜追踪和资金流向分析。

### ✨ 主要特性

- **📈 技术指标热力图**: MACD、KDJ、RSI等多维度指标分析
- **🏭 板块轮动监控**: 实时追踪热门概念板块表现
- **🐲 龙虎榜分析**: 主力资金动向一目了然  
- **💰 资金流向追踪**: 主力资金vs散户资金流向对比
- **📊 个股技术面板**: 5,150只A股完整技术指标展示
- **🔄 实时数据更新**: 30秒自动刷新机制

### 📊 数据规模

- **股票覆盖**: 5,150只A股
- **历史数据**: 2015-2025年 (10年+)
- **数据量**: 970万+条记录
- **技术指标**: 27,950条指标记录，覆盖2,805只股票
- **市场数据**: 27,743条龙虎榜、资金流等数据

## 🚀 快速启动

### 环境要求

- Python 3.8+
- MySQL 8.0+
- 8GB+ RAM (推荐)
- TuShare Pro Token

### 安装步骤

1. **克隆项目**
```bash
git clone <repository-url>
cd tushare
```

2. **安装依赖**
```bash
pip install -r requirements.txt
```

3. **配置数据库**
```bash
# 修改 config.py 中的数据库配置
DB_HOST = "127.0.0.1"
DB_PORT = 3306
DB_USER = "your_username"
DB_PASSWORD = "your_password"
DB_NAME = "ljwx_stock"
TS_TOKEN = "your_tushare_token"
```

4. **启动系统**
```bash
./start_dashboard.sh
```

5. **访问仪表板**
```
http://localhost:5005/market-dashboard
```

### 快速启动命令

```bash
# 启动完整系统
./start_dashboard.sh

# 停止系统
./stop_dashboard.sh

# 查看实时日志
tail -f logs/historical_sync.log
tail -f logs/fix_indicators.log
```

## 🎨 界面功能

### 主仪表板

![Market Dashboard](docs/dashboard-preview.png)

#### 1. 快速统计卡片
- 活跃股票数量
- 技术指标覆盖率
- 板块数量统计
- 资金净流入情况

#### 2. 技术指标热力图
- **MACD分布**: 显示各行业MACD信号强度
- **KDJ指标**: K值、D值交叉信号分析
- **RSI分析**: 超买超卖区域分布

#### 3. 热门板块监控
- 实时板块涨跌幅排行
- 板块内股票数量统计
- 板块轮动趋势分析

#### 4. 龙虎榜追踪
- 买入榜/卖出榜切换
- 主力资金流向分析
- 上榜原因详细说明

#### 5. 资金流向图表
- 主力资金vs散户资金
- 实时流向趋势图
- 净流入/流出统计

#### 6. 个股技术指标表格
- 实时股票搜索
- 多维度排序功能
- 买入/卖出信号提醒

## 📡 API接口

### 市场统计
```
GET /api/market/stats
```

### 热门板块
```
GET /api/market/hot-sectors
```

### 龙虎榜数据
```
GET /api/market/dragon-tiger
```

### 资金流向
```
GET /api/market/money-flow
```

### 股票技术指标
```
GET /api/market/stocks?limit=100&search=平安银行
```

### 技术指标热力图
```
GET /api/market/heatmap/macd
GET /api/market/heatmap/kdj
GET /api/market/heatmap/rsi
```

## 🏗️ 系统架构

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   前端界面       │    │   Flask API     │    │   数据处理       │
│                 │    │                 │    │                 │
│ - React/Alpine  │◄──►│ - 市场数据API   │◄──►│ - 历史数据同步   │
│ - ECharts图表   │    │ - 技术指标API   │    │ - 技术指标计算   │
│ - TailwindCSS   │    │ - 实时数据API   │    │ - 策略验证      │
└─────────────────┘    └─────────────────┘    └─────────────────┘
                                │
                                ▼
                       ┌─────────────────┐
                       │   MySQL数据库    │
                       │                 │
                       │ - 日线数据      │
                       │ - 技术指标      │
                       │ - 市场数据      │
                       │ - 龙虎榜       │
                       └─────────────────┘
```

## 🔧 核心组件

### 1. 数据同步系统 (`scheduler/`)
- `historical_data_sync.py`: 5年历史数据全量同步
- `fix_technical_indicators.py`: 技术指标修复和重算
- `data_validator.py`: 数据完整性验证
- `strategy_validator.py`: 投资策略验证

### 2. Web接口 (`api/`)
- `app.py`: Flask主应用和市场数据API
- `market_dashboard_api.py`: 专用仪表板API

### 3. 前端界面 (`templates/`)
- `market_dashboard.html`: 市场仪表板主界面
- `base.html`: 基础模板和导航

### 4. 数据库管理 (`database/`)
- `db_manager.py`: 数据库连接和操作管理

## 📈 投资策略支持

系统支持以下7种投资策略的数据分析：

1. ✅ **涨幅大于5%的非新股** - 完全支持
2. ✅ **均线多头排列的股票** - 完全支持  
3. ✅ **今天的新股开板/上市** - 完全支持
4. 🔄 **MACD与KDJ双金叉** - 数据修复中
5. 🔄 **昨天涨停不含新股** - 数据获取中
6. 🔄 **最近10天有2次涨停** - 数据获取中
7. 🔄 **市盈率小于15的股票** - 数据获取中

**当前支持率**: 3/7 (42.9%) → 目标 7/7 (100%)

## 🗄️ 数据表结构

### 核心数据表

- `stock_basic`: 股票基本信息 (5,150条)
- `stock_daily_YYYYMM`: 月度日线数据 (970万+条)
- `technical_indicators`: 技术指标 (27,950条)
- `stock_indicators`: 基本指标 (PE、PB等)
- `t_dragon_tiger_list`: 龙虎榜 (666条)
- `t_money_flow`: 资金流 (20,906条)
- `t_concept`: 概念板块 (879条)
- `t_concept_detail`: 概念成分股 (1,185条)

## 🔍 监控和日志

### 实时监控
```bash
# 检查系统状态
curl http://localhost:5005/api/market/stats

# 查看进程状态
ps aux | grep -E "(historical_data_sync|fix_technical_indicators|app.py)"

# 监控数据库
python scheduler/data_validator.py
```

### 日志文件
- `logs/historical_sync.log`: 历史数据同步日志
- `logs/fix_indicators.log`: 技术指标修复日志
- 系统日志直接输出到控制台

## 🚨 故障排除

### 常见问题

1. **数据库连接失败**
```bash
# 检查配置
python -c "from database.db_manager import DatabaseManager; DatabaseManager()"
```

2. **端口占用**
```bash
# 释放端口
lsof -ti:5005 | xargs kill -9
```

3. **技术指标为空**
```bash
# 手动修复指标
python scheduler/fix_technical_indicators.py
```

4. **数据不完整**
```bash
# 验证数据完整性
python scheduler/data_validator.py
python scheduler/strategy_validator.py
```

### 性能优化

1. **数据库优化**
- 为频繁查询字段添加索引
- 定期清理过期数据
- 调整MySQL配置参数

2. **API优化**
- 启用API响应缓存
- 数据分页加载
- 异步数据更新

## 📚 开发指南

### 添加新的技术指标

1. 修改 `scheduler/fix_technical_indicators.py`
2. 更新数据库表结构
3. 添加API接口
4. 更新前端显示

### 扩展投资策略

1. 在 `scheduler/strategy_validator.py` 添加验证函数
2. 更新API返回策略列表
3. 前端添加策略选择

### 自定义仪表板

1. 创建新的模板文件
2. 添加对应的API路由
3. 更新导航菜单

## 📞 支持和贡献

### 技术支持
- 📧 Email: support@example.com
- 📋 Issues: [GitHub Issues](https://github.com/your-repo/issues)

### 贡献指南
1. Fork项目
2. 创建功能分支
3. 提交Pull Request

## 📄 许可证

MIT License - 详见 [LICENSE](LICENSE) 文件

---

**🎯 TuShare 市场数据仪表板 - 让数据驱动投资决策**