# AI大模型股票推荐系统

## 系统概述

ljwx-stock是一个基于AI大语言模型的智能股票分析和推荐系统，整合了实时数据流、专业技术分析和AI驱动的投资建议。系统结合传统技术分析与大语言模型，提供全面的股票市场洞察。

## 系统架构

### 核心架构图

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

### 技术栈

- **后端**: Python 3.13, Flask, MySQL, Redis
- **AI/LLM**: Ollama (lingjingwanxiang:70b模型)
- **数据源**: TuShare Pro API
- **前端**: HTML5, JavaScript, ECharts, TailwindCSS
- **实时通信**: WebSocket
- **移动端**: HarmonyOS原生应用

## AI推荐系统核心功能

### 1. 智能推荐生成

#### 1.1 多策略推荐引擎

系统实现了三种不同的分析策略：

**综合分析策略 (comprehensive_analysis)**
```python
# 包含技术分析、趋势判断、风险评估的全面分析
template = """
请对股票{stock_name}({stock_code})进行综合分析并提供投资建议：

市场数据：
- 当前价格: {current_price}元
- 价格变动: {price_change}元 ({pct_change}%)
- 成交量: {volume}
- 市值: {market_cap}

技术指标：
- 5日均线: {ma5}元
- 20日均线: {ma20}元  
- 60日均线: {ma60}元
- RSI: {rsi}
- MACD: {macd}

请从技术分析、趋势判断、风险评估等角度，给出投资建议。
"""
```

**技术分析策略 (technical_analysis)**
```python
# 基于技术指标的专业分析
template = """
请基于以下技术分析数据，对股票{stock_name}({stock_code})提供投资建议：

技术指标数据：
- 当前价格: {current_price}元
- 5日均线: {ma5}元  
- 20日均线: {ma20}元
- RSI(14日): {rsi}
- MACD: {macd}
- 成交量: {volume}
- 涨跌幅: {pct_change}%

请提供具体的投资建议，包括：
1. 买入/卖出/持有建议
2. 目标价格（如果有）
3. 风险控制建议
4. 预期时间周期
"""
```

**风险评估策略 (risk_assessment)**
```python
# 专注于风险控制的分析
template = """
请对股票{stock_name}({stock_code})进行风险评估并提供投资建议：

风险相关数据：
- 当前价格: {current_price}元
- 近期波动率: {volatility}%
- 最大回撤: {max_drawdown}%
- 成交量变化: {volume_change}%
- 行业表现: {industry_performance}

请从风险控制角度分析：
1. 当前投资风险等级
2. 适合的仓位配置
3. 具体操作建议
4. 风险控制措施
5. 适合的投资期限
"""
```

#### 1.2 智能股票选择算法

**多市场股票池分配**
```python
def _select_diversified_stocks(self, stocks_df: pd.DataFrame, limit: int) -> List[str]:
    # 按市场分类选择
    sz_count = int(limit * 0.3)  # 30% 深市主板 (000, 002)
    sh_count = int(limit * 0.4)  # 40% 沪市主板 (600, 601)
    cy_count = int(limit * 0.2)  # 20% 创业板 (300)
    kc_count = limit - sz_count - sh_count - cy_count  # 剩余科创板 (688)
```

**智能评分系统** (100分制)
```python
def _calculate_recommendation_score(self, stock_data: Dict) -> float:
    score = 50.0  # 基础分数
    
    # 技术指标得分 (30分)
    rsi = stock_data.get('rsi', 50)
    if 30 <= rsi <= 70:  # RSI在合理区间
        score += 10
    elif rsi < 30:  # 超卖，有反弹机会
        score += 15
    
    # 均线得分 (20分)
    if current_price > ma5 > ma20:  # 多头排列
        score += 20
    elif current_price > ma5:  # 短期趋势向上
        score += 10
    
    # 成交量得分 (15分)
    if 1.2 <= volume_ratio <= 3:  # 成交量适中放大
        score += 15
    
    # 波动率得分 (15分)
    if 15 <= volatility <= 35:  # 适中波动
        score += 15
    
    # 价格合理性得分 (10分)
    if 5 <= current_price <= 100:  # 价格在合理区间
        score += 10
    
    # 最大回撤得分 (10分)
    if max_drawdown < 10:  # 回撤较小
        score += 10
    
    return max(0, min(100, score))  # 限制在0-100分之间
```

### 2. 推荐跟踪与回测系统

#### 2.1 推荐记录数据结构

```python
@dataclass
class StockRecommendation:
    # 基础信息
    id: str
    model_name: str                    # 模型名称
    stock_code: str                    # 股票代码
    stock_name: str                    # 股票名称
    recommendation_type: str           # 推荐类型 (buy/sell/hold)
    recommendation_text: str           # 推荐原文
    confidence_score: float            # 置信度分数
    strategy_type: str                 # 策略类型
    target_price: Optional[float]      # 目标价格
    stop_loss: Optional[float]         # 止损价格
    time_horizon: int                  # 时间周期(天)
    
    # 推荐时的市场数据
    recommend_date: str                # 推荐日期
    recommend_price: float             # 推荐时价格
    recommend_volume: float            # 推荐时成交量
    
    # 验证数据
    validation_date: Optional[str] = None      # 验证日期
    validation_price: Optional[float] = None   # 验证时价格
    actual_return: Optional[float] = None      # 实际收益率
    result: str = "pending"                    # 推荐结果
```

#### 2.2 回测指标体系

```python
@dataclass
class BacktestMetrics:
    model_name: str
    period_start: str
    period_end: str
    
    # 基础统计
    total_recommendations: int         # 总推荐数
    validated_recommendations: int     # 已验证推荐数
    hit_rate: float                   # 命中率
    avg_return: float                 # 平均收益率
    
    # 风险指标
    sharpe_ratio: float               # 夏普比率
    max_drawdown: float               # 最大回撤
    volatility: float                 # 波动率
    
    # 分类统计
    buy_recommendations: int          # 买入推荐数
    sell_recommendations: int         # 卖出推荐数
    hold_recommendations: int         # 持有推荐数
    buy_hit_rate: float              # 买入命中率
    sell_hit_rate: float             # 卖出命中率
    hold_hit_rate: float             # 持有命中率
```

### 3. 实时数据流系统

#### 3.1 WebSocket实时推送

**连接管理**
```python
class RealtimeServer:
    def __init__(self):
        self.connections = set()
        self.subscriptions = defaultdict(set)
    
    async def handle_connection(self, websocket, path):
        self.connections.add(websocket)
        try:
            async for message in websocket:
                await self.handle_message(websocket, message)
        finally:
            self.connections.remove(websocket)
```

**数据推送类型**
- 实时股价更新
- 技术指标变化
- AI推荐生成
- 市场预警信号

#### 3.2 数据处理流水线

1. **数据获取**: TuShare Pro API → 原始市场数据
2. **数据处理**: 技术指标计算 → 特征工程
3. **数据存储**: MySQL数据库 → 智能缓存
4. **实时广播**: WebSocket推送 → 客户端更新
5. **AI分析**: LLM推理 → 投资建议生成

## API接口文档

### REST API (端口: 5005)

#### 推荐相关接口

**获取最新推荐**
```http
GET /api/recommendations/recent?limit=10&strategy_type=all
```

响应示例：
```json
{
  "recommendations": [
    {
      "id": "uuid-string",
      "stock_code": "000001.SZ",
      "stock_name": "平安银行",
      "recommendation_type": "buy",
      "confidence_score": 0.85,
      "strategy_type": "comprehensive_analysis",
      "strategy_name": "综合智能策略",
      "reasoning": "技术指标显示多头排列，建议逢低买入",
      "target_price": 12.50,
      "current_price": 11.20,
      "created_at": "2024-08-12T10:30:00Z"
    }
  ],
  "total": 1,
  "strategy_distribution": {
    "comprehensive_analysis": 3,
    "technical_analysis": 4,
    "risk_assessment": 3
  }
}
```

**按策略筛选推荐**
```http
GET /api/recommendations/by_strategy?strategy=technical_analysis&limit=5
```

**获取Top5/Top10推荐**
```http
GET /api/recommendations/top?limit=5&sort_by=score
```

#### 性能分析接口

**策略性能统计**
```http
GET /api/strategy/performance?strategy_type=comprehensive_analysis&days=30
```

**回测报告**
```http
GET /api/backtest/report?model=ljwx-stock&start_date=2024-07-01&end_date=2024-08-01
```

### WebSocket API (端口: 8765)

**连接地址**
```
ws://localhost:8765/realtime
```

**消息类型**
```json
// 订阅股票数据
{
  "type": "subscribe",
  "data": {
    "stocks": ["000001.SZ", "600000.SH"],
    "indicators": ["price", "volume", "technical"]
  }
}

// 实时价格推送
{
  "type": "price_update",
  "data": {
    "stock_code": "000001.SZ",
    "price": 11.25,
    "change": 0.05,
    "change_pct": 0.45,
    "volume": 1234567,
    "timestamp": "2024-08-12T10:30:00Z"
  }
}

// AI推荐推送
{
  "type": "recommendation",
  "data": {
    "stock_code": "000001.SZ",
    "recommendation_type": "buy",
    "strategy_type": "technical_analysis",
    "confidence": 0.85,
    "reasoning": "突破20日均线，建议买入"
  }
}
```

## 前端用户界面

### 1. 主控制面板

**功能特性**
- 实时市场概览
- AI推荐列表
- 策略性能图表
- 个人投资组合

**技术实现**
- 响应式设计 (TailwindCSS)
- 实时图表 (ECharts)
- WebSocket实时更新
- 交互式筛选控件

### 2. 推荐系统界面

**策略筛选控件**
```html
<div class="strategy-filters">
  <button class="filter-btn active" data-strategy="all">全部策略</button>
  <button class="filter-btn" data-strategy="comprehensive_analysis">综合分析</button>
  <button class="filter-btn" data-strategy="technical_analysis">技术分析</button>
  <button class="filter-btn" data-strategy="risk_assessment">风险评估</button>
</div>

<div class="top-filters">
  <button class="top-btn" data-limit="5">Top 5</button>
  <button class="top-btn" data-limit="10">Top 10</button>
  <button class="top-btn" data-limit="20">Top 20</button>
</div>
```

**推荐卡片显示**
```html
<div class="recommendation-card" data-strategy="comprehensive_analysis">
  <div class="stock-info">
    <h3>平安银行 (000001.SZ)</h3>
    <span class="price">¥11.20</span>
  </div>
  
  <div class="recommendation-tags">
    <span class="tag tag-buy">买入</span>
    <span class="tag tag-confidence">置信度: 85%</span>
    <span class="tag tag-strategy">综合分析策略</span>
  </div>
  
  <div class="reasoning">
    技术指标显示多头排列，建议逢低买入
  </div>
  
  <div class="metrics">
    <span>目标价: ¥12.50</span>
    <span>当前得分: 87.5</span>
  </div>
</div>
```

### 3. 策略性能可视化

**性能图表**
```javascript
// ECharts配置
const performanceChart = {
  title: { text: '策略表现对比' },
  tooltip: { trigger: 'axis' },
  legend: {
    data: ['综合分析策略', '技术分析策略', '风险评估策略']
  },
  xAxis: {
    type: 'category',
    data: ['1月', '2月', '3月', '4月', '5月', '6月']
  },
  yAxis: { type: 'value' },
  series: [
    {
      name: '综合分析策略',
      type: 'line',
      data: [12.3, 15.8, 18.2, 22.1, 19.5, 25.3]
    },
    {
      name: '技术分析策略', 
      type: 'line',
      data: [8.5, 12.3, 16.7, 18.9, 17.2, 21.8]
    },
    {
      name: '风险评估策略',
      type: 'line', 
      data: [6.2, 8.9, 11.4, 13.7, 15.1, 16.8]
    }
  ]
};
```

## 移动端应用 (HarmonyOS)

### 1. 原生应用架构

**核心页面**
- MainPage: 主界面和市场概览
- RecommendationPage: AI推荐列表
- StockDetailPage: 股票详情分析
- WatchlistPage: 自选股管理
- SettingsPage: 应用设置

**数据管理**
```typescript
// DataManager.ets
export class DataManager {
  private apiService: ApiService;
  
  // 获取推荐数据
  async getRecommendations(limit: number = 10): Promise<Recommendation[]> {
    return await this.apiService.get(`/api/recommendations/recent?limit=${limit}`);
  }
  
  // 获取策略性能
  async getStrategyPerformance(strategy: string): Promise<PerformanceData> {
    return await this.apiService.get(`/api/strategy/performance?strategy_type=${strategy}`);
  }
}
```

### 2. 实时数据同步

**WebSocket集成**
```typescript
// ApiService.ets  
export class ApiService {
  private wsConnection: WebSocket;
  
  connectWebSocket() {
    this.wsConnection = new WebSocket('ws://your-server:8765/realtime');
    
    this.wsConnection.onMessage = (message) => {
      const data = JSON.parse(message.data);
      this.handleRealtimeUpdate(data);
    };
  }
  
  private handleRealtimeUpdate(data: any) {
    switch(data.type) {
      case 'recommendation':
        this.updateRecommendations(data.data);
        break;
      case 'price_update':
        this.updateStockPrices(data.data);
        break;
    }
  }
}
```

## 部署与运维

### 1. 系统启动

**完整系统启动**
```bash
# 启动完整实时系统 (推荐)
python3 start_realtime_system.py

# 初始化系统依赖和数据库
python3 start_realtime_system.py init

# 测试系统连接
python3 start_realtime_system.py test
```

**单独组件启动**
```bash
# 仅启动Flask API
python3 api/app.py

# 仅启动WebSocket服务器
python3 api/websocket_server.py

# 启动最小依赖API
python3 start_api_only.py
```

### 2. 数据管理

**历史数据获取**
```bash
# 全面数据获取
python3 run.py fetch-comprehensive --mode full --category all

# 10年历史数据
python3 run.py fetch-10years

# 基础股票信息
python3 scripts/fetch_stock_basic.py

# 定时数据更新
python3 run.py scheduler --scheduler-cmd start
```

**AI模型训练**
```bash
# 创建基础Ollama模型
python3 create_ljwx_stock_model.py

# TuShare数据训练
python3 train_ljwx_stock.py

# 全面训练 (50k样本, 5k股票)
python3 comprehensive_training.py

# 持续学习
python3 continuous_training.py
```

### 3. 系统监控

**健康检查**
```bash
# 集成测试
python3 test_integration.py

# AI系统测试
python3 test_ai_system.py

# WebSocket连接测试
python3 test_websocket_connection.py

# 系统健康检查
python3 run.py health-check
```

**性能监控**
- 系统资源使用率
- API响应时间
- WebSocket连接数
- 数据库性能指标
- AI模型推理延迟

### 4. 配置管理

**统一配置 (unified_config.json)**
```json
{
  "tushare": {
    "token": "your-tushare-token",
    "rate_limit": 500,
    "batch_size": 100
  },
  "database": {
    "host": "localhost",
    "port": 3306,
    "user": "root",
    "password": "your-password",
    "database": "ljwx_stock"
  },
  "ollama": {
    "model": "lingjingwanxiang:70b",
    "api_url": "http://localhost:11434",
    "timeout": 30
  },
  "api": {
    "host": "0.0.0.0",
    "port": 5005,
    "debug": false
  }
}
```

## 性能优化

### 1. Apple M2 Ultra优化

**原生ARM64环境**
```bash
# 设置原生Python环境
python3 -m venv tushare_venv
source tushare_venv/bin/activate

# 优化的TA-Lib编译
pip install --no-cache-dir TA-Lib
```

**并行处理优化**
```python
# 多线程数据处理
import concurrent.futures
from config_manager import get_config

config = get_config()
max_workers = config.hardware.max_workers  # 20 for M2 Ultra

with concurrent.futures.ThreadPoolExecutor(max_workers=max_workers) as executor:
    futures = [executor.submit(process_stock, stock_code) for stock_code in stock_codes]
    results = [future.result() for future in concurrent.futures.as_completed(futures)]
```

### 2. 数据库优化

**连接池配置**
```python
# MySQL连接池
connection_params = {
    'host': config.database.host,
    'port': config.database.port,
    'user': config.database.user,
    'password': config.database.password,
    'database': config.database.database,
    'charset': 'utf8mb4',
    'autocommit': True,
    'pool_name': 'ljwx_pool',
    'pool_size': 20
}
```

**索引优化**
```sql
-- 推荐查询优化索引
CREATE INDEX idx_recommendations_model_date ON recommendations(model_name, recommend_date DESC);
CREATE INDEX idx_recommendations_strategy ON recommendations(strategy_type, confidence_score DESC);
CREATE INDEX idx_recommendations_stock ON recommendations(stock_code, created_at DESC);
```

### 3. 缓存策略

**Redis缓存**
```python
# 实时数据缓存
cache_key = f"stock_data:{stock_code}:{date}"
cached_data = redis_client.get(cache_key)

if cached_data:
    return json.loads(cached_data)
else:
    data = fetch_from_database(stock_code, date)
    redis_client.setex(cache_key, 3600, json.dumps(data))  # 1小时缓存
    return data
```

## 故障排除

### 常见问题

**1. 数据库连接问题**
```bash
# 检查MySQL服务状态
brew services list | grep mysql

# 重启MySQL服务
brew services restart mysql
```

**2. TuShare API限制**
```python
# 检查API配额
import tushare as ts
ts.set_token('your-token')
pro = ts.pro_api()

# 实现API限流
import time
time.sleep(0.2)  # 200ms延迟避免频率限制
```

**3. Ollama模型问题**
```bash
# 检查Ollama服务
ollama list

# 重新加载模型
ollama pull lingjingwanxiang:70b
```

**4. WebSocket连接问题**
```python
# 检查防火墙设置
sudo ufw status
sudo ufw allow 8765

# 测试WebSocket连接
python3 test_websocket_connection.py
```

### 日志系统

**日志文件位置**
- `logs/api_server.log` - API服务日志
- `logs/ai_training.log` - AI训练日志
- `logs/system_startup.log` - 系统启动日志
- `logs/comprehensive_training_*.log` - 训练会话日志

**日志级别配置**
```python
import logging

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler('logs/api_server.log'),
        logging.StreamHandler()
    ]
)
```

## 扩展开发

### 1. 添加新的分析策略

```python
# 在model_recommender.py中添加新策略模板
new_strategy_template = """
请基于以下数据对股票{stock_name}({stock_code})进行{strategy_name}分析：

分析数据：
{analysis_data}

分析要求：
{analysis_requirements}
"""

# 注册新策略
self.recommendation_templates['new_strategy'] = new_strategy_template
```

### 2. 集成新的数据源

```python
class NewDataSource:
    def __init__(self, api_key):
        self.api_key = api_key
    
    def fetch_data(self, stock_code, start_date, end_date):
        # 实现数据获取逻辑
        pass
    
    def get_real_time_data(self, stock_code):
        # 实现实时数据获取
        pass
```

### 3. 自定义技术指标

```python
def custom_indicator(data: pd.DataFrame) -> pd.Series:
    """
    自定义技术指标实现
    """
    # 指标计算逻辑
    result = data['close'].rolling(window=20).apply(lambda x: custom_calculation(x))
    return result

# 注册到技术指标系统
technical_indicators.register('custom_indicator', custom_indicator)
```

## 安全考虑

### 1. API安全

**JWT认证**
```python
from flask_jwt_extended import JWTManager, create_access_token, jwt_required

app.config['JWT_SECRET_KEY'] = 'your-secret-key'
jwt = JWTManager(app)

@app.route('/api/login', methods=['POST'])
def login():
    # 验证用户凭据
    access_token = create_access_token(identity=user_id)
    return {'access_token': access_token}

@app.route('/api/recommendations/recent')
@jwt_required()
def get_recommendations():
    # 受保护的端点
    pass
```

**API限流**
```python
from flask_limiter import Limiter
from flask_limiter.util import get_remote_address

limiter = Limiter(
    app,
    key_func=get_remote_address,
    default_limits=["1000 per hour"]
)

@app.route('/api/recommendations/recent')
@limiter.limit("100 per minute")
def get_recommendations():
    pass
```

### 2. 数据安全

**敏感数据加密**
```python
from cryptography.fernet import Fernet

# 生成加密密钥
key = Fernet.generate_key()
cipher_suite = Fernet(key)

# 加密敏感配置
encrypted_token = cipher_suite.encrypt(tushare_token.encode())
```

**数据库连接安全**
```python
# 使用环境变量存储敏感信息
import os
from dotenv import load_dotenv

load_dotenv()

DB_PASSWORD = os.getenv('DB_PASSWORD')
TUSHARE_TOKEN = os.getenv('TUSHARE_TOKEN')
```

## 总结

ljwx-stock AI大模型股票推荐系统是一个功能完整的智能投资分析平台，具备：

✅ **核心功能完备**
- 多策略AI推荐引擎
- 实时数据流处理
- 专业技术分析
- 智能股票筛选

✅ **系统架构先进**
- 微服务化设计
- 实时WebSocket通信
- 分布式缓存系统
- 移动端原生应用

✅ **AI能力突出**
- 大语言模型集成
- 多策略智能分析
- 自适应学习机制
- 风险评估体系

✅ **用户体验优秀**
- 响应式Web界面
- 实时数据可视化
- 移动端同步
- 个性化推荐

✅ **运维支持完善**
- 一键部署脚本
- 完整监控体系
- 错误恢复机制
- 性能优化方案

系统已通过Apple M2 Ultra平台优化，支持高并发处理和大规模数据分析，为用户提供专业、准确、及时的AI驱动投资建议。