# LJWX-Stock API 接口文档

## 📚 API接口文档

| 文档类型 | API接口说明 |
|---------|------------|
| API版本 | v2.0.0 |
| 基础URL | http://localhost:5005 |
| 更新日期 | 2025-08-05 |
| 协议 | HTTP/HTTPS + WebSocket |

## 🌐 接口概览

LJWX-Stock 提供完整的RESTful API和WebSocket实时接口，支持股票分析、AI推荐、策略管理等核心功能。

### 🏗️ API架构
```
API层级结构:
├── /api/                    # 通用API接口
├── /api/mobile/             # 移动端优化接口  
├── /api/admin/              # 管理员接口
└── /websocket/              # WebSocket实时接口
```

### 🔐 认证方式
- **开发模式**: 无需认证
- **生产模式**: Bearer Token认证
- **管理接口**: 需要管理员权限

## 📊 核心接口详解

### 1. 系统状态接口

#### GET /api/status
**功能**: 获取系统运行状态

**请求参数**: 无

**响应格式**:
```json
{
  "status": "running",
  "timestamp": "2025-08-05T13:05:33",
  "version": "2.0.0",
  "services": {
    "database": "connected",
    "ai_service": "available", 
    "data_sync": "up_to_date",
    "scheduler": "running"
  },
  "performance": {
    "cpu_usage": 15.6,
    "memory_usage": 45.2,
    "active_users": 12
  }
}
```

**状态码**:
- `200`: 系统正常运行
- `503`: 系统服务异常

---

#### GET /api/mobile/dashboard
**功能**: 获取移动端仪表板数据

**请求参数**: 无

**响应格式**:
```json
{
  "training_summary": {
    "total_models": 5,
    "latest_model": "ljwx-stock-comprehensive-20250805_1810",
    "last_training": "2025-08-05T18:10:00"
  },
  "recommendation_summary": {
    "total_recommendations": 1250,
    "today_recommendations": 10,
    "hit_rate": 0.68,
    "avg_confidence": 0.75
  },
  "strategy_summary": {
    "total_strategies": 8,
    "active_strategies": 5,
    "best_performing": "双均线交叉策略"
  },
  "market_summary": {
    "total_stocks": 5150,
    "data_updated": "2025-08-05T17:30:00",
    "market_status": "closed"
  }
}
```

### 2. AI模型管理接口

#### GET /api/models-info
**功能**: 获取可用AI模型信息

**请求参数**: 无

**响应格式**:
```json
{
  "models": [
    {
      "name": "ljwx-stock-comprehensive-20250805_1810",
      "size": "2.1GB",
      "created": "2025-08-05T18:10:00",
      "description": "基于5年全市场数据训练的综合分析模型",
      "performance": {
        "accuracy": 0.68,
        "hit_rate": 0.72,
        "training_samples": 50000
      }
    },
    {
      "name": "ljwx-stock:latest", 
      "size": "1.8GB",
      "created": "2025-08-01T10:00:00",
      "description": "系统默认股票分析模型",
      "performance": {
        "accuracy": 0.65,
        "hit_rate": 0.70,
        "training_samples": 30000
      }
    }
  ],
  "total": 2,
  "default_model": "ljwx-stock:latest"
}
```

---

#### POST /api/train-model
**功能**: 启动AI模型训练

**请求参数**:
```json
{
  "base_model": "ljwx-stock",
  "training_file": "comprehensive_training_data_20250805.jsonl",
  "model_name": "my_custom_model",
  "sample_count": 200
}
```

**响应格式**:
```json
{
  "status": "started",
  "task_id": "train_20250805_140532",
  "estimated_time": "45 minutes",
  "message": "模型训练已启动，请通过WebSocket监控进度"
}
```

**状态码**:
- `200`: 训练任务启动成功
- `400`: 参数错误
- `409`: 已有训练任务在进行中

---

#### GET /api/training-data-info
**功能**: 获取训练数据文件信息

**响应格式**:
```json
{
  "files": [
    {
      "filename": "comprehensive_training_data_20250805.jsonl",
      "size": "125.6MB",
      "sample_count": 50000,
      "created": "2025-08-05T18:10:00",
      "description": "全面训练数据集"
    },
    {
      "filename": "stock_training_data_20250803.jsonl",
      "size": "85.2MB", 
      "sample_count": 30000,
      "created": "2025-08-03T10:38:00",
      "description": "基础训练数据集"
    }
  ],
  "total_files": 2,
  "total_samples": 80000
}
```

### 3. 推荐系统接口

#### POST /api/recommendations/generate
**功能**: 生成AI股票推荐

**请求参数**:
```json
{
  "model_name": "ljwx-stock-comprehensive-20250805_1810",
  "num_stocks": 5,
  "analysis_type": "comprehensive_analysis",
  "use_daily_mode": true,
  "tushare_token": "your_token_here",
  "min_confidence": 0.6
}
```

**参数说明**:
- `model_name`: 使用的AI模型名称
- `num_stocks`: 推荐股票数量(1-10)
- `analysis_type`: 分析类型
  - `comprehensive_analysis`: 综合分析
  - `technical_analysis`: 技术分析
  - `risk_assessment`: 风险评估
- `use_daily_mode`: 是否启用每日推荐模式
- `tushare_token`: TuShare API token(可选)
- `min_confidence`: 最小置信度阈值(0.0-1.0)

**响应格式**:
```json
{
  "status": "started",
  "task_id": "rec_20250805_140832",
  "config": {
    "model_name": "ljwx-stock-comprehensive-20250805_1810",
    "num_stocks": 5,
    "analysis_type": "comprehensive_analysis"
  },
  "estimated_time": "30 seconds",
  "message": "推荐生成已启动，请通过WebSocket获取实时结果"
}
```

**状态码**:
- `200`: 推荐任务启动成功
- `400`: 参数错误
- `404`: 指定模型不存在

---

#### GET /api/mobile/recommendations/latest
**功能**: 获取最新推荐结果

**查询参数**:
- `limit`: 返回数量限制(默认3，最大20)
- `model_name`: 过滤特定模型的推荐

**请求示例**:
```
GET /api/mobile/recommendations/latest?limit=5&model_name=ljwx-stock:latest
```

**响应格式**:
```json
{
  "recommendations": [
    {
      "id": "rec_20250805_140832_001",
      "stock_code": "000001.SZ",
      "stock_name": "平安银行",
      "recommendation_type": "buy",
      "confidence_score": 0.78,
      "target_price": 15.20,
      "current_price": 12.50,
      "expected_return": 0.216,
      "risk_level": "medium",
      "time_horizon": 30,
      "reasoning": "技术分析显示RSI从超卖区域反弹，MACD金叉形成，建议逢低买入",
      "created_time": "2025-08-05T14:08:32",
      "model_name": "ljwx-stock-comprehensive-20250805_1810"
    },
    {
      "id": "rec_20250805_140832_002", 
      "stock_code": "600519.SH",
      "stock_name": "贵州茅台",
      "recommendation_type": "hold",
      "confidence_score": 0.72,
      "target_price": null,
      "current_price": 1680.00,
      "expected_return": 0.05,
      "risk_level": "low",
      "time_horizon": 60,
      "reasoning": "基本面良好但估值偏高，建议持有观望等待回调机会",
      "created_time": "2025-08-05T14:08:32",
      "model_name": "ljwx-stock-comprehensive-20250805_1810"
    }
  ],
  "total": 2,
  "generated_time": "2025-08-05T14:08:32",
  "model_info": {
    "name": "ljwx-stock-comprehensive-20250805_1810",
    "hit_rate": 0.72
  }
}
```

### 4. 快速分析接口

#### POST /api/mobile/quick-analysis
**功能**: 快速股票分析

**请求参数**:
```json
{
  "stock_code": "000001.SZ",
  "analysis_type": "quick"
}
```

**响应格式**:
```json
{
  "stock_code": "000001.SZ",
  "stock_name": "平安银行",
  "analysis": {
    "current_price": 12.50,
    "price_change": 0.024,
    "price_change_percent": "2.4%",
    "volume": 1256789,
    "volume_ratio": 1.35,
    "ma5": 12.20,
    "ma20": 11.80,
    "rsi": 65.2,
    "macd": 0.08,
    "trend": "上涨",
    "trend_strength": "中等",
    "support_level": 11.50,
    "resistance_level": 13.20,
    "volatility": 0.18,
    "analysis_time": "2025-08-05T14:10:15"
  },
  "recommendation": {
    "action": "buy",
    "confidence": 0.68,
    "reason": "技术指标显示上涨趋势，成交量放大确认"
  }
}
```

**错误响应**:
```json
{
  "error": "股票代码不存在或格式错误",
  "suggestion": "请检查股票代码格式，如：000001.SZ",
  "analysis": {
    "error": "Invalid stock code format"
  }
}
```

### 5. 策略管理接口

#### GET /api/strategies
**功能**: 获取投资策略列表

**查询参数**:
- `user_id`: 用户ID(默认default)
- `strategy_type`: 策略类型过滤

**响应格式**:
```json
{
  "strategies": [
    {
      "id": "default_ma_cross",
      "name": "双均线交叉",
      "strategy_type": "technical",
      "description": "基于短期和长期移动平均线交叉的策略",
      "status": "active",
      "tags": ["均线", "趋势"],
      "created_at": "2025-01-01",
      "updated_at": "2025-01-01",
      "buy_rules_count": 2,
      "sell_rules_count": 2,
      "performance": {
        "hit_rate": 0.72,
        "avg_return": 0.085,
        "max_drawdown": 0.12
      }
    },
    {
      "id": "default_rsi_oversold",
      "name": "RSI超卖反弹",
      "strategy_type": "technical",
      "description": "基于RSI指标的超卖反弹策略", 
      "status": "active",
      "tags": ["RSI", "反弹"],
      "created_at": "2025-01-01",
      "updated_at": "2025-01-01",
      "buy_rules_count": 1,
      "sell_rules_count": 1,
      "performance": {
        "hit_rate": 0.68,
        "avg_return": 0.062,
        "max_drawdown": 0.08
      }
    }
  ],
  "total": 2,
  "user_id": "default"
}
```

### 6. 数据查询接口

#### GET /api/stock/basic
**功能**: 获取股票基本信息

**查询参数**:
- `ts_code`: 股票代码(可选)
- `limit`: 返回数量限制

**请求示例**:
```
GET /api/stock/basic?ts_code=000001.SZ
```

**响应格式**:
```json
{
  "stocks": [
    {
      "ts_code": "000001.SZ",
      "symbol": "000001",
      "name": "平安银行",
      "area": "深圳",
      "industry": "银行",
      "market": "主板",
      "list_date": "1991-04-03",
      "is_hs": "S"
    }
  ],
  "total": 1
}
```

---

#### GET /api/stock/daily
**功能**: 获取股票日线数据

**查询参数**:
- `ts_code`: 股票代码(必需)
- `start_date`: 开始日期(YYYY-MM-DD)
- `end_date`: 结束日期(YYYY-MM-DD)
- `limit`: 返回数量限制

**请求示例**:
```
GET /api/stock/daily?ts_code=000001.SZ&start_date=2025-08-01&end_date=2025-08-05&limit=10
```

**响应格式**:
```json
{
  "data": [
    {
      "ts_code": "000001.SZ",
      "trade_date": "2025-08-05",
      "open": 12.30,
      "high": 12.65,
      "low": 12.20,
      "close": 12.50,
      "pre_close": 12.20,
      "change": 0.30,
      "pct_chg": 2.46,
      "vol": 125678900,
      "amount": 1567892345.67
    }
  ],
  "total": 1,
  "ts_code": "000001.SZ",
  "period": "2025-08-01 to 2025-08-05"
}
```

### 7. 管理员接口

#### GET /api/admin/system-info
**功能**: 获取系统详细信息(需要管理员权限)

**响应格式**:
```json
{
  "system": {
    "version": "2.0.0",
    "uptime": "15 days, 6 hours",
    "python_version": "3.12.4",
    "platform": "Darwin-24.5.0-arm64"
  },
  "database": {
    "host": "14.127.218.229",
    "database": "ljwx_stock", 
    "connection_pool": {
      "size": 20,
      "active": 8,
      "idle": 12
    },
    "tables": {
      "stock_basic": 5150,
      "stock_daily_202508": 156780,
      "recommendations": 1250
    }
  },
  "performance": {
    "cpu_usage": 15.6,
    "memory_usage": 45.2,
    "disk_usage": 68.5,
    "load_average": [0.8, 0.9, 1.2]
  },
  "services": {
    "ollama": {
      "status": "running",
      "models": 3,
      "memory_usage": "4.2GB"
    },
    "scheduler": {
      "status": "running", 
      "next_job": "2025-08-05T17:30:00",
      "completed_jobs": 245
    }
  }
}
```

---

#### POST /api/admin/clear-cache
**功能**: 清理系统缓存

**请求参数**:
```json
{
  "cache_type": "all"  // all, data, models, recommendations
}
```

**响应格式**:
```json
{
  "status": "success",
  "cleared": {
    "data_cache": "125MB",
    "model_cache": "2.1GB", 
    "recommendation_cache": "15MB"
  },
  "message": "缓存清理完成"
}
```

## 🔄 WebSocket实时接口

### 连接地址
```
ws://localhost:5005/socket.io/?EIO=4&transport=websocket
```

### 事件监听

#### 'connected'
**描述**: 客户端连接成功

**数据格式**:
```json
{
  "message": "连接成功",
  "client_id": "abc123",
  "timestamp": "2025-08-05T14:15:30"
}
```

#### 'recommendation_generated'
**描述**: 推荐生成完成

**数据格式**:
```json
{
  "task_id": "rec_20250805_140832",
  "model_name": "ljwx-stock:latest",
  "total_recommendations": 5,
  "completion_time": "2025-08-05T14:08:45",
  "recommendations": [
    {
      "stock_code": "000001.SZ",
      "recommendation_type": "buy",
      "confidence_score": 0.78
    }
  ]
}
```

#### 'task_failed'
**描述**: 任务执行失败

**数据格式**:
```json
{
  "task_id": "rec_20250805_140832",
  "task_type": "recommendation_generation",
  "error": "模型服务不可用",
  "timestamp": "2025-08-05T14:08:45"
}
```

#### 'progress_update'
**描述**: 任务进度更新

**数据格式**:
```json
{
  "task_id": "train_20250805_140532",
  "task_type": "model_training",
  "progress": 65,
  "current_step": "模型验证中",
  "estimated_remaining": "15 minutes"
}
```

### 客户端示例

#### JavaScript客户端
```javascript
// 连接WebSocket
const socket = io();

// 监听连接事件
socket.on('connected', (data) => {
    console.log('连接成功:', data);
});

// 监听推荐生成完成
socket.on('recommendation_generated', (data) => {
    console.log('推荐生成完成:', data);
    updateRecommendationList(data.recommendations);
});

// 监听任务失败
socket.on('task_failed', (data) => {
    console.error('任务失败:', data.error);
    showErrorMessage(data.error);
});

// 监听进度更新
socket.on('progress_update', (data) => {
    updateProgressBar(data.progress);
    updateStatusText(data.current_step);
});
```

#### Python客户端
```python
import socketio

# 创建SocketIO客户端
sio = socketio.SimpleClient()

# 连接服务器
sio.connect('http://localhost:5005')

# 监听事件
@sio.event
def recommendation_generated(data):
    print(f"推荐生成完成: {data['total_recommendations']}个")
    for rec in data['recommendations']:
        print(f"  {rec['stock_code']}: {rec['recommendation_type']}")

@sio.event
def task_failed(data):
    print(f"任务失败: {data['error']}")

# 保持连接
sio.wait()
```

## 🚨 错误码说明

### HTTP状态码
- `200`: 成功
- `400`: 请求参数错误
- `401`: 未授权访问
- `403`: 权限不足
- `404`: 资源不存在
- `409`: 资源冲突(如重复操作)
- `429`: 请求频率限制
- `500`: 服务器内部错误
- `503`: 服务不可用

### 业务错误码
```json
{
  "error_code": "INVALID_STOCK_CODE",
  "error_message": "股票代码格式错误",
  "error_details": {
    "provided": "000001",
    "expected_format": "000001.SZ 或 600000.SH"
  },
  "suggestion": "请使用正确的股票代码格式"
}
```

常见错误码:
- `INVALID_STOCK_CODE`: 股票代码格式错误
- `MODEL_NOT_FOUND`: AI模型不存在
- `INSUFFICIENT_DATA`: 数据不足无法分析
- `SERVICE_UNAVAILABLE`: 服务暂时不可用
- `RATE_LIMIT_EXCEEDED`: 请求频率超限
- `TRAINING_IN_PROGRESS`: 已有训练任务进行中

## 🔧 请求限制

### 频率限制
- **普通用户**: 60次/分钟
- **高级用户**: 120次/分钟
- **管理员**: 无限制

### 数据限制
- **单次查询**: 最大1000条记录
- **时间范围**: 最大查询2年历史数据
- **推荐数量**: 单次最多10只股票

### 响应格式
所有API响应均采用JSON格式，包含以下标准字段:
```json
{
  "success": true,
  "data": {},
  "message": "操作成功",
  "timestamp": "2025-08-05T14:15:30",
  "request_id": "req_20250805_141530_001"
}
```

## 📚 SDK支持

### Python SDK示例
```python
from ljwx_stock_sdk import LJWXStockClient

# 初始化客户端
client = LJWXStockClient(
    base_url='http://localhost:5005',
    api_key='your_api_key'  # 生产环境需要
)

# 获取推荐
recommendations = client.get_recommendations(
    model_name='ljwx-stock:latest',
    num_stocks=5
)

# 快速分析
analysis = client.quick_analysis('000001.SZ')

# 获取股票数据
stock_data = client.get_stock_daily(
    ts_code='000001.SZ',
    start_date='2025-08-01',
    end_date='2025-08-05'
)
```

### JavaScript SDK示例
```javascript
import { LJWXStockClient } from 'ljwx-stock-sdk';

// 初始化客户端
const client = new LJWXStockClient({
    baseURL: 'http://localhost:5005',
    apiKey: 'your_api_key'  // 生产环境需要
});

// 获取推荐
const recommendations = await client.getRecommendations({
    modelName: 'ljwx-stock:latest',
    numStocks: 5
});

// 快速分析
const analysis = await client.quickAnalysis('000001.SZ');

// WebSocket连接
client.connectWebSocket();
client.on('recommendation_generated', (data) => {
    console.log('新推荐生成:', data);
});
```

---

**注意事项**:
1. 所有时间格式均使用ISO 8601标准
2. 金额单位为人民币元
3. 比例数据使用小数表示(如0.68表示68%)
4. 股票代码必须包含交易所后缀(.SZ/.SH)

*API文档版本: v2.0.0 | 最后更新: 2025-08-05*