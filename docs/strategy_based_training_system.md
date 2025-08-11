# 基于策略的AI股票推荐训练系统设计方案

## 1. 系统概述

本系统设计了一个基于投资策略的AI股票推荐训练框架，通过将不同投资策略转化为对应的训练数据集，使用统一的大语言模型进行训练，最终生成个性化的股票推荐。系统支持通用策略推荐和用户定制化策略推荐。

## 2. 系统架构

### 2.1 整体架构图

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   策略定义层     │    │   训练数据生成   │    │   统一模型训练   │
│                │    │                │    │                │
│ ┌─────────────┐ │    │ ┌─────────────┐ │    │ ┌─────────────┐ │
│ │ 技术分析策略 │ │───▶│ │技术指标训练集│ │───▶│ │             │ │
│ └─────────────┘ │    │ └─────────────┘ │    │ │             │ │
│ ┌─────────────┐ │    │ ┌─────────────┐ │    │ │  Ollama LLM  │ │
│ │ 基本面策略  │ │───▶│ │基本面训练集 │ │───▶│ │   统一模型   │ │
│ └─────────────┘ │    │ └─────────────┘ │    │ │             │ │
│ ┌─────────────┐ │    │ ┌─────────────┐ │    │ │             │ │
│ │ 用户定制策略 │ │───▶│ │定制化训练集 │ │───▶│ └─────────────┘ │
│ └─────────────┘ │    │ └─────────────┘ │    └─────────────────┘
└─────────────────┘    └─────────────────┘             │
                                                      │
┌─────────────────┐    ┌─────────────────┐             │
│   推荐生成层     │    │   异步调度系统   │             │
│                │    │                │◀────────────┘
│ ┌─────────────┐ │    │ ┌─────────────┐ │
│ │ 通用推荐引擎 │ │    │ │ 定时任务调度 │ │
│ └─────────────┘ │    │ └─────────────┘ │
│ ┌─────────────┐ │    │ ┌─────────────┐ │
│ │ 个性化推荐  │ │    │ │ 异步队列管理 │ │
│ └─────────────┘ │    │ └─────────────┘ │
└─────────────────┘    └─────────────────┘
```

### 2.2 核心组件

#### 2.2.1 策略定义层
- **技术分析策略**: 基于技术指标（MA、RSI、MACD、KDJ等）
- **基本面策略**: 基于财务指标（PE、PB、ROE、EPS增长等）
- **量化策略**: 基于统计模型和数学算法
- **混合策略**: 多因子组合策略
- **用户定制策略**: 用户个性化配置的策略

#### 2.2.2 训练数据生成引擎
- **策略执行器**: 将策略规则应用到历史数据
- **标签生成器**: 根据策略执行结果生成训练标签
- **数据增强器**: 增加训练样本的多样性
- **质量过滤器**: 过滤低质量训练样本

#### 2.2.3 统一模型训练系统
- **Ollama集成**: 使用ljwx-stock模型作为基础
- **增量训练**: 支持新策略的增量学习
- **多策略融合**: 将多种策略知识融合到统一模型
- **性能评估**: 模型效果评估和优化

## 3. 详细设计

### 3.1 策略到训练集转换流程

#### 3.1.1 技术分析策略转换示例

```python
# 策略定义
technical_strategy = {
    "name": "RSI-MACD组合策略",
    "buy_conditions": [
        {"indicator": "RSI", "operator": "<", "threshold": 30},
        {"indicator": "MACD", "operator": ">", "threshold": 0}
    ],
    "sell_conditions": [
        {"indicator": "RSI", "operator": ">", "threshold": 70},
        {"indicator": "MACD", "operator": "<", "threshold": 0}
    ]
}

# 训练数据生成
def generate_training_data(strategy, historical_data):
    training_samples = []
    
    for stock_code in stock_list:
        # 1. 获取历史数据
        stock_data = get_historical_data(stock_code, period="2Y")
        
        # 2. 计算技术指标
        indicators = calculate_indicators(stock_data)
        
        # 3. 应用策略规则
        signals = apply_strategy(strategy, indicators)
        
        # 4. 生成训练样本
        for date, signal in signals.items():
            sample = {
                "input": format_market_context(stock_data, indicators, date),
                "output": format_recommendation(signal, stock_code, date),
                "strategy_type": strategy["name"]
            }
            training_samples.append(sample)
    
    return training_samples
```

#### 3.1.2 训练数据格式标准化

```json
{
    "strategy_id": "rsi_macd_combo",
    "strategy_type": "technical_analysis",
    "training_samples": [
        {
            "input": "股票代码：000001.SZ，当前价格：10.50元，RSI(14)：28.5，MACD：0.15，成交量放大1.8倍...",
            "output": "【买入推荐】基于RSI-MACD组合策略分析，该股票RSI处于超卖区域(28.5<30)，同时MACD金叉向上，建议买入。目标价位：11.20元，止损价位：9.80元，建议持有期：10-15个交易日。",
            "metadata": {
                "stock_code": "000001.SZ",
                "date": "2024-01-15",
                "signal_strength": 0.85,
                "confidence": 0.78
            }
        }
    ]
}
```

### 3.2 统一模型训练架构

#### 3.2.1 多策略知识融合

```python
class StrategyAwareTrainer:
    def __init__(self, base_model="ljwx-stock"):
        self.base_model = base_model
        self.strategy_encoders = {}
        self.unified_model = None
    
    def prepare_multi_strategy_dataset(self, strategy_datasets):
        """准备多策略融合数据集"""
        unified_dataset = []
        
        for strategy_id, dataset in strategy_datasets.items():
            # 添加策略标识
            for sample in dataset:
                sample["strategy_context"] = f"[策略:{strategy_id}]"
                sample["input"] = sample["strategy_context"] + sample["input"]
                unified_dataset.append(sample)
        
        return unified_dataset
    
    def train_unified_model(self, unified_dataset):
        """训练统一模型"""
        # 1. 数据预处理
        processed_data = self.preprocess_training_data(unified_dataset)
        
        # 2. 使用Ollama进行模型训练
        training_config = {
            "base_model": self.base_model,
            "training_data": processed_data,
            "epochs": 10,
            "learning_rate": 1e-5,
            "batch_size": 32
        }
        
        self.unified_model = self.train_with_ollama(training_config)
        
        return self.unified_model
```

### 3.3 异步推荐生成系统

#### 3.3.1 定时调度架构

```python
from celery import Celery
from apscheduler.schedulers.background import BackgroundScheduler

class RecommendationScheduler:
    def __init__(self):
        self.app = Celery('stock_recommendations')
        self.scheduler = BackgroundScheduler()
        self.recommendation_cache = {}
    
    def setup_scheduled_tasks(self):
        """设置定时任务"""
        # 通用推荐：每日早上8点生成
        self.scheduler.add_job(
            func=self.generate_general_recommendations,
            trigger="cron",
            hour=8,
            minute=0,
            id='general_recommendations'
        )
        
        # 个性化推荐：每2小时更新
        self.scheduler.add_job(
            func=self.generate_personalized_recommendations,
            trigger="interval",
            hours=2,
            id='personalized_recommendations'
        )
        
        # 实时推荐：市场开盘期间每15分钟更新
        self.scheduler.add_job(
            func=self.generate_realtime_recommendations,
            trigger="cron",
            minute="*/15",
            hour="9-15",
            day_of_week="mon-fri",
            id='realtime_recommendations'
        )
    
    @self.app.task
    def generate_general_recommendations(self):
        """生成通用推荐"""
        try:
            # 使用主流策略生成推荐
            mainstream_strategies = self.get_mainstream_strategies()
            recommendations = []
            
            for strategy in mainstream_strategies:
                recs = self.model_recommender.generate_recommendations(
                    strategy=strategy,
                    num_stocks=10,
                    market_filter="active"
                )
                recommendations.extend(recs)
            
            # 缓存推荐结果
            self.cache_recommendations("general", recommendations)
            
            # 推送到前端
            self.broadcast_recommendations("general", recommendations)
            
        except Exception as e:
            logger.error(f"生成通用推荐失败: {e}")
    
    @self.app.task
    def generate_personalized_recommendations(self):
        """生成个性化推荐"""
        users = self.get_active_users()
        
        for user_id in users:
            try:
                # 获取用户策略偏好
                user_strategies = self.get_user_strategies(user_id)
                
                recommendations = []
                for strategy in user_strategies:
                    recs = self.model_recommender.generate_recommendations(
                        strategy=strategy,
                        num_stocks=5,
                        user_profile=self.get_user_profile(user_id)
                    )
                    recommendations.extend(recs)
                
                # 个性化排序和过滤
                personalized_recs = self.personalize_recommendations(
                    recommendations, user_id
                )
                
                # 缓存和推送
                self.cache_recommendations(f"user_{user_id}", personalized_recs)
                self.send_user_recommendations(user_id, personalized_recs)
                
            except Exception as e:
                logger.error(f"生成用户{user_id}个性化推荐失败: {e}")
```

#### 3.3.2 实时推荐更新系统

```python
class RealtimeRecommendationUpdater:
    def __init__(self):
        self.websocket_manager = WebSocketManager()
        self.recommendation_cache = RedisCache()
    
    def update_recommendations_panel(self, recommendation_type="all"):
        """更新推荐面板"""
        try:
            if recommendation_type == "all" or recommendation_type == "general":
                # 更新通用推荐
                general_recs = self.recommendation_cache.get("general")
                self.websocket_manager.broadcast({
                    "type": "recommendation_update",
                    "category": "general",
                    "data": general_recs,
                    "timestamp": datetime.now().isoformat()
                })
            
            if recommendation_type == "all" or recommendation_type == "personalized":
                # 更新个性化推荐
                active_users = self.websocket_manager.get_active_users()
                for user_id in active_users:
                    user_recs = self.recommendation_cache.get(f"user_{user_id}")
                    if user_recs:
                        self.websocket_manager.send_to_user(user_id, {
                            "type": "recommendation_update",
                            "category": "personalized",
                            "data": user_recs,
                            "timestamp": datetime.now().isoformat()
                        })
        
        except Exception as e:
            logger.error(f"更新推荐面板失败: {e}")
```

### 3.4 通用vs定制化推荐策略

#### 3.4.1 通用策略推荐

```python
class GeneralRecommendationEngine:
    """通用策略推荐引擎"""
    
    def __init__(self):
        self.mainstream_strategies = self.load_mainstream_strategies()
        self.market_analyzer = MarketAnalyzer()
    
    def generate_daily_recommendations(self):
        """生成每日通用推荐"""
        market_condition = self.market_analyzer.analyze_market_condition()
        
        # 根据市场环境选择适合的策略
        suitable_strategies = self.select_strategies_by_market(market_condition)
        
        recommendations = []
        for strategy in suitable_strategies:
            strategy_recs = self.apply_strategy(strategy)
            recommendations.extend(strategy_recs)
        
        # 去重和排序
        final_recs = self.deduplicate_and_rank(recommendations)
        
        return final_recs[:20]  # 返回top20推荐
    
    def select_strategies_by_market(self, market_condition):
        """根据市场环境选择策略"""
        strategy_mapping = {
            "bull_market": ["momentum_strategy", "breakout_strategy", "growth_investment"],
            "bear_market": ["dividend_yield_strategy", "value_investment", "defensive_strategy"],
            "sideways_market": ["mean_reversion", "bollinger_bands", "pairs_trading"],
            "high_volatility": ["volatility_breakout", "options_strategy"],
            "low_volatility": ["carry_trade", "momentum_strategy"]
        }
        
        return strategy_mapping.get(market_condition, ["balanced_strategy"])
```

#### 3.4.2 定制化策略推荐

```python
class PersonalizedRecommendationEngine:
    """个性化策略推荐引擎"""
    
    def __init__(self):
        self.user_profiler = UserProfiler()
        self.strategy_customizer = StrategyCustomizer()
    
    def generate_personalized_recommendations(self, user_id):
        """生成个性化推荐"""
        # 1. 分析用户画像
        user_profile = self.user_profiler.get_user_profile(user_id)
        
        # 2. 获取用户自定义策略
        custom_strategies = self.get_user_custom_strategies(user_id)
        
        # 3. 根据用户风险偏好调整策略参数
        adjusted_strategies = []
        for strategy in custom_strategies:
            adjusted = self.strategy_customizer.adjust_for_user(
                strategy, user_profile
            )
            adjusted_strategies.append(adjusted)
        
        # 4. 生成推荐
        recommendations = []
        for strategy in adjusted_strategies:
            recs = self.apply_personalized_strategy(strategy, user_profile)
            recommendations.extend(recs)
        
        # 5. 个性化排序
        ranked_recs = self.rank_by_user_preference(recommendations, user_profile)
        
        return ranked_recs
    
    def adjust_strategy_parameters(self, strategy, user_profile):
        """根据用户画像调整策略参数"""
        risk_level = user_profile["risk_level"]
        investment_horizon = user_profile["investment_horizon"]
        
        if risk_level == "conservative":
            strategy["risk_management"]["stop_loss"] *= 0.8  # 更严格的止损
            strategy["risk_management"]["max_position_size"] *= 0.6  # 更小的仓位
        elif risk_level == "aggressive":
            strategy["risk_management"]["stop_loss"] *= 1.2  # 更宽松的止损
            strategy["risk_management"]["max_position_size"] *= 1.4  # 更大的仓位
        
        return strategy
```

## 4. 实施方案

### 4.1 分阶段实施计划

#### 阶段一：基础架构搭建（1-2周）
1. 搭建策略定义框架
2. 实现训练数据生成引擎
3. 集成Ollama训练pipeline
4. 建立基础的异步任务系统

#### 阶段二：核心功能开发（2-3周）
1. 实现主流策略的训练数据生成
2. 开发统一模型训练系统
3. 构建通用推荐引擎
4. 实现定时推荐生成

#### 阶段三：个性化功能（2-3周）
1. 开发用户策略定制功能
2. 实现个性化推荐引擎
3. 构建实时推荐更新系统
4. 优化推荐算法性能

#### 阶段四：系统优化（1-2周）
1. 性能调优和缓存优化
2. 监控和告警系统
3. A/B测试框架
4. 用户体验优化

### 4.2 技术栈选择

- **后端**: Python + Flask + Celery
- **数据库**: MySQL + Redis
- **消息队列**: RabbitMQ
- **机器学习**: Ollama + PyTorch
- **实时通信**: WebSocket
- **定时任务**: APScheduler
- **监控**: Prometheus + Grafana

### 4.3 部署架构

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   Web服务器     │    │   任务队列       │    │   模型服务器     │
│   (Flask)       │    │   (Celery)      │    │   (Ollama)      │
│                │    │                │    │                │
│ ┌─────────────┐ │    │ ┌─────────────┐ │    │ ┌─────────────┐ │
│ │ API Gateway │ │    │ │ Worker Pool │ │    │ │ LLM Models  │ │
│ └─────────────┘ │    │ └─────────────┘ │    │ └─────────────┘ │
│ ┌─────────────┐ │    │ ┌─────────────┐ │    │ ┌─────────────┐ │
│ │ WebSocket   │ │    │ │ Scheduler   │ │    │ │ GPU Pool    │ │
│ └─────────────┘ │    │ └─────────────┘ │    │ └─────────────┘ │
└─────────────────┘    └─────────────────┘    └─────────────────┘
         │                       │                       │
         └───────────────────────┼───────────────────────┘
                                │
┌─────────────────────────────────────────────────────────────────┐
│                     数据存储层                                    │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐              │
│  │   MySQL     │  │    Redis    │  │  MongoDB    │              │
│  │  (核心数据)  │  │   (缓存)    │  │ (训练数据)   │              │
│  └─────────────┘  └─────────────┘  └─────────────┘              │
└─────────────────────────────────────────────────────────────────┘
```

## 5. 性能指标和监控

### 5.1 关键性能指标(KPIs)

- **推荐准确率**: 推荐股票的收益率统计
- **响应时间**: 推荐生成和更新的延迟
- **系统吞吐量**: 每秒处理的推荐请求数
- **用户参与度**: 用户对推荐的交互率
- **模型性能**: 训练损失和验证指标

### 5.2 监控和告警

```python
class RecommendationMonitor:
    def __init__(self):
        self.metrics_collector = MetricsCollector()
        self.alerting = AlertingSystem()
    
    def track_recommendation_performance(self):
        """监控推荐性能"""
        metrics = {
            "recommendation_accuracy": self.calculate_accuracy(),
            "response_time": self.measure_response_time(),
            "cache_hit_rate": self.get_cache_hit_rate(),
            "model_inference_time": self.measure_model_time(),
            "user_satisfaction": self.get_user_feedback_score()
        }
        
        self.metrics_collector.record(metrics)
        
        # 检查告警条件
        if metrics["recommendation_accuracy"] < 0.6:
            self.alerting.send_alert("推荐准确率过低", "critical")
        
        if metrics["response_time"] > 5000:  # 5秒
            self.alerting.send_alert("响应时间过长", "warning")
```

## 6. 未来扩展

### 6.1 多模态推荐
- 结合新闻情感分析
- 整合社交媒体数据
- 加入宏观经济指标

### 6.2 强化学习优化
- 基于用户反馈的在线学习
- 动态策略参数调整
- 多臂老虎机算法应用

### 6.3 跨资产类别
- 扩展到债券、期货、期权
- 国际市场股票推荐
- 加密货币推荐

## 7. 结论

本方案提供了一个完整的基于策略的AI股票推荐训练系统，通过策略驱动的训练数据生成、统一模型训练和异步推荐系统，实现了通用和个性化推荐的有机结合。系统具有良好的扩展性和可维护性，能够满足不同用户的投资需求。