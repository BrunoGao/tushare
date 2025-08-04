#!/usr/bin/env python3
"""
股票策略数据模型
基于量化交易标准实现策略定义、回测和评估
"""

import json
import sqlite3
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Any, Tuple
from dataclasses import dataclass, asdict
from enum import Enum
import pandas as pd
import numpy as np
import os

class StrategyType(Enum):
    """策略类型"""
    TECHNICAL = "technical"  # 技术分析策略
    FUNDAMENTAL = "fundamental"  # 基本面策略
    QUANTITATIVE = "quantitative"  # 量化策略
    HYBRID = "hybrid"  # 混合策略

class SignalType(Enum):
    """信号类型"""
    BUY = "buy"
    SELL = "sell"
    HOLD = "hold"

class IndicatorType(Enum):
    """技术指标类型"""
    MA = "ma"  # 移动平均线
    RSI = "rsi"  # 相对强弱指数
    MACD = "macd"  # MACD
    BOLLINGER = "bollinger"  # 布林带
    KDJ = "kdj"  # KDJ
    VOLUME = "volume"  # 成交量
    CUSTOM = "custom"  # 自定义

class ConditionOperator(Enum):
    """条件操作符"""
    GT = ">"  # 大于
    LT = "<"  # 小于
    GTE = ">="  # 大于等于
    LTE = "<="  # 小于等于
    EQ = "=="  # 等于
    CROSS_UP = "cross_up"  # 上穿
    CROSS_DOWN = "cross_down"  # 下穿

@dataclass
class TradingCondition:
    """交易条件"""
    indicator_type: str  # 指标类型
    indicator_params: Dict[str, Any]  # 指标参数
    operator: str  # 操作符
    threshold: float  # 阈值
    description: str  # 描述

@dataclass
class StrategyRule:
    """策略规则"""
    name: str  # 规则名称
    conditions: List[TradingCondition]  # 条件列表
    logic_operator: str  # 逻辑操作符 (AND/OR)
    signal_type: str  # 信号类型
    weight: float = 1.0  # 权重

@dataclass
class RiskManagement:
    """风险管理"""
    stop_loss: float = 0.05  # 止损比例 (5%)
    take_profit: float = 0.10  # 止盈比例 (10%)
    max_position_size: float = 0.20  # 最大仓位比例 (20%)
    max_drawdown: float = 0.15  # 最大回撤 (15%)

@dataclass
class Strategy:
    """策略模型"""
    id: Optional[str] = None
    name: str = ""
    description: str = ""
    strategy_type: str = StrategyType.TECHNICAL.value
    user_id: str = "default"
    
    # 策略规则
    buy_rules: List[StrategyRule] = None
    sell_rules: List[StrategyRule] = None
    
    # 风险管理
    risk_management: RiskManagement = None
    
    # 回测参数
    initial_capital: float = 100000.0  # 初始资金
    commission: float = 0.0003  # 手续费
    slippage: float = 0.0001  # 滑点
    
    # 元数据
    created_at: str = ""
    updated_at: str = ""
    is_public: bool = False
    tags: List[str] = None
    
    def __post_init__(self):
        if self.buy_rules is None:
            self.buy_rules = []
        if self.sell_rules is None:
            self.sell_rules = []
        if self.risk_management is None:
            self.risk_management = RiskManagement()
        if self.tags is None:
            self.tags = []
        if not self.created_at:
            self.created_at = datetime.now().isoformat()
        if not self.updated_at:
            self.updated_at = datetime.now().isoformat()

@dataclass
class BacktestResult:
    """回测结果"""
    strategy_id: str
    start_date: str
    end_date: str
    
    # 基础指标
    total_return: float = 0.0  # 总收益率
    annual_return: float = 0.0  # 年化收益率
    max_drawdown: float = 0.0  # 最大回撤
    sharpe_ratio: float = 0.0  # 夏普比率
    volatility: float = 0.0  # 波动率
    
    # 交易统计
    total_trades: int = 0  # 总交易次数
    win_rate: float = 0.0  # 胜率
    avg_win: float = 0.0  # 平均盈利
    avg_loss: float = 0.0  # 平均亏损
    profit_factor: float = 0.0  # 盈亏比
    
    # 时间统计
    backtest_duration: float = 0.0  # 回测耗时
    created_at: str = ""
    
    # 详细数据
    equity_curve: List[Dict] = None  # 资金曲线
    trades: List[Dict] = None  # 交易记录
    
    def __post_init__(self):
        if self.equity_curve is None:
            self.equity_curve = []
        if self.trades is None:
            self.trades = []
        if not self.created_at:
            self.created_at = datetime.now().isoformat()

class StrategyDatabase:
    """策略数据库管理"""
    
    def __init__(self, db_path: str = "data/strategies.db"):
        self.db_path = db_path
        os.makedirs(os.path.dirname(db_path), exist_ok=True)
        self.init_database()
    
    def init_database(self):
        """初始化数据库"""
        with sqlite3.connect(self.db_path) as conn:
            # 策略表
            conn.execute('''
                CREATE TABLE IF NOT EXISTS strategies (
                    id TEXT PRIMARY KEY,
                    name TEXT NOT NULL,
                    description TEXT,
                    strategy_type TEXT,
                    user_id TEXT,
                    strategy_data TEXT,  -- JSON格式存储策略数据
                    created_at TEXT,
                    updated_at TEXT,
                    is_public BOOLEAN DEFAULT FALSE
                )
            ''')
            
            # 回测结果表
            conn.execute('''
                CREATE TABLE IF NOT EXISTS backtest_results (
                    id TEXT PRIMARY KEY,
                    strategy_id TEXT,
                    start_date TEXT,
                    end_date TEXT,
                    result_data TEXT,  -- JSON格式存储回测结果
                    created_at TEXT,
                    FOREIGN KEY (strategy_id) REFERENCES strategies (id)
                )
            ''')
            
            # 策略标签表
            conn.execute('''
                CREATE TABLE IF NOT EXISTS strategy_tags (
                    strategy_id TEXT,
                    tag TEXT,
                    PRIMARY KEY (strategy_id, tag),
                    FOREIGN KEY (strategy_id) REFERENCES strategies (id)
                )
            ''')
            
            conn.commit()
    
    def save_strategy(self, strategy: Strategy) -> str:
        """保存策略"""
        if not strategy.id:
            import uuid
            strategy.id = str(uuid.uuid4())
        
        strategy.updated_at = datetime.now().isoformat()
        
        with sqlite3.connect(self.db_path) as conn:
            # 保存策略
            conn.execute('''
                INSERT OR REPLACE INTO strategies 
                (id, name, description, strategy_type, user_id, strategy_data, created_at, updated_at, is_public)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
            ''', (
                strategy.id,
                strategy.name,
                strategy.description,
                strategy.strategy_type,
                strategy.user_id,
                json.dumps(asdict(strategy), ensure_ascii=False),
                strategy.created_at,
                strategy.updated_at,
                strategy.is_public
            ))
            
            # 保存标签
            conn.execute('DELETE FROM strategy_tags WHERE strategy_id = ?', (strategy.id,))
            for tag in strategy.tags:
                conn.execute('INSERT INTO strategy_tags (strategy_id, tag) VALUES (?, ?)', 
                           (strategy.id, tag))
            
            conn.commit()
        
        return strategy.id
    
    def get_strategy(self, strategy_id: str) -> Optional[Strategy]:
        """获取策略"""
        with sqlite3.connect(self.db_path) as conn:
            conn.row_factory = sqlite3.Row
            cursor = conn.execute(
                'SELECT * FROM strategies WHERE id = ?', 
                (strategy_id,)
            )
            row = cursor.fetchone()
            
            if row:
                strategy_data = json.loads(row['strategy_data'])
                
                # 获取标签
                tag_cursor = conn.execute(
                    'SELECT tag FROM strategy_tags WHERE strategy_id = ?',
                    (strategy_id,)
                )
                tags = [tag_row['tag'] for tag_row in tag_cursor.fetchall()]
                strategy_data['tags'] = tags
                
                return self._dict_to_strategy(strategy_data)
        
        return None
    
    def list_strategies(self, user_id: str = None, strategy_type: str = None, 
                       public_only: bool = False) -> List[Strategy]:
        """列出策略"""
        conditions = []
        params = []
        
        if user_id:
            conditions.append('user_id = ?')
            params.append(user_id)
        
        if strategy_type:
            conditions.append('strategy_type = ?')
            params.append(strategy_type)
        
        if public_only:
            conditions.append('is_public = TRUE')
        
        where_clause = ' AND '.join(conditions) if conditions else '1=1'
        
        with sqlite3.connect(self.db_path) as conn:
            conn.row_factory = sqlite3.Row
            cursor = conn.execute(
                f'SELECT * FROM strategies WHERE {where_clause} ORDER BY updated_at DESC',
                params
            )
            
            strategies = []
            for row in cursor.fetchall():
                strategy_data = json.loads(row['strategy_data'])
                
                # 获取标签
                tag_cursor = conn.execute(
                    'SELECT tag FROM strategy_tags WHERE strategy_id = ?',
                    (row['id'],)
                )
                tags = [tag_row['tag'] for tag_row in tag_cursor.fetchall()]
                strategy_data['tags'] = tags
                
                strategies.append(self._dict_to_strategy(strategy_data))
            
            return strategies
    
    def delete_strategy(self, strategy_id: str) -> bool:
        """删除策略"""
        with sqlite3.connect(self.db_path) as conn:
            # 删除相关数据
            conn.execute('DELETE FROM strategy_tags WHERE strategy_id = ?', (strategy_id,))
            conn.execute('DELETE FROM backtest_results WHERE strategy_id = ?', (strategy_id,))
            cursor = conn.execute('DELETE FROM strategies WHERE id = ?', (strategy_id,))
            conn.commit()
            
            return cursor.rowcount > 0
    
    def save_backtest_result(self, result: BacktestResult) -> str:
        """保存回测结果"""
        import uuid
        result_id = str(uuid.uuid4())
        
        with sqlite3.connect(self.db_path) as conn:
            conn.execute('''
                INSERT INTO backtest_results 
                (id, strategy_id, start_date, end_date, result_data, created_at)
                VALUES (?, ?, ?, ?, ?, ?)
            ''', (
                result_id,
                result.strategy_id,
                result.start_date,
                result.end_date,
                json.dumps(asdict(result), ensure_ascii=False),
                result.created_at
            ))
            conn.commit()
        
        return result_id
    
    def get_backtest_results(self, strategy_id: str) -> List[BacktestResult]:
        """获取回测结果"""
        with sqlite3.connect(self.db_path) as conn:
            conn.row_factory = sqlite3.Row
            cursor = conn.execute(
                'SELECT * FROM backtest_results WHERE strategy_id = ? ORDER BY created_at DESC',
                (strategy_id,)
            )
            
            results = []
            for row in cursor.fetchall():
                result_data = json.loads(row['result_data'])
                results.append(self._dict_to_backtest_result(result_data))
            
            return results
    
    def _dict_to_strategy(self, data: Dict) -> Strategy:
        """字典转策略对象"""
        # 处理嵌套对象
        if 'buy_rules' in data and data['buy_rules']:
            data['buy_rules'] = [self._dict_to_strategy_rule(rule) for rule in data['buy_rules']]
        
        if 'sell_rules' in data and data['sell_rules']:
            data['sell_rules'] = [self._dict_to_strategy_rule(rule) for rule in data['sell_rules']]
        
        if 'risk_management' in data and data['risk_management']:
            data['risk_management'] = RiskManagement(**data['risk_management'])
        
        return Strategy(**data)
    
    def _dict_to_strategy_rule(self, data: Dict) -> StrategyRule:
        """字典转策略规则对象"""
        if 'conditions' in data and data['conditions']:
            data['conditions'] = [TradingCondition(**cond) for cond in data['conditions']]
        
        return StrategyRule(**data)
    
    def _dict_to_backtest_result(self, data: Dict) -> BacktestResult:
        """字典转回测结果对象"""
        return BacktestResult(**data)

# 预定义策略模板
STRATEGY_TEMPLATES = {
    "ma_crossover": {
        "name": "双均线交叉策略",
        "description": "基于短期和长期移动平均线交叉的经典策略",
        "strategy_type": StrategyType.TECHNICAL.value,
        "buy_rules": [
            StrategyRule(
                name="金叉买入",
                conditions=[
                    TradingCondition(
                        indicator_type=IndicatorType.MA.value,
                        indicator_params={"period": 5},
                        operator=ConditionOperator.CROSS_UP.value,
                        threshold=20,  # 与20日均线比较
                        description="5日均线上穿20日均线"
                    )
                ],
                logic_operator="AND",
                signal_type=SignalType.BUY.value,
                weight=1.0
            )
        ],
        "sell_rules": [
            StrategyRule(
                name="死叉卖出",
                conditions=[
                    TradingCondition(
                        indicator_type=IndicatorType.MA.value,
                        indicator_params={"period": 5},
                        operator=ConditionOperator.CROSS_DOWN.value,
                        threshold=20,
                        description="5日均线下穿20日均线"
                    )
                ],
                logic_operator="AND",
                signal_type=SignalType.SELL.value,
                weight=1.0
            )
        ],
        "tags": ["技术分析", "均线", "经典策略"]
    },
    
    "rsi_oversold": {
        "name": "RSI超卖策略",
        "description": "基于RSI指标的超卖反弹策略",
        "strategy_type": StrategyType.TECHNICAL.value,
        "buy_rules": [
            StrategyRule(
                name="RSI超卖买入",
                conditions=[
                    TradingCondition(
                        indicator_type=IndicatorType.RSI.value,
                        indicator_params={"period": 14},
                        operator=ConditionOperator.LT.value,
                        threshold=30,
                        description="RSI小于30"
                    )
                ],
                logic_operator="AND",
                signal_type=SignalType.BUY.value,
                weight=1.0
            )
        ],
        "sell_rules": [
            StrategyRule(
                name="RSI超买卖出",
                conditions=[
                    TradingCondition(
                        indicator_type=IndicatorType.RSI.value,
                        indicator_params={"period": 14},
                        operator=ConditionOperator.GT.value,
                        threshold=70,
                        description="RSI大于70"
                    )
                ],
                logic_operator="AND",
                signal_type=SignalType.SELL.value,
                weight=1.0
            )
        ],
        "tags": ["技术分析", "RSI", "超卖反弹"]
    }
}