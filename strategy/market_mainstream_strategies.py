#!/usr/bin/env python3
"""
市场主流投资策略模板
包含技术分析、基本面分析、量化策略和混合策略
"""

from .strategy_models import (
    StrategyRule, TradingCondition, RiskManagement,
    StrategyType, IndicatorType, ConditionOperator, SignalType
)

# 市场主流投资策略模板
MAINSTREAM_STRATEGIES = {
    # ==================== 技术分析策略 ====================
    
    "triple_ma_strategy": {
        "name": "三均线策略",
        "description": "基于5、10、20日均线的多重确认策略",
        "strategy_type": StrategyType.TECHNICAL.value,
        "buy_rules": [
            StrategyRule(
                name="三均线多头排列",
                conditions=[
                    TradingCondition(
                        indicator_type=IndicatorType.MA.value,
                        indicator_params={"period": 5},
                        operator=ConditionOperator.GT.value,
                        threshold=10,  # 5日线>10日线
                        description="5日均线大于10日均线"
                    ),
                    TradingCondition(
                        indicator_type=IndicatorType.MA.value,
                        indicator_params={"period": 10},
                        operator=ConditionOperator.GT.value,
                        threshold=20,  # 10日线>20日线
                        description="10日均线大于20日均线"
                    ),
                    TradingCondition(
                        indicator_type=IndicatorType.VOLUME.value,
                        indicator_params={},
                        operator=ConditionOperator.GT.value,
                        threshold=1.2,  # 成交量放大
                        description="成交量放大1.2倍"
                    )
                ],
                logic_operator="AND",
                signal_type=SignalType.BUY.value,
                weight=1.0
            )
        ],
        "sell_rules": [
            StrategyRule(
                name="均线空头排列",
                conditions=[
                    TradingCondition(
                        indicator_type=IndicatorType.MA.value,
                        indicator_params={"period": 5},
                        operator=ConditionOperator.LT.value,
                        threshold=10,
                        description="5日均线小于10日均线"
                    )
                ],
                logic_operator="AND",
                signal_type=SignalType.SELL.value,
                weight=1.0
            )
        ],
        "risk_management": RiskManagement(
            stop_loss=0.08,
            take_profit=0.15,
            max_position_size=0.25
        ),
        "tags": ["技术分析", "均线", "多重确认"]
    },
    
    "rsi_macd_combo": {
        "name": "RSI-MACD组合策略",
        "description": "结合RSI超买超卖和MACD背离的组合策略",
        "strategy_type": StrategyType.TECHNICAL.value,
        "buy_rules": [
            StrategyRule(
                name="RSI-MACD买入信号",
                conditions=[
                    TradingCondition(
                        indicator_type=IndicatorType.RSI.value,
                        indicator_params={"period": 14},
                        operator=ConditionOperator.LT.value,
                        threshold=35,
                        description="RSI小于35"
                    ),
                    TradingCondition(
                        indicator_type=IndicatorType.MACD.value,
                        indicator_params={"fast": 12, "slow": 26, "signal": 9, "type": "histogram"},
                        operator=ConditionOperator.GT.value,
                        threshold=0,
                        description="MACD柱状图转正"
                    )
                ],
                logic_operator="AND",
                signal_type=SignalType.BUY.value,
                weight=1.0
            )
        ],
        "sell_rules": [
            StrategyRule(
                name="RSI-MACD卖出信号",
                conditions=[
                    TradingCondition(
                        indicator_type=IndicatorType.RSI.value,
                        indicator_params={"period": 14},
                        operator=ConditionOperator.GT.value,
                        threshold=65,
                        description="RSI大于65"
                    ),
                    TradingCondition(
                        indicator_type=IndicatorType.MACD.value,
                        indicator_params={"fast": 12, "slow": 26, "signal": 9, "type": "histogram"},
                        operator=ConditionOperator.LT.value,
                        threshold=0,
                        description="MACD柱状图转负"
                    )
                ],
                logic_operator="OR",
                signal_type=SignalType.SELL.value,
                weight=1.0
            )
        ],
        "risk_management": RiskManagement(
            stop_loss=0.06,
            take_profit=0.12,
            max_position_size=0.20
        ),
        "tags": ["技术分析", "RSI", "MACD", "组合策略"]
    },
    
    "breakout_strategy": {
        "name": "突破策略",
        "description": "基于价格突破关键阻力位的动量策略",
        "strategy_type": StrategyType.TECHNICAL.value,
        "buy_rules": [
            StrategyRule(
                name="突破买入",
                conditions=[
                    TradingCondition(
                        indicator_type="BREAKOUT",
                        indicator_params={"period": 20},
                        operator=ConditionOperator.GT.value,
                        threshold=1.02,  # 突破20日最高价2%
                        description="价格突破20日最高价2%"
                    ),
                    TradingCondition(
                        indicator_type=IndicatorType.VOLUME.value,
                        indicator_params={},
                        operator=ConditionOperator.GT.value,
                        threshold=2.0,  # 成交量放大2倍
                        description="成交量放大2倍"
                    )
                ],
                logic_operator="AND",
                signal_type=SignalType.BUY.value,
                weight=1.0
            )
        ],
        "sell_rules": [
            StrategyRule(
                name="跌破止损",
                conditions=[
                    TradingCondition(
                        indicator_type="BREAKDOWN",
                        indicator_params={"period": 10},
                        operator=ConditionOperator.LT.value,
                        threshold=0.95,  # 跌破10日最低价5%
                        description="价格跌破10日最低价5%"
                    )
                ],
                logic_operator="AND",
                signal_type=SignalType.SELL.value,
                weight=1.0
            )
        ],
        "risk_management": RiskManagement(
            stop_loss=0.10,
            take_profit=0.25,
            max_position_size=0.15
        ),
        "tags": ["技术分析", "突破", "动量", "高风险高收益"]
    },
    
    # ==================== 基本面分析策略 ====================
    
    "dividend_yield_strategy": {
        "name": "高股息策略",
        "description": "基于股息率的稳健投资策略",
        "strategy_type": StrategyType.FUNDAMENTAL.value,
        "buy_rules": [
            StrategyRule(
                name="高股息买入",
                conditions=[
                    TradingCondition(
                        indicator_type="DIVIDEND_YIELD",
                        indicator_params={},
                        operator=ConditionOperator.GT.value,
                        threshold=0.04,  # 股息率大于4%
                        description="股息率大于4%"
                    ),
                    TradingCondition(
                        indicator_type="PAYOUT_RATIO",
                        indicator_params={},
                        operator=ConditionOperator.LT.value,
                        threshold=0.70,  # 分红比例小于70%
                        description="分红比例小于70%"
                    ),
                    TradingCondition(
                        indicator_type="DEBT_TO_EQUITY",
                        indicator_params={},
                        operator=ConditionOperator.LT.value,
                        threshold=0.60,  # 资产负债率小于60%
                        description="资产负债率小于60%"
                    )
                ],
                logic_operator="AND",
                signal_type=SignalType.BUY.value,
                weight=1.0
            )
        ],
        "sell_rules": [
            StrategyRule(
                name="股息下降卖出",
                conditions=[
                    TradingCondition(
                        indicator_type="DIVIDEND_YIELD",
                        indicator_params={},
                        operator=ConditionOperator.LT.value,
                        threshold=0.02,  # 股息率小于2%
                        description="股息率小于2%"
                    )
                ],
                logic_operator="OR",
                signal_type=SignalType.SELL.value,
                weight=1.0
            )
        ],
        "risk_management": RiskManagement(
            stop_loss=0.15,
            take_profit=0.30,
            max_position_size=0.30
        ),
        "tags": ["基本面分析", "高股息", "稳健投资", "长期持有"]
    },
    
    "peg_strategy": {
        "name": "PEG策略",
        "description": "基于PEG比率的成长价值平衡策略",
        "strategy_type": StrategyType.FUNDAMENTAL.value,
        "buy_rules": [
            StrategyRule(
                name="低PEG买入",
                conditions=[
                    TradingCondition(
                        indicator_type="PEG",
                        indicator_params={},
                        operator=ConditionOperator.LT.value,
                        threshold=1.0,  # PEG小于1
                        description="PEG小于1"
                    ),
                    TradingCondition(
                        indicator_type="EPS_GROWTH",
                        indicator_params={},
                        operator=ConditionOperator.GT.value,
                        threshold=0.15,  # EPS增长率大于15%
                        description="EPS增长率大于15%"
                    ),
                    TradingCondition(
                        indicator_type="PE",
                        indicator_params={},
                        operator=ConditionOperator.LT.value,
                        threshold=25,  # PE小于25倍
                        description="PE小于25倍"
                    )
                ],
                logic_operator="AND",
                signal_type=SignalType.BUY.value,
                weight=1.0
            )
        ],
        "sell_rules": [
            StrategyRule(
                name="高PEG卖出",
                conditions=[
                    TradingCondition(
                        indicator_type="PEG",
                        indicator_params={},
                        operator=ConditionOperator.GT.value,
                        threshold=2.0,  # PEG大于2
                        description="PEG大于2"
                    )
                ],
                logic_operator="OR",
                signal_type=SignalType.SELL.value,
                weight=1.0
            )
        ],
        "risk_management": RiskManagement(
            stop_loss=0.12,
            take_profit=0.40,
            max_position_size=0.25
        ),
        "tags": ["基本面分析", "PEG", "成长价值", "平衡投资"]
    },
    
    # ==================== 量化策略 ====================
    
    "pairs_trading_strategy": {
        "name": "配对交易策略",
        "description": "基于股票对相关性的统计套利策略",
        "strategy_type": StrategyType.PAIRS_TRADING.value,
        "buy_rules": [
            StrategyRule(
                name="价差回归买入",
                conditions=[
                    TradingCondition(
                        indicator_type="SPREAD_ZSCORE",
                        indicator_params={"lookback": 60},
                        operator=ConditionOperator.LT.value,
                        threshold=-2.0,  # 价差Z-Score小于-2
                        description="价差Z-Score小于-2标准差"
                    ),
                    TradingCondition(
                        indicator_type="CORRELATION",
                        indicator_params={"period": 60},
                        operator=ConditionOperator.GT.value,
                        threshold=0.70,  # 相关性大于0.7
                        description="60日相关性大于0.7"
                    )
                ],
                logic_operator="AND",
                signal_type=SignalType.BUY.value,
                weight=1.0
            )
        ],
        "sell_rules": [
            StrategyRule(
                name="价差收敛卖出",
                conditions=[
                    TradingCondition(
                        indicator_type="SPREAD_ZSCORE",
                        indicator_params={"lookback": 60},
                        operator=ConditionOperator.GT.value,
                        threshold=0.0,  # 价差回归至均值
                        description="价差回归至均值"
                    )
                ],
                logic_operator="AND",
                signal_type=SignalType.SELL.value,
                weight=1.0
            )
        ],
        "risk_management": RiskManagement(
            stop_loss=0.05,
            take_profit=0.08,
            max_position_size=0.10
        ),
        "tags": ["量化策略", "配对交易", "统计套利", "市场中性"]
    },
    
    "volatility_breakout": {
        "name": "波动率突破策略",
        "description": "基于ATR波动率的突破策略",
        "strategy_type": StrategyType.QUANTITATIVE.value,
        "buy_rules": [
            StrategyRule(
                name="波动率突破买入",
                conditions=[
                    TradingCondition(
                        indicator_type=IndicatorType.ATR.value,
                        indicator_params={"period": 14},
                        operator=ConditionOperator.GT.value,
                        threshold=1.5,  # ATR大于1.5倍平均值
                        description="ATR大于1.5倍历史平均"
                    ),
                    TradingCondition(
                        indicator_type="PRICE_CHANGE",
                        indicator_params={},
                        operator=ConditionOperator.GT.value,
                        threshold=0.03,  # 当日涨幅大于3%
                        description="当日涨幅大于3%"
                    )
                ],
                logic_operator="AND",
                signal_type=SignalType.BUY.value,
                weight=1.0
            )
        ],
        "sell_rules": [
            StrategyRule(
                name="波动率收敛卖出",
                conditions=[
                    TradingCondition(
                        indicator_type=IndicatorType.ATR.value,
                        indicator_params={"period": 14},
                        operator=ConditionOperator.LT.value,
                        threshold=0.8,  # ATR小于0.8倍平均值
                        description="ATR小于0.8倍历史平均"
                    )
                ],
                logic_operator="AND",
                signal_type=SignalType.SELL.value,
                weight=1.0
            )
        ],
        "risk_management": RiskManagement(
            stop_loss=0.08,
            take_profit=0.20,
            max_position_size=0.15
        ),
        "tags": ["量化策略", "波动率", "ATR", "突破"]
    },
    
    # ==================== 混合策略 ====================
    
    "quality_momentum": {
        "name": "质量动量策略",
        "description": "结合财务质量和价格动量的混合策略",
        "strategy_type": StrategyType.HYBRID.value,
        "buy_rules": [
            StrategyRule(
                name="高质量动量买入",
                conditions=[
                    # 财务质量指标
                    TradingCondition(
                        indicator_type="ROE",
                        indicator_params={},
                        operator=ConditionOperator.GT.value,
                        threshold=0.15,
                        description="ROE大于15%"
                    ),
                    TradingCondition(
                        indicator_type="DEBT_TO_EQUITY",
                        indicator_params={},
                        operator=ConditionOperator.LT.value,
                        threshold=0.50,
                        description="负债权益比小于50%"
                    ),
                    # 动量指标
                    TradingCondition(
                        indicator_type="MOMENTUM",
                        indicator_params={"period": 60},
                        operator=ConditionOperator.GT.value,
                        threshold=0.10,
                        description="60日动量大于10%"
                    ),
                    # 技术指标
                    TradingCondition(
                        indicator_type=IndicatorType.RSI.value,
                        indicator_params={"period": 14},
                        operator=ConditionOperator.GT.value,
                        threshold=50,
                        description="RSI大于50"
                    )
                ],
                logic_operator="AND",
                signal_type=SignalType.BUY.value,
                weight=1.0
            )
        ],
        "sell_rules": [
            StrategyRule(
                name="质量恶化或动量衰减卖出",
                conditions=[
                    TradingCondition(
                        indicator_type="ROE",
                        indicator_params={},
                        operator=ConditionOperator.LT.value,
                        threshold=0.08,
                        description="ROE小于8%"
                    ),
                    TradingCondition(
                        indicator_type="MOMENTUM",
                        indicator_params={"period": 20},
                        operator=ConditionOperator.LT.value,
                        threshold=-0.05,
                        description="20日动量小于-5%"
                    )
                ],
                logic_operator="OR",
                signal_type=SignalType.SELL.value,
                weight=1.0
            )
        ],
        "risk_management": RiskManagement(
            stop_loss=0.10,
            take_profit=0.35,
            max_position_size=0.20
        ),
        "tags": ["混合策略", "质量", "动量", "多因子"]
    },
    
    "sector_rotation": {
        "name": "行业轮动策略",
        "description": "基于宏观经济周期的行业轮动策略",
        "strategy_type": StrategyType.SECTOR_ROTATION.value,
        "buy_rules": [
            StrategyRule(
                name="强势行业买入",
                conditions=[
                    TradingCondition(
                        indicator_type="SECTOR_MOMENTUM",
                        indicator_params={"period": 20},
                        operator=ConditionOperator.GT.value,
                        threshold=0.05,
                        description="行业20日动量大于5%"
                    ),
                    TradingCondition(
                        indicator_type="SECTOR_RELATIVE_STRENGTH",
                        indicator_params={},
                        operator=ConditionOperator.GT.value,
                        threshold=1.2,
                        description="行业相对强度大于1.2"
                    ),
                    TradingCondition(
                        indicator_type="ECONOMIC_CYCLE_SCORE",
                        indicator_params={},
                        operator=ConditionOperator.GT.value,
                        threshold=0.6,
                        description="经济周期适应度大于0.6"
                    )
                ],
                logic_operator="AND",
                signal_type=SignalType.BUY.value,
                weight=1.0
            )
        ],
        "sell_rules": [
            StrategyRule(
                name="行业转弱卖出",
                conditions=[
                    TradingCondition(
                        indicator_type="SECTOR_MOMENTUM",
                        indicator_params={"period": 10},
                        operator=ConditionOperator.LT.value,
                        threshold=-0.03,
                        description="行业10日动量小于-3%"
                    )
                ],
                logic_operator="AND",
                signal_type=SignalType.SELL.value,
                weight=1.0
            )
        ],
        "risk_management": RiskManagement(
            stop_loss=0.12,
            take_profit=0.25,
            max_position_size=0.35  # 行业轮动可以更集中
        ),
        "tags": ["混合策略", "行业轮动", "宏观经济", "周期性"]
    }
}

# 策略风险等级定义
STRATEGY_RISK_LEVELS = {
    "conservative": {
        "max_position_size": 0.15,
        "stop_loss": 0.05,
        "take_profit": 0.10,
        "max_drawdown": 0.08,
        "strategies": ["dividend_yield_strategy", "value_investment"]
    },
    "moderate": {
        "max_position_size": 0.25,
        "stop_loss": 0.08,
        "take_profit": 0.20,
        "max_drawdown": 0.12,
        "strategies": ["triple_ma_strategy", "rsi_macd_combo", "peg_strategy"]
    },
    "aggressive": {
        "max_position_size": 0.35,
        "stop_loss": 0.12,
        "take_profit": 0.35,
        "max_drawdown": 0.20,
        "strategies": ["breakout_strategy", "momentum_strategy", "volatility_breakout"]
    },
    "speculative": {
        "max_position_size": 0.15,  # 虽然风险高但仓位要控制
        "stop_loss": 0.05,
        "take_profit": 0.08,
        "max_drawdown": 0.10,
        "strategies": ["pairs_trading_strategy", "mean_reversion"]
    }
}

# 市场环境适应性
MARKET_ENVIRONMENT_STRATEGIES = {
    "bull_market": [
        "momentum_strategy", "breakout_strategy", "growth_investment",
        "quality_momentum", "sector_rotation"
    ],
    "bear_market": [
        "dividend_yield_strategy", "value_investment", "pairs_trading_strategy",
        "mean_reversion"
    ],
    "sideways_market": [
        "rsi_oversold", "bollinger_bands", "pairs_trading_strategy",
        "volatility_breakout"
    ],
    "high_volatility": [
        "volatility_breakout", "pairs_trading_strategy", "mean_reversion"
    ],
    "low_volatility": [
        "momentum_strategy", "breakout_strategy", "sector_rotation"
    ]
}

def get_strategies_by_risk_level(risk_level: str) -> list:
    """根据风险等级获取适合的策略"""
    return STRATEGY_RISK_LEVELS.get(risk_level, {}).get("strategies", [])

def get_strategies_by_market_environment(environment: str) -> list:
    """根据市场环境获取适合的策略"""
    return MARKET_ENVIRONMENT_STRATEGIES.get(environment, [])

def get_strategy_template(strategy_id: str) -> dict:
    """获取策略模板"""
    return MAINSTREAM_STRATEGIES.get(strategy_id, {})

def get_all_strategy_ids() -> list:
    """获取所有策略ID"""
    return list(MAINSTREAM_STRATEGIES.keys())

def get_strategies_by_type(strategy_type: str) -> list:
    """根据策略类型获取策略"""
    return [
        strategy_id for strategy_id, strategy in MAINSTREAM_STRATEGIES.items()
        if strategy.get("strategy_type") == strategy_type
    ]