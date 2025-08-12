#!/usr/bin/env python3
"""
策略验证模块
提供完整的投资策略回测、评估和分析功能
"""

from .advanced_validator import (
    AdvancedStrategyValidator,
    StrategyConfig,
    ValidationResult,
    TechnicalIndicators,
    rsi_mean_reversion_strategy,
    macd_trend_strategy
)

__version__ = "1.0.0"
__author__ = "LJWX-Stock Team"

__all__ = [
    'AdvancedStrategyValidator',
    'StrategyConfig', 
    'ValidationResult',
    'TechnicalIndicators',
    'rsi_mean_reversion_strategy',
    'macd_trend_strategy'
]