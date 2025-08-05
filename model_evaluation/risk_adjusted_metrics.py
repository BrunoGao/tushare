#!/usr/bin/env python3
"""
风险调整指标计算器
实现专业的风险调整性能指标计算
"""

import logging
import numpy as np
import pandas as pd
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Any, Tuple, Union
from dataclasses import dataclass
from scipy import stats

@dataclass
class RiskMetrics:
    """风险指标结果"""
    sharpe_ratio: float
    sortino_ratio: float
    calmar_ratio: float
    information_ratio: float
    treynor_ratio: float
    jensen_alpha: float
    beta: float
    tracking_error: float
    max_drawdown: float
    var_95: float
    cvar_95: float
    downside_deviation: float
    upside_capture: float
    downside_capture: float
    win_rate: float
    loss_rate: float
    avg_win: float
    avg_loss: float
    profit_factor: float
    recovery_factor: float
    pain_index: float
    sterling_ratio: float
    burke_ratio: float
    calculation_period: str
    annualization_factor: float

class RiskAdjustedMetrics:
    """风险调整指标计算器"""
    
    def __init__(self, 
                 risk_free_rate: float = 0.02,
                 trading_days_per_year: int = 252,
                 confidence_level: float = 0.95):
        """
        初始化风险调整指标计算器
        
        Args:
            risk_free_rate: 无风险收益率（年化）
            trading_days_per_year: 每年交易日数
            confidence_level: 置信水平（用于VaR和CVaR计算）
        """
        self.logger = logging.getLogger(__name__)
        self.risk_free_rate = risk_free_rate
        self.trading_days_per_year = trading_days_per_year
        self.confidence_level = confidence_level
        
        self.logger.info(f"风险调整指标计算器初始化完成 - 无风险利率: {risk_free_rate:.2%}")
    
    def calculate_all_metrics(self, 
                            returns: Union[pd.Series, np.ndarray],
                            benchmark_returns: Union[pd.Series, np.ndarray] = None,
                            market_returns: Union[pd.Series, np.ndarray] = None,
                            prices: Union[pd.Series, np.ndarray] = None) -> RiskMetrics:
        """
        计算所有风险调整指标
        
        Args:
            returns: 投资组合收益率序列
            benchmark_returns: 基准收益率序列
            market_returns: 市场收益率序列（用于Beta计算）
            prices: 价格序列（用于计算收益率和回撤）
            
        Returns:
            风险指标结果
        """
        try:
            # 数据预处理
            returns = self._ensure_series(returns)
            
            if prices is not None:
                prices = self._ensure_series(prices)
                # 如果提供了价格序列，重新计算收益率以确保一致性
                returns = prices.pct_change().dropna()
            
            if benchmark_returns is not None:
                benchmark_returns = self._ensure_series(benchmark_returns)
                # 确保数据长度一致
                min_length = min(len(returns), len(benchmark_returns))
                returns = returns.iloc[-min_length:]
                benchmark_returns = benchmark_returns.iloc[-min_length:]
            
            if market_returns is not None:
                market_returns = self._ensure_series(market_returns)
                min_length = min(len(returns), len(market_returns))
                returns = returns.iloc[-min_length:]
                market_returns = market_returns.iloc[-min_length:]
            
            # 计算年化因子
            if len(returns) <= 1:
                raise ValueError("收益率序列长度不足")
            
            # 推断数据频率并计算年化因子
            annualization_factor = self._calculate_annualization_factor(returns)
            
            # 计算各项指标
            metrics = RiskMetrics(
                sharpe_ratio=self.sharpe_ratio(returns),
                sortino_ratio=self.sortino_ratio(returns),
                calmar_ratio=self.calmar_ratio(returns, prices),
                information_ratio=self.information_ratio(returns, benchmark_returns) if benchmark_returns is not None else np.nan,
                treynor_ratio=self.treynor_ratio(returns, market_returns) if market_returns is not None else np.nan,
                jensen_alpha=self.jensen_alpha(returns, market_returns) if market_returns is not None else np.nan,
                beta=self.beta(returns, market_returns) if market_returns is not None else np.nan,
                tracking_error=self.tracking_error(returns, benchmark_returns) if benchmark_returns is not None else np.nan,
                max_drawdown=self.max_drawdown(returns, prices),
                var_95=self.value_at_risk(returns, self.confidence_level),
                cvar_95=self.conditional_value_at_risk(returns, self.confidence_level),
                downside_deviation=self.downside_deviation(returns),
                upside_capture=self.upside_capture_ratio(returns, benchmark_returns) if benchmark_returns is not None else np.nan,
                downside_capture=self.downside_capture_ratio(returns, benchmark_returns) if benchmark_returns is not None else np.nan,
                win_rate=self.win_rate(returns),
                loss_rate=self.loss_rate(returns),
                avg_win=self.average_win(returns),
                avg_loss=self.average_loss(returns),
                profit_factor=self.profit_factor(returns),
                recovery_factor=self.recovery_factor(returns, prices),
                pain_index=self.pain_index(returns, prices),
                sterling_ratio=self.sterling_ratio(returns, prices),
                burke_ratio=self.burke_ratio(returns, prices),
                calculation_period=f"{returns.index[0]} to {returns.index[-1]}" if hasattr(returns, 'index') else "N/A",
                annualization_factor=annualization_factor
            )
            
            self.logger.info("风险调整指标计算完成")
            return metrics
            
        except Exception as e:
            self.logger.error(f"计算风险调整指标失败: {e}")
            raise e
    
    def _ensure_series(self, data: Union[pd.Series, np.ndarray, List]) -> pd.Series:
        """确保数据为pandas Series格式"""
        if isinstance(data, pd.Series):
            return data
        elif isinstance(data, np.ndarray):
            return pd.Series(data)
        elif isinstance(data, list):
            return pd.Series(data)
        else:
            raise ValueError(f"不支持的数据类型: {type(data)}")
    
    def _calculate_annualization_factor(self, returns: pd.Series) -> float:
        """计算年化因子"""
        if hasattr(returns, 'index') and hasattr(returns.index, 'freq'):
            # 尝试从索引频率推断
            freq = returns.index.freq
            if freq is not None:
                if 'D' in str(freq):  # 日频
                    return self.trading_days_per_year
                elif 'W' in str(freq):  # 周频
                    return 52
                elif 'M' in str(freq):  # 月频
                    return 12
                elif 'Q' in str(freq):  # 季频
                    return 4
                elif 'Y' in str(freq):  # 年频
                    return 1
        
        # 根据数据长度和时间跨度推断
        if hasattr(returns, 'index') and len(returns) > 1:
            try:
                time_span = (returns.index[-1] - returns.index[0]).days
                if time_span > 0:
                    periods_per_year = len(returns) * 365.25 / time_span
                    return periods_per_year
            except:
                pass
        
        # 默认假设为日频数据
        return self.trading_days_per_year
    
    def sharpe_ratio(self, returns: Union[pd.Series, np.ndarray]) -> float:
        """
        计算夏普比率
        
        Args:
            returns: 收益率序列
            
        Returns:
            夏普比率
        """
        try:
            returns = self._ensure_series(returns)
            if len(returns) == 0:
                return np.nan
            
            annualization_factor = self._calculate_annualization_factor(returns)
            daily_risk_free = self.risk_free_rate / annualization_factor
            
            excess_returns = returns - daily_risk_free
            
            if returns.std() == 0:
                return 0.0 if excess_returns.mean() == 0 else np.inf
            
            return np.sqrt(annualization_factor) * excess_returns.mean() / returns.std()
            
        except Exception as e:
            self.logger.error(f"计算夏普比率失败: {e}")
            return np.nan
    
    def sortino_ratio(self, returns: Union[pd.Series, np.ndarray]) -> float:
        """
        计算索提诺比率
        
        Args:
            returns: 收益率序列
            
        Returns:
            索提诺比率
        """
        try:
            returns = self._ensure_series(returns)
            if len(returns) == 0:
                return np.nan
            
            annualization_factor = self._calculate_annualization_factor(returns)
            daily_risk_free = self.risk_free_rate / annualization_factor
            
            excess_returns = returns - daily_risk_free
            downside_returns = returns[returns < daily_risk_free]
            
            if len(downside_returns) == 0 or downside_returns.std() == 0:
                return np.inf if excess_returns.mean() > 0 else 0.0
            
            downside_std = np.sqrt(np.mean((downside_returns - daily_risk_free) ** 2))
            
            return np.sqrt(annualization_factor) * excess_returns.mean() / downside_std
            
        except Exception as e:
            self.logger.error(f"计算索提诺比率失败: {e}")
            return np.nan
    
    def calmar_ratio(self, returns: Union[pd.Series, np.ndarray], prices: Union[pd.Series, np.ndarray] = None) -> float:
        """
        计算卡尔玛比率
        
        Args:
            returns: 收益率序列
            prices: 价格序列
            
        Returns:
            卡尔玛比率
        """
        try:
            returns = self._ensure_series(returns)
            if len(returns) == 0:
                return np.nan
            
            annualization_factor = self._calculate_annualization_factor(returns)
            annual_return = (1 + returns).prod() ** (annualization_factor / len(returns)) - 1
            
            max_dd = self.max_drawdown(returns, prices)
            
            if abs(max_dd) < 1e-6:  # 避免除零
                return np.inf if annual_return > 0 else 0.0
            
            return annual_return / abs(max_dd)
            
        except Exception as e:
            self.logger.error(f"计算卡尔玛比率失败: {e}")
            return np.nan
    
    def information_ratio(self, returns: Union[pd.Series, np.ndarray], benchmark_returns: Union[pd.Series, np.ndarray]) -> float:
        """
        计算信息比率
        
        Args:
            returns: 投资组合收益率
            benchmark_returns: 基准收益率
            
        Returns:
            信息比率
        """
        try:
            returns = self._ensure_series(returns)
            benchmark_returns = self._ensure_series(benchmark_returns)
            
            if len(returns) == 0 or len(benchmark_returns) == 0:
                return np.nan
            
            # 确保长度一致
            min_length = min(len(returns), len(benchmark_returns))
            returns = returns.iloc[-min_length:]
            benchmark_returns = benchmark_returns.iloc[-min_length:]
            
            active_returns = returns - benchmark_returns
            tracking_error = active_returns.std()
            
            if tracking_error == 0:
                return 0.0 if active_returns.mean() == 0 else np.inf
            
            annualization_factor = self._calculate_annualization_factor(returns)
            return active_returns.mean() * annualization_factor / (tracking_error * np.sqrt(annualization_factor))
            
        except Exception as e:
            self.logger.error(f"计算信息比率失败: {e}")
            return np.nan
    
    def treynor_ratio(self, returns: Union[pd.Series, np.ndarray], market_returns: Union[pd.Series, np.ndarray]) -> float:
        """
        计算特雷诺比率
        
        Args:
            returns: 投资组合收益率
            market_returns: 市场收益率
            
        Returns:
            特雷诺比率
        """
        try:
            beta_val = self.beta(returns, market_returns)
            if abs(beta_val) < 1e-6:  # 避免除零
                return np.nan
            
            returns = self._ensure_series(returns)
            annualization_factor = self._calculate_annualization_factor(returns)
            annual_return = (1 + returns).prod() ** (annualization_factor / len(returns)) - 1
            
            return (annual_return - self.risk_free_rate) / beta_val
            
        except Exception as e:
            self.logger.error(f"计算特雷诺比率失败: {e}")
            return np.nan
    
    def jensen_alpha(self, returns: Union[pd.Series, np.ndarray], market_returns: Union[pd.Series, np.ndarray]) -> float:
        """
        计算詹森阿尔法
        
        Args:
            returns: 投资组合收益率
            market_returns: 市场收益率
            
        Returns:
            詹森阿尔法
        """
        try:
            returns = self._ensure_series(returns)
            market_returns = self._ensure_series(market_returns)
            
            # 确保长度一致
            min_length = min(len(returns), len(market_returns))
            returns = returns.iloc[-min_length:]
            market_returns = market_returns.iloc[-min_length:]
            
            if len(returns) == 0:
                return np.nan
            
            annualization_factor = self._calculate_annualization_factor(returns)
            daily_risk_free = self.risk_free_rate / annualization_factor
            
            excess_portfolio_returns = returns - daily_risk_free
            excess_market_returns = market_returns - daily_risk_free
            
            if len(excess_market_returns) == 0 or excess_market_returns.std() == 0:
                return np.nan
            
            # 使用线性回归计算alpha
            beta_val = np.cov(excess_portfolio_returns, excess_market_returns)[0, 1] / np.var(excess_market_returns)
            alpha = excess_portfolio_returns.mean() - beta_val * excess_market_returns.mean()
            
            return alpha * annualization_factor
            
        except Exception as e:
            self.logger.error(f"计算詹森阿尔法失败: {e}")
            return np.nan
    
    def beta(self, returns: Union[pd.Series, np.ndarray], market_returns: Union[pd.Series, np.ndarray]) -> float:
        """
        计算贝塔系数
        
        Args:
            returns: 投资组合收益率
            market_returns: 市场收益率
            
        Returns:
            贝塔系数
        """
        try:
            returns = self._ensure_series(returns)
            market_returns = self._ensure_series(market_returns)
            
            # 确保长度一致
            min_length = min(len(returns), len(market_returns))
            returns = returns.iloc[-min_length:]
            market_returns = market_returns.iloc[-min_length:]
            
            if len(returns) == 0 or market_returns.var() == 0:
                return np.nan
            
            return np.cov(returns, market_returns)[0, 1] / np.var(market_returns)
            
        except Exception as e:
            self.logger.error(f"计算贝塔系数失败: {e}")
            return np.nan
    
    def tracking_error(self, returns: Union[pd.Series, np.ndarray], benchmark_returns: Union[pd.Series, np.ndarray]) -> float:
        """
        计算跟踪误差
        
        Args:
            returns: 投资组合收益率
            benchmark_returns: 基准收益率
            
        Returns:
            跟踪误差（年化）
        """
        try:
            returns = self._ensure_series(returns)
            benchmark_returns = self._ensure_series(benchmark_returns)
            
            # 确保长度一致
            min_length = min(len(returns), len(benchmark_returns))
            returns = returns.iloc[-min_length:]
            benchmark_returns = benchmark_returns.iloc[-min_length:]
            
            if len(returns) == 0:
                return np.nan
            
            active_returns = returns - benchmark_returns
            annualization_factor = self._calculate_annualization_factor(returns)
            
            return active_returns.std() * np.sqrt(annualization_factor)
            
        except Exception as e:
            self.logger.error(f"计算跟踪误差失败: {e}")
            return np.nan
    
    def max_drawdown(self, returns: Union[pd.Series, np.ndarray], prices: Union[pd.Series, np.ndarray] = None) -> float:
        """
        计算最大回撤
        
        Args:
            returns: 收益率序列
            prices: 价格序列
            
        Returns:
            最大回撤
        """
        try:
            if prices is not None:
                prices = self._ensure_series(prices)
                cumulative = prices
            else:
                returns = self._ensure_series(returns)
                cumulative = (1 + returns).cumprod()
            
            if len(cumulative) == 0:
                return np.nan
            
            rolling_max = cumulative.expanding().max()
            drawdown = (cumulative - rolling_max) / rolling_max
            
            return drawdown.min()
            
        except Exception as e:
            self.logger.error(f"计算最大回撤失败: {e}")
            return np.nan
    
    def value_at_risk(self, returns: Union[pd.Series, np.ndarray], confidence_level: float = None) -> float:
        """
        计算风险价值（VaR）
        
        Args:
            returns: 收益率序列
            confidence_level: 置信水平
            
        Returns:
            VaR值
        """
        try:
            returns = self._ensure_series(returns)
            if len(returns) == 0:
                return np.nan
            
            if confidence_level is None:
                confidence_level = self.confidence_level
            
            return np.percentile(returns, (1 - confidence_level) * 100)
            
        except Exception as e:
            self.logger.error(f"计算VaR失败: {e}")
            return np.nan
    
    def conditional_value_at_risk(self, returns: Union[pd.Series, np.ndarray], confidence_level: float = None) -> float:
        """
        计算条件风险价值（CVaR）
        
        Args:
            returns: 收益率序列
            confidence_level: 置信水平
            
        Returns:
            CVaR值
        """
        try:
            returns = self._ensure_series(returns)
            if len(returns) == 0:
                return np.nan
            
            if confidence_level is None:
                confidence_level = self.confidence_level
            
            var = self.value_at_risk(returns, confidence_level)
            return returns[returns <= var].mean()
            
        except Exception as e:
            self.logger.error(f"计算CVaR失败: {e}")
            return np.nan
    
    def downside_deviation(self, returns: Union[pd.Series, np.ndarray], target_return: float = 0.0) -> float:
        """
        计算下行偏差
        
        Args:
            returns: 收益率序列
            target_return: 目标收益率
            
        Returns:
            下行偏差
        """
        try:
            returns = self._ensure_series(returns)
            if len(returns) == 0:
                return np.nan
            
            downside_returns = returns[returns < target_return]
            if len(downside_returns) == 0:
                return 0.0
            
            return np.sqrt(np.mean((downside_returns - target_return) ** 2))
            
        except Exception as e:
            self.logger.error(f"计算下行偏差失败: {e}")
            return np.nan
    
    def upside_capture_ratio(self, returns: Union[pd.Series, np.ndarray], benchmark_returns: Union[pd.Series, np.ndarray]) -> float:
        """
        计算上行捕获比率
        
        Args:
            returns: 投资组合收益率
            benchmark_returns: 基准收益率
            
        Returns:
            上行捕获比率
        """
        try:
            returns = self._ensure_series(returns)
            benchmark_returns = self._ensure_series(benchmark_returns)
            
            # 确保长度一致
            min_length = min(len(returns), len(benchmark_returns))
            returns = returns.iloc[-min_length:]
            benchmark_returns = benchmark_returns.iloc[-min_length:]
            
            # 筛选基准上涨的时期
            upside_mask = benchmark_returns > 0
            upside_portfolio = returns[upside_mask]
            upside_benchmark = benchmark_returns[upside_mask]
            
            if len(upside_benchmark) == 0 or upside_benchmark.mean() == 0:
                return np.nan
            
            return upside_portfolio.mean() / upside_benchmark.mean()
            
        except Exception as e:
            self.logger.error(f"计算上行捕获比率失败: {e}")
            return np.nan
    
    def downside_capture_ratio(self, returns: Union[pd.Series, np.ndarray], benchmark_returns: Union[pd.Series, np.ndarray]) -> float:
        """
        计算下行捕获比率
        
        Args:
            returns: 投资组合收益率
            benchmark_returns: 基准收益率
            
        Returns:
            下行捕获比率
        """
        try:
            returns = self._ensure_series(returns)
            benchmark_returns = self._ensure_series(benchmark_returns)
            
            # 确保长度一致
            min_length = min(len(returns), len(benchmark_returns))
            returns = returns.iloc[-min_length:]
            benchmark_returns = benchmark_returns.iloc[-min_length:]
            
            # 筛选基准下跌的时期
            downside_mask = benchmark_returns < 0
            downside_portfolio = returns[downside_mask]
            downside_benchmark = benchmark_returns[downside_mask]
            
            if len(downside_benchmark) == 0 or downside_benchmark.mean() == 0:
                return np.nan
            
            return downside_portfolio.mean() / downside_benchmark.mean()
            
        except Exception as e:
            self.logger.error(f"计算下行捕获比率失败: {e}")
            return np.nan
    
    def win_rate(self, returns: Union[pd.Series, np.ndarray]) -> float:
        """
        计算胜率
        
        Args:
            returns: 收益率序列
            
        Returns:
            胜率
        """
        try:
            returns = self._ensure_series(returns)
            if len(returns) == 0:
                return np.nan
            
            winning_periods = (returns > 0).sum()
            return winning_periods / len(returns)
            
        except Exception as e:
            self.logger.error(f"计算胜率失败: {e}")
            return np.nan
    
    def loss_rate(self, returns: Union[pd.Series, np.ndarray]) -> float:
        """
        计算败率
        
        Args:
            returns: 收益率序列
            
        Returns:
            败率
        """
        try:
            returns = self._ensure_series(returns)
            if len(returns) == 0:
                return np.nan
            
            losing_periods = (returns < 0).sum()
            return losing_periods / len(returns)
            
        except Exception as e:
            self.logger.error(f"计算败率失败: {e}")
            return np.nan
    
    def average_win(self, returns: Union[pd.Series, np.ndarray]) -> float:
        """
        计算平均盈利
        
        Args:
            returns: 收益率序列
            
        Returns:
            平均盈利
        """
        try:
            returns = self._ensure_series(returns)
            winning_returns = returns[returns > 0]
            
            if len(winning_returns) == 0:
                return 0.0
            
            return winning_returns.mean()
            
        except Exception as e:
            self.logger.error(f"计算平均盈利失败: {e}")
            return np.nan
    
    def average_loss(self, returns: Union[pd.Series, np.ndarray]) -> float:
        """
        计算平均亏损
        
        Args:
            returns: 收益率序列
            
        Returns:
            平均亏损
        """
        try:
            returns = self._ensure_series(returns)
            losing_returns = returns[returns < 0]
            
            if len(losing_returns) == 0:
                return 0.0
            
            return losing_returns.mean()
            
        except Exception as e:
            self.logger.error(f"计算平均亏损失败: {e}")
            return np.nan
    
    def profit_factor(self, returns: Union[pd.Series, np.ndarray]) -> float:
        """
        计算盈利因子
        
        Args:
            returns: 收益率序列
            
        Returns:
            盈利因子
        """
        try:
            total_gains = returns[returns > 0].sum()
            total_losses = abs(returns[returns < 0].sum())
            
            if total_losses == 0:
                return np.inf if total_gains > 0 else 0.0
            
            return total_gains / total_losses
            
        except Exception as e:
            self.logger.error(f"计算盈利因子失败: {e}")
            return np.nan
    
    def recovery_factor(self, returns: Union[pd.Series, np.ndarray], prices: Union[pd.Series, np.ndarray] = None) -> float:
        """
        计算恢复因子
        
        Args:
            returns: 收益率序列
            prices: 价格序列
            
        Returns:
            恢复因子
        """
        try:
            returns = self._ensure_series(returns)
            if len(returns) == 0:
                return np.nan
            
            total_return = (1 + returns).prod() - 1
            max_dd = abs(self.max_drawdown(returns, prices))
            
            if max_dd == 0:
                return np.inf if total_return > 0 else 0.0
            
            return total_return / max_dd
            
        except Exception as e:
            self.logger.error(f"计算恢复因子失败: {e}")
            return np.nan
    
    def pain_index(self, returns: Union[pd.Series, np.ndarray], prices: Union[pd.Series, np.ndarray] = None) -> float:
        """
        计算疼痛指数
        
        Args:
            returns: 收益率序列
            prices: 价格序列
            
        Returns:
            疼痛指数
        """
        try:
            if prices is not None:
                prices = self._ensure_series(prices)
                cumulative = prices
            else:
                returns = self._ensure_series(returns)
                cumulative = (1 + returns).cumprod()
            
            if len(cumulative) == 0:
                return np.nan
            
            rolling_max = cumulative.expanding().max()
            drawdown = (rolling_max - cumulative) / rolling_max
            
            return drawdown.mean()
            
        except Exception as e:
            self.logger.error(f"计算疼痛指数失败: {e}")
            return np.nan
    
    def sterling_ratio(self, returns: Union[pd.Series, np.ndarray], prices: Union[pd.Series, np.ndarray] = None) -> float:
        """
        计算斯特林比率
        
        Args:
            returns: 收益率序列
            prices: 价格序列
            
        Returns:
            斯特林比率
        """
        try:
            returns = self._ensure_series(returns)
            if len(returns) == 0:
                return np.nan
            
            annualization_factor = self._calculate_annualization_factor(returns)
            annual_return = (1 + returns).prod() ** (annualization_factor / len(returns)) - 1
            
            avg_drawdown = self.pain_index(returns, prices)
            
            if avg_drawdown == 0:
                return np.inf if annual_return > 0 else 0.0
            
            return annual_return / avg_drawdown
            
        except Exception as e:
            self.logger.error(f"计算斯特林比率失败: {e}")
            return np.nan
    
    def burke_ratio(self, returns: Union[pd.Series, np.ndarray], prices: Union[pd.Series, np.ndarray] = None) -> float:
        """
        计算伯克比率
        
        Args:
            returns: 收益率序列
            prices: 价格序列
            
        Returns:
            伯克比率
        """
        try:
            returns = self._ensure_series(returns)
            if len(returns) == 0:
                return np.nan
            
            annualization_factor = self._calculate_annualization_factor(returns)
            annual_return = (1 + returns).prod() ** (annualization_factor / len(returns)) - 1
            
            # 计算回撤的平方根
            if prices is not None:
                prices = self._ensure_series(prices)
                cumulative = prices
            else:
                cumulative = (1 + returns).cumprod()
            
            rolling_max = cumulative.expanding().max()
            drawdown = (rolling_max - cumulative) / rolling_max
            
            drawdown_squared_sum = (drawdown ** 2).sum()
            
            if drawdown_squared_sum == 0:
                return np.inf if annual_return > 0 else 0.0
            
            return annual_return / np.sqrt(drawdown_squared_sum)
            
        except Exception as e:
            self.logger.error(f"计算伯克比率失败: {e}")
            return np.nan
    
    def generate_report(self, metrics: RiskMetrics, save_path: str = None) -> str:
        """
        生成风险指标报告
        
        Args:
            metrics: 风险指标结果
            save_path: 保存路径
            
        Returns:
            报告文件路径
        """
        try:
            if save_path is None:
                save_path = f"risk_metrics_report_{datetime.now().strftime('%Y%m%d_%H%M%S')}.txt"
            
            report_lines = []
            report_lines.append("=" * 80)
            report_lines.append("风险调整指标分析报告")
            report_lines.append("=" * 80)
            report_lines.append(f"计算期间: {metrics.calculation_period}")
            report_lines.append(f"年化因子: {metrics.annualization_factor:.2f}")
            report_lines.append("")
            
            # 核心风险调整比率
            report_lines.append("核心风险调整比率:")
            report_lines.append("-" * 40)
            report_lines.append(f"夏普比率 (Sharpe Ratio): {metrics.sharpe_ratio:.4f}")
            report_lines.append(f"索提诺比率 (Sortino Ratio): {metrics.sortino_ratio:.4f}")
            report_lines.append(f"卡尔玛比率 (Calmar Ratio): {metrics.calmar_ratio:.4f}")
            if not np.isnan(metrics.information_ratio):
                report_lines.append(f"信息比率 (Information Ratio): {metrics.information_ratio:.4f}")
            if not np.isnan(metrics.treynor_ratio):
                report_lines.append(f"特雷诺比率 (Treynor Ratio): {metrics.treynor_ratio:.4f}")
            report_lines.append("")
            
            # 市场相关指标
            if not np.isnan(metrics.beta):
                report_lines.append("市场相关指标:")
                report_lines.append("-" * 40)
                report_lines.append(f"贝塔系数 (Beta): {metrics.beta:.4f}")
                if not np.isnan(metrics.jensen_alpha):
                    report_lines.append(f"詹森阿尔法 (Jensen Alpha): {metrics.jensen_alpha:.4f}")
                if not np.isnan(metrics.tracking_error):
                    report_lines.append(f"跟踪误差 (Tracking Error): {metrics.tracking_error:.4f}")
                report_lines.append("")
            
            # 风险指标
            report_lines.append("风险指标:")
            report_lines.append("-" * 40)
            report_lines.append(f"最大回撤 (Max Drawdown): {metrics.max_drawdown:.4f}")
            report_lines.append(f"95% VaR: {metrics.var_95:.4f}")
            report_lines.append(f"95% CVaR: {metrics.cvar_95:.4f}")
            report_lines.append(f"下行偏差 (Downside Deviation): {metrics.downside_deviation:.4f}")
            report_lines.append(f"疼痛指数 (Pain Index): {metrics.pain_index:.4f}")
            report_lines.append("")
            
            # 捕获比率
            if not np.isnan(metrics.upside_capture):
                report_lines.append("捕获比率:")
                report_lines.append("-" * 40)
                report_lines.append(f"上行捕获比率 (Upside Capture): {metrics.upside_capture:.4f}")
                report_lines.append(f"下行捕获比率 (Downside Capture): {metrics.downside_capture:.4f}")
                report_lines.append("")
            
            # 交易统计
            report_lines.append("交易统计:")
            report_lines.append("-" * 40)
            report_lines.append(f"胜率 (Win Rate): {metrics.win_rate:.2%}")
            report_lines.append(f"败率 (Loss Rate): {metrics.loss_rate:.2%}")
            report_lines.append(f"平均盈利 (Average Win): {metrics.avg_win:.4f}")
            report_lines.append(f"平均亏损 (Average Loss): {metrics.avg_loss:.4f}")
            report_lines.append(f"盈利因子 (Profit Factor): {metrics.profit_factor:.4f}")
            report_lines.append("")
            
            # 其他比率
            report_lines.append("其他比率:")
            report_lines.append("-" * 40)
            report_lines.append(f"恢复因子 (Recovery Factor): {metrics.recovery_factor:.4f}")
            report_lines.append(f"斯特林比率 (Sterling Ratio): {metrics.sterling_ratio:.4f}")
            report_lines.append(f"伯克比率 (Burke Ratio): {metrics.burke_ratio:.4f}")
            report_lines.append("")
            
            # 写入文件
            with open(save_path, 'w', encoding='utf-8') as f:
                f.write('\n'.join(report_lines))
            
            self.logger.info(f"风险指标报告已生成: {save_path}")
            return save_path
            
        except Exception as e:
            self.logger.error(f"生成报告失败: {e}")
            return ""