#!/usr/bin/env python3
"""
股票策略执行引擎 - 统一大模型架构
基于ljwx-stock-advanced模型，通过策略参数化处理所有投资策略
支持价值投资、技术分析、量化交易、动量策略等
"""

import pandas as pd
import numpy as np
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Tuple, Any
# Try to import talib, provide fallback if not available
try:
    import talib
    TALIB_AVAILABLE = True
except ImportError:
    TALIB_AVAILABLE = False
    print("Warning: TA-Lib not available, using basic implementations")
import logging

from .strategy_models import (
    Strategy, StrategyRule, TradingCondition, BacktestResult,
    IndicatorType, ConditionOperator, SignalType
)

class UnifiedModelInterface:
    """统一大模型接口 - 一个模型处理所有策略"""
    
    def __init__(self, model_name: str = "ljwx-stock-advanced"):
        self.model_name = model_name
        self.strategy_prompts = self._initialize_strategy_prompts()
    
    def _initialize_strategy_prompts(self) -> Dict[str, Dict]:
        """初始化不同策略的提示词模板"""
        return {
            "value_investment": {
                "name": "价值投资策略",
                "prompt_template": """
                作为专业的价值投资分析师，基于以下财务数据分析股票投资价值：
                - PE比率: {pe_ratio}
                - PB比率: {pb_ratio}  
                - ROE: {roe}
                - 负债率: {debt_ratio}
                - 现金流: {cash_flow}
                
                请分析该股票的内在价值并给出投资建议。
                """,
                "focus_metrics": ["pe_ratio", "pb_ratio", "roe", "debt_ratio", "dividend_yield"]
            },
            
            "technical_analysis": {
                "name": "技术分析策略", 
                "prompt_template": """
                作为专业的技术分析师，基于以下技术指标分析股票走势：
                - RSI: {rsi}
                - MACD: {macd_line}, {signal_line}
                - 布林带: 上轨{bb_upper}, 中轨{bb_middle}, 下轨{bb_lower}
                - 成交量: {volume}
                - K线形态: {candlestick_pattern}
                
                请分析技术面信号并给出交易建议。
                """,
                "focus_metrics": ["rsi", "macd", "bollinger_bands", "volume", "support_resistance"]
            },
            
            "quantitative_trading": {
                "name": "量化交易策略",
                "prompt_template": """
                作为量化交易专家，基于以下量化指标制定交易策略：
                - 夏普比率: {sharpe_ratio}
                - 最大回撤: {max_drawdown}
                - 波动率: {volatility}
                - 贝塔系数: {beta}
                - 阿尔法: {alpha}
                - 相关性: {correlation}
                
                请基于量化模型给出精确的交易信号。
                """,
                "focus_metrics": ["sharpe_ratio", "volatility", "beta", "alpha", "correlation"]
            },
            
            "momentum_strategy": {
                "name": "动量策略",
                "prompt_template": """
                作为动量交易专家，基于以下动量指标分析趋势：
                - 价格动量: {price_momentum}
                - 成交量动量: {volume_momentum}
                - 相对强度: {relative_strength}
                - 趋势强度: {trend_strength}
                - 突破信号: {breakout_signal}
                
                请分析动量信号并给出趋势跟踪建议。
                """,
                "focus_metrics": ["momentum", "relative_strength", "trend_strength", "breakout"]
            },
            
            "arbitrage_strategy": {
                "name": "套利策略",
                "prompt_template": """
                作为套利交易专家，基于以下市场数据寻找套利机会：
                - 价差: {price_spread}
                - 相关性: {correlation}
                - 基差: {basis}
                - 流动性: {liquidity}
                - 交易成本: {transaction_cost}
                
                请识别套利机会并评估风险收益比。
                """,
                "focus_metrics": ["spread", "correlation", "liquidity", "transaction_cost"]
            }
        }
    
    def generate_strategy_analysis(self, strategy_type: str, market_data: Dict) -> Dict:
        """使用统一模型生成策略分析"""
        if strategy_type not in self.strategy_prompts:
            raise ValueError(f"不支持的策略类型: {strategy_type}")
        
        strategy_config = self.strategy_prompts[strategy_type]
        
        # 构建上下文相关的提示词
        prompt = strategy_config["prompt_template"].format(**market_data)
        
        # 这里会调用实际的大模型 (ljwx-stock-advanced)
        analysis_result = self._call_unified_model(prompt, strategy_type)
        
        return {
            "strategy_type": strategy_type,
            "strategy_name": strategy_config["name"],
            "model_used": self.model_name,
            "analysis": analysis_result,
            "confidence": analysis_result.get("confidence", 0.85),
            "recommendation": analysis_result.get("recommendation", "hold"),
            "risk_level": analysis_result.get("risk_level", "medium")
        }
    
    def _call_unified_model(self, prompt: str, strategy_type: str) -> Dict:
        """调用统一大模型进行分析"""
        # 这里是实际调用大模型的接口
        # 可以是 Ollama API、OpenAI API 或本地模型推理
        
        try:
            # 模拟模型调用结果
            # 实际实现中会调用 ljwx-stock-advanced 模型
            return {
                "recommendation": self._simulate_model_response(strategy_type),
                "confidence": np.random.uniform(0.7, 0.95),
                "risk_level": self._assess_risk_level(strategy_type),
                "reasoning": f"基于{strategy_type}策略的专业分析结果",
                "target_price": None,
                "stop_loss": None,
                "time_horizon": self._get_time_horizon(strategy_type)
            }
        except Exception as e:
            return {
                "error": str(e),
                "recommendation": "hold",
                "confidence": 0.5,
                "risk_level": "medium"
            }
    
    def _simulate_model_response(self, strategy_type: str) -> str:
        """模拟不同策略的模型响应"""
        responses = {
            "value_investment": np.random.choice(["buy", "hold", "sell"], p=[0.3, 0.5, 0.2]),
            "technical_analysis": np.random.choice(["buy", "hold", "sell"], p=[0.4, 0.3, 0.3]),
            "quantitative_trading": np.random.choice(["buy", "hold", "sell"], p=[0.35, 0.35, 0.3]),
            "momentum_strategy": np.random.choice(["buy", "hold", "sell"], p=[0.45, 0.25, 0.3]),
            "arbitrage_strategy": np.random.choice(["buy", "hold", "sell"], p=[0.4, 0.4, 0.2])
        }
        return responses.get(strategy_type, "hold")
    
    def _assess_risk_level(self, strategy_type: str) -> str:
        """评估不同策略的风险级别"""
        risk_levels = {
            "value_investment": "low",
            "technical_analysis": "medium", 
            "quantitative_trading": "medium",
            "momentum_strategy": "high",
            "arbitrage_strategy": "low"
        }
        return risk_levels.get(strategy_type, "medium")
    
    def _get_time_horizon(self, strategy_type: str) -> str:
        """获取不同策略的时间周期"""
        horizons = {
            "value_investment": "long_term",  # 长期持有
            "technical_analysis": "short_term",  # 短期交易
            "quantitative_trading": "medium_term",  # 中期交易
            "momentum_strategy": "short_term",  # 短期趋势
            "arbitrage_strategy": "very_short_term"  # 极短期
        }
        return horizons.get(strategy_type, "medium_term")

class TechnicalIndicators:
    """技术指标计算器"""
    
    @staticmethod
    def calculate_ma(data: pd.Series, period: int) -> pd.Series:
        """移动平均线"""
        return data.rolling(window=period).mean()
    
    @staticmethod
    def calculate_rsi(data: pd.Series, period: int = 14) -> pd.Series:
        """RSI指标"""
        if TALIB_AVAILABLE:
            return pd.Series(talib.RSI(data.values, timeperiod=period), index=data.index)
        else:
            # 基础RSI实现
            delta = data.diff()
            gain = (delta.where(delta > 0, 0)).rolling(window=period).mean()
            loss = (-delta.where(delta < 0, 0)).rolling(window=period).mean()
            rs = gain / loss
            rsi = 100 - (100 / (1 + rs))
            return rsi
    
    @staticmethod
    def calculate_macd(data: pd.Series, fast: int = 12, slow: int = 26, signal: int = 9) -> Tuple[pd.Series, pd.Series, pd.Series]:
        """MACD指标"""
        if TALIB_AVAILABLE:
            macd, macdsignal, macdhist = talib.MACD(data.values, fastperiod=fast, slowperiod=slow, signalperiod=signal)
            return pd.Series(macd, index=data.index), pd.Series(macdsignal, index=data.index), pd.Series(macdhist, index=data.index)
        else:
            # 基础MACD实现
            ema_fast = data.ewm(span=fast).mean()
            ema_slow = data.ewm(span=slow).mean()
            macd = ema_fast - ema_slow
            macd_signal = macd.ewm(span=signal).mean()
            macd_hist = macd - macd_signal
            return macd, macd_signal, macd_hist
    
    @staticmethod
    def calculate_bollinger_bands(data: pd.Series, period: int = 20, std: float = 2) -> Tuple[pd.Series, pd.Series, pd.Series]:
        """布林带"""
        if TALIB_AVAILABLE:
            upper, middle, lower = talib.BBANDS(data.values, timeperiod=period, nbdevup=std, nbdevdn=std)
            return pd.Series(upper, index=data.index), pd.Series(middle, index=data.index), pd.Series(lower, index=data.index)
        else:
            # 基础布林带实现
            middle = data.rolling(window=period).mean()
            rolling_std = data.rolling(window=period).std()
            upper = middle + (rolling_std * std)
            lower = middle - (rolling_std * std)
            return upper, middle, lower
    
    @staticmethod
    def calculate_kdj(high: pd.Series, low: pd.Series, close: pd.Series, period: int = 9) -> Tuple[pd.Series, pd.Series, pd.Series]:
        """KDJ指标"""
        if TALIB_AVAILABLE:
            k, d = talib.STOCH(high.values, low.values, close.values, 
                              fastk_period=period, slowk_period=3, slowd_period=3)
            k_series = pd.Series(k, index=close.index)
            d_series = pd.Series(d, index=close.index)
            j_series = 3 * k_series - 2 * d_series
            return k_series, d_series, j_series
        else:
            # 基础KDJ实现
            lowest_low = low.rolling(window=period).min()
            highest_high = high.rolling(window=period).max()
            rsv = (close - lowest_low) / (highest_high - lowest_low) * 100
            k = rsv.ewm(com=2).mean()  # K = SMA(RSV, 3)
            d = k.ewm(com=2).mean()    # D = SMA(K, 3)
            j = 3 * k - 2 * d          # J = 3*K - 2*D
            return k, d, j

class SignalCalculator:
    """信号计算器"""
    
    def __init__(self):
        self.indicators = TechnicalIndicators()
        self.logger = logging.getLogger(__name__)
    
    def calculate_signals(self, data: pd.DataFrame, strategy: Strategy) -> pd.DataFrame:
        """计算策略信号"""
        signals = pd.DataFrame(index=data.index)
        signals['buy_signal'] = 0
        signals['sell_signal'] = 0
        signals['price'] = data['close']
        
        # 计算买入信号
        for rule in strategy.buy_rules:
            rule_signal = self._evaluate_rule(data, rule)
            signals['buy_signal'] += rule_signal * rule.weight
        
        # 计算卖出信号
        for rule in strategy.sell_rules:
            rule_signal = self._evaluate_rule(data, rule)
            signals['sell_signal'] += rule_signal * rule.weight
        
        # 归一化信号
        signals['buy_signal'] = (signals['buy_signal'] > 0).astype(int)
        signals['sell_signal'] = (signals['sell_signal'] > 0).astype(int)
        
        return signals
    
    def _evaluate_rule(self, data: pd.DataFrame, rule: StrategyRule) -> pd.Series:
        """评估单个规则"""
        condition_results = []
        
        for condition in rule.conditions:
            result = self._evaluate_condition(data, condition)
            condition_results.append(result)
        
        if not condition_results:
            return pd.Series(0, index=data.index)
        
        # 应用逻辑操作符
        if rule.logic_operator.upper() == "AND":
            final_result = condition_results[0]
            for result in condition_results[1:]:
                final_result = final_result & result
        else:  # OR
            final_result = condition_results[0]
            for result in condition_results[1:]:
                final_result = final_result | result
        
        return final_result.astype(int)
    
    def _evaluate_condition(self, data: pd.DataFrame, condition: TradingCondition) -> pd.Series:
        """评估单个条件"""
        # 计算指标值
        indicator_value = self._calculate_indicator(data, condition)
        
        if indicator_value is None:
            return pd.Series(False, index=data.index)
        
        # 应用条件操作符
        operator = condition.operator
        threshold = condition.threshold
        
        if operator == ConditionOperator.GT.value:
            return indicator_value > threshold
        elif operator == ConditionOperator.LT.value:
            return indicator_value < threshold
        elif operator == ConditionOperator.GTE.value:
            return indicator_value >= threshold
        elif operator == ConditionOperator.LTE.value:
            return indicator_value <= threshold
        elif operator == ConditionOperator.EQ.value:
            return np.abs(indicator_value - threshold) < 0.001
        elif operator == ConditionOperator.CROSS_UP.value:
            return self._detect_cross_up(indicator_value, threshold)
        elif operator == ConditionOperator.CROSS_DOWN.value:
            return self._detect_cross_down(indicator_value, threshold)
        else:
            return pd.Series(False, index=data.index)
    
    def _calculate_indicator(self, data: pd.DataFrame, condition: TradingCondition) -> Optional[pd.Series]:
        """计算技术指标"""
        indicator_type = condition.indicator_type
        params = condition.indicator_params
        
        try:
            if indicator_type == IndicatorType.MA.value:
                period = params.get('period', 20)
                return self.indicators.calculate_ma(data['close'], period)
            
            elif indicator_type == IndicatorType.RSI.value:
                period = params.get('period', 14)
                rsi_values = self.indicators.calculate_rsi(data['close'], period)
                return pd.Series(rsi_values, index=data.index)
            
            elif indicator_type == IndicatorType.MACD.value:
                fast = params.get('fast', 12)
                slow = params.get('slow', 26)
                signal = params.get('signal', 9)
                macd, macd_signal, macd_hist = self.indicators.calculate_macd(data['close'], fast, slow, signal)
                
                # 根据参数决定返回哪个值
                macd_type = params.get('type', 'macd')  # macd, signal, histogram
                if macd_type == 'signal':
                    return macd_signal
                elif macd_type == 'histogram':
                    return macd_hist
                else:
                    return macd
            
            elif indicator_type == IndicatorType.BOLLINGER.value:
                period = params.get('period', 20)
                std = params.get('std', 2)
                upper, middle, lower = self.indicators.calculate_bollinger_bands(data['close'], period, std)
                
                # 根据参数决定返回哪个值
                band_type = params.get('type', 'middle')  # upper, middle, lower
                if band_type == 'upper':
                    return upper
                elif band_type == 'lower':
                    return lower
                else:
                    return middle
            
            elif indicator_type == IndicatorType.KDJ.value:
                period = params.get('period', 9)
                k, d, j = self.indicators.calculate_kdj(data['high'], data['low'], data['close'], period)
                
                # 根据参数决定返回哪个值
                kdj_type = params.get('type', 'k')  # k, d, j
                if kdj_type == 'd':
                    return d
                elif kdj_type == 'j':
                    return j
                else:
                    return k
            
            elif indicator_type == IndicatorType.VOLUME.value:
                return data['volume']
            
            elif indicator_type == IndicatorType.CUSTOM.value:
                # 自定义指标，需要用户提供计算逻辑
                return None
            
        except Exception as e:
            self.logger.error(f"计算指标失败: {indicator_type}, {e}")
            return None
        
        return None
    
    def _detect_cross_up(self, series: pd.Series, threshold: float) -> pd.Series:
        """检测上穿"""
        if isinstance(threshold, (int, float)):
            # 与固定值比较
            prev_below = (series.shift(1) <= threshold)
            curr_above = (series > threshold)
            return prev_below & curr_above
        else:
            # 与另一个序列比较（如双均线交叉）
            # 这里简化处理，实际应该传入另一个序列
            return pd.Series(False, index=series.index)
    
    def _detect_cross_down(self, series: pd.Series, threshold: float) -> pd.Series:
        """检测下穿"""
        if isinstance(threshold, (int, float)):
            # 与固定值比较
            prev_above = (series.shift(1) >= threshold)
            curr_below = (series < threshold)
            return prev_above & curr_below
        else:
            # 与另一个序列比较
            return pd.Series(False, index=series.index)

class BacktestEngine:
    """回测引擎"""
    
    def __init__(self):
        self.signal_calculator = SignalCalculator()
        self.logger = logging.getLogger(__name__)
    
    def run_backtest(self, data: pd.DataFrame, strategy: Strategy, 
                    start_date: str = None, end_date: str = None) -> BacktestResult:
        """运行回测"""
        try:
            # 数据预处理
            if start_date:
                data = data[data.index >= start_date]
            if end_date:
                data = data[data.index <= end_date]
            
            if len(data) < 2:
                raise ValueError("数据量不足，无法进行回测")
            
            # 计算信号
            signals = self.signal_calculator.calculate_signals(data, strategy)
            
            # 执行交易模拟
            trades, equity_curve = self._simulate_trading(data, signals, strategy)
            
            # 计算评估指标
            metrics = self._calculate_metrics(equity_curve, trades, strategy.initial_capital)
            
            # 构建回测结果
            result = BacktestResult(
                strategy_id=strategy.id or "",
                start_date=str(data.index[0].date()) if len(data) > 0 else "",
                end_date=str(data.index[-1].date()) if len(data) > 0 else "",
                **metrics,
                equity_curve=equity_curve,
                trades=trades
            )
            
            return result
            
        except Exception as e:
            self.logger.error(f"回测执行失败: {e}")
            raise
    
    def _simulate_trading(self, data: pd.DataFrame, signals: pd.DataFrame, 
                         strategy: Strategy) -> Tuple[List[Dict], List[Dict]]:
        """模拟交易"""
        trades = []
        equity_curve = []
        
        cash = strategy.initial_capital
        position = 0  # 持仓数量
        position_value = 0  # 持仓价值
        
        for i, (date, row) in enumerate(data.iterrows()):
            price = row['close']
            buy_signal = signals.loc[date, 'buy_signal'] if date in signals.index else 0
            sell_signal = signals.loc[date, 'sell_signal'] if date in signals.index else 0
            
            # 更新持仓价值
            if position > 0:
                position_value = position * price
            
            # 处理买入信号
            if buy_signal and position == 0 and cash > 0:
                # 计算可买入数量（考虑最大仓位限制）
                max_position_value = cash * strategy.risk_management.max_position_size
                shares_to_buy = int(max_position_value / price)
                
                if shares_to_buy > 0:
                    cost = shares_to_buy * price * (1 + strategy.commission + strategy.slippage)
                    
                    if cost <= cash:
                        cash -= cost
                        position = shares_to_buy
                        position_value = position * price
                        
                        trades.append({
                            'date': str(date.date()),
                            'type': 'buy',
                            'price': price,
                            'shares': shares_to_buy,
                            'value': cost,
                            'cash': cash,
                            'position_value': position_value
                        })
            
            # 处理卖出信号
            elif sell_signal and position > 0:
                # 全部卖出
                revenue = position * price * (1 - strategy.commission - strategy.slippage)
                cash += revenue
                
                trades.append({
                    'date': str(date.date()),
                    'type': 'sell',
                    'price': price,
                    'shares': position,
                    'value': revenue,
                    'cash': cash,
                    'position_value': 0
                })
                
                position = 0
                position_value = 0
            
            # 风险管理检查
            if position > 0:
                # 止损检查
                if len(trades) > 0:
                    last_buy_price = None
                    for trade in reversed(trades):
                        if trade['type'] == 'buy':
                            last_buy_price = trade['price']
                            break
                    
                    if last_buy_price:
                        loss_ratio = (last_buy_price - price) / last_buy_price
                        if loss_ratio >= strategy.risk_management.stop_loss:
                            # 执行止损
                            revenue = position * price * (1 - strategy.commission - strategy.slippage)
                            cash += revenue
                            
                            trades.append({
                                'date': str(date.date()),
                                'type': 'stop_loss',
                                'price': price,
                                'shares': position,
                                'value': revenue,
                                'cash': cash,
                                'position_value': 0
                            })
                            
                            position = 0
                            position_value = 0
                
                # 止盈检查
                if position > 0 and len(trades) > 0:
                    last_buy_price = None
                    for trade in reversed(trades):
                        if trade['type'] == 'buy':
                            last_buy_price = trade['price']
                            break
                    
                    if last_buy_price:
                        profit_ratio = (price - last_buy_price) / last_buy_price
                        if profit_ratio >= strategy.risk_management.take_profit:
                            # 执行止盈
                            revenue = position * price * (1 - strategy.commission - strategy.slippage)
                            cash += revenue
                            
                            trades.append({
                                'date': str(date.date()),
                                'type': 'take_profit',
                                'price': price,
                                'shares': position,
                                'value': revenue,
                                'cash': cash,
                                'position_value': 0
                            })
                            
                            position = 0
                            position_value = 0
            
            # 记录资金曲线
            total_value = cash + position_value
            equity_curve.append({
                'date': str(date.date()),
                'cash': cash,
                'position_value': position_value,
                'total_value': total_value,
                'return': (total_value - strategy.initial_capital) / strategy.initial_capital
            })
        
        return trades, equity_curve
    
    def _calculate_metrics(self, equity_curve: List[Dict], trades: List[Dict], 
                          initial_capital: float) -> Dict:
        """计算评估指标"""
        if not equity_curve or not trades:
            return {
                'total_return': 0.0,
                'annual_return': 0.0,
                'max_drawdown': 0.0,
                'sharpe_ratio': 0.0,
                'volatility': 0.0,
                'total_trades': 0,
                'win_rate': 0.0,
                'avg_win': 0.0,
                'avg_loss': 0.0,
                'profit_factor': 0.0
            }
        
        # 总收益率
        final_value = equity_curve[-1]['total_value']
        total_return = (final_value - initial_capital) / initial_capital
        
        # 年化收益率
        days = len(equity_curve)
        annual_return = (1 + total_return) ** (365 / days) - 1 if days > 0 else 0
        
        # 最大回撤
        values = [point['total_value'] for point in equity_curve]
        peak = values[0]
        max_drawdown = 0
        
        for value in values:
            if value > peak:
                peak = value
            drawdown = (peak - value) / peak
            if drawdown > max_drawdown:
                max_drawdown = drawdown
        
        # 波动率
        returns = []
        for i in range(1, len(equity_curve)):
            prev_value = equity_curve[i-1]['total_value']
            curr_value = equity_curve[i]['total_value']
            daily_return = (curr_value - prev_value) / prev_value
            returns.append(daily_return)
        
        volatility = np.std(returns) * np.sqrt(252) if returns else 0  # 年化波动率
        
        # 夏普比率
        avg_return = np.mean(returns) if returns else 0
        sharpe_ratio = (avg_return * 252) / volatility if volatility > 0 else 0
        
        # 交易统计
        total_trades = len([t for t in trades if t['type'] in ['buy', 'sell']])
        
        # 计算盈亏
        buy_trades = {i: t for i, t in enumerate(trades) if t['type'] == 'buy'}
        sell_trades = [t for t in trades if t['type'] in ['sell', 'stop_loss', 'take_profit']]
        
        wins = []
        losses = []
        
        for sell_trade in sell_trades:
            # 找到对应的买入交易
            buy_price = None
            for buy_trade in buy_trades.values():
                if buy_trade['date'] <= sell_trade['date']:
                    buy_price = buy_trade['price']
                    break
            
            if buy_price:
                profit = (sell_trade['price'] - buy_price) / buy_price
                if profit > 0:
                    wins.append(profit)
                else:
                    losses.append(abs(profit))
        
        win_rate = len(wins) / len(sell_trades) if sell_trades else 0
        avg_win = np.mean(wins) if wins else 0
        avg_loss = np.mean(losses) if losses else 0
        profit_factor = avg_win / avg_loss if avg_loss > 0 else 0
        
        return {
            'total_return': total_return,
            'annual_return': annual_return,
            'max_drawdown': max_drawdown,
            'sharpe_ratio': sharpe_ratio,
            'volatility': volatility,
            'total_trades': total_trades,
            'win_rate': win_rate,
            'avg_win': avg_win,
            'avg_loss': avg_loss,
            'profit_factor': profit_factor
        }


class StrategyEngine:
    """策略执行引擎"""
    
    def __init__(self):
        self.backtest_engine = BacktestEngine()
        self.active_strategies = []
        
    def add_strategy(self, strategy):
        """添加策略"""
        self.active_strategies.append(strategy)
        
    def get_signals(self, symbol: str, data: pd.DataFrame) -> Dict:
        """获取交易信号"""
        signals = []
        for strategy in self.active_strategies:
            try:
                signal = self._evaluate_strategy(strategy, symbol, data)
                if signal:
                    signals.append(signal)
            except Exception as e:
                logging.warning(f"策略 {strategy.name} 评估失败: {e}")
        
        return {
            'symbol': symbol,
            'signals': signals,
            'timestamp': datetime.now().isoformat()
        }
        
    def _evaluate_strategy(self, strategy, symbol: str, data: pd.DataFrame) -> Optional[Dict]:
        """评估单个策略"""
        if data.empty:
            return None
            
        latest_data = data.iloc[-1]
        
        # 简单的示例信号生成逻辑
        if len(data) >= 20:
            ma5 = data['close'].rolling(5).mean().iloc[-1]
            ma20 = data['close'].rolling(20).mean().iloc[-1]
            
            if ma5 > ma20 and data['close'].iloc[-1] > ma5:
                return {
                    'strategy': strategy.name,
                    'signal': 'buy',
                    'confidence': 0.7,
                    'reason': 'MA金叉且价格突破'
                }
            elif ma5 < ma20 and data['close'].iloc[-1] < ma5:
                return {
                    'strategy': strategy.name,
                    'signal': 'sell',
                    'confidence': 0.6,
                    'reason': 'MA死叉且价格跌破'
                }
        
        return None
        
    def run_backtest(self, strategy, symbol: str, data: pd.DataFrame, initial_capital: float = 100000) -> Dict:
        """运行回测"""
        return self.backtest_engine.run_backtest(strategy, symbol, data, initial_capital)