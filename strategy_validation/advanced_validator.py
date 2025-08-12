#!/usr/bin/env python3
"""
高级策略验证器
实现完整的策略回测、评估和分析功能
"""

import sys
import os
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import pandas as pd
import numpy as np
import tushare as ts
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Tuple, Any, Callable
import logging
import json
from dataclasses import dataclass, asdict
import uuid
import warnings
warnings.filterwarnings('ignore')

# 导入项目组件
try:
    import config
    from database.db_manager import DatabaseManager
    from utils.db_helper import get_db_connection
except ImportError as e:
    print(f"导入项目组件失败: {e}")

@dataclass
class StrategyConfig:
    """策略配置"""
    name: str
    description: str
    parameters: Dict[str, Any]
    risk_params: Dict[str, float]
    benchmark: str = "000300.SH"  # 沪深300指数

@dataclass 
class ValidationResult:
    """验证结果"""
    strategy_name: str
    test_period: str
    total_return: float
    annualized_return: float
    volatility: float
    sharpe_ratio: float
    max_drawdown: float
    calmar_ratio: float
    win_rate: float
    profit_loss_ratio: float
    total_trades: int
    benchmark_return: float
    excess_return: float
    information_ratio: float
    detailed_trades: List[Dict]
    monthly_returns: List[float]
    drawdown_series: List[float]
    validation_id: str = ""
    created_at: str = ""

class TechnicalIndicators:
    """技术指标计算器"""
    
    @staticmethod
    def rsi(prices: pd.Series, period: int = 14) -> pd.Series:
        """相对强弱指数"""
        delta = prices.diff()
        gain = delta.where(delta > 0, 0).rolling(window=period).mean()
        loss = (-delta.where(delta < 0, 0)).rolling(window=period).mean()
        rs = gain / loss
        return 100 - (100 / (1 + rs))
    
    @staticmethod
    def macd(prices: pd.Series, fast: int = 12, slow: int = 26, signal: int = 9) -> Tuple[pd.Series, pd.Series, pd.Series]:
        """MACD指标"""
        ema_fast = prices.ewm(span=fast).mean()
        ema_slow = prices.ewm(span=slow).mean()
        macd_line = ema_fast - ema_slow
        signal_line = macd_line.ewm(span=signal).mean()
        histogram = macd_line - signal_line
        return macd_line, signal_line, histogram
    
    @staticmethod
    def bollinger_bands(prices: pd.Series, window: int = 20, num_std: float = 2) -> Tuple[pd.Series, pd.Series, pd.Series]:
        """布林带"""
        sma = prices.rolling(window=window).mean()
        std = prices.rolling(window=window).std()
        upper_band = sma + (std * num_std)
        lower_band = sma - (std * num_std)
        return upper_band, sma, lower_band

class AdvancedStrategyValidator:
    """高级策略验证器"""
    
    def __init__(self, config: StrategyConfig):
        self.config = config
        self.logger = self._setup_logger()
        self.db_manager = None
        self._init_data_source()
        self._init_database()
    
    def _setup_logger(self) -> logging.Logger:
        """设置日志系统"""
        logger = logging.getLogger(f"validator_{self.config.name.replace(' ', '_')}")
        
        # 避免重复添加handler
        if not logger.handlers:
            handler = logging.StreamHandler()
            formatter = logging.Formatter(
                '%(asctime)s - %(name)s - %(levelname)s - %(message)s'
            )
            handler.setFormatter(formatter)
            logger.addHandler(handler)
            logger.setLevel(logging.INFO)
        
        return logger
    
    def _init_data_source(self):
        """初始化数据源"""
        try:
            if hasattr(config, 'TS_TOKEN'):
                ts.set_token(config.TS_TOKEN)
                self.pro = ts.pro_api()
                self.logger.info("TuShare数据源初始化成功")
            else:
                self.logger.warning("未找到TuShare配置，将使用模拟数据")
                self.pro = None
        except Exception as e:
            self.logger.error(f"数据源初始化失败: {e}")
            self.pro = None
    
    def _init_database(self):
        """初始化数据库连接"""
        try:
            self.db_manager = DatabaseManager()
            self._create_validation_tables()
            self.logger.info("数据库连接初始化成功")
        except Exception as e:
            self.logger.warning(f"数据库初始化失败: {e}")
            self.db_manager = None
    
    def _create_validation_tables(self):
        """创建验证结果表"""
        if not self.db_manager:
            return
        
        create_table_sql = """
        CREATE TABLE IF NOT EXISTS strategy_validations (
            id VARCHAR(36) PRIMARY KEY,
            strategy_name VARCHAR(100) NOT NULL,
            config_json TEXT,
            test_period VARCHAR(50),
            total_return DECIMAL(10,6),
            annualized_return DECIMAL(10,6),
            volatility DECIMAL(10,6),
            sharpe_ratio DECIMAL(10,4),
            max_drawdown DECIMAL(10,6),
            calmar_ratio DECIMAL(10,4),
            win_rate DECIMAL(10,6),
            profit_loss_ratio DECIMAL(10,4),
            total_trades INT,
            benchmark_return DECIMAL(10,6),
            excess_return DECIMAL(10,6),
            information_ratio DECIMAL(10,4),
            detailed_results LONGTEXT,
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            INDEX idx_strategy_name (strategy_name),
            INDEX idx_created_at (created_at)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
        """
        
        try:
            self.db_manager.execute_sql(create_table_sql)
            self.logger.info("验证结果表创建成功")
        except Exception as e:
            self.logger.error(f"创建验证结果表失败: {e}")
    
    def comprehensive_validation(self, 
                                stock_codes: List[str],
                                start_date: str,
                                end_date: str,
                                strategy_func: Callable,
                                save_results: bool = True) -> ValidationResult:
        """
        综合策略验证
        
        Args:
            stock_codes: 测试股票代码列表
            start_date: 开始日期 (YYYY-MM-DD)
            end_date: 结束日期 (YYYY-MM-DD)
            strategy_func: 策略函数，返回交易信号
            save_results: 是否保存结果到数据库
            
        Returns:
            ValidationResult: 详细验证结果
        """
        validation_id = str(uuid.uuid4())
        self.logger.info(f"开始验证策略: {self.config.name} [ID: {validation_id}]")
        self.logger.info(f"测试期间: {start_date} 至 {end_date}")
        self.logger.info(f"测试股票: {len(stock_codes)} 只")
        
        try:
            # 1. 数据获取与预处理
            all_data = self._prepare_data(stock_codes, start_date, end_date)
            if not all_data:
                raise ValueError("无法获取有效的历史数据")
            
            # 2. 策略信号生成
            all_signals = {}
            for stock_code, data in all_data.items():
                try:
                    signals = strategy_func(data, self.config.parameters)
                    all_signals[stock_code] = signals
                    self.logger.info(f"{stock_code}: 生成 {len(signals)} 个信号")
                except Exception as e:
                    self.logger.error(f"生成 {stock_code} 信号失败: {e}")
                    all_signals[stock_code] = []
            
            # 3. 交易模拟执行
            portfolio_performance = self._simulate_trading(all_data, all_signals)
            
            # 4. 基准数据获取
            benchmark_performance = self._get_benchmark_performance(start_date, end_date)
            
            # 5. 绩效指标计算
            metrics = self._calculate_comprehensive_metrics(
                portfolio_performance, 
                benchmark_performance
            )
            
            # 6. 生成验证结果
            result = self._generate_validation_result(
                metrics, 
                portfolio_performance,
                start_date,
                end_date,
                validation_id
            )
            
            # 7. 保存结果到数据库
            if save_results and self.db_manager:
                self._save_validation_result(result)
            
            self.logger.info(f"验证完成 - 年化收益: {result.annualized_return:.2%}, "
                           f"夏普比率: {result.sharpe_ratio:.2f}, "
                           f"最大回撤: {result.max_drawdown:.2%}")
            
            return result
            
        except Exception as e:
            self.logger.error(f"策略验证失败: {e}")
            raise
    
    def _prepare_data(self, stock_codes: List[str], start_date: str, end_date: str) -> Dict[str, pd.DataFrame]:
        """准备历史数据"""
        all_data = {}
        
        # 扩展开始日期以获取足够的技术指标计算数据
        extended_start = (datetime.strptime(start_date, '%Y-%m-%d') - timedelta(days=120)).strftime('%Y-%m-%d')
        
        for i, stock_code in enumerate(stock_codes, 1):
            self.logger.info(f"获取数据 [{i}/{len(stock_codes)}]: {stock_code}")
            
            try:
                if self.pro:
                    # 使用TuShare获取真实数据
                    data = self._fetch_real_data(stock_code, extended_start, end_date)
                else:
                    # 使用模拟数据
                    data = self._generate_mock_data(stock_code, extended_start, end_date)
                
                if data.empty:
                    self.logger.warning(f"{stock_code}: 无有效数据")
                    continue
                
                # 数据预处理
                data = data.sort_values('trade_date').reset_index(drop=True)
                
                # 添加技术指标
                data = self._add_technical_indicators(data)
                
                # 过滤到实际测试期间
                data = data[data['trade_date'] >= start_date].reset_index(drop=True)
                
                # 数据质量验证
                if self._validate_data_quality(data):
                    all_data[stock_code] = data
                    self.logger.info(f"{stock_code}: 获取 {len(data)} 条有效数据")
                else:
                    self.logger.warning(f"{stock_code}: 数据质量不合格")
                
            except Exception as e:
                self.logger.error(f"获取 {stock_code} 数据失败: {e}")
                continue
        
        return all_data
    
    def _fetch_real_data(self, stock_code: str, start_date: str, end_date: str) -> pd.DataFrame:
        """获取真实历史数据"""
        try:
            data = self.pro.daily(
                ts_code=stock_code,
                start_date=start_date.replace('-', ''),
                end_date=end_date.replace('-', '')
            )
            
            if data.empty:
                return pd.DataFrame()
            
            data['trade_date'] = pd.to_datetime(data['trade_date'])
            return data
            
        except Exception as e:
            self.logger.error(f"获取真实数据失败 {stock_code}: {e}")
            return pd.DataFrame()
    
    def _generate_mock_data(self, stock_code: str, start_date: str, end_date: str) -> pd.DataFrame:
        """生成模拟数据（用于测试）"""
        try:
            date_range = pd.date_range(start=start_date, end=end_date, freq='D')
            # 过滤掉周末
            date_range = date_range[date_range.weekday < 5]
            
            n_days = len(date_range)
            if n_days == 0:
                return pd.DataFrame()
            
            # 生成随机价格数据
            np.random.seed(hash(stock_code) % 2**32)  # 基于股票代码的固定种子
            
            base_price = 10.0
            returns = np.random.normal(0.0005, 0.02, n_days)  # 日收益率
            prices = base_price * np.cumprod(1 + returns)
            
            # 生成OHLC数据
            close_prices = prices
            high_prices = close_prices * (1 + np.abs(np.random.normal(0, 0.01, n_days)))
            low_prices = close_prices * (1 - np.abs(np.random.normal(0, 0.01, n_days)))
            open_prices = np.roll(close_prices, 1)
            open_prices[0] = base_price
            
            # 生成成交量
            volumes = np.random.lognormal(10, 0.5, n_days) * 100
            
            data = pd.DataFrame({
                'ts_code': stock_code,
                'trade_date': date_range,
                'open': open_prices,
                'high': high_prices,
                'low': low_prices,
                'close': close_prices,
                'pre_close': np.roll(close_prices, 1),
                'change': close_prices - np.roll(close_prices, 1),
                'pct_chg': returns * 100,
                'vol': volumes,
                'amount': volumes * close_prices
            })
            
            data.iloc[0, data.columns.get_loc('pre_close')] = base_price
            data.iloc[0, data.columns.get_loc('change')] = 0
            data.iloc[0, data.columns.get_loc('pct_chg')] = 0
            
            return data
            
        except Exception as e:
            self.logger.error(f"生成模拟数据失败 {stock_code}: {e}")
            return pd.DataFrame()
    
    def _add_technical_indicators(self, data: pd.DataFrame) -> pd.DataFrame:
        """添加技术指标"""
        try:
            # RSI
            data['rsi'] = TechnicalIndicators.rsi(data['close'])
            
            # MACD
            macd, signal, histogram = TechnicalIndicators.macd(data['close'])
            data['macd'] = macd
            data['macd_signal'] = signal
            data['macd_histogram'] = histogram
            
            # 布林带
            upper, middle, lower = TechnicalIndicators.bollinger_bands(data['close'])
            data['bb_upper'] = upper
            data['bb_middle'] = middle
            data['bb_lower'] = lower
            
            # 移动平均
            data['ma_5'] = data['close'].rolling(5).mean()
            data['ma_20'] = data['close'].rolling(20).mean()
            data['ma_60'] = data['close'].rolling(60).mean()
            
            # 成交量指标
            data['volume_ma'] = data['vol'].rolling(20).mean()
            data['volume_ratio'] = data['vol'] / data['volume_ma']
            
            # 收益率
            data['returns'] = data['close'].pct_change()
            
            return data
            
        except Exception as e:
            self.logger.error(f"计算技术指标失败: {e}")
            return data
    
    def _validate_data_quality(self, data: pd.DataFrame) -> bool:
        """验证数据质量"""
        if data.empty or len(data) < 60:  # 至少60天数据
            return False
        
        # 检查必需列
        required_columns = ['trade_date', 'open', 'high', 'low', 'close', 'vol']
        if not all(col in data.columns for col in required_columns):
            return False
        
        # 检查价格逻辑
        if (data['high'] < data['low']).any():
            return False
        
        if (data['close'] > data['high']).any() or (data['close'] < data['low']).any():
            return False
        
        # 检查缺失值比例
        missing_ratio = data[required_columns].isnull().sum() / len(data)
        if missing_ratio.max() > 0.1:  # 缺失值不超过10%
            return False
        
        return True
    
    def _simulate_trading(self, all_data: Dict[str, pd.DataFrame], all_signals: Dict[str, List]) -> Dict:
        """模拟交易执行"""
        portfolio = {
            'equity_curve': [],
            'trades': [],
            'positions': {},
            'cash': 100000,  # 初始资金10万
            'total_value': 100000
        }
        
        # 获取所有交易日
        all_dates = set()
        for data in all_data.values():
            all_dates.update(data['trade_date'])
        all_dates = sorted(list(all_dates))
        
        # 预处理所有信号，按日期索引
        signals_by_date = {}
        for stock_code, signals in all_signals.items():
            for signal in signals:
                signal_date = signal['date']
                if signal_date not in signals_by_date:
                    signals_by_date[signal_date] = []
                signal['stock_code'] = stock_code
                signals_by_date[signal_date].append(signal)
        
        # 逐日模拟交易
        for date in all_dates:
            # 更新持仓市值
            self._update_positions_value(portfolio, all_data, date)
            
            # 处理当日信号
            if date in signals_by_date:
                for signal in signals_by_date[date]:
                    self._execute_signal(portfolio, signal, all_data)
            
            # 记录每日净值
            total_value = portfolio['cash']
            for stock_code, position in portfolio['positions'].items():
                current_price = self._get_price_on_date(all_data[stock_code], date)
                if current_price:
                    total_value += position['shares'] * current_price
            
            portfolio['equity_curve'].append({
                'date': date,
                'total_value': total_value,
                'cash': portfolio['cash'],
                'positions_value': total_value - portfolio['cash']
            })
        
        return portfolio
    
    def _update_positions_value(self, portfolio: Dict, all_data: Dict, date):
        """更新持仓市值"""
        for stock_code in list(portfolio['positions'].keys()):
            if stock_code not in all_data:
                continue
            
            current_price = self._get_price_on_date(all_data[stock_code], date)
            if current_price:
                portfolio['positions'][stock_code]['current_price'] = current_price
    
    def _execute_signal(self, portfolio: Dict, signal: Dict, all_data: Dict):
        """执行交易信号"""
        stock_code = signal['stock_code']
        signal_type = signal['type']
        price = signal['price']
        date = signal['date']
        
        if signal_type == 'buy':
            self._execute_buy(portfolio, stock_code, price, date)
        elif signal_type == 'sell':
            self._execute_sell(portfolio, stock_code, price, date)
    
    def _execute_buy(self, portfolio: Dict, stock_code: str, price: float, date):
        """执行买入"""
        max_position_size = self.config.risk_params.get('max_position_size', 0.1)
        
        if stock_code not in portfolio['positions']:
            # 计算买入金额（考虑最大仓位限制）
            available_cash = portfolio['cash']
            max_investment = portfolio['cash'] * max_position_size / (1 - len(portfolio['positions']) * max_position_size + max_position_size)
            trade_amount = min(max_investment, available_cash * 0.95)  # 保留5%现金
            
            if trade_amount > 1000:  # 最小交易金额
                shares = int(trade_amount / price / 100) * 100  # 整百股
                
                if shares > 0:
                    cost = shares * price * 1.001  # 考虑交易成本0.1%
                    
                    if cost <= portfolio['cash']:
                        portfolio['cash'] -= cost
                        
                        portfolio['positions'][stock_code] = {
                            'shares': shares,
                            'avg_price': price,
                            'entry_date': date,
                            'current_price': price
                        }
                        
                        self.logger.debug(f"买入 {stock_code}: {shares}股 @ {price:.2f}")
    
    def _execute_sell(self, portfolio: Dict, stock_code: str, price: float, date):
        """执行卖出"""
        if stock_code in portfolio['positions']:
            position = portfolio['positions'][stock_code]
            shares = position['shares']
            proceeds = shares * price * 0.999  # 考虑交易成本0.1%
            
            portfolio['cash'] += proceeds
            
            # 计算收益
            cost_basis = shares * position['avg_price']
            pnl = proceeds - cost_basis
            pnl_pct = pnl / cost_basis if cost_basis > 0 else 0
            holding_days = (date - position['entry_date']).days
            
            portfolio['trades'].append({
                'stock_code': stock_code,
                'entry_date': position['entry_date'],
                'exit_date': date,
                'entry_price': position['avg_price'],
                'exit_price': price,
                'shares': shares,
                'pnl': pnl,
                'return': pnl_pct,
                'holding_days': holding_days
            })
            
            # 清除持仓
            del portfolio['positions'][stock_code]
            
            self.logger.debug(f"卖出 {stock_code}: {shares}股 @ {price:.2f}, 收益: {pnl_pct:.2%}")
    
    def _get_price_on_date(self, data: pd.DataFrame, date) -> Optional[float]:
        """获取指定日期的价格"""
        try:
            row = data[data['trade_date'] == date]
            if not row.empty:
                return float(row.iloc[0]['close'])
            return None
        except:
            return None
    
    def _get_benchmark_performance(self, start_date: str, end_date: str) -> pd.DataFrame:
        """获取基准表现"""
        try:
            if self.pro and self.config.benchmark:
                benchmark_data = self.pro.daily(
                    ts_code=self.config.benchmark,
                    start_date=start_date.replace('-', ''),
                    end_date=end_date.replace('-', '')
                )
                
                if not benchmark_data.empty:
                    benchmark_data['trade_date'] = pd.to_datetime(benchmark_data['trade_date'])
                    return benchmark_data.sort_values('trade_date')
            
            # 返回空DataFrame如果无法获取基准
            return pd.DataFrame()
            
        except Exception as e:
            self.logger.warning(f"获取基准数据失败: {e}")
            return pd.DataFrame()
    
    def _calculate_comprehensive_metrics(self, portfolio: Dict, benchmark: pd.DataFrame) -> Dict:
        """计算综合绩效指标"""
        try:
            equity_curve = pd.DataFrame(portfolio['equity_curve'])
            
            if equity_curve.empty:
                return self._empty_metrics()
            
            equity_curve['returns'] = equity_curve['total_value'].pct_change().fillna(0)
            
            # 基础收益指标
            total_return = (equity_curve['total_value'].iloc[-1] / equity_curve['total_value'].iloc[0]) - 1
            trading_days = len(equity_curve)
            annualized_return = (1 + total_return) ** (252 / trading_days) - 1 if trading_days > 0 else 0
            
            # 风险指标
            returns = equity_curve['returns'].dropna()
            volatility = returns.std() * np.sqrt(252) if len(returns) > 1 else 0
            
            # 最大回撤
            cummax = equity_curve['total_value'].cummax()
            drawdown = (equity_curve['total_value'] - cummax) / cummax
            max_drawdown = abs(drawdown.min())
            
            # 夏普比率
            risk_free_rate = 0.03  # 假设无风险收益率3%
            excess_returns = returns - risk_free_rate/252
            sharpe_ratio = excess_returns.mean() / excess_returns.std() * np.sqrt(252) if excess_returns.std() != 0 else 0
            
            # 卡尔马比率
            calmar_ratio = annualized_return / max_drawdown if max_drawdown != 0 else 0
            
            # 交易指标
            trades = portfolio['trades']
            profitable_trades = [t for t in trades if t['return'] > 0]
            win_rate = len(profitable_trades) / len(trades) if trades else 0
            
            if profitable_trades and any(t['return'] < 0 for t in trades):
                avg_profit = np.mean([t['return'] for t in profitable_trades])
                avg_loss = np.mean([t['return'] for t in trades if t['return'] < 0])
                profit_loss_ratio = abs(avg_profit / avg_loss)
            else:
                profit_loss_ratio = 0
            
            # 基准比较
            benchmark_return = 0
            excess_return = total_return
            information_ratio = 0
            
            if not benchmark.empty and len(benchmark) > 1:
                benchmark_return = (benchmark['close'].iloc[-1] / benchmark['close'].iloc[0]) - 1
                excess_return = total_return - benchmark_return
                
                # 信息比率
                benchmark_returns = benchmark['close'].pct_change().dropna()
                if len(benchmark_returns) == len(returns):
                    tracking_error = (returns - benchmark_returns).std() * np.sqrt(252)
                    information_ratio = excess_return / tracking_error if tracking_error != 0 else 0
            
            # 月度收益
            monthly_returns = self._calculate_monthly_returns(equity_curve)
            
            return {
                'total_return': total_return,
                'annualized_return': annualized_return,
                'volatility': volatility,
                'sharpe_ratio': sharpe_ratio,
                'max_drawdown': max_drawdown,
                'calmar_ratio': calmar_ratio,
                'win_rate': win_rate,
                'profit_loss_ratio': profit_loss_ratio,
                'total_trades': len(trades),
                'benchmark_return': benchmark_return,
                'excess_return': excess_return,
                'information_ratio': information_ratio,
                'equity_curve': equity_curve,
                'drawdown_series': drawdown.tolist(),
                'monthly_returns': monthly_returns
            }
            
        except Exception as e:
            self.logger.error(f"计算绩效指标失败: {e}")
            return self._empty_metrics()
    
    def _calculate_monthly_returns(self, equity_curve: pd.DataFrame) -> List[float]:
        """计算月度收益率"""
        try:
            equity_curve['month'] = pd.to_datetime(equity_curve['date']).dt.to_period('M')
            monthly_values = equity_curve.groupby('month')['total_value'].last()
            monthly_returns = monthly_values.pct_change().dropna()
            return monthly_returns.tolist()
        except Exception as e:
            self.logger.error(f"计算月度收益失败: {e}")
            return []
    
    def _empty_metrics(self) -> Dict:
        """返回空指标"""
        return {
            'total_return': 0,
            'annualized_return': 0,
            'volatility': 0,
            'sharpe_ratio': 0,
            'max_drawdown': 0,
            'calmar_ratio': 0,
            'win_rate': 0,
            'profit_loss_ratio': 0,
            'total_trades': 0,
            'benchmark_return': 0,
            'excess_return': 0,
            'information_ratio': 0,
            'equity_curve': pd.DataFrame(),
            'drawdown_series': [],
            'monthly_returns': []
        }
    
    def _generate_validation_result(self, metrics: Dict, portfolio: Dict, 
                                  start_date: str, end_date: str, validation_id: str) -> ValidationResult:
        """生成验证结果"""
        return ValidationResult(
            validation_id=validation_id,
            strategy_name=self.config.name,
            test_period=f"{start_date} 至 {end_date}",
            total_return=metrics['total_return'],
            annualized_return=metrics['annualized_return'],
            volatility=metrics['volatility'],
            sharpe_ratio=metrics['sharpe_ratio'],
            max_drawdown=metrics['max_drawdown'],
            calmar_ratio=metrics['calmar_ratio'],
            win_rate=metrics['win_rate'],
            profit_loss_ratio=metrics['profit_loss_ratio'],
            total_trades=metrics['total_trades'],
            benchmark_return=metrics['benchmark_return'],
            excess_return=metrics['excess_return'],
            information_ratio=metrics['information_ratio'],
            detailed_trades=portfolio['trades'],
            monthly_returns=metrics['monthly_returns'],
            drawdown_series=metrics['drawdown_series'],
            created_at=datetime.now().isoformat()
        )
    
    def _save_validation_result(self, result: ValidationResult):
        """保存验证结果到数据库"""
        try:
            # 转换详细结果，确保所有数据都可以JSON序列化
            detailed_results = {
                'config': self._serialize_config(asdict(self.config)),
                'detailed_trades': self._serialize_trades(result.detailed_trades),
                'monthly_returns': result.monthly_returns,
                'drawdown_series': result.drawdown_series
            }
            
            insert_sql = """
            INSERT INTO strategy_validations (
                id, strategy_name, config_json, test_period, total_return,
                annualized_return, volatility, sharpe_ratio, max_drawdown,
                calmar_ratio, win_rate, profit_loss_ratio, total_trades,
                benchmark_return, excess_return, information_ratio, detailed_results
            ) VALUES (
                %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s
            )
            """
            
            params = (
                result.validation_id,
                result.strategy_name,
                json.dumps(asdict(self.config), ensure_ascii=False),
                result.test_period,
                result.total_return,
                result.annualized_return,
                result.volatility,
                result.sharpe_ratio,
                result.max_drawdown,
                result.calmar_ratio,
                result.win_rate,
                result.profit_loss_ratio,
                result.total_trades,
                result.benchmark_return,
                result.excess_return,
                result.information_ratio,
                json.dumps(detailed_results, ensure_ascii=False)
            )
            
            self.db_manager.execute_sql(insert_sql, params)
            self.logger.info(f"验证结果已保存到数据库: {result.validation_id}")
            
        except Exception as e:
            self.logger.error(f"保存验证结果失败: {e}")
    
    def _serialize_config(self, config_dict):
        """序列化配置字典"""
        serialized = {}
        for key, value in config_dict.items():
            if isinstance(value, dict):
                serialized[key] = self._serialize_config(value)
            elif hasattr(value, 'isoformat'):  # datetime对象
                serialized[key] = value.isoformat()
            elif isinstance(value, (bool, int, float, str, type(None))):
                serialized[key] = value
            else:
                serialized[key] = str(value)
        return serialized
    
    def _serialize_trades(self, trades):
        """序列化交易记录"""
        serialized_trades = []
        for trade in trades:
            serialized_trade = {}
            for key, value in trade.items():
                if hasattr(value, 'isoformat'):  # datetime对象
                    serialized_trade[key] = value.isoformat()
                elif isinstance(value, (pd.Timestamp)):
                    serialized_trade[key] = value.isoformat()
                elif isinstance(value, (bool, int, float, str, type(None))):
                    serialized_trade[key] = value
                else:
                    serialized_trade[key] = str(value)
            serialized_trades.append(serialized_trade)
        return serialized_trades

# 策略实现示例
def rsi_mean_reversion_strategy(data: pd.DataFrame, params: Dict) -> List[Dict]:
    """RSI均值回归策略"""
    signals = []
    position = None
    
    rsi_oversold = params.get('rsi_oversold', 30)
    rsi_overbought = params.get('rsi_overbought', 70)
    max_holding_days = params.get('max_holding_days', 30)
    
    for i in range(1, len(data)):
        if pd.isna(data.iloc[i]['rsi']) or pd.isna(data.iloc[i-1]['rsi']):
            continue
            
        current_rsi = data.iloc[i]['rsi']
        prev_rsi = data.iloc[i-1]['rsi']
        
        # 买入信号：RSI从超卖区域反弹
        if position is None and prev_rsi < rsi_oversold and current_rsi > rsi_oversold:
            signals.append({
                'type': 'buy',
                'date': data.iloc[i]['trade_date'],
                'price': data.iloc[i]['close'],
                'rsi': current_rsi,
                'index': i
            })
            position = 'long'
        
        # 卖出信号：RSI进入超买区域或持有超过最大天数
        elif position == 'long' and (
            current_rsi > rsi_overbought or 
            (len(signals) > 0 and i - signals[-1]['index'] > max_holding_days)
        ):
            signals.append({
                'type': 'sell',
                'date': data.iloc[i]['trade_date'],
                'price': data.iloc[i]['close'],
                'rsi': current_rsi,
                'index': i
            })
            position = None
    
    return signals

def macd_trend_strategy(data: pd.DataFrame, params: Dict) -> List[Dict]:
    """MACD趋势跟踪策略"""
    signals = []
    position = None
    
    for i in range(1, len(data)):
        if pd.isna(data.iloc[i]['macd']) or pd.isna(data.iloc[i-1]['macd']):
            continue
            
        current_macd = data.iloc[i]['macd']
        current_signal = data.iloc[i]['macd_signal']
        prev_macd = data.iloc[i-1]['macd']
        prev_signal = data.iloc[i-1]['macd_signal']
        
        # 金叉买入信号
        if position is None and prev_macd <= prev_signal and current_macd > current_signal:
            signals.append({
                'type': 'buy',
                'date': data.iloc[i]['trade_date'],
                'price': data.iloc[i]['close'],
                'macd': current_macd,
                'index': i
            })
            position = 'long'
        
        # 死叉卖出信号
        elif position == 'long' and prev_macd >= prev_signal and current_macd < current_signal:
            signals.append({
                'type': 'sell',
                'date': data.iloc[i]['trade_date'],
                'price': data.iloc[i]['close'],
                'macd': current_macd,
                'index': i
            })
            position = None
    
    return signals

# 使用示例
def run_validation_example():
    """运行验证示例"""
    print("🚀 启动高级策略验证演示")
    
    # 配置策略
    strategy_config = StrategyConfig(
        name="RSI均值回归策略",
        description="基于RSI指标的超买超卖交易策略，优化版本",
        parameters={
            'rsi_oversold': 25,
            'rsi_overbought': 75,
            'max_holding_days': 25
        },
        risk_params={
            'max_position_size': 0.15,
            'stop_loss': 0.08,
            'take_profit': 0.15
        },
        benchmark="000300.SH"
    )
    
    # 初始化验证器
    validator = AdvancedStrategyValidator(strategy_config)
    
    # 测试股票池（选择不同行业代表性股票）
    test_stocks = [
        '000001.SZ',  # 平安银行 - 金融
        '000002.SZ',  # 万科A - 房地产
        '600036.SH',  # 招商银行 - 银行
        '600519.SH',  # 贵州茅台 - 消费
        '000858.SZ',  # 五粮液 - 消费
        '002415.SZ',  # 海康威视 - 科技
    ]
    
    print(f"📋 测试股票池: {test_stocks}")
    
    try:
        # 执行验证
        result = validator.comprehensive_validation(
            stock_codes=test_stocks,
            start_date='2023-01-01',
            end_date='2024-12-31',
            strategy_func=rsi_mean_reversion_strategy,
            save_results=True
        )
        
        # 打印详细结果
        print("\n" + "="*80)
        print(f"📊 策略验证报告: {result.strategy_name}")
        print("="*80)
        print(f"🗓️  测试期间: {result.test_period}")
        print(f"🆔 验证ID: {result.validation_id}")
        print("-"*80)
        print("💰 收益指标:")
        print(f"    总收益率: {result.total_return:>10.2%}")
        print(f"    年化收益率: {result.annualized_return:>8.2%}")
        print(f"    超额收益率: {result.excess_return:>8.2%}")
        print(f"    基准收益率: {result.benchmark_return:>8.2%}")
        print("-"*80)
        print("⚖️  风险指标:")
        print(f"    年化波动率: {result.volatility:>8.2%}")
        print(f"    最大回撤: {result.max_drawdown:>10.2%}")
        print(f"    夏普比率: {result.sharpe_ratio:>10.2f}")
        print(f"    卡尔马比率: {result.calmar_ratio:>8.2f}")
        print(f"    信息比率: {result.information_ratio:>8.2f}")
        print("-"*80)
        print("📈 交易统计:")
        print(f"    总交易次数: {result.total_trades:>8d}")
        print(f"    胜率: {result.win_rate:>14.2%}")
        print(f"    盈亏比: {result.profit_loss_ratio:>12.2f}")
        print("="*80)
        
        # 生成评级
        rating = _rate_strategy_performance(result)
        print(f"🏆 综合评级: {rating}")
        
        # 保存详细报告
        report_file = f"validation_report_{result.validation_id}.json"
        with open(report_file, 'w', encoding='utf-8') as f:
            result_dict = asdict(result)
            # 转换不可序列化的对象
            for key, value in result_dict.items():
                if isinstance(value, (pd.Timestamp, datetime)):
                    result_dict[key] = str(value)
            json.dump(result_dict, f, ensure_ascii=False, indent=2, default=str)
        
        print(f"💾 详细报告已保存: {report_file}")
        
        return result
        
    except Exception as e:
        print(f"❌ 验证过程失败: {e}")
        return None

def _rate_strategy_performance(result: ValidationResult) -> str:
    """策略表现评级"""
    score = 0
    
    # 收益评分 (40%)
    if result.annualized_return >= 0.20:
        score += 40
    elif result.annualized_return >= 0.15:
        score += 32
    elif result.annualized_return >= 0.10:
        score += 24
    elif result.annualized_return >= 0.05:
        score += 16
    elif result.annualized_return >= 0:
        score += 8
    
    # 风险控制评分 (30%)
    if result.max_drawdown <= 0.10:
        score += 30
    elif result.max_drawdown <= 0.15:
        score += 24
    elif result.max_drawdown <= 0.20:
        score += 18
    elif result.max_drawdown <= 0.25:
        score += 12
    elif result.max_drawdown <= 0.30:
        score += 6
    
    # 夏普比率评分 (20%)
    if result.sharpe_ratio >= 2.0:
        score += 20
    elif result.sharpe_ratio >= 1.5:
        score += 16
    elif result.sharpe_ratio >= 1.0:
        score += 12
    elif result.sharpe_ratio >= 0.5:
        score += 8
    elif result.sharpe_ratio >= 0:
        score += 4
    
    # 交易效率评分 (10%)
    if result.win_rate >= 0.65 and result.profit_loss_ratio >= 1.5:
        score += 10
    elif result.win_rate >= 0.60 and result.profit_loss_ratio >= 1.2:
        score += 8
    elif result.win_rate >= 0.55:
        score += 6
    elif result.win_rate >= 0.50:
        score += 4
    elif result.win_rate >= 0.45:
        score += 2
    
    # 评级映射
    if score >= 85:
        return "A+ (优秀)"
    elif score >= 75:
        return "A (良好)"
    elif score >= 65:
        return "B+ (一般)"
    elif score >= 55:
        return "B (及格)"
    elif score >= 45:
        return "C (偏弱)"
    else:
        return "D (不佳)"

if __name__ == "__main__":
    result = run_validation_example()