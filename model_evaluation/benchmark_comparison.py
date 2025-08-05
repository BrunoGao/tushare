#!/usr/bin/env python3
"""
基准比较框架
实现投资策略与多个市场基准的系统化比较分析
"""

import logging
import numpy as np
import pandas as pd
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Any, Tuple, Union
from dataclasses import dataclass, asdict
from pathlib import Path

from .risk_adjusted_metrics import RiskAdjustedMetrics, RiskMetrics

@dataclass
class BenchmarkData:
    """基准数据"""
    name: str
    symbol: str
    returns: pd.Series
    prices: pd.Series
    description: str
    category: str
    start_date: str
    end_date: str

@dataclass
class ComparisonResult:
    """比较结果"""
    strategy_name: str
    benchmark_name: str
    correlation: float
    beta: float
    alpha: float
    tracking_error: float
    information_ratio: float
    relative_return: float
    relative_volatility: float
    upside_capture: float
    downside_capture: float
    max_relative_drawdown: float
    batting_average: float
    win_loss_ratio: float
    strategy_metrics: RiskMetrics
    benchmark_metrics: RiskMetrics
    comparison_period: str

class BenchmarkComparison:
    """基准比较框架"""
    
    def __init__(self, 
                 benchmarks: List[str] = None,
                 data_source: str = "tushare"):
        """
        初始化基准比较框架
        
        Args:
            benchmarks: 基准列表
            data_source: 数据源
        """
        self.logger = logging.getLogger(__name__)
        self.data_source = data_source
        self.risk_calculator = RiskAdjustedMetrics()
        
        # 默认基准配置
        if benchmarks is None:
            benchmarks = [
                '000300.SH',  # 沪深300
                '000905.SH',  # 中证500  
                '399006.SZ',  # 创业板指
                '000001.SH',  # 上证指数
                '399001.SZ'   # 深证成指
            ]
        
        self.benchmark_configs = self._init_benchmark_configs(benchmarks)
        self.benchmark_data = {}
        
        self.logger.info(f"基准比较框架初始化完成 - 基准数量: {len(benchmarks)}")
    
    def _init_benchmark_configs(self, benchmarks: List[str]) -> Dict[str, Dict]:
        """初始化基准配置"""
        configs = {}
        
        benchmark_info = {
            '000300.SH': {
                'name': '沪深300指数',
                'description': '沪深两市最具代表性的300只股票',
                'category': '大盘指数'
            },
            '000905.SH': {
                'name': '中证500指数', 
                'description': '中等市值股票代表',
                'category': '中盘指数'
            },
            '399006.SZ': {
                'name': '创业板指数',
                'description': '创业板市场代表指数',
                'category': '成长指数'
            },
            '000001.SH': {
                'name': '上证指数',
                'description': '上海证券交易所综合指数',
                'category': '市场指数'
            },
            '399001.SZ': {
                'name': '深证成指',
                'description': '深圳证券交易所综合指数', 
                'category': '市场指数'
            },
            'SPY': {
                'name': 'S&P 500 ETF',
                'description': '美国标普500指数ETF',
                'category': '国际指数'
            },
            'QQQ': {
                'name': 'NASDAQ-100 ETF',
                'description': '美国纳斯达克100指数ETF',
                'category': '国际指数'
            }
        }
        
        for benchmark in benchmarks:
            if benchmark in benchmark_info:
                configs[benchmark] = benchmark_info[benchmark]
            else:
                configs[benchmark] = {
                    'name': benchmark,
                    'description': f'自定义基准 {benchmark}',
                    'category': '其他'
                }
        
        return configs
    
    def load_benchmark_data(self, 
                           start_date: str,
                           end_date: str,
                           data_provider: Any = None) -> bool:
        """
        加载基准数据
        
        Args:
            start_date: 开始日期
            end_date: 结束日期  
            data_provider: 数据提供者（如TuShareDataExtractor实例）
            
        Returns:
            是否成功加载
        """
        try:
            self.logger.info(f"开始加载基准数据: {start_date} ~ {end_date}")
            
            for symbol, config in self.benchmark_configs.items():
                try:
                    # 尝试从数据提供者获取数据
                    if data_provider and hasattr(data_provider, 'get_index_daily'):
                        try:
                            data = data_provider.get_index_daily(
                                ts_code=symbol,
                                start_date=start_date.replace('-', ''),
                                end_date=end_date.replace('-', '')
                            )
                            
                            if not data.empty:
                                data['trade_date'] = pd.to_datetime(data['trade_date'])
                                data = data.set_index('trade_date').sort_index()
                                
                                prices = data['close']
                                returns = prices.pct_change().dropna()
                                
                                self.benchmark_data[symbol] = BenchmarkData(
                                    name=config['name'],
                                    symbol=symbol,
                                    returns=returns,
                                    prices=prices,
                                    description=config['description'],
                                    category=config['category'],
                                    start_date=str(returns.index[0].date()),
                                    end_date=str(returns.index[-1].date())
                                )
                                
                                self.logger.info(f"成功加载基准数据: {config['name']}")
                                continue
                        except Exception as e:
                            self.logger.warning(f"从数据提供者获取 {symbol} 失败: {e}")
                    
                    # 如果数据提供者获取失败，生成模拟数据
                    mock_data = self._generate_mock_benchmark_data(
                        symbol, start_date, end_date, config
                    )
                    self.benchmark_data[symbol] = mock_data
                    self.logger.info(f"使用模拟数据: {config['name']}")
                    
                except Exception as e:
                    self.logger.error(f"加载基准 {symbol} 失败: {e}")
                    continue
            
            self.logger.info(f"基准数据加载完成 - 成功加载 {len(self.benchmark_data)} 个基准")
            return len(self.benchmark_data) > 0
            
        except Exception as e:
            self.logger.error(f"加载基准数据失败: {e}")
            return False
    
    def _generate_mock_benchmark_data(self, 
                                    symbol: str,
                                    start_date: str, 
                                    end_date: str,
                                    config: Dict) -> BenchmarkData:
        """生成模拟基准数据"""
        try:
            # 创建日期范围
            date_range = pd.date_range(start=start_date, end=end_date, freq='D')
            
            # 根据基准类型设置不同的参数
            if '300' in symbol:  # 沪深300
                base_return = 0.0008
                volatility = 0.018
                initial_price = 4000
            elif '500' in symbol:  # 中证500
                base_return = 0.0006
                volatility = 0.022
                initial_price = 6000
            elif '399006' in symbol:  # 创业板
                base_return = 0.0010
                volatility = 0.025
                initial_price = 2500
            elif 'SPY' in symbol:  # S&P 500
                base_return = 0.0007
                volatility = 0.016
                initial_price = 400
            else:  # 其他指数
                base_return = 0.0005
                volatility = 0.020
                initial_price = 3000
            
            # 生成收益率序列
            np.random.seed(hash(symbol) % 2**32)  # 确保可重复性
            returns = np.random.normal(base_return, volatility, len(date_range))
            
            # 添加一些市场特征
            # 1. 趋势性 - 长期上涨趋势
            trend = np.linspace(0, base_return * 0.5, len(date_range))
            returns += trend
            
            # 2. 波动聚集性 - GARCH效应
            for i in range(1, len(returns)):
                if abs(returns[i-1]) > volatility:
                    returns[i] *= 1.2  # 增大波动
            
            # 3. 周末效应模拟（排除周末）
            business_days = pd.bdate_range(start=start_date, end=end_date)
            if len(business_days) < len(returns):
                returns = returns[:len(business_days)]
            elif len(business_days) > len(returns):
                additional_returns = np.random.normal(base_return, volatility, 
                                                    len(business_days) - len(returns))
                returns = np.concatenate([returns, additional_returns])
            
            # 创建价格序列
            prices = pd.Series(index=business_days, data=initial_price)
            for i, ret in enumerate(returns):
                if i > 0:
                    prices.iloc[i] = prices.iloc[i-1] * (1 + ret)
            
            # 创建收益率序列
            returns_series = prices.pct_change().dropna()
            
            return BenchmarkData(
                name=config['name'],
                symbol=symbol,
                returns=returns_series,
                prices=prices,
                description=config['description'],
                category=config['category'],
                start_date=str(business_days[0].date()),
                end_date=str(business_days[-1].date())
            )
            
        except Exception as e:
            self.logger.error(f"生成模拟基准数据失败 {symbol}: {e}")
            # 返回最简单的模拟数据
            dates = pd.date_range(start=start_date, end=end_date, freq='B')[:100]
            prices = pd.Series(index=dates, data=100 * np.cumprod(1 + np.random.normal(0.001, 0.02, len(dates))))
            returns = prices.pct_change().dropna()
            
            return BenchmarkData(
                name=config['name'],
                symbol=symbol,
                returns=returns,
                prices=prices,
                description=config['description'],
                category=config['category'],
                start_date=str(dates[0].date()),
                end_date=str(dates[-1].date())
            )
    
    def compare_strategy(self, 
                        strategy_returns: Union[pd.Series, np.ndarray],
                        strategy_name: str = "Investment Strategy",
                        strategy_prices: Union[pd.Series, np.ndarray] = None,
                        benchmarks: List[str] = None) -> Dict[str, ComparisonResult]:
        """
        比较策略与基准的性能
        
        Args:
            strategy_returns: 策略收益率序列
            strategy_name: 策略名称
            strategy_prices: 策略价格序列
            benchmarks: 要比较的基准列表
            
        Returns:
            比较结果字典
        """
        try:
            self.logger.info(f"开始比较策略: {strategy_name}")
            
            # 数据预处理
            if isinstance(strategy_returns, np.ndarray):
                strategy_returns = pd.Series(strategy_returns)
            
            if benchmarks is None:
                benchmarks = list(self.benchmark_data.keys())
            
            comparison_results = {}
            
            for benchmark_symbol in benchmarks:
                if benchmark_symbol not in self.benchmark_data:
                    self.logger.warning(f"基准 {benchmark_symbol} 数据未加载")
                    continue
                
                try:
                    benchmark_data = self.benchmark_data[benchmark_symbol]
                    result = self._compare_with_benchmark(
                        strategy_returns, 
                        strategy_name,
                        benchmark_data,
                        strategy_prices
                    )
                    comparison_results[benchmark_symbol] = result
                    
                except Exception as e:
                    self.logger.error(f"与基准 {benchmark_symbol} 比较失败: {e}")
                    continue
            
            self.logger.info(f"策略比较完成 - 成功比较 {len(comparison_results)} 个基准")
            return comparison_results
            
        except Exception as e:
            self.logger.error(f"策略比较失败: {e}")
            return {}
    
    def _compare_with_benchmark(self, 
                              strategy_returns: pd.Series,
                              strategy_name: str,
                              benchmark_data: BenchmarkData,
                              strategy_prices: pd.Series = None) -> ComparisonResult:
        """与单个基准进行比较"""
        try:
            benchmark_returns = benchmark_data.returns
            
            # 对齐数据
            aligned_data = self._align_data(strategy_returns, benchmark_returns)
            if aligned_data is None:
                raise ValueError("无法对齐策略和基准数据")
            
            strategy_aligned, benchmark_aligned = aligned_data
            
            # 计算基础指标
            correlation = strategy_aligned.corr(benchmark_aligned)
            
            # 计算Beta和Alpha
            beta = self.risk_calculator.beta(strategy_aligned, benchmark_aligned)
            alpha = self.risk_calculator.jensen_alpha(strategy_aligned, benchmark_aligned)
            
            # 计算跟踪误差和信息比率
            tracking_error = self.risk_calculator.tracking_error(strategy_aligned, benchmark_aligned)
            information_ratio = self.risk_calculator.information_ratio(strategy_aligned, benchmark_aligned)
            
            # 计算相对收益和波动
            strategy_total_return = (1 + strategy_aligned).prod() - 1
            benchmark_total_return = (1 + benchmark_aligned).prod() - 1
            relative_return = strategy_total_return - benchmark_total_return
            
            strategy_volatility = strategy_aligned.std() * np.sqrt(252)
            benchmark_volatility = benchmark_aligned.std() * np.sqrt(252)
            relative_volatility = strategy_volatility - benchmark_volatility
            
            # 计算捕获比率
            upside_capture = self.risk_calculator.upside_capture_ratio(strategy_aligned, benchmark_aligned)
            downside_capture = self.risk_calculator.downside_capture_ratio(strategy_aligned, benchmark_aligned)
            
            # 计算相对回撤
            max_relative_drawdown = self._calculate_relative_drawdown(strategy_aligned, benchmark_aligned)
            
            # 计算击败基准的频率
            batting_average = self._calculate_batting_average(strategy_aligned, benchmark_aligned)
            
            # 计算盈亏比
            win_loss_ratio = self._calculate_win_loss_ratio(strategy_aligned, benchmark_aligned)
            
            # 计算风险指标
            strategy_metrics = self.risk_calculator.calculate_all_metrics(
                strategy_aligned, benchmark_aligned, benchmark_aligned, strategy_prices
            )
            
            benchmark_metrics = self.risk_calculator.calculate_all_metrics(
                benchmark_aligned
            )
            
            return ComparisonResult(
                strategy_name=strategy_name,
                benchmark_name=benchmark_data.name,
                correlation=correlation,
                beta=beta,
                alpha=alpha,
                tracking_error=tracking_error,
                information_ratio=information_ratio,
                relative_return=relative_return,
                relative_volatility=relative_volatility,
                upside_capture=upside_capture,
                downside_capture=downside_capture,
                max_relative_drawdown=max_relative_drawdown,
                batting_average=batting_average,
                win_loss_ratio=win_loss_ratio,
                strategy_metrics=strategy_metrics,
                benchmark_metrics=benchmark_metrics,
                comparison_period=f"{strategy_aligned.index[0]} to {strategy_aligned.index[-1]}"
            )
            
        except Exception as e:
            self.logger.error(f"基准比较计算失败: {e}")
            raise e
    
    def _align_data(self, strategy_returns: pd.Series, benchmark_returns: pd.Series) -> Optional[Tuple[pd.Series, pd.Series]]:
        """对齐策略和基准数据"""
        try:
            # 如果策略数据没有时间索引，创建一个
            if not isinstance(strategy_returns.index, pd.DatetimeIndex):
                if isinstance(benchmark_returns.index, pd.DatetimeIndex):
                    # 使用基准的时间索引
                    min_length = min(len(strategy_returns), len(benchmark_returns))
                    strategy_returns = pd.Series(
                        strategy_returns.values[-min_length:],
                        index=benchmark_returns.index[-min_length:]
                    )
                else:
                    # 都没有时间索引，创建简单的整数索引
                    min_length = min(len(strategy_returns), len(benchmark_returns))
                    common_index = range(min_length)
                    strategy_returns = pd.Series(strategy_returns.values[-min_length:], index=common_index)
                    benchmark_returns = pd.Series(benchmark_returns.values[-min_length:], index=common_index)
                    return strategy_returns, benchmark_returns
            
            # 找到共同的时间范围
            common_index = strategy_returns.index.intersection(benchmark_returns.index)
            
            if len(common_index) == 0:
                # 如果没有共同时间点，使用重叠的数据长度
                min_length = min(len(strategy_returns), len(benchmark_returns))
                if min_length == 0:
                    return None
                
                strategy_aligned = strategy_returns.iloc[-min_length:]
                benchmark_aligned = benchmark_returns.iloc[-min_length:]
                
                # 创建共同索引
                common_index = range(min_length)
                strategy_aligned.index = common_index
                benchmark_aligned.index = common_index
                
                return strategy_aligned, benchmark_aligned
            
            # 使用共同索引对齐数据
            strategy_aligned = strategy_returns.loc[common_index]
            benchmark_aligned = benchmark_returns.loc[common_index]
            
            # 移除NaN值
            valid_mask = ~(strategy_aligned.isna() | benchmark_aligned.isna())
            strategy_aligned = strategy_aligned[valid_mask]
            benchmark_aligned = benchmark_aligned[valid_mask]
            
            if len(strategy_aligned) == 0:
                return None
            
            return strategy_aligned, benchmark_aligned
            
        except Exception as e:
            self.logger.error(f"数据对齐失败: {e}")
            return None
    
    def _calculate_relative_drawdown(self, strategy_returns: pd.Series, benchmark_returns: pd.Series) -> float:
        """计算相对回撤"""
        try:
            relative_returns = strategy_returns - benchmark_returns
            relative_cumulative = (1 + relative_returns).cumprod()
            
            rolling_max = relative_cumulative.expanding().max()
            relative_drawdown = (relative_cumulative - rolling_max) / rolling_max
            
            return relative_drawdown.min()
            
        except Exception as e:
            self.logger.error(f"计算相对回撤失败: {e}")
            return np.nan
    
    def _calculate_batting_average(self, strategy_returns: pd.Series, benchmark_returns: pd.Series) -> float:
        """计算击败基准的频率"""
        try:
            outperformance = strategy_returns > benchmark_returns
            return outperformance.mean()
            
        except Exception as e:
            self.logger.error(f"计算击败频率失败: {e}")
            return np.nan
    
    def _calculate_win_loss_ratio(self, strategy_returns: pd.Series, benchmark_returns: pd.Series) -> float:
        """计算相对收益的盈亏比"""
        try:
            relative_returns = strategy_returns - benchmark_returns
            
            wins = relative_returns[relative_returns > 0]
            losses = relative_returns[relative_returns < 0]
            
            if len(losses) == 0:
                return np.inf if len(wins) > 0 else 0.0
            
            avg_win = wins.mean() if len(wins) > 0 else 0.0
            avg_loss = abs(losses.mean())
            
            return avg_win / avg_loss if avg_loss > 0 else 0.0
            
        except Exception as e:
            self.logger.error(f"计算盈亏比失败: {e}")
            return np.nan
    
    def rank_strategies(self, comparison_results: Dict[str, Dict[str, ComparisonResult]], 
                       ranking_criteria: List[str] = None) -> pd.DataFrame:
        """
        对策略进行排名
        
        Args:
            comparison_results: 比较结果 {策略名: {基准名: ComparisonResult}}
            ranking_criteria: 排名标准
            
        Returns:
            排名结果DataFrame
        """
        try:
            if ranking_criteria is None:
                ranking_criteria = ['information_ratio', 'alpha', 'relative_return', 'batting_average']
            
            ranking_data = []
            
            for strategy_name, strategy_results in comparison_results.items():
                for benchmark_name, result in strategy_results.items():
                    row = {
                        'strategy': strategy_name,
                        'benchmark': benchmark_name,
                        'information_ratio': result.information_ratio,
                        'alpha': result.alpha,
                        'relative_return': result.relative_return,
                        'batting_average': result.batting_average,
                        'sharpe_ratio': result.strategy_metrics.sharpe_ratio,
                        'correlation': result.correlation,
                        'tracking_error': result.tracking_error,
                        'max_relative_drawdown': result.max_relative_drawdown
                    }
                    ranking_data.append(row)
            
            if not ranking_data:
                return pd.DataFrame()
            
            df = pd.DataFrame(ranking_data)
            
            # 计算综合得分
            df['composite_score'] = 0.0
            weights = {
                'information_ratio': 0.3,
                'alpha': 0.25,
                'relative_return': 0.25,
                'batting_average': 0.2
            }
            
            for criterion, weight in weights.items():
                if criterion in df.columns:
                    # 标准化得分 (0-1)
                    criterion_scores = df[criterion].fillna(0)
                    if criterion_scores.std() > 0:
                        normalized_scores = (criterion_scores - criterion_scores.min()) / (criterion_scores.max() - criterion_scores.min())
                        df['composite_score'] += weight * normalized_scores
            
            # 按综合得分排序
            df = df.sort_values('composite_score', ascending=False)
            
            return df
            
        except Exception as e:
            self.logger.error(f"策略排名失败: {e}")
            return pd.DataFrame()
    
    def generate_comparison_report(self, 
                                 comparison_results: Dict[str, ComparisonResult],
                                 strategy_name: str,
                                 save_path: str = None) -> str:
        """
        生成基准比较报告
        
        Args:
            comparison_results: 比较结果
            strategy_name: 策略名称
            save_path: 保存路径
            
        Returns:
            报告文件路径
        """
        try:
            if save_path is None:
                save_path = f"benchmark_comparison_{strategy_name}_{datetime.now().strftime('%Y%m%d_%H%M%S')}.txt"
            
            report_lines = []
            report_lines.append("=" * 80)
            report_lines.append(f"基准比较分析报告 - {strategy_name}")
            report_lines.append("=" * 80)
            report_lines.append(f"报告生成时间: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
            report_lines.append("")
            
            # 摘要统计
            report_lines.append("比较摘要:")
            report_lines.append("-" * 40)
            report_lines.append(f"比较基准数量: {len(comparison_results)}")
            
            if comparison_results:
                avg_alpha = np.mean([r.alpha for r in comparison_results.values() if not np.isnan(r.alpha)])
                avg_info_ratio = np.mean([r.information_ratio for r in comparison_results.values() if not np.isnan(r.information_ratio)])
                avg_batting = np.mean([r.batting_average for r in comparison_results.values() if not np.isnan(r.batting_average)])
                
                report_lines.append(f"平均阿尔法: {avg_alpha:.4f}")
                report_lines.append(f"平均信息比率: {avg_info_ratio:.4f}")
                report_lines.append(f"平均击败频率: {avg_batting:.2%}")
            
            report_lines.append("")
            
            # 详细比较结果
            for benchmark_symbol, result in comparison_results.items():
                report_lines.append(f"vs {result.benchmark_name} ({benchmark_symbol}):")
                report_lines.append("-" * 60)
                report_lines.append(f"比较期间: {result.comparison_period}")
                report_lines.append("")
                
                # 相对性能指标
                report_lines.append("相对性能指标:")
                report_lines.append(f"  相对收益: {result.relative_return:.2%}")
                report_lines.append(f"  阿尔法: {result.alpha:.4f}")
                report_lines.append(f"  贝塔: {result.beta:.4f}")
                report_lines.append(f"  信息比率: {result.information_ratio:.4f}")
                report_lines.append(f"  跟踪误差: {result.tracking_error:.4f}")
                report_lines.append("")
                
                # 相关性和捕获比率
                report_lines.append("相关性分析:")
                report_lines.append(f"  相关系数: {result.correlation:.4f}")
                report_lines.append(f"  上行捕获: {result.upside_capture:.4f}")
                report_lines.append(f"  下行捕获: {result.downside_capture:.4f}")
                report_lines.append("")
                
                # 风险指标
                report_lines.append("风险指标:")
                report_lines.append(f"  相对波动率: {result.relative_volatility:.4f}")
                report_lines.append(f"  最大相对回撤: {result.max_relative_drawdown:.4f}")
                report_lines.append("")
                
                # 交易统计
                report_lines.append("交易统计:")
                report_lines.append(f"  击败基准频率: {result.batting_average:.2%}")
                report_lines.append(f"  相对盈亏比: {result.win_loss_ratio:.4f}")
                report_lines.append("")
                
                # 策略vs基准核心指标对比
                report_lines.append("核心指标对比:")
                report_lines.append(f"  夏普比率: 策略({result.strategy_metrics.sharpe_ratio:.4f}) vs 基准({result.benchmark_metrics.sharpe_ratio:.4f})")
                report_lines.append(f"  最大回撤: 策略({result.strategy_metrics.max_drawdown:.4f}) vs 基准({result.benchmark_metrics.max_drawdown:.4f})")
                report_lines.append(f"  胜率: 策略({result.strategy_metrics.win_rate:.2%}) vs 基准({result.benchmark_metrics.win_rate:.2%})")
                report_lines.append("")
                report_lines.append("=" * 60)
                report_lines.append("")
            
            # 写入文件
            with open(save_path, 'w', encoding='utf-8') as f:
                f.write('\n'.join(report_lines))
            
            self.logger.info(f"基准比较报告已生成: {save_path}")
            return save_path
            
        except Exception as e:
            self.logger.error(f"生成比较报告失败: {e}")
            return ""
    
    def get_available_benchmarks(self) -> List[Dict[str, str]]:
        """
        获取可用基准列表
        
        Returns:
            基准信息列表
        """
        benchmarks = []
        for symbol, config in self.benchmark_configs.items():
            benchmark_info = {
                'symbol': symbol,
                'name': config['name'],
                'description': config['description'],
                'category': config['category'],
                'available': symbol in self.benchmark_data
            }
            
            if symbol in self.benchmark_data:
                benchmark_data = self.benchmark_data[symbol]
                benchmark_info.update({
                    'start_date': benchmark_data.start_date,
                    'end_date': benchmark_data.end_date,
                    'data_points': len(benchmark_data.returns)
                })
            
            benchmarks.append(benchmark_info)
        
        return benchmarks
    
    def add_custom_benchmark(self, 
                           symbol: str,
                           name: str,
                           returns: Union[pd.Series, np.ndarray],
                           prices: Union[pd.Series, np.ndarray] = None,
                           description: str = "",
                           category: str = "自定义") -> bool:
        """
        添加自定义基准
        
        Args:
            symbol: 基准代码
            name: 基准名称
            returns: 收益率序列
            prices: 价格序列
            description: 描述
            category: 类别
            
        Returns:
            是否成功添加
        """
        try:
            if isinstance(returns, np.ndarray):
                returns = pd.Series(returns)
            
            if prices is None:
                # 从收益率计算价格
                prices = (1 + returns).cumprod() * 100
            elif isinstance(prices, np.ndarray):
                prices = pd.Series(prices)
            
            benchmark_data = BenchmarkData(
                name=name,
                symbol=symbol,
                returns=returns,
                prices=prices,
                description=description,
                category=category,
                start_date=str(returns.index[0].date()) if hasattr(returns, 'index') else 'N/A',
                end_date=str(returns.index[-1].date()) if hasattr(returns, 'index') else 'N/A'
            )
            
            self.benchmark_data[symbol] = benchmark_data
            self.benchmark_configs[symbol] = {
                'name': name,
                'description': description,
                'category': category
            }
            
            self.logger.info(f"成功添加自定义基准: {name}")
            return True
            
        except Exception as e:
            self.logger.error(f"添加自定义基准失败: {e}")
            return False