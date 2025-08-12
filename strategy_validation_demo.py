#!/usr/bin/env python3
"""
策略验证演示脚本
展示如何用实际交易行情来评估和验证投资策略
"""

import sys
import os
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

import pandas as pd
import numpy as np
import tushare as ts
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Tuple
import logging
import json

# 导入项目组件
try:
    from recommendation_backtest.recommendation_tracker import RecommendationTracker, StockRecommendation
    from model_evaluation.walk_forward_analysis import WalkForwardAnalysis
    from database.db_manager import DatabaseManager
    import config
except ImportError as e:
    print(f"导入模块失败: {e}")
    exit(1)

class StrategyValidator:
    """策略验证器"""
    
    def __init__(self):
        """初始化"""
        self.logger = self._setup_logger()
        self.db_manager = DatabaseManager()
        
        # 初始化TuShare
        ts.set_token(config.TS_TOKEN)
        self.pro = ts.pro_api()
        
        # 初始化推荐跟踪器
        self.tracker = RecommendationTracker()
        
        self.logger.info("策略验证器初始化完成")
    
    def _setup_logger(self) -> logging.Logger:
        """设置日志"""
        logging.basicConfig(
            level=logging.INFO,
            format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
        )
        return logging.getLogger(__name__)
    
    def fetch_real_market_data(self, stock_code: str, start_date: str, end_date: str) -> pd.DataFrame:
        """获取真实市场数据"""
        try:
            self.logger.info(f"获取 {stock_code} 从 {start_date} 到 {end_date} 的行情数据")
            
            # 获取日线数据
            daily_data = self.pro.daily(
                ts_code=stock_code,
                start_date=start_date.replace('-', ''),
                end_date=end_date.replace('-', '')
            )
            
            if daily_data.empty:
                self.logger.warning(f"未获取到 {stock_code} 的数据")
                return pd.DataFrame()
            
            # 数据预处理
            daily_data['trade_date'] = pd.to_datetime(daily_data['trade_date'])
            daily_data = daily_data.sort_values('trade_date').reset_index(drop=True)
            
            # 计算技术指标
            daily_data['returns'] = daily_data['close'].pct_change()
            daily_data['ma_5'] = daily_data['close'].rolling(5).mean()
            daily_data['ma_20'] = daily_data['close'].rolling(20).mean()
            daily_data['volatility'] = daily_data['returns'].rolling(20).std()
            
            self.logger.info(f"成功获取 {len(daily_data)} 条数据")
            return daily_data
            
        except Exception as e:
            self.logger.error(f"获取市场数据失败: {e}")
            return pd.DataFrame()
    
    def validate_rsi_strategy(self, stock_codes: List[str], test_period_months: int = 6) -> Dict:
        """验证RSI均值回归策略"""
        self.logger.info("=== 开始验证RSI均值回归策略 ===")
        
        # 设置时间窗口
        end_date = datetime.now()
        start_date = end_date - timedelta(days=test_period_months * 30 + 60)  # 额外60天用于指标计算
        
        start_str = start_date.strftime('%Y-%m-%d')
        end_str = end_date.strftime('%Y-%m-%d')
        
        strategy_results = {
            'strategy_name': 'RSI均值回归',
            'test_period': f"{start_str} 到 {end_str}",
            'stocks_tested': len(stock_codes),
            'signals_generated': 0,
            'profitable_trades': 0,
            'total_return': 0.0,
            'win_rate': 0.0,
            'avg_holding_days': 0,
            'max_drawdown': 0.0,
            'sharpe_ratio': 0.0,
            'detailed_results': []
        }
        
        all_returns = []
        all_holding_days = []
        profitable_count = 0
        
        for stock_code in stock_codes:
            try:
                # 获取历史数据
                data = self.fetch_real_market_data(stock_code, start_str, end_str)
                if data.empty:
                    continue
                
                # 计算RSI指标
                data = self._calculate_rsi(data)
                
                # 生成交易信号
                signals = self._generate_rsi_signals(data)
                
                # 计算策略收益
                stock_results = self._backtest_signals(data, signals, stock_code)
                
                if stock_results:
                    strategy_results['detailed_results'].append(stock_results)
                    strategy_results['signals_generated'] += len(signals)
                    
                    # 累积结果
                    if stock_results['returns']:
                        all_returns.extend(stock_results['returns'])
                        all_holding_days.extend(stock_results['holding_days'])
                        profitable_count += len([r for r in stock_results['returns'] if r > 0])
                
            except Exception as e:
                self.logger.error(f"处理股票 {stock_code} 失败: {e}")
                continue
        
        # 计算总体指标
        if all_returns:
            strategy_results['total_return'] = sum(all_returns)
            strategy_results['profitable_trades'] = profitable_count
            strategy_results['win_rate'] = profitable_count / len(all_returns) if all_returns else 0
            strategy_results['avg_holding_days'] = np.mean(all_holding_days) if all_holding_days else 0
            strategy_results['sharpe_ratio'] = self._calculate_sharpe_ratio(all_returns)
            strategy_results['max_drawdown'] = self._calculate_max_drawdown(all_returns)
        
        self._print_strategy_summary(strategy_results)
        return strategy_results
    
    def _calculate_rsi(self, data: pd.DataFrame, period: int = 14) -> pd.DataFrame:
        """计算RSI指标"""
        delta = data['close'].diff()
        gain = (delta.where(delta > 0, 0)).rolling(window=period).mean()
        loss = (-delta.where(delta < 0, 0)).rolling(window=period).mean()
        rs = gain / loss
        data['rsi'] = 100 - (100 / (1 + rs))
        return data
    
    def _generate_rsi_signals(self, data: pd.DataFrame) -> List[Dict]:
        """基于RSI生成交易信号"""
        signals = []
        position = None
        
        for i in range(1, len(data)):
            current_rsi = data.iloc[i]['rsi']
            prev_rsi = data.iloc[i-1]['rsi']
            
            # 买入信号：RSI从超卖区域(30以下)反弹
            if position is None and prev_rsi < 30 and current_rsi > 30:
                signals.append({
                    'type': 'buy',
                    'date': data.iloc[i]['trade_date'],
                    'price': data.iloc[i]['close'],
                    'rsi': current_rsi,
                    'index': i
                })
                position = 'long'
            
            # 卖出信号：RSI进入超买区域(70以上)或持有超过20天
            elif position == 'long' and (current_rsi > 70 or 
                    (len(signals) > 0 and (i - signals[-1]['index']) > 20)):
                signals.append({
                    'type': 'sell',
                    'date': data.iloc[i]['trade_date'],
                    'price': data.iloc[i]['close'],
                    'rsi': current_rsi,
                    'index': i
                })
                position = None
        
        return signals
    
    def _backtest_signals(self, data: pd.DataFrame, signals: List[Dict], stock_code: str) -> Dict:
        """回测交易信号"""
        if len(signals) < 2:
            return {}
        
        trades = []
        returns = []
        holding_days = []
        
        # 配对买卖信号
        for i in range(0, len(signals) - 1, 2):
            if i + 1 >= len(signals):
                break
                
            buy_signal = signals[i]
            sell_signal = signals[i + 1]
            
            if buy_signal['type'] == 'buy' and sell_signal['type'] == 'sell':
                # 计算收益
                buy_price = buy_signal['price']
                sell_price = sell_signal['price']
                trade_return = (sell_price - buy_price) / buy_price
                
                # 计算持有天数
                holding_period = (sell_signal['date'] - buy_signal['date']).days
                
                trades.append({
                    'entry_date': buy_signal['date'],
                    'exit_date': sell_signal['date'],
                    'entry_price': buy_price,
                    'exit_price': sell_price,
                    'return': trade_return,
                    'holding_days': holding_period,
                    'entry_rsi': buy_signal['rsi'],
                    'exit_rsi': sell_signal['rsi']
                })
                
                returns.append(trade_return)
                holding_days.append(holding_period)
        
        return {
            'stock_code': stock_code,
            'trades': trades,
            'returns': returns,
            'holding_days': holding_days,
            'total_trades': len(trades),
            'profitable_trades': len([r for r in returns if r > 0]),
            'avg_return': np.mean(returns) if returns else 0,
            'total_return': sum(returns) if returns else 0,
            'win_rate': len([r for r in returns if r > 0]) / len(returns) if returns else 0
        }
    
    def _calculate_sharpe_ratio(self, returns: List[float], risk_free_rate: float = 0.03) -> float:
        """计算夏普比率"""
        if not returns or len(returns) < 2:
            return 0.0
        
        returns_array = np.array(returns)
        excess_returns = returns_array - risk_free_rate / 252  # 日风险免费收益率
        
        if np.std(excess_returns) == 0:
            return 0.0
        
        return np.mean(excess_returns) / np.std(excess_returns) * np.sqrt(252)
    
    def _calculate_max_drawdown(self, returns: List[float]) -> float:
        """计算最大回撤"""
        if not returns:
            return 0.0
        
        cumulative_returns = np.cumprod(1 + np.array(returns))
        peak = np.maximum.accumulate(cumulative_returns)
        drawdown = (peak - cumulative_returns) / peak
        return np.max(drawdown)
    
    def _print_strategy_summary(self, results: Dict):
        """打印策略验证总结"""
        print("\n" + "="*60)
        print(f"📊 {results['strategy_name']} 策略验证报告")
        print("="*60)
        print(f"🗓️  测试期间: {results['test_period']}")
        print(f"📈 测试股票数: {results['stocks_tested']}")
        print(f"⚡ 生成信号数: {results['signals_generated']}")
        print(f"💰 盈利交易数: {results['profitable_trades']}")
        print(f"📊 总收益率: {results['total_return']:.2%}")
        print(f"🎯 胜率: {results['win_rate']:.2%}")
        print(f"⏱️  平均持有天数: {results['avg_holding_days']:.1f}")
        print(f"📉 最大回撤: {results['max_drawdown']:.2%}")
        print(f"⚖️  夏普比率: {results['sharpe_ratio']:.2f}")
        print("="*60)
        
        # 详细结果
        if results['detailed_results']:
            print("\n📋 个股详细结果:")
            for stock_result in results['detailed_results'][:5]:  # 显示前5个
                print(f"  {stock_result['stock_code']}: "
                      f"交易{stock_result['total_trades']}次, "
                      f"胜率{stock_result['win_rate']:.1%}, "
                      f"总收益{stock_result['total_return']:.2%}")

def main():
    """主函数"""
    print("🚀 启动策略验证演示")
    
    # 初始化验证器
    validator = StrategyValidator()
    
    # 选择测试股票（一些活跃的股票代码）
    test_stocks = [
        '000001.SZ',  # 平安银行
        '000002.SZ',  # 万科A
        '600036.SH',  # 招商银行
        '600519.SH',  # 贵州茅台
        '000858.SZ',  # 五粮液
    ]
    
    print(f"📋 将测试以下股票: {', '.join(test_stocks)}")
    
    try:
        # 验证RSI策略
        rsi_results = validator.validate_rsi_strategy(test_stocks, test_period_months=6)
        
        # 保存结果
        results_file = f"strategy_validation_results_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
        with open(results_file, 'w', encoding='utf-8') as f:
            json.dump(rsi_results, f, ensure_ascii=False, indent=2, default=str)
        
        print(f"\n💾 详细结果已保存到: {results_file}")
        
        # 提供策略改进建议
        print("\n💡 策略优化建议:")
        if rsi_results['win_rate'] < 0.5:
            print("  - 胜率偏低，建议优化进场条件")
            print("  - 考虑结合其他技术指标过滤信号")
        if rsi_results['avg_holding_days'] > 30:
            print("  - 持有时间较长，可考虑设置止损条件")
        if rsi_results['sharpe_ratio'] < 1.0:
            print("  - 风险调整收益偏低，需要优化风险管理")
        
        return rsi_results
        
    except Exception as e:
        print(f"❌ 策略验证过程中出错: {e}")
        return None

if __name__ == "__main__":
    results = main()