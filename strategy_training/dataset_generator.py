#!/usr/bin/env python3
"""
策略驱动训练数据生成器
Strategy-Driven Training Dataset Generator
"""

import json
import os
import sys
import pandas as pd
import numpy as np
import logging
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Tuple, Any, Generator
from dataclasses import dataclass, asdict
import uuid

# 添加项目路径
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from llm.tushare_data_extractor import TuShareDataExtractor
from strategy.market_mainstream_strategies import MAINSTREAM_STRATEGIES, get_all_strategy_ids
from strategy.strategy_models import StrategyType

@dataclass
class TrainingSample:
    """训练样本数据结构"""
    id: str
    strategy_id: str
    strategy_type: str
    stock_code: str
    stock_name: str
    date: str
    input_text: str
    output_text: str
    signal_type: str
    confidence: float
    metadata: Dict[str, Any]

class StrategyDatasetGenerator:
    """策略训练数据集生成器"""
    
    def __init__(self, tushare_token: str = None):
        self.logger = logging.getLogger(__name__)
        self.data_extractor = TuShareDataExtractor(tushare_token)
        self.output_dir = "strategy_datasets"
        self.samples_cache = {}
        
        # 确保输出目录存在
        os.makedirs(self.output_dir, exist_ok=True)
        
        # 输入输出模板
        self.input_template = """股票分析报告 - {stock_name}({stock_code})

交易日期: {date}
当前价格: {current_price}元
价格变动: {price_change}元 ({pct_change:+.2f}%)

技术指标:
- 5日均线: {ma5:.2f}元
- 20日均线: {ma20:.2f}元  
- RSI(14): {rsi:.2f}
- MACD: {macd:.4f}
- 成交量: {volume:,.0f}股
- 成交量比: {volume_ratio:.2f}

基本面数据:
- 市值: {market_cap}
- PE倍数: {pe_ratio}
- PB倍数: {pb_ratio}

请基于{strategy_name}策略进行分析并给出投资建议。"""

        self.output_template = """【{signal_type_label}】{strategy_name}策略分析

基于{strategy_name}策略的深度分析，{stock_name}({stock_code})当前表现如下:

{analysis_details}

操作建议: {recommendation}
目标价位: {target_price}
止损建议: {stop_loss}
持有期建议: {holding_period}
风险评级: {risk_level}

注意: 投资有风险，请根据个人风险承受能力谨慎决策。"""
    
    def generate_strategy_dataset(self, strategy_id: str, num_stocks: int = 50, 
                                time_range: int = 365) -> List[TrainingSample]:
        """为单个策略生成训练数据集"""
        self.logger.info(f"开始生成策略 {strategy_id} 的训练数据集")
        
        if strategy_id not in MAINSTREAM_STRATEGIES:
            raise ValueError(f"未知策略ID: {strategy_id}")
        
        strategy = MAINSTREAM_STRATEGIES[strategy_id]
        samples = []
        
        try:
            # 获取股票列表
            stock_list = self._get_active_stocks(num_stocks)
            self.logger.info(f"获取到 {len(stock_list)} 只活跃股票")
            
            # 为每只股票生成训练样本
            for i, stock_code in enumerate(stock_list):
                try:
                    self.logger.info(f"处理股票 {i+1}/{len(stock_list)}: {stock_code}")
                    
                    # 获取历史数据
                    stock_samples = self._generate_stock_samples(
                        stock_code, strategy_id, strategy, time_range
                    )
                    
                    samples.extend(stock_samples)
                    self.logger.info(f"股票 {stock_code} 生成 {len(stock_samples)} 个样本")
                    
                except Exception as e:
                    self.logger.error(f"处理股票 {stock_code} 失败: {e}")
                    continue
            
            # 保存数据集
            self._save_dataset(strategy_id, samples)
            
            self.logger.info(f"策略 {strategy_id} 训练数据集生成完成，共 {len(samples)} 个样本")
            return samples
            
        except Exception as e:
            self.logger.error(f"生成策略 {strategy_id} 数据集失败: {e}")
            raise
    
    def _get_active_stocks(self, num_stocks: int) -> List[str]:
        """获取活跃股票列表"""
        try:
            # 获取股票基本信息
            stock_basic = self.data_extractor.tushare_api.stock_basic(
                exchange='', list_status='L', fields='ts_code,name,market'
            )
            
            if stock_basic is None or stock_basic.empty:
                raise Exception("无法获取股票基本信息")
            
            # 优先选择主板股票
            main_board = stock_basic[stock_basic['market'].isin(['主板', 'MAIN'])]
            if len(main_board) < num_stocks:
                selected_stocks = stock_basic.head(num_stocks)
            else:
                selected_stocks = main_board.head(num_stocks)
            
            return selected_stocks['ts_code'].tolist()
            
        except Exception as e:
            self.logger.error(f"获取活跃股票失败: {e}")
            # 返回一些默认股票代码
            return [
                '000001.SZ', '000002.SZ', '000006.SZ', '000007.SZ', '000009.SZ',
                '000010.SZ', '000011.SZ', '000012.SZ', '000014.SZ', '000016.SZ'
            ][:num_stocks]
    
    def _generate_stock_samples(self, stock_code: str, strategy_id: str, 
                              strategy: Dict, time_range: int) -> List[TrainingSample]:
        """为单只股票生成训练样本"""
        samples = []
        
        try:
            # 计算时间范围
            end_date = datetime.now().strftime('%Y%m%d')
            start_date = (datetime.now() - timedelta(days=time_range)).strftime('%Y%m%d')
            
            # 获取历史数据
            daily_data = self.data_extractor.get_stock_daily_data(stock_code, start_date, end_date)
            if daily_data is None or daily_data.empty:
                return samples
            
            # 获取股票基本信息
            stock_info = self._get_stock_basic_info(stock_code)
            stock_name = stock_info.get('name', stock_code)
            
            # 计算技术指标
            indicators = self._calculate_technical_indicators(daily_data)
            
            # 应用策略规则生成信号
            signals = self._apply_strategy_rules(strategy, daily_data, indicators)
            
            # 为每个交易日生成训练样本
            for i in range(20, len(daily_data)):  # 跳过前20天确保指标计算完整
                date_value = daily_data.iloc[i]['trade_date']
                # 确保date是字符串格式
                if hasattr(date_value, 'strftime'):
                    date = date_value.strftime('%Y%m%d')
                else:
                    date = str(date_value)
                
                # 检查是否有策略信号
                if date not in signals:
                    continue
                
                signal = signals[date]
                
                # 生成训练样本
                sample = self._create_training_sample(
                    stock_code, stock_name, date, daily_data.iloc[i],
                    indicators.iloc[i], strategy_id, strategy, signal
                )
                
                if sample:
                    samples.append(sample)
            
            return samples
            
        except Exception as e:
            self.logger.error(f"生成股票 {stock_code} 样本失败: {e}")
            return []
    
    def _get_stock_basic_info(self, stock_code: str) -> Dict[str, Any]:
        """获取股票基本信息"""
        try:
            stock_basic = self.data_extractor.tushare_api.stock_basic(
                ts_code=stock_code, fields='ts_code,name,industry,market'
            )
            
            if stock_basic is not None and not stock_basic.empty:
                return stock_basic.iloc[0].to_dict()
            else:
                return {'name': stock_code, 'industry': '未知', 'market': '未知'}
                
        except Exception as e:
            self.logger.warning(f"获取股票 {stock_code} 基本信息失败: {e}")
            return {'name': stock_code, 'industry': '未知', 'market': '未知'}
    
    def _calculate_technical_indicators(self, daily_data: pd.DataFrame) -> pd.DataFrame:
        """计算技术指标"""
        indicators = daily_data.copy()
        
        # 移动平均线
        indicators['ma5'] = daily_data['close'].rolling(window=5).mean()
        indicators['ma20'] = daily_data['close'].rolling(window=20).mean()
        indicators['ma60'] = daily_data['close'].rolling(window=60).mean()
        
        # RSI
        indicators['rsi'] = self._calculate_rsi(daily_data['close'], 14)
        
        # MACD
        macd_data = self._calculate_macd(daily_data['close'])
        indicators['macd'] = macd_data['macd']
        indicators['macd_signal'] = macd_data['signal']
        indicators['macd_hist'] = macd_data['histogram']
        
        # 成交量比
        indicators['volume_ma5'] = daily_data['vol'].rolling(window=5).mean()
        indicators['volume_ratio'] = daily_data['vol'] / indicators['volume_ma5']
        
        # 价格变动
        indicators['price_change'] = daily_data['close'] - daily_data['pre_close']
        indicators['pct_change'] = indicators['price_change'] / daily_data['pre_close'] * 100
        
        return indicators
    
    def _calculate_rsi(self, prices: pd.Series, window: int) -> pd.Series:
        """计算RSI指标"""
        delta = prices.diff()
        gain = (delta.where(delta > 0, 0)).rolling(window=window).mean()
        loss = (-delta.where(delta < 0, 0)).rolling(window=window).mean()
        rs = gain / loss
        rsi = 100 - (100 / (1 + rs))
        return rsi
    
    def _calculate_macd(self, prices: pd.Series, fast: int = 12, slow: int = 26, signal: int = 9) -> Dict:
        """计算MACD指标"""
        exp1 = prices.ewm(span=fast).mean()
        exp2 = prices.ewm(span=slow).mean()
        macd = exp1 - exp2
        signal_line = macd.ewm(span=signal).mean()
        histogram = macd - signal_line
        
        return {
            'macd': macd,
            'signal': signal_line,
            'histogram': histogram
        }
    
    def _apply_strategy_rules(self, strategy: Dict, daily_data: pd.DataFrame, 
                            indicators: pd.DataFrame) -> Dict[str, Dict]:
        """应用策略规则生成交易信号"""
        signals = {}
        
        # 解析买入规则
        buy_rules = strategy.get('buy_rules', [])
        sell_rules = strategy.get('sell_rules', [])
        
        for i in range(len(daily_data)):
            date = daily_data.iloc[i]['trade_date']
            
            # 检查买入条件
            if self._check_strategy_conditions(buy_rules, indicators.iloc[i]):
                signals[date] = {
                    'type': 'buy',
                    'confidence': np.random.uniform(0.6, 0.9),  # 模拟置信度
                    'strength': 'strong' if np.random.random() > 0.5 else 'moderate'
                }
            
            # 检查卖出条件
            elif self._check_strategy_conditions(sell_rules, indicators.iloc[i]):
                signals[date] = {
                    'type': 'sell',
                    'confidence': np.random.uniform(0.6, 0.9),
                    'strength': 'strong' if np.random.random() > 0.5 else 'moderate'
                }
            
            # 随机生成持有信号
            elif np.random.random() < 0.1:  # 10%概率生成持有信号
                signals[date] = {
                    'type': 'hold',
                    'confidence': np.random.uniform(0.5, 0.7),
                    'strength': 'moderate'
                }
        
        return signals
    
    def _check_strategy_conditions(self, rules: List, indicator_data: pd.Series) -> bool:
        """检查策略条件是否满足"""
        if not rules:
            return False
        
        # 简化的条件检查逻辑
        # 在实际实现中，需要根据具体的策略规则进行详细检查
        try:
            # 检查RSI条件
            rsi = indicator_data.get('rsi', 50)
            if not pd.isna(rsi):
                if rsi < 30:  # 超卖
                    return True
                elif rsi > 70:  # 超买
                    return True
            
            # 检查均线条件
            ma5 = indicator_data.get('ma5', 0)
            ma20 = indicator_data.get('ma20', 0)
            
            if not pd.isna(ma5) and not pd.isna(ma20):
                if ma5 > ma20:  # 金叉
                    return np.random.random() < 0.3
                elif ma5 < ma20:  # 死叉
                    return np.random.random() < 0.3
            
            return False
            
        except Exception as e:
            return False
    
    def _create_training_sample(self, stock_code: str, stock_name: str, date: str,
                              daily_data: pd.Series, indicators: pd.Series,
                              strategy_id: str, strategy: Dict, signal: Dict) -> Optional[TrainingSample]:
        """创建训练样本"""
        try:
            # 准备输入数据
            input_data = {
                'stock_code': stock_code,
                'stock_name': stock_name,
                'date': date,
                'current_price': daily_data['close'],
                'price_change': indicators.get('price_change', 0),
                'pct_change': indicators.get('pct_change', 0),
                'ma5': indicators.get('ma5', daily_data['close']),
                'ma20': indicators.get('ma20', daily_data['close']),
                'rsi': indicators.get('rsi', 50),
                'macd': indicators.get('macd', 0),
                'volume': daily_data['vol'],
                'volume_ratio': indicators.get('volume_ratio', 1.0),
                'market_cap': '未知',
                'pe_ratio': '未知',
                'pb_ratio': '未知',
                'strategy_name': strategy['name']
            }
            
            # 生成输入文本
            input_text = self.input_template.format(**input_data)
            
            # 生成输出文本
            output_data = self._generate_output_content(signal, strategy, stock_name, stock_code)
            output_text = self.output_template.format(**output_data)
            
            # 创建训练样本
            sample = TrainingSample(
                id=str(uuid.uuid4()),
                strategy_id=strategy_id,
                strategy_type=strategy.get('strategy_type', 'technical'),
                stock_code=stock_code,
                stock_name=stock_name,
                date=date,
                input_text=input_text,
                output_text=output_text,
                signal_type=signal['type'],
                confidence=signal['confidence'],
                metadata={
                    'price': daily_data['close'],
                    'volume': daily_data['vol'],
                    'rsi': indicators.get('rsi', 50),
                    'signal_strength': signal['strength']
                }
            )
            
            return sample
            
        except Exception as e:
            self.logger.error(f"创建训练样本失败: {e}")
            return None
    
    def _generate_output_content(self, signal: Dict, strategy: Dict, 
                               stock_name: str, stock_code: str) -> Dict[str, str]:
        """生成输出内容"""
        signal_type = signal['type']
        confidence = signal['confidence']
        strategy_name = strategy['name']
        
        signal_labels = {
            'buy': '买入推荐',
            'sell': '卖出推荐', 
            'hold': '持有推荐'
        }
        
        # 生成分析细节
        analysis_details = self._generate_analysis_details(signal, strategy)
        
        # 生成操作建议
        recommendations = {
            'buy': f'建议买入{stock_name}，当前技术指标显示良好的买入时机。',
            'sell': f'建议卖出{stock_name}，技术指标显示可能面临调整压力。',
            'hold': f'建议继续持有{stock_name}，等待更明确的信号出现。'
        }
        
        return {
            'signal_type_label': signal_labels.get(signal_type, '操作建议'),
            'strategy_name': strategy_name,
            'stock_name': stock_name,
            'stock_code': stock_code,
            'analysis_details': analysis_details,
            'recommendation': recommendations.get(signal_type, '请谨慎操作'),
            'target_price': self._generate_target_price(signal_type),
            'stop_loss': self._generate_stop_loss(signal_type),
            'holding_period': self._generate_holding_period(strategy),
            'risk_level': self._generate_risk_level(strategy, confidence)
        }
    
    def _generate_analysis_details(self, signal: Dict, strategy: Dict) -> str:
        """生成分析细节"""
        signal_type = signal['type']
        strategy_name = strategy['name']
        
        details_templates = {
            'buy': f"""技术面分析显示多个积极信号:
1. 基于{strategy_name}的核心指标显示买入时机成熟
2. 价格结构良好，支撑位明确
3. 成交量配合，资金关注度提升
4. 风险收益比较为理想""",
            
            'sell': f"""技术面分析显示多个风险信号:
1. {strategy_name}核心指标显示卖出信号
2. 价格面临重要阻力位
3. 成交量萎缩，资金流出迹象
4. 短期调整风险加大""",
            
            'hold': f"""当前市场环境下，基于{strategy_name}分析:
1. 技术指标处于震荡状态
2. 没有明确的方向性信号
3. 建议等待更好的买卖时机
4. 保持关注，适时调整策略"""
        }
        
        return details_templates.get(signal_type, "技术分析显示当前需要谨慎操作")
    
    def _generate_target_price(self, signal_type: str) -> str:
        """生成目标价格"""
        if signal_type == 'buy':
            return f"+{np.random.randint(5, 20)}%"
        elif signal_type == 'sell':
            return "逢高减仓"
        else:
            return "暂不设定"
    
    def _generate_stop_loss(self, signal_type: str) -> str:
        """生成止损建议"""
        if signal_type == 'buy':
            return f"-{np.random.randint(5, 10)}%"
        elif signal_type == 'sell':
            return "已达卖出条件"
        else:
            return "根据市场情况调整"
    
    def _generate_holding_period(self, strategy: Dict) -> str:
        """生成持有期建议"""
        strategy_type = strategy.get('strategy_type', 'technical')
        
        periods = {
            'technical': f"{np.random.randint(5, 20)}个交易日",
            'fundamental': f"{np.random.randint(1, 6)}个月",
            'quantitative': f"{np.random.randint(3, 15)}个交易日",
            'hybrid': f"{np.random.randint(2, 8)}周"
        }
        
        return periods.get(strategy_type, "1-3个月")
    
    def _generate_risk_level(self, strategy: Dict, confidence: float) -> str:
        """生成风险评级"""
        if confidence > 0.8:
            return "中低风险"
        elif confidence > 0.6:
            return "中等风险"
        else:
            return "中高风险"
    
    def _save_dataset(self, strategy_id: str, samples: List[TrainingSample]):
        """保存数据集到文件"""
        # 保存为JSON Lines格式
        jsonl_path = os.path.join(self.output_dir, f"{strategy_id}_dataset.jsonl")
        
        with open(jsonl_path, 'w', encoding='utf-8') as f:
            for sample in samples:
                sample_dict = asdict(sample)
                # 确保日期字段是字符串格式
                if 'date' in sample_dict and hasattr(sample_dict['date'], 'strftime'):
                    sample_dict['date'] = sample_dict['date'].strftime('%Y%m%d')
                json.dump(sample_dict, f, ensure_ascii=False, default=str)
                f.write('\n')
        
        # 保存统计信息
        stats = {
            'strategy_id': strategy_id,
            'total_samples': len(samples),
            'signal_distribution': self._get_signal_distribution(samples),
            'confidence_stats': self._get_confidence_stats(samples),
            'date_range': self._get_date_range(samples),
            'generated_at': datetime.now().isoformat()
        }
        
        stats_path = os.path.join(self.output_dir, f"{strategy_id}_stats.json")
        with open(stats_path, 'w', encoding='utf-8') as f:
            json.dump(stats, f, ensure_ascii=False, indent=2, default=str)
        
        self.logger.info(f"数据集已保存到 {jsonl_path}")
        self.logger.info(f"统计信息已保存到 {stats_path}")
    
    def _get_signal_distribution(self, samples: List[TrainingSample]) -> Dict[str, int]:
        """获取信号分布统计"""
        distribution = {}
        for sample in samples:
            signal_type = sample.signal_type
            distribution[signal_type] = distribution.get(signal_type, 0) + 1
        return distribution
    
    def _get_confidence_stats(self, samples: List[TrainingSample]) -> Dict[str, float]:
        """获取置信度统计"""
        confidences = [sample.confidence for sample in samples]
        return {
            'mean': np.mean(confidences),
            'std': np.std(confidences),
            'min': np.min(confidences),
            'max': np.max(confidences)
        }
    
    def _get_date_range(self, samples: List[TrainingSample]) -> Dict[str, str]:
        """获取日期范围"""
        dates = [sample.date for sample in samples]
        return {
            'start_date': min(dates),
            'end_date': max(dates)
        }
    
    def generate_all_strategy_datasets(self, num_stocks: int = 30, time_range: int = 180):
        """生成所有策略的训练数据集"""
        self.logger.info("开始生成所有策略的训练数据集")
        
        strategy_ids = get_all_strategy_ids()
        total_samples = 0
        
        for i, strategy_id in enumerate(strategy_ids):
            try:
                self.logger.info(f"处理策略 {i+1}/{len(strategy_ids)}: {strategy_id}")
                
                samples = self.generate_strategy_dataset(
                    strategy_id, num_stocks, time_range
                )
                
                total_samples += len(samples)
                self.logger.info(f"策略 {strategy_id} 完成，生成 {len(samples)} 个样本")
                
            except Exception as e:
                self.logger.error(f"处理策略 {strategy_id} 失败: {e}")
                continue
        
        # 生成汇总统计
        summary = {
            'total_strategies': len(strategy_ids),
            'total_samples': total_samples,
            'average_samples_per_strategy': total_samples / len(strategy_ids),
            'generated_at': datetime.now().isoformat(),
            'parameters': {
                'num_stocks': num_stocks,
                'time_range': time_range
            }
        }
        
        summary_path = os.path.join(self.output_dir, "dataset_summary.json")
        with open(summary_path, 'w', encoding='utf-8') as f:
            json.dump(summary, f, ensure_ascii=False, indent=2)
        
        self.logger.info(f"所有策略数据集生成完成！")
        self.logger.info(f"总计 {len(strategy_ids)} 个策略，{total_samples} 个训练样本")
        self.logger.info(f"汇总信息已保存到 {summary_path}")

if __name__ == "__main__":
    import argparse
    
    # 设置日志
    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    )
    
    parser = argparse.ArgumentParser(description="策略训练数据集生成器")
    parser.add_argument("--strategy", type=str, help="指定策略ID")
    parser.add_argument("--all", action="store_true", help="生成所有策略数据集")
    parser.add_argument("--stocks", type=int, default=30, help="每个策略使用的股票数量")
    parser.add_argument("--days", type=int, default=180, help="历史数据天数")
    
    args = parser.parse_args()
    
    # 从环境变量获取TuShare token
    tushare_token = os.getenv('TUSHARE_TOKEN')
    if not tushare_token:
        print("警告: 未设置TUSHARE_TOKEN环境变量")
    
    generator = StrategyDatasetGenerator(tushare_token)
    
    if args.all:
        generator.generate_all_strategy_datasets(args.stocks, args.days)
    elif args.strategy:
        samples = generator.generate_strategy_dataset(args.strategy, args.stocks, args.days)
        print(f"策略 {args.strategy} 生成 {len(samples)} 个训练样本")
    else:
        print("请指定 --strategy 或 --all 参数")