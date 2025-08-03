"""
大模型训练数据生成器
将TuShare历史数据转换为适合大模型训练的格式
"""

import pandas as pd
import json
import logging
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Tuple, Any
import numpy as np
import os
from dataclasses import dataclass

@dataclass
class TrainingExample:
    """训练样本数据结构"""
    instruction: str
    input: str
    output: str
    metadata: Dict[str, Any]

class LLMTrainingDataGenerator:
    """大模型训练数据生成器"""
    
    def __init__(self):
        self.logger = logging.getLogger(__name__)
        
        # 定义各种提示模板
        self.prompt_templates = {
            'stock_analysis': [
                "分析以下股票的技术指标和价格走势，并给出投资建议：",
                "根据提供的股票数据，分析该股票的投资价值：",
                "请对以下股票数据进行专业分析：",
                "基于历史数据，评估这只股票的投资潜力："
            ],
            'price_prediction': [
                "根据历史价格数据，预测该股票未来的价格趋势：",
                "分析价格走势并预测短期内的价格变化：",
                "基于技术分析，判断股票后续走势：",
                "请预测该股票在未来几个交易日的表现："
            ],
            'market_sentiment': [
                "分析市场情绪对该股票价格的影响：",
                "评估当前市场环境下的投资策略：",
                "解读市场信号并提供交易建议：",
                "分析市场趋势对该股票的潜在影响："
            ],
            'risk_assessment': [
                "评估投资该股票的风险等级：",
                "分析该股票的风险收益比：",
                "识别该股票投资的主要风险因素：",
                "提供风险管理建议："
            ],
            'financial_analysis': [
                "基于财务数据分析公司基本面：",
                "评估公司的财务健康状况：",
                "分析公司的盈利能力和成长性：",
                "解读财务报表并给出投资建议："
            ]
        }
        
    def calculate_technical_indicators(self, df: pd.DataFrame) -> pd.DataFrame:
        """
        计算技术指标
        
        Args:
            df: 包含OHLCV数据的DataFrame
            
        Returns:
            添加了技术指标的DataFrame
        """
        if df.empty:
            return df
            
        df = df.copy()
        df = df.sort_values('trade_date')
        
        # 移动平均线
        df['ma5'] = df['close'].rolling(5).mean()
        df['ma20'] = df['close'].rolling(20).mean()
        df['ma60'] = df['close'].rolling(60).mean()
        
        # RSI
        delta = df['close'].diff()
        gain = delta.where(delta > 0, 0).rolling(14).mean()
        loss = (-delta).where(delta < 0, 0).rolling(14).mean()
        rs = gain / loss
        df['rsi'] = 100 - (100 / (1 + rs))
        
        # MACD
        exp1 = df['close'].ewm(span=12).mean()
        exp2 = df['close'].ewm(span=26).mean()
        df['macd'] = exp1 - exp2
        df['signal'] = df['macd'].ewm(span=9).mean()
        df['histogram'] = df['macd'] - df['signal']
        
        # 布林带
        df['bb_middle'] = df['close'].rolling(20).mean()
        bb_std = df['close'].rolling(20).std()
        df['bb_upper'] = df['bb_middle'] + (bb_std * 2)
        df['bb_lower'] = df['bb_middle'] - (bb_std * 2)
        
        # 波动率
        df['volatility'] = df['close'].pct_change().rolling(20).std() * np.sqrt(252)
        
        # 价格变化
        df['price_change'] = df['close'].pct_change()
        df['price_change_5d'] = df['close'].pct_change(5)
        df['price_change_20d'] = df['close'].pct_change(20)
        
        return df
    
    def format_stock_data_for_prompt(self, row: pd.Series, include_future: bool = False) -> str:
        """
        将股票数据格式化为提示文本
        
        Args:
            row: 股票数据行
            include_future: 是否包含未来数据（用于生成答案）
            
        Returns:
            格式化的文本
        """
        data_text = f"""股票代码: {row.get('ts_code', 'N/A')}
交易日期: {row.get('trade_date', 'N/A')}
开盘价: {row.get('open', 0):.2f}
最高价: {row.get('high', 0):.2f}
最低价: {row.get('low', 0):.2f}
收盘价: {row.get('close', 0):.2f}
成交量: {row.get('vol', 0):,.0f}"""

        # 添加技术指标
        if 'ma5' in row and pd.notna(row['ma5']):
            data_text += f"""

技术指标:
5日均线: {row['ma5']:.2f}
20日均线: {row.get('ma20', 0):.2f}
RSI: {row.get('rsi', 0):.2f}
MACD: {row.get('macd', 0):.4f}
波动率: {row.get('volatility', 0):.2f}"""

        # 添加价格变化信息
        if 'price_change' in row and pd.notna(row['price_change']):
            data_text += f"""

价格变化:
日涨跌幅: {row['price_change']*100:.2f}%
5日涨跌幅: {row.get('price_change_5d', 0)*100:.2f}%
20日涨跌幅: {row.get('price_change_20d', 0)*100:.2f}%"""

        return data_text
    
    def generate_investment_advice(self, row: pd.Series) -> str:
        """
        基于数据生成投资建议
        
        Args:
            row: 股票数据行
            
        Returns:
            投资建议文本
        """
        advice_parts = []
        
        # 趋势分析
        if pd.notna(row.get('ma5')) and pd.notna(row.get('ma20')):
            if row['ma5'] > row['ma20']:
                advice_parts.append("技术面呈上升趋势，短期均线位于长期均线之上")
            else:
                advice_parts.append("技术面呈下降趋势，短期均线位于长期均线之下")
        
        # RSI分析
        rsi = row.get('rsi', 50)
        if rsi > 70:
            advice_parts.append("RSI显示超买状态，建议谨慎操作")
        elif rsi < 30:
            advice_parts.append("RSI显示超卖状态，可能存在反弹机会")
        else:
            advice_parts.append("RSI处于正常区间")
        
        # 价格变化分析
        price_change = row.get('price_change', 0) * 100
        if abs(price_change) > 5:
            if price_change > 0:
                advice_parts.append(f"当日大幅上涨{price_change:.2f}%，注意获利回吐风险")
            else:
                advice_parts.append(f"当日大幅下跌{abs(price_change):.2f}%，可关注支撑位")
        
        # 波动率分析
        volatility = row.get('volatility', 0)
        if volatility > 0.3:
            advice_parts.append("波动率较高，投资风险相对较大")
        elif volatility < 0.15:
            advice_parts.append("波动率较低，价格相对稳定")
        
        # 综合建议
        if not advice_parts:
            advice_parts.append("建议结合基本面分析进行综合判断")
        
        return "。".join(advice_parts) + "。请注意控制风险，理性投资。"
    
    def generate_price_prediction(self, historical_data: pd.DataFrame, target_row_idx: int) -> str:
        """
        生成价格预测（基于历史数据）
        
        Args:
            historical_data: 历史数据
            target_row_idx: 目标行索引
            
        Returns:
            价格预测文本
        """
        if target_row_idx + 5 >= len(historical_data):
            return "数据不足，无法进行预测"
        
        current_price = historical_data.iloc[target_row_idx]['close']
        future_prices = historical_data.iloc[target_row_idx+1:target_row_idx+6]['close']
        
        # 计算未来几天的实际表现
        predictions = []
        for i, future_price in enumerate(future_prices, 1):
            change = (future_price - current_price) / current_price * 100
            if change > 2:
                trend = "上涨"
            elif change < -2:
                trend = "下跌"
            else:
                trend = "震荡"
            predictions.append(f"第{i}日预期{trend}，幅度约{change:.2f}%")
        
        return "基于技术分析预测：" + "；".join(predictions[:3]) + "。"
    
    def create_training_examples(self, stock_data: pd.DataFrame, max_examples: int = 1000) -> List[TrainingExample]:
        """
        创建训练样本
        
        Args:
            stock_data: 股票数据
            max_examples: 最大样本数
            
        Returns:
            训练样本列表
        """
        examples = []
        
        if stock_data.empty:
            return examples
        
        # 计算技术指标
        stock_data = self.calculate_technical_indicators(stock_data)
        
        # 按股票代码分组
        for ts_code, group_data in stock_data.groupby('ts_code'):
            group_data = group_data.sort_values('trade_date').reset_index(drop=True)
            
            # 为每只股票生成多个训练样本
            sample_indices = np.linspace(20, len(group_data)-10, min(50, len(group_data)-30), dtype=int)
            
            for idx in sample_indices:
                if idx >= len(group_data):
                    continue
                    
                row = group_data.iloc[idx]
                
                # 跳过有缺失值的行
                if pd.isna(row[['open', 'high', 'low', 'close', 'vol']]).any():
                    continue
                
                # 生成不同类型的训练样本
                self._generate_analysis_examples(row, group_data, idx, examples)
                self._generate_prediction_examples(row, group_data, idx, examples)
                
                if len(examples) >= max_examples:
                    break
                    
            if len(examples) >= max_examples:
                break
        
        self.logger.info(f"✅ 生成训练样本: {len(examples)}个")
        return examples[:max_examples]
    
    def _generate_analysis_examples(self, row: pd.Series, group_data: pd.DataFrame, idx: int, examples: List[TrainingExample]):
        """生成分析类训练样本"""
        stock_text = self.format_stock_data_for_prompt(row)
        advice = self.generate_investment_advice(row)
        
        # 股票分析样本
        analysis_prompt = np.random.choice(self.prompt_templates['stock_analysis'])
        examples.append(TrainingExample(
            instruction=analysis_prompt,
            input=stock_text,
            output=advice,
            metadata={
                'type': 'stock_analysis',
                'ts_code': row.get('ts_code'),
                'trade_date': str(row.get('trade_date'))
            }
        ))
        
        # 风险评估样本
        risk_prompt = np.random.choice(self.prompt_templates['risk_assessment'])
        risk_assessment = self._generate_risk_assessment(row)
        examples.append(TrainingExample(
            instruction=risk_prompt,
            input=stock_text,
            output=risk_assessment,
            metadata={
                'type': 'risk_assessment',
                'ts_code': row.get('ts_code'),
                'trade_date': str(row.get('trade_date'))
            }
        ))
    
    def _generate_prediction_examples(self, row: pd.Series, group_data: pd.DataFrame, idx: int, examples: List[TrainingExample]):
        """生成预测类训练样本"""
        stock_text = self.format_stock_data_for_prompt(row)
        prediction = self.generate_price_prediction(group_data, idx)
        
        prediction_prompt = np.random.choice(self.prompt_templates['price_prediction'])
        examples.append(TrainingExample(
            instruction=prediction_prompt,
            input=stock_text,
            output=prediction,
            metadata={
                'type': 'price_prediction',
                'ts_code': row.get('ts_code'),
                'trade_date': str(row.get('trade_date'))
            }
        ))
    
    def _generate_risk_assessment(self, row: pd.Series) -> str:
        """生成风险评估"""
        risk_factors = []
        risk_level = "中等"
        
        # 波动率风险
        volatility = row.get('volatility', 0)
        if volatility > 0.4:
            risk_factors.append("高波动率")
            risk_level = "较高"
        elif volatility < 0.1:
            risk_factors.append("低波动率")
        
        # RSI风险
        rsi = row.get('rsi', 50)
        if rsi > 80:
            risk_factors.append("严重超买")
            risk_level = "较高"
        elif rsi < 20:
            risk_factors.append("严重超卖")
        
        # 价格风险
        price_change = abs(row.get('price_change', 0)) * 100
        if price_change > 8:
            risk_factors.append("价格剧烈波动")
            risk_level = "较高"
        
        risk_text = f"风险等级：{risk_level}。"
        if risk_factors:
            risk_text += f"主要风险因素：{', '.join(risk_factors)}。"
        
        risk_text += "建议：根据个人风险承受能力合理配置仓位，设置止损点。"
        
        return risk_text
    
    def save_training_data(self, examples: List[TrainingExample], output_file: str, format: str = 'jsonl'):
        """
        保存训练数据
        
        Args:
            examples: 训练样本列表
            output_file: 输出文件路径
            format: 输出格式 ('jsonl', 'json', 'alpaca')
        """
        os.makedirs(os.path.dirname(output_file), exist_ok=True)
        
        if format == 'jsonl':
            with open(output_file, 'w', encoding='utf-8') as f:
                for example in examples:
                    json_obj = {
                        'instruction': example.instruction,
                        'input': example.input,
                        'output': example.output,
                        'metadata': example.metadata
                    }
                    f.write(json.dumps(json_obj, ensure_ascii=False) + '\n')
                    
        elif format == 'alpaca':
            # Alpaca格式
            alpaca_data = []
            for example in examples:
                alpaca_data.append({
                    'instruction': example.instruction,
                    'input': example.input,
                    'output': example.output
                })
            
            with open(output_file, 'w', encoding='utf-8') as f:
                json.dump(alpaca_data, f, ensure_ascii=False, indent=2)
                
        else:  # json
            data = [example.__dict__ for example in examples]
            with open(output_file, 'w', encoding='utf-8') as f:
                json.dump(data, f, ensure_ascii=False, indent=2)
        
        self.logger.info(f"✅ 训练数据已保存到: {output_file}")
        self.logger.info(f"   格式: {format}, 样本数: {len(examples)}")


def main():
    """测试训练数据生成器"""
    logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
    
    # 初始化生成器
    generator = LLMTrainingDataGenerator()
    
    # 检查是否有现有的数据文件
    data_dir = "data/tushare_dataset"
    csv_files = [f for f in os.listdir(data_dir) if f.endswith('.csv') and 'daily_data' in f] if os.path.exists(data_dir) else []
    
    if csv_files:
        # 使用最新的数据文件
        latest_file = sorted(csv_files)[-1]
        data_path = os.path.join(data_dir, latest_file)
        print(f"📂 使用数据文件: {data_path}")
        
        # 读取数据
        stock_data = pd.read_csv(data_path)
        print(f"📊 加载数据: {len(stock_data)}条记录")
        
    else:
        # 生成模拟数据进行测试
        print("📝 生成模拟数据进行测试...")
        dates = pd.date_range('2024-01-01', '2024-06-30', freq='D')
        stock_data = []
        
        for ts_code in ['000001.SZ', '000002.SZ', '600000.SH']:
            for date in dates:
                if date.weekday() < 5:  # 工作日
                    stock_data.append({
                        'ts_code': ts_code,
                        'trade_date': date,
                        'open': 10 + np.random.normal(0, 0.5),
                        'high': 10.5 + np.random.normal(0, 0.5),
                        'low': 9.5 + np.random.normal(0, 0.5),
                        'close': 10 + np.random.normal(0, 0.5),
                        'vol': np.random.randint(1000000, 10000000)
                    })
        
        stock_data = pd.DataFrame(stock_data)
    
    # 生成训练样本
    examples = generator.create_training_examples(stock_data, max_examples=100)
    
    if examples:
        # 保存训练数据
        os.makedirs('data/llm_training', exist_ok=True)
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        
        # 保存为不同格式
        generator.save_training_data(examples, f'data/llm_training/stock_training_data_{timestamp}.jsonl', 'jsonl')
        generator.save_training_data(examples, f'data/llm_training/stock_training_data_alpaca_{timestamp}.json', 'alpaca')
        
        print(f"\n📋 训练数据生成完成！")
        print(f"   样本数量: {len(examples)}")
        print(f"   示例输出:")
        print(f"   指令: {examples[0].instruction}")
        print(f"   输入: {examples[0].input[:200]}...")
        print(f"   输出: {examples[0].output}")
        
    else:
        print("❌ 未能生成训练样本")


if __name__ == "__main__":
    main()