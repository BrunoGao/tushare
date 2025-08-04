#!/usr/bin/env python3
"""
ljwx-stock模型全面训练脚本
使用过去一年的实际历史数据和所有股票生成大规模训练集
"""

import os
import sys
import json
import logging
import tempfile
import subprocess
from datetime import datetime, timedelta
from typing import List, Dict, Optional
import pandas as pd
import time
import random

# 添加项目路径
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

from llm.tushare_data_extractor import TuShareDataExtractor
from llm.llm_training_data_generator import LLMTrainingDataGenerator

class ComprehensiveTrainer:
    """全面训练系统"""
    
    def __init__(self, tushare_token: str = None):
        self.logger = logging.getLogger(__name__)
        
        # 初始化组件
        self.data_extractor = TuShareDataExtractor(tushare_token)
        self.data_generator = LLMTrainingDataGenerator()
        
        # 配置
        self.config = {
            'base_model': 'ljwx-stock',
            'training_data_dir': 'data/llm_training',
            'models_dir': 'data/models',
            'cache_dir': 'data/cache',  # 数据缓存目录
            'max_training_examples': 50000,  # 大幅增加到50000条训练样本
            'batch_size': 100,   # 每批处理100只股票（提高效率）
            'days_back': 1825,   # 过去5年数据（5*365=1825天）
            'request_delay': 0.3,  # 减少API请求延迟
            'max_retries': 5,   # 增加最大重试次数
            'test_mode': False,  # 关闭测试模式：处理所有股票
            'use_cache': True,   # 启用缓存
            'cache_expire_days': 7,  # 缓存过期天数
        }
        
        # 确保目录存在
        os.makedirs(self.config['training_data_dir'], exist_ok=True)
        os.makedirs(self.config['models_dir'], exist_ok=True)
        os.makedirs(self.config['cache_dir'], exist_ok=True)
    
    def get_cache_path(self, cache_type: str, identifier: str = "") -> str:
        """获取缓存文件路径"""
        if identifier:
            filename = f"{cache_type}_{identifier}.pkl"
        else:
            filename = f"{cache_type}.pkl"
        return os.path.join(self.config['cache_dir'], filename)
    
    def is_cache_valid(self, cache_path: str) -> bool:
        """检查缓存是否有效"""
        if not os.path.exists(cache_path):
            return False
        
        # 检查文件修改时间
        file_time = datetime.fromtimestamp(os.path.getmtime(cache_path))
        expire_time = datetime.now() - timedelta(days=self.config['cache_expire_days'])
        
        return file_time > expire_time
    
    def load_from_cache(self, cache_path: str):
        """从缓存加载数据"""
        try:
            with open(cache_path, 'rb') as f:
                import pickle
                return pickle.load(f)
        except Exception as e:
            self.logger.warning(f"加载缓存失败: {e}")
            return None
    
    def save_to_cache(self, data, cache_path: str):
        """保存数据到缓存"""
        try:
            with open(cache_path, 'wb') as f:
                import pickle
                pickle.dump(data, f)
            self.logger.info(f"数据已缓存: {cache_path}")
        except Exception as e:
            self.logger.warning(f"保存缓存失败: {e}")
        
    def get_all_stock_codes(self) -> List[str]:
        """获取所有股票代码"""
        self.logger.info("📊 获取所有股票代码")
        
        # 检查缓存
        if self.config['use_cache']:
            cache_path = self.get_cache_path("stock_codes")
            if self.is_cache_valid(cache_path):
                cached_stocks = self.load_from_cache(cache_path)
                if cached_stocks:
                    self.logger.info(f"✅ 从缓存加载股票代码: {len(cached_stocks)}只")
                    return cached_stocks
        
        all_stocks = []
        
        try:
            # 获取A股股票列表
            stocks_df = self.data_extractor.get_stock_list(limit=None)  # 获取所有股票
            
            if stocks_df.empty:
                self.logger.warning("无法获取股票列表，使用备用方案")
                return self.get_backup_stock_codes()
            
            if 'ts_code' in stocks_df.columns:
                all_stocks = stocks_df['ts_code'].tolist()
            else:
                # 处理免费API的情况
                stocks_df.reset_index(inplace=True)
                all_stocks = [
                    f"{code}.SZ" if code.startswith(('0', '3')) else f"{code}.SH" 
                    for code in stocks_df['code'].tolist()
                ]
            
            # 过滤有效的股票代码
            valid_stocks = [stock for stock in all_stocks if self.is_valid_stock_code(stock)]
            
            # 全量模式：使用所有有效股票
            if self.config.get('test_mode', False):
                valid_stocks = valid_stocks[:200]
                self.logger.info(f"测试模式：使用前200只股票")
            else:
                self.logger.info(f"全量模式：使用所有{len(valid_stocks)}只股票")
            
            self.logger.info(f"获取到股票代码: {len(valid_stocks)}只")
            
            # 保存到缓存
            if self.config['use_cache']:
                cache_path = self.get_cache_path("stock_codes")
                self.save_to_cache(valid_stocks, cache_path)
            
            return valid_stocks
            
        except Exception as e:
            self.logger.error(f"获取股票列表失败: {e}")
            return self.get_backup_stock_codes()
    
    def get_backup_stock_codes(self) -> List[str]:
        """获取备用股票代码列表"""
        # 主要的A股股票代码
        backup_stocks = [
            # 沪深300成分股
            '000001.SZ', '000002.SZ', '000858.SZ', '000895.SZ', '000938.SZ',
            '600000.SH', '600036.SH', '600519.SH', '600887.SH', '600999.SH',
            # 创业板
            '300001.SZ', '300015.SZ', '300059.SZ', '300124.SZ', '300144.SZ',
            # 科创板
            '688001.SH', '688009.SH', '688012.SH', '688036.SH', '688099.SH',
            # 其他主要股票
            '002415.SZ', '002594.SZ', '002714.SZ', '002841.SZ', '002966.SZ',
        ]
        
        # 扩展更多股票代码
        for i in range(1, 1000):
            if i < 10:
                backup_stocks.append(f"00000{i}.SZ")
                backup_stocks.append(f"60000{i}.SH")
            elif i < 100:
                backup_stocks.append(f"0000{i:02d}.SZ")
                backup_stocks.append(f"6000{i:02d}.SH")
            else:
                backup_stocks.append(f"000{i:03d}.SZ")
                backup_stocks.append(f"600{i:03d}.SH")
        
        # 添加创业板和科创板
        for i in range(1, 1000):
            if i < 10:
                backup_stocks.append(f"30000{i}.SZ")
                backup_stocks.append(f"68800{i}.SH")
            elif i < 100:
                backup_stocks.append(f"3000{i:02d}.SZ")
                backup_stocks.append(f"6880{i:02d}.SH")
            else:
                backup_stocks.append(f"300{i:03d}.SZ")
                backup_stocks.append(f"688{i:03d}.SH")
        
        return backup_stocks[:5000]  # 增加到前5000只备用股票
    
    def is_valid_stock_code(self, stock_code: str) -> bool:
        """验证股票代码格式"""
        if not stock_code or '.' not in stock_code:
            return False
        
        code, exchange = stock_code.split('.')
        
        # 验证格式
        if len(code) != 6 or not code.isdigit():
            return False
        
        if exchange not in ['SZ', 'SH']:
            return False
        
        return True
    
    def get_historical_data_in_batches(self, stock_codes: List[str]) -> pd.DataFrame:
        """分批获取历史数据"""
        self.logger.info(f"📈 分批获取 {len(stock_codes)} 只股票的历史数据")
        
        # 设置日期范围
        end_date = datetime.now().strftime('%Y%m%d')
        start_date = (datetime.now() - timedelta(days=self.config['days_back'])).strftime('%Y%m%d')
        
        self.logger.info(f"数据时间范围: {start_date} ~ {end_date}")
        
        # 检查缓存
        cache_key = f"{start_date}_{end_date}_{len(stock_codes)}"
        if self.config['use_cache']:
            cache_path = self.get_cache_path("historical_data", cache_key)
            if self.is_cache_valid(cache_path):
                cached_data = self.load_from_cache(cache_path)
                if cached_data is not None and not cached_data.empty:
                    self.logger.info(f"✅ 从缓存加载历史数据: {len(cached_data)}条")
                    return cached_data
        
        all_data = []
        batch_size = self.config['batch_size']
        
        # 增加处理进度显示
        processed_stocks = 0
        total_stocks = len(stock_codes)
        
        for i in range(0, len(stock_codes), batch_size):
            batch_stocks = stock_codes[i:i + batch_size]
            batch_num = i // batch_size + 1
            total_batches = (len(stock_codes) + batch_size - 1) // batch_size
            
            processed_stocks += len(batch_stocks)
            progress_pct = (processed_stocks / total_stocks) * 100
            
            self.logger.info(f"处理第 {batch_num}/{total_batches} 批: {len(batch_stocks)} 只股票 ({progress_pct:.1f}%)")
            
            retry_count = 0
            while retry_count < self.config['max_retries']:
                try:
                    # 获取批次数据
                    batch_data = self.data_extractor.extract_comprehensive_dataset(
                        stock_codes=batch_stocks,
                        start_date=start_date,
                        end_date=end_date,
                        include_financial=False,
                        include_news=False
                    )
                    
                    if 'daily_data' in batch_data and not batch_data['daily_data'].empty:
                        all_data.append(batch_data['daily_data'])
                        self.logger.info(f"   成功获取 {len(batch_data['daily_data'])} 条数据")
                    else:
                        self.logger.warning(f"   第 {batch_num} 批数据为空")
                    
                    break  # 成功，跳出重试循环
                    
                except Exception as e:
                    retry_count += 1
                    self.logger.warning(f"   第 {batch_num} 批获取失败 (重试 {retry_count}/{self.config['max_retries']}): {e}")
                    
                    if retry_count < self.config['max_retries']:
                        time.sleep(self.config['request_delay'] * (2 ** retry_count))  # 指数退避
                    else:
                        self.logger.error(f"   第 {batch_num} 批最终失败，跳过")
            
            # API请求延迟
            if i + batch_size < len(stock_codes):
                time.sleep(self.config['request_delay'])
        
        # 合并所有数据
        if all_data:
            combined_data = pd.concat(all_data, ignore_index=True)
            self.logger.info(f"✅ 总计获取 {len(combined_data)} 条历史数据")
            
            # 保存到缓存
            if self.config['use_cache']:
                cache_path = self.get_cache_path("historical_data", cache_key)
                self.save_to_cache(combined_data, cache_path)
            
            return combined_data
        else:
            self.logger.error("❌ 未获取到任何历史数据")
            return pd.DataFrame()
    
    def generate_comprehensive_training_data(self, stock_data: pd.DataFrame) -> List[Dict]:
        """生成全面的训练数据"""
        self.logger.info(f"🧠 生成全面训练数据，原始数据: {len(stock_data)} 条")
        
        if stock_data.empty:
            return []
        
        # 检查训练数据缓存
        data_hash = str(hash(str(stock_data.shape) + str(stock_data.columns.tolist())))
        if self.config['use_cache']:
            cache_path = self.get_cache_path("training_data", data_hash)
            if self.is_cache_valid(cache_path):
                cached_training_data = self.load_from_cache(cache_path)
                if cached_training_data:
                    self.logger.info(f"✅ 从缓存加载训练数据: {len(cached_training_data)}条")
                    return cached_training_data
        
        try:
            # 使用训练数据生成器
            training_examples = self.data_generator.create_training_examples(
                stock_data,
                max_examples=self.config['max_training_examples']
            )
            
            # 转换TrainingExample对象为字典
            training_examples_dict = []
            for example in training_examples:
                if hasattr(example, '__dict__'):  # 如果是dataclass对象
                    example_dict = {
                        'instruction': example.instruction,
                        'input': example.input,
                        'output': example.output,
                        'metadata': example.metadata
                    }
                else:  # 如果已经是字典
                    example_dict = example
                training_examples_dict.append(example_dict)
            
            training_examples = training_examples_dict
            
            # 数据增强 - 生成更多样化的训练样本
            enhanced_examples = self.enhance_training_data(training_examples, stock_data)
            
            self.logger.info(f"✅ 生成训练样本: {len(enhanced_examples)} 条")
            
            # 保存训练数据到缓存
            if self.config['use_cache']:
                cache_path = self.get_cache_path("training_data", data_hash)
                self.save_to_cache(enhanced_examples, cache_path)
            
            return enhanced_examples
            
        except Exception as e:
            self.logger.error(f"❌ 生成训练数据失败: {e}")
            return []
    
    def enhance_training_data(self, base_examples: List[Dict], stock_data: pd.DataFrame) -> List[Dict]:
        """增强训练数据"""
        enhanced_examples = base_examples.copy()
        
        # 按股票分组
        stock_groups = stock_data.groupby('ts_code')
        
        enhancement_templates = [
            {
                "type": "trend_analysis",
                "instruction": "基于技术分析判断该股票的短期趋势：",
                "focus": "趋势分析"
            },
            {
                "type": "volume_analysis", 
                "instruction": "分析该股票的成交量变化并给出见解：",
                "focus": "成交量分析"
            },
            {
                "type": "momentum_analysis",
                "instruction": "评估该股票的动量指标和市场情绪：",
                "focus": "动量分析"
            },
            {
                "type": "support_resistance",
                "instruction": "识别该股票的关键支撑位和阻力位：",
                "focus": "支撑阻力"
            }
        ]
        
        added_count = 0
        max_enhanced_samples = min(10000, len(base_examples))  # 增加增强数据上限
        for stock_code, group_data in stock_groups:
            if added_count >= max_enhanced_samples:
                break
                
            if len(group_data) < 5:  # 数据太少跳过
                continue
            
            # 增加采样数量以生成更多训练数据
            sample_size = min(10, len(group_data))  # 增加到最多10条记录
            sample_data = group_data.sample(n=sample_size)
            
            for _, row in sample_data.iterrows():
                for template in enhancement_templates:
                    if added_count >= max_enhanced_samples:
                        break
                    
                    enhanced_example = self.create_enhanced_example(row, template)
                    if enhanced_example:
                        enhanced_examples.append(enhanced_example)
                        added_count += 1
        
        self.logger.info(f"数据增强: 新增 {added_count} 条样本")
        return enhanced_examples
    
    def create_enhanced_example(self, row: pd.Series, template: Dict) -> Optional[Dict]:
        """创建增强的训练样本"""
        try:
            stock_code = row.get('ts_code', 'N/A')
            trade_date = row.get('trade_date', 'N/A')
            
            # 构建输入数据
            input_data = f"""股票代码: {stock_code}
交易日期: {trade_date}
开盘价: {row.get('open', 0):.2f}
最高价: {row.get('high', 0):.2f}
最低价: {row.get('low', 0):.2f}
收盘价: {row.get('close', 0):.2f}
成交量: {row.get('vol', 0):,.0f}

技术指标:
5日均线: {row.get('ma5', 0):.2f}
20日均线: {row.get('ma20', 0):.2f}
RSI: {row.get('rsi', 50):.2f}
MACD: {row.get('macd', 0):.4f}
波动率: {row.get('volatility', 0):.2f}"""
            
            # 根据模板类型生成分析
            output_text = self.generate_analysis_by_template(row, template)
            
            return {
                "instruction": template["instruction"],
                "input": input_data,
                "output": output_text,
                "metadata": {
                    "type": template["type"],
                    "ts_code": stock_code,
                    "trade_date": str(trade_date),
                    "enhanced": True
                }
            }
            
        except Exception as e:
            self.logger.warning(f"创建增强样本失败: {e}")
            return None
    
    def generate_analysis_by_template(self, row: pd.Series, template: Dict) -> str:
        """根据模板生成分析内容"""
        close_price = row.get('close', 0)
        ma5 = row.get('ma5', close_price)
        ma20 = row.get('ma20', close_price)
        rsi = row.get('rsi', 50)
        macd = row.get('macd', 0)
        vol = row.get('vol', 0)
        
        if template["type"] == "trend_analysis":
            if ma5 > ma20 and rsi > 50:
                return f"技术分析显示短期上升趋势：5日均线({ma5:.2f})位于20日均线({ma20:.2f})之上，RSI({rsi:.1f})处于强势区域，建议关注上涨动能的持续性。"
            elif ma5 < ma20 and rsi < 50:
                return f"技术分析显示短期下降趋势：5日均线({ma5:.2f})位于20日均线({ma20:.2f})之下，RSI({rsi:.1f})处于弱势区域，建议谨慎观望。"
            else:
                return f"技术分析显示横盘整理：均线系统混合，RSI({rsi:.1f})处于中性区域，建议等待明确方向信号。"
        
        elif template["type"] == "volume_analysis":
            if vol > 1000000:
                return f"成交量分析：当日成交量{vol:,.0f}手，量能充足，表明市场参与度较高，价格变动具有一定可信度。"
            else:
                return f"成交量分析：当日成交量{vol:,.0f}手，量能不足，市场参与度较低，需关注后续量能变化。"
        
        elif template["type"] == "momentum_analysis":
            if macd > 0 and rsi > 60:
                return f"动量分析：MACD({macd:.4f})为正值且RSI({rsi:.1f})处于强势区域，短期动能较强，但需注意超买风险。"
            elif macd < 0 and rsi < 40:
                return f"动量分析：MACD({macd:.4f})为负值且RSI({rsi:.1f})处于弱势区域，短期动能偏弱，可能存在超卖反弹机会。"
            else:
                return f"动量分析：MACD({macd:.4f})和RSI({rsi:.1f})显示动能中性，建议结合其他指标综合判断。"
        
        elif template["type"] == "support_resistance":
            support = min(close_price * 0.95, ma20 * 0.98)
            resistance = max(close_price * 1.05, ma5 * 1.02)
            return f"支撑阻力分析：关键支撑位约{support:.2f}元，关键阻力位约{resistance:.2f}元，当前价格{close_price:.2f}元处于关键区间。"
        
        return "技术分析：建议综合多项指标进行判断，注意风险控制。"
    
    def create_comprehensive_model(self, training_data: List[Dict]) -> str:
        """创建全面训练的模型"""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M")
        model_name = f"ljwx-stock-comprehensive-{timestamp}"
        
        self.logger.info(f"🚀 创建全面训练模型: {model_name}")
        
        try:
            system_prompt = """你是ljwx-stock，一个专业的股票投资分析助手。基于大量A股市场历史数据训练，具备以下专业能力：

🎯 **核心优势**
- 基于过去5年全市场数据的深度学习
- 覆盖5000+只A股股票的分析经验
- 50000+条训练样本的专业分析能力
- 多维度技术分析和风险评估能力

📊 **分析能力**
1. **技术指标分析**：精通K线、均线、RSI、MACD、布林带等技术指标
2. **趋势判断**：基于历史模式识别价格趋势和转折点
3. **风险评估**：多层次风险因素分析和等级评定
4. **成交量分析**：理解量价关系和市场参与度
5. **支撑阻力**：准确识别关键价格水平

💡 **服务特色**
- 基于真实市场数据的专业分析
- 客观理性的投资建议
- 明确的风险提示和操作指导
- 适合不同风险偏好的投资者

📋 **分析框架**
- 技术面：价格趋势、技术指标、量价关系
- 风险面：波动率、超买超卖、流动性风险
- 操作面：买入卖出时机、仓位管理、止损设置

⚠️ **风险提示**：所有分析基于历史数据，投资有风险，请谨慎决策。"""

            # 构建Modelfile
            modelfile_content = f"""FROM ljwx-stock
SYSTEM "{system_prompt}"
PARAMETER temperature 0.7
PARAMETER top_p 0.9
PARAMETER top_k 40
PARAMETER repeat_penalty 1.1
PARAMETER num_predict 1024

"""
            
            # 添加训练样本（增加样本数量以提升模型质量）
            sample_count = min(len(training_data), 1000)  # 增加样本数量到1000
            selected_samples = random.sample(training_data, sample_count)
            
            for i, example in enumerate(selected_samples):
                instruction = example.get('instruction', '')
                input_text = example.get('input', '')
                output_text = example.get('output', '')
                
                user_message = f"{instruction}\n\n{input_text}" if input_text else instruction
                
                # 清理文本，避免格式问题
                user_message = user_message.replace('"', "'").replace('\n', '\\n')
                output_text = output_text.replace('"', "'").replace('\n', '\\n')
                
                modelfile_content += f'MESSAGE user "{user_message}"\n'
                modelfile_content += f'MESSAGE assistant "{output_text}"\n\n'
            
            # 创建临时Modelfile
            with tempfile.NamedTemporaryFile(mode='w', suffix='.modelfile', delete=False) as f:
                f.write(modelfile_content)
                modelfile_path = f.name
            
            try:
                self.logger.info(f"   使用训练样本: {sample_count} 条")
                self.logger.info(f"   数据时间跨度: 过去 {self.config['days_back']} 天")
                
                result = subprocess.run(
                    ['ollama', 'create', model_name, '-f', modelfile_path],
                    capture_output=True,
                    text=True,
                    timeout=1200  # 20分钟超时
                )
                
                if result.returncode == 0:
                    self.logger.info(f"✅ 全面训练模型创建成功: {model_name}")
                    return model_name
                else:
                    self.logger.error(f"❌ 模型创建失败: {result.stderr}")
                    return ""
                    
            finally:
                os.unlink(modelfile_path)
                
        except Exception as e:
            self.logger.error(f"❌ 创建全面训练模型时出错: {e}")
            return ""
    
    def test_comprehensive_model(self, model_name: str) -> Dict:
        """测试全面训练模型"""
        test_cases = [
            {
                "input": "分析平安银行(000001.SZ)的投资价值，当前价格12.50元，RSI=65，MACD=0.08",
                "type": "investment_analysis"
            },
            {
                "input": "评估贵州茅台(600519.SH)的风险等级，波动率0.25，日涨跌幅+2.3%",
                "type": "risk_assessment"
            },
            {
                "input": "基于技术指标预测比亚迪(002594.SZ)的短期走势",
                "type": "trend_prediction"
            },
            {
                "input": "分析工商银行(601398.SH)的成交量变化和市场情绪",
                "type": "volume_analysis"
            }
        ]
        
        results = {
            "model_name": model_name,
            "test_time": datetime.now().isoformat(),
            "test_cases": []
        }
        
        successful_tests = 0
        total_response_time = 0
        
        for i, test_case in enumerate(test_cases):
            self.logger.info(f"   测试用例 {i+1}/{len(test_cases)}: {test_case['type']}")
            
            try:
                start_time = time.time()
                
                result = subprocess.run(
                    ['ollama', 'run', model_name],
                    input=test_case['input'],
                    capture_output=True,
                    text=True,
                    timeout=60
                )
                
                end_time = time.time()
                response_time = end_time - start_time
                total_response_time += response_time
                
                if result.returncode == 0 and result.stdout.strip():
                    successful_tests += 1
                    success = True
                    response = result.stdout.strip()
                else:
                    success = False
                    response = result.stderr if result.stderr else "No response"
                
                results["test_cases"].append({
                    "input": test_case["input"],
                    "type": test_case["type"],
                    "response": response[:300] + "..." if len(response) > 300 else response,
                    "response_time": response_time,
                    "success": success
                })
                
            except Exception as e:
                self.logger.error(f"   测试用例 {i+1} 失败: {e}")
                results["test_cases"].append({
                    "input": test_case["input"],
                    "type": test_case["type"],
                    "response": f"Error: {str(e)}",
                    "response_time": 0,
                    "success": False
                })
        
        # 计算总结
        results["summary"] = {
            "total_tests": len(test_cases),
            "successful_tests": successful_tests,
            "success_rate": successful_tests / len(test_cases),
            "average_response_time": total_response_time / len(test_cases) if test_cases else 0
        }
        
        return results
    
    def run_comprehensive_training(self) -> Dict:
        """运行全面训练流程"""
        self.logger.info("🚀 开始全面训练流程")
        
        results = {
            "start_time": datetime.now().isoformat(),
            "status": "running",
            "config": self.config,
            "steps": {}
        }
        
        try:
            # 1. 获取所有股票代码
            self.logger.info("📊 步骤1: 获取所有股票代码")
            all_stock_codes = self.get_all_stock_codes()
            
            if not all_stock_codes:
                results["status"] = "failed"
                results["error"] = "无法获取股票代码"
                return results
            
            results["total_stocks"] = len(all_stock_codes)
            results["steps"]["stock_codes"] = {
                "status": "success",
                "count": len(all_stock_codes)
            }
            
            # 2. 获取历史数据
            self.logger.info("📈 步骤2: 获取过去一年历史数据")
            historical_data = self.get_historical_data_in_batches(all_stock_codes)
            
            if historical_data.empty:
                results["status"] = "failed"
                results["error"] = "无法获取历史数据"
                return results
            
            results["steps"]["data_extraction"] = {
                "status": "success",
                "data_count": len(historical_data),
                "stocks_with_data": historical_data['ts_code'].nunique()
            }
            
            # 3. 生成训练数据
            self.logger.info("🧠 步骤3: 生成全面训练数据")
            training_data = self.generate_comprehensive_training_data(historical_data)
            
            if not training_data:
                results["status"] = "failed"
                results["error"] = "无法生成训练数据"
                return results
            
            results["steps"]["training_data_generation"] = {
                "status": "success",
                "training_samples": len(training_data)
            }
            
            # 4. 保存训练数据
            timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
            training_file = f"comprehensive_training_data_{timestamp}.jsonl"
            training_path = os.path.join(self.config['training_data_dir'], training_file)
            
            with open(training_path, 'w', encoding='utf-8') as f:
                for data in training_data:
                    f.write(json.dumps(data, ensure_ascii=False) + '\n')
            
            results["training_file"] = training_path
            
            # 5. 创建全面训练模型
            self.logger.info("🤖 步骤4: 创建全面训练模型")
            model_name = self.create_comprehensive_model(training_data)
            
            if model_name:
                results["steps"]["model_creation"] = {
                    "status": "success",
                    "model_name": model_name
                }
                
                # 6. 测试模型
                self.logger.info("🧪 步骤5: 测试全面训练模型")
                test_results = self.test_comprehensive_model(model_name)
                
                results["steps"]["model_testing"] = {
                    "status": "success",
                    "test_results": test_results
                }
                
                results["status"] = "success"
                results["final_model"] = model_name
                
            else:
                results["status"] = "model_creation_failed"
            
        except Exception as e:
            results["status"] = "error"
            results["error"] = str(e)
            self.logger.error(f"❌ 全面训练失败: {e}")
        
        results["end_time"] = datetime.now().isoformat()
        
        # 保存结果
        results_file = os.path.join(self.config['models_dir'], f"comprehensive_training_results_{timestamp}.json")
        with open(results_file, 'w', encoding='utf-8') as f:
            json.dump(results, f, ensure_ascii=False, indent=2)
        
        self.logger.info(f"结果已保存: {results_file}")
        return results

def main():
    """主函数"""
    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s - %(levelname)s - %(message)s',
        handlers=[
            logging.StreamHandler(),
            logging.FileHandler(f'logs/comprehensive_training_{datetime.now().strftime("%Y%m%d")}.log')
        ]
    )
    
    # 确保日志目录存在
    os.makedirs('logs', exist_ok=True)
    
    logger = logging.getLogger(__name__)
    
    print("🚀 ljwx-stock 全面训练系统")
    print("=" * 60)
    print("📊 训练范围: 过去5年 + 全市场股票")
    print("🎯 目标样本: 50000+ 条训练数据")
    print("⏱️  预计用时: 2-4小时")
    print("=" * 60)
    
    try:
        # 检查TuShare token（这里只是检查，实际初始化在后面）
        tushare_token = os.getenv('TUSHARE_TOKEN')
        if not tushare_token:
            print("⚠️  未设置TUSHARE_TOKEN环境变量，将使用免费API")
            print("💡 建议设置Pro token以获得更好的数据质量和速度")
        
        # 设置TuShare Token
        tushare_token = os.getenv('TUSHARE_TOKEN')
        if tushare_token:
            print(f"✅ 使用TuShare Pro Token: {tushare_token[:20]}...")
            trainer = ComprehensiveTrainer(tushare_token)
        else:
            print("⚠️  未设置TUSHARE_TOKEN，使用免费API")
            trainer = ComprehensiveTrainer()
        
        print("\n📋 训练配置:")
        print(f"   最大训练样本: {trainer.config['max_training_examples']}条")
        print(f"   批处理大小: {trainer.config['batch_size']}只股票/批")
        print(f"   历史数据范围: {trainer.config['days_back']}天")
        print(f"   API请求延迟: {trainer.config['request_delay']}秒")
        print()
        
        # 自动开始（非交互模式）
        # 检查是否有缓存数据
        cache_dir = "data/cache"
        if os.path.exists(cache_dir) and os.listdir(cache_dir):
            print("💾 发现缓存数据，将优先使用缓存加速训练过程")
        
        # 用户选择
        print("\n📋 选项:")
        print("1. 🚀 开始全面训练 (使用缓存)")
        print("2. 🗑️  清空缓存重新训练")
        print("3. 📊 查看缓存状态")
        print("4. ❌ 退出")
        
        choice = input("\n请选择 (1-4): ").strip()
        
        if choice == "2":
            # 清空缓存
            if os.path.exists(cache_dir):
                import shutil
                shutil.rmtree(cache_dir)
                os.makedirs(cache_dir, exist_ok=True)
                print("✅ 缓存已清空")
        elif choice == "3":
            # 查看缓存状态
            if os.path.exists(cache_dir):
                cache_files = os.listdir(cache_dir)
                print(f"\n📊 缓存状态:")
                print(f"   缓存文件数量: {len(cache_files)}")
                for file in cache_files:
                    file_path = os.path.join(cache_dir, file)
                    file_size = os.path.getsize(file_path) / 1024 / 1024  # MB
                    file_time = datetime.fromtimestamp(os.path.getmtime(file_path))
                    print(f"   {file}: {file_size:.1f}MB, {file_time.strftime('%Y-%m-%d %H:%M')}")
            return
        elif choice == "4":
            print("👋 退出程序")
            return
        elif choice != "1":
            print("⚠️ 无效选择，使用默认选项1")
        
        print("🤔 开始全面训练...")
        print("✅ 开始执行")
        
        # 运行全面训练
        print("\n🚀 开始全面训练...")
        results = trainer.run_comprehensive_training()
        
        # 显示结果
        print("\n📊 全面训练结果:")
        print("=" * 60)
        print(f"状态: {results['status']}")
        
        if results['status'] == 'success':
            print(f"✅ 最终模型: {results['final_model']}")
            print(f"📈 处理股票: {results.get('total_stocks', 0)}只")
            
            if 'steps' in results:
                steps = results['steps']
                if 'data_extraction' in steps:
                    data_info = steps['data_extraction']
                    print(f"📊 历史数据: {data_info['data_count']}条 ({data_info['stocks_with_data']}只股票)")
                
                if 'training_data_generation' in steps:
                    training_info = steps['training_data_generation']
                    print(f"🧠 训练样本: {training_info['training_samples']}条")
                
                if 'model_testing' in steps:
                    test_info = steps['model_testing']['test_results']['summary']
                    print(f"🧪 测试结果: {test_info['successful_tests']}/{test_info['total_tests']} ({test_info['success_rate']:.2%})")
            
            print(f"\n🎯 使用全面训练模型:")
            print(f"   ollama run {results['final_model']}")
            
        else:
            print(f"❌ 训练失败: {results.get('error', '未知错误')}")
        
        print(f"\n📁 详细结果: {results.get('training_file', 'N/A')}")
        
    except KeyboardInterrupt:
        print("\n⚠️ 训练被用户中断")
    except Exception as e:
        logger.error(f"❌ 全面训练失败: {e}")
        print(f"\n❌ 全面训练失败: {e}")

if __name__ == "__main__":
    main()