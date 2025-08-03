#!/usr/bin/env python3
"""
ljwx-stock持续训练测试脚本
使用模拟数据测试持续训练功能
"""

import os
import sys
import json
import logging
import tempfile
import subprocess
from datetime import datetime, timedelta
from typing import List, Dict
import pandas as pd
import numpy as np

class MockDataGenerator:
    """模拟数据生成器"""
    
    def __init__(self):
        self.stock_codes = [
            '000002.SZ', '000003.SZ', '000004.SZ', '000005.SZ', '000006.SZ',
            '600001.SH', '600002.SH', '600003.SH', '600004.SH', '600005.SH'
        ]
    
    def generate_mock_stock_data(self, stock_codes: List[str], days: int = 30) -> pd.DataFrame:
        """生成模拟股票数据"""
        data = []
        base_date = datetime.now() - timedelta(days=days)
        
        for stock_code in stock_codes:
            base_price = np.random.uniform(8.0, 50.0)
            
            for i in range(days):
                trade_date = base_date + timedelta(days=i)
                
                # 生成价格数据（随机游走）
                price_change = np.random.normal(0, 0.03)
                open_price = base_price * (1 + price_change)
                high_price = open_price * (1 + abs(np.random.normal(0, 0.02)))
                low_price = open_price * (1 - abs(np.random.normal(0, 0.02)))
                close_price = open_price * (1 + np.random.normal(0, 0.02))
                
                volume = np.random.randint(100000, 10000000)
                
                data.append({
                    'ts_code': stock_code,
                    'trade_date': trade_date.strftime('%Y%m%d'),
                    'open': round(open_price, 2),
                    'high': round(high_price, 2),
                    'low': round(low_price, 2),
                    'close': round(close_price, 2),
                    'vol': volume,
                    'ma5': round(open_price * (1 + np.random.normal(0, 0.01)), 2),
                    'ma20': round(open_price * (1 + np.random.normal(0, 0.015)), 2),
                    'rsi': round(np.random.uniform(20, 80), 2),
                    'macd': round(np.random.normal(0, 0.1), 4),
                    'volatility': round(np.random.uniform(0.1, 0.8), 2)
                })
                
                base_price = close_price
        
        return pd.DataFrame(data)

class TestContinuousTrainer:
    """测试持续训练器"""
    
    def __init__(self):
        self.logger = logging.getLogger(__name__)
        self.mock_generator = MockDataGenerator()
        
        # 添加项目路径
        sys.path.append(os.path.dirname(os.path.abspath(__file__)))
        
        try:
            from llm.llm_training_data_generator import LLMTrainingDataGenerator
            self.data_generator = LLMTrainingDataGenerator()
        except ImportError as e:
            self.logger.error(f"导入训练数据生成器失败: {e}")
            self.data_generator = None
        
        self.config = {
            'training_data_dir': 'data/llm_training',
            'models_dir': 'data/models',
            'max_training_examples': 50
        }
        
        os.makedirs(self.config['training_data_dir'], exist_ok=True)
        os.makedirs(self.config['models_dir'], exist_ok=True)
    
    def generate_mock_training_data(self) -> List[Dict]:
        """生成模拟训练数据"""
        self.logger.info("🎯 生成模拟训练数据")
        
        # 生成模拟股票数据
        mock_stock_data = self.mock_generator.generate_mock_stock_data(
            self.mock_generator.stock_codes[:5], 
            days=20
        )
        
        if self.data_generator is None:
            # 如果无法导入生成器，创建简单的模拟数据
            return self.create_simple_mock_training_data(mock_stock_data)
        
        try:
            # 使用真实的生成器
            training_examples = self.data_generator.create_training_examples(
                mock_stock_data, 
                max_examples=self.config['max_training_examples']
            )
            return training_examples
        except Exception as e:
            self.logger.warning(f"使用真实生成器失败: {e}，使用简单模拟数据")
            return self.create_simple_mock_training_data(mock_stock_data)
    
    def create_simple_mock_training_data(self, stock_data: pd.DataFrame) -> List[Dict]:
        """创建简单的模拟训练数据"""
        training_data = []
        
        for _, row in stock_data.iterrows():
            stock_code = row['ts_code']
            price = row['close']
            rsi = row['rsi']
            macd = row['macd']
            
            # 生成投资建议样本
            input_text = f"""股票代码: {stock_code}
当前价格: {price}元
RSI: {rsi}
MACD: {macd}
5日均线: {row['ma5']}元
20日均线: {row['ma20']}元
波动率: {row['volatility']}"""
            
            # 简单的分析逻辑
            if rsi > 70:
                advice = "技术指标显示超买，建议谨慎操作，注意风险控制。"
            elif rsi < 30:
                advice = "技术指标显示超卖，可能存在反弹机会，建议关注。"
            else:
                advice = "技术指标处于正常区间，建议结合基本面分析做出投资决策。"
            
            training_data.append({
                "instruction": "分析以下股票的技术指标和价格走势，并给出投资建议：",
                "input": input_text,
                "output": advice,
                "metadata": {
                    "type": "investment_advice",
                    "ts_code": stock_code,
                    "trade_date": row['trade_date']
                }
            })
            
            if len(training_data) >= self.config['max_training_examples']:
                break
        
        return training_data
    
    def create_test_model(self, base_model: str, training_data: List[Dict], model_name: str) -> bool:
        """创建测试模型"""
        try:
            self.logger.info(f"🚀 创建测试模型: {model_name}")
            
            system_prompt = """你是ljwx-stock，一个专业的股票投资分析助手。基于技术分析为投资者提供专业建议。"""
            
            # 构建Modelfile
            modelfile_content = f"""FROM {base_model}
SYSTEM "{system_prompt}"
PARAMETER temperature 0.7
PARAMETER top_p 0.9
PARAMETER repeat_penalty 1.1

"""
            
            # 添加训练样本
            for i, example in enumerate(training_data[:20]):  # 限制样本数量
                instruction = example.get('instruction', '')
                input_text = example.get('input', '')
                output_text = example.get('output', '')
                
                user_message = f"{instruction}\n\n{input_text}" if input_text else instruction
                
                modelfile_content += f'MESSAGE user "{user_message}"\n'
                modelfile_content += f'MESSAGE assistant "{output_text}"\n\n'
            
            # 创建临时Modelfile
            with tempfile.NamedTemporaryFile(mode='w', suffix='.modelfile', delete=False) as f:
                f.write(modelfile_content)
                modelfile_path = f.name
            
            try:
                result = subprocess.run(
                    ['ollama', 'create', model_name, '-f', modelfile_path],
                    capture_output=True,
                    text=True,
                    timeout=300
                )
                
                if result.returncode == 0:
                    self.logger.info(f"✅ 测试模型创建成功: {model_name}")
                    return True
                else:
                    self.logger.error(f"❌ 测试模型创建失败: {result.stderr}")
                    return False
                    
            finally:
                os.unlink(modelfile_path)
                
        except Exception as e:
            self.logger.error(f"❌ 创建测试模型时出错: {e}")
            return False
    
    def test_model_simple(self, model_name: str) -> Dict:
        """简单测试模型"""
        test_input = "分析股票000001.SZ，当前价格10.50元，RSI=60，给出投资建议"
        
        try:
            result = subprocess.run(
                ['ollama', 'run', model_name],
                input=test_input,
                capture_output=True,
                text=True,
                timeout=30
            )
            
            if result.returncode == 0 and result.stdout.strip():
                return {
                    "success": True,
                    "response": result.stdout.strip()[:200] + "..."
                }
            else:
                return {
                    "success": False,
                    "error": result.stderr
                }
                
        except Exception as e:
            return {
                "success": False,
                "error": str(e)
            }
    
    def run_test_continuous_training(self):
        """运行测试持续训练"""
        self.logger.info("🧪 开始测试持续训练流程")
        
        results = {
            "start_time": datetime.now().isoformat(),
            "status": "running",
            "steps": {}
        }
        
        try:
            # 1. 生成模拟训练数据
            self.logger.info("📊 步骤1: 生成模拟训练数据")
            training_data = self.generate_mock_training_data()
            
            if not training_data:
                results["status"] = "failed"
                results["error"] = "无法生成训练数据"
                return results
            
            results["steps"]["data_generation"] = {
                "status": "success",
                "data_count": len(training_data)
            }
            
            # 2. 保存训练数据
            timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
            training_file = f"test_training_data_{timestamp}.jsonl"
            training_path = os.path.join(self.config['training_data_dir'], training_file)
            
            with open(training_path, 'w', encoding='utf-8') as f:
                for data in training_data:
                    f.write(json.dumps(data, ensure_ascii=False) + '\n')
            
            self.logger.info(f"📁 训练数据已保存: {training_path}")
            
            # 3. 创建测试模型
            self.logger.info("🤖 步骤2: 创建测试模型")
            test_model_name = f"ljwx-stock-test-{timestamp}"
            
            success = self.create_test_model(
                base_model="ljwx-stock",
                training_data=training_data,
                model_name=test_model_name
            )
            
            if success:
                results["steps"]["model_creation"] = {
                    "status": "success",
                    "model_name": test_model_name
                }
                
                # 4. 测试模型
                self.logger.info("🧪 步骤3: 测试模型")
                test_result = self.test_model_simple(test_model_name)
                
                results["steps"]["model_testing"] = {
                    "status": "success" if test_result["success"] else "failed",
                    "test_result": test_result
                }
                
                results["status"] = "success"
                results["final_model"] = test_model_name
                
            else:
                results["status"] = "model_creation_failed"
            
        except Exception as e:
            results["status"] = "error"
            results["error"] = str(e)
            self.logger.error(f"❌ 测试失败: {e}")
        
        results["end_time"] = datetime.now().isoformat()
        
        # 保存结果
        results_file = os.path.join(self.config['models_dir'], f"test_results_{timestamp}.json")
        with open(results_file, 'w', encoding='utf-8') as f:
            json.dump(results, f, ensure_ascii=False, indent=2)
        
        return results

def main():
    """主函数"""
    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s - %(levelname)s - %(message)s'
    )
    
    print("🧪 ljwx-stock 持续训练测试系统")
    print("=" * 60)
    
    trainer = TestContinuousTrainer()
    results = trainer.run_test_continuous_training()
    
    print("\n📊 测试结果:")
    print("=" * 60)
    print(f"状态: {results['status']}")
    
    if results['status'] == 'success':
        print(f"✅ 测试模型: {results['final_model']}")
        
        if 'steps' in results:
            for step_name, step_result in results['steps'].items():
                print(f"📋 {step_name}: {step_result['status']}")
                if step_name == 'data_generation':
                    print(f"   数据量: {step_result['data_count']}条")
                elif step_name == 'model_testing' and 'test_result' in step_result:
                    test = step_result['test_result']
                    print(f"   测试结果: {'通过' if test['success'] else '失败'}")
        
        print(f"\n🎯 使用测试模型:")
        print(f"   ollama run {results['final_model']}")
    
    else:
        print(f"❌ 测试失败: {results.get('error', '未知错误')}")

if __name__ == "__main__":
    main()