#!/usr/bin/env python3
"""
Training script for ljwx-stock model using existing TuShare training data
"""

import os
import sys
import json
import logging
import tempfile
import subprocess
from datetime import datetime

class LjwxStockTrainer:
    """ljwx-stock model trainer"""
    
    def __init__(self):
        self.logger = logging.getLogger(__name__)
        
    def load_training_data(self, jsonl_file: str) -> list:
        """Load training data from JSONL file"""
        training_data = []
        
        try:
            with open(jsonl_file, 'r', encoding='utf-8') as f:
                for line in f:
                    if line.strip():
                        data = json.loads(line)
                        training_data.append(data)
            
            self.logger.info(f"✅ 加载训练数据: {len(training_data)}条")
            return training_data
            
        except Exception as e:
            self.logger.error(f"❌ 加载训练数据失败: {e}")
            return []
    
    def create_fine_tuned_modelfile(self, base_model: str, training_data: list) -> str:
        """Create Modelfile for fine-tuning with training examples"""
        
        system_prompt = """你是ljwx-stock，一个专业的股票投资分析助手。你具备以下核心能力：

🎯 **专业定位**
- 专注于A股市场分析和投资建议
- 基于技术分析和基本面分析提供专业意见
- 为投资者提供客观、理性的决策支持

📊 **分析能力**
1. **技术指标分析**：熟练解读K线、均线、RSI、MACD、布林带等技术指标
2. **价格走势预测**：基于历史数据和技术模式判断价格趋势
3. **风险评估**：客观评估投资风险等级和风险因素
4. **市场情绪分析**：理解市场心理和资金流向

💡 **服务原则**
- 始终提供客观、专业的分析意见
- 明确标注风险等级和注意事项
- 不做绝对化的投资承诺
- 提醒投资者理性投资、风险自担

📋 **输出格式**
- 分析结论简洁明了，逻辑清晰
- 提供具体的操作建议（买入/持有/卖出）
- 标明风险等级（低/中/高）
- 给出关键支撑位和阻力位

请记住：股市有风险，投资需谨慎。所有分析仅供参考，不构成投资建议。"""

        # Build Modelfile with training examples
        modelfile_content = f"""FROM {base_model}
SYSTEM "{system_prompt}"
PARAMETER temperature 0.7
PARAMETER top_p 0.9
PARAMETER top_k 40
PARAMETER repeat_penalty 1.1
PARAMETER num_predict 1024

"""
        
        # Add training examples as MESSAGE pairs (limited to first 50 for performance)
        for i, example in enumerate(training_data[:50]):
            instruction = example.get('instruction', '')
            input_text = example.get('input', '')
            output_text = example.get('output', '')
            
            # Combine instruction and input
            user_message = f"{instruction}\n\n{input_text}" if input_text else instruction
            
            # Add MESSAGE pairs
            modelfile_content += f'MESSAGE user "{user_message}"\n'
            modelfile_content += f'MESSAGE assistant "{output_text}"\n\n'
        
        return modelfile_content
    
    def fine_tune_model(self, base_model: str, training_data_file: str, model_name: str) -> bool:
        """Fine-tune the ljwx-stock model with training data"""
        try:
            self.logger.info(f"🚀 开始微调模型: {model_name}")
            
            # Load training data
            training_data = self.load_training_data(training_data_file)
            if not training_data:
                return False
            
            # Create Modelfile with training examples
            modelfile_content = self.create_fine_tuned_modelfile(base_model, training_data)
            
            # Create temporary Modelfile
            with tempfile.NamedTemporaryFile(mode='w', suffix='.modelfile', delete=False) as f:
                f.write(modelfile_content)
                modelfile_path = f.name
            
            try:
                # Use ollama create command
                self.logger.info(f"   使用训练数据: {len(training_data)}条")
                self.logger.info(f"   基础模型: {base_model}")
                
                result = subprocess.run(
                    ['ollama', 'create', model_name, '-f', modelfile_path],
                    capture_output=True,
                    text=True,
                    timeout=600  # 10分钟超时
                )
                
                if result.returncode == 0:
                    self.logger.info(f"✅ 模型微调完成: {model_name}")
                    return True
                else:
                    self.logger.error(f"❌ 模型微调失败: {result.stderr}")
                    return False
                    
            finally:
                # Clean up temporary file
                os.unlink(modelfile_path)
                
        except Exception as e:
            self.logger.error(f"❌ 微调模型时出错: {e}")
            return False
    
    def test_fine_tuned_model(self, model_name: str) -> dict:
        """Test the fine-tuned model"""
        test_cases = [
            {
                "input": "分析以下股票的技术指标和价格走势，并给出投资建议：\n\n股票代码: 000001.SZ\n交易日期: 2024-06-15\n开盘价: 12.50\n最高价: 12.80\n最低价: 12.30\n收盘价: 12.75\n成交量: 8,500,000\n\n技术指标:\n5日均线: 12.60\n20日均线: 12.40\nRSI: 65.50\nMACD: 0.0820\n波动率: 0.25",
                "expected_type": "investment_advice"
            },
            {
                "input": "根据历史价格数据，预测该股票未来的价格趋势：\n\n股票代码: 600000.SH\n当前价格: 15.20\n5日涨跌幅: +3.50%\n20日涨跌幅: +8.20%\nRSI: 72.0\nMACD: 0.1200",
                "expected_type": "price_prediction"
            },
            {
                "input": "评估投资该股票的风险等级：\n\n股票代码: 002001.SZ\n波动率: 0.45\nRSI: 85.0\n日涨跌幅: +9.80%\n成交量放大: 300%",
                "expected_type": "risk_assessment"
            }
        ]
        
        results = {
            "model_name": model_name,
            "test_time": datetime.now().isoformat(),
            "test_cases": [],
            "summary": {}
        }
        
        successful_tests = 0
        total_response_time = 0
        
        for i, test_case in enumerate(test_cases):
            self.logger.info(f"   测试用例 {i+1}/{len(test_cases)}")
            
            try:
                import time
                start_time = time.time()
                
                # Test with ollama run command
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
                    "expected_type": test_case["expected_type"],
                    "response": response[:500] + "..." if len(response) > 500 else response,
                    "response_time": response_time,
                    "success": success
                })
                
            except Exception as e:
                self.logger.error(f"   测试用例 {i+1} 失败: {e}")
                results["test_cases"].append({
                    "input": test_case["input"],
                    "expected_type": test_case["expected_type"],
                    "response": f"Error: {str(e)}",
                    "response_time": 0,
                    "success": False
                })
        
        # Calculate summary
        results["summary"] = {
            "total_tests": len(test_cases),
            "successful_tests": successful_tests,
            "success_rate": successful_tests / len(test_cases),
            "average_response_time": total_response_time / len(test_cases) if test_cases else 0
        }
        
        return results

def main():
    """Main function"""
    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s - %(levelname)s - %(message)s'
    )
    
    logger = logging.getLogger(__name__)
    
    print("🚀 ljwx-stock 模型训练系统")
    print("=" * 50)
    
    try:
        trainer = LjwxStockTrainer()
        
        # Find the latest training data file
        training_data_dir = "data/llm_training"
        if not os.path.exists(training_data_dir):
            print(f"❌ 训练数据目录不存在: {training_data_dir}")
            return
        
        # Get the latest .jsonl file
        jsonl_files = [f for f in os.listdir(training_data_dir) if f.endswith('.jsonl')]
        if not jsonl_files:
            print(f"❌ 未找到训练数据文件 (.jsonl)")
            return
        
        # Use the latest file
        latest_file = sorted(jsonl_files)[-1]
        training_data_file = os.path.join(training_data_dir, latest_file)
        
        print(f"📊 使用训练数据文件: {latest_file}")
        
        # Create fine-tuned model name
        timestamp = datetime.now().strftime("%Y%m%d_%H%M")
        fine_tuned_model = f"ljwx-stock-trained-{timestamp}"
        
        print(f"🎯 微调模型名称: {fine_tuned_model}")
        print()
        
        # Fine-tune the model
        success = trainer.fine_tune_model(
            base_model="ljwx-stock",
            training_data_file=training_data_file,
            model_name=fine_tuned_model
        )
        
        if success:
            print(f"\n✅ 模型微调成功: {fine_tuned_model}")
            
            # Test the fine-tuned model
            print(f"\n🧪 测试微调后的模型...")
            test_results = trainer.test_fine_tuned_model(fine_tuned_model)
            
            # Save test results
            results_file = f"data/llm_training/ljwx_test_results_{timestamp}.json"
            os.makedirs(os.path.dirname(results_file), exist_ok=True)
            with open(results_file, 'w', encoding='utf-8') as f:
                json.dump(test_results, f, ensure_ascii=False, indent=2)
            
            # Display results
            summary = test_results['summary']
            print(f"\n📊 测试结果:")
            print(f"   测试用例: {summary['total_tests']}个")
            print(f"   成功数量: {summary['successful_tests']}个")
            print(f"   成功率: {summary['success_rate']:.2%}")
            print(f"   平均响应时间: {summary['average_response_time']:.2f}秒")
            
            print(f"\n🎯 使用方法:")
            print(f"   ollama run {fine_tuned_model}")
            print(f"\n📋 测试结果已保存: {results_file}")
            
        else:
            print(f"\n❌ 模型微调失败")
            
    except KeyboardInterrupt:
        print("\n⚠️ 训练被用户中断")
    except Exception as e:
        logger.error(f"❌ 训练失败: {e}")
        print(f"\n❌ 训练失败: {e}")

if __name__ == "__main__":
    main()