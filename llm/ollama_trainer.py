"""
Ollama大模型训练器
用于对lingjingwanxiang:70b模型进行微调训练
"""

import requests
import json
import logging
from typing import Dict, List, Optional, Any
import time
import os
from datetime import datetime
import subprocess
import asyncio
import aiohttp

class OllamaTrainer:
    """Ollama模型训练器"""
    
    def __init__(self, base_url: str = "http://localhost:11434"):
        """
        初始化Ollama训练器
        
        Args:
            base_url: Ollama服务地址
        """
        self.base_url = base_url
        self.logger = logging.getLogger(__name__)
        
        # 检查Ollama服务状态
        self._check_ollama_service()
        
    def _check_ollama_service(self):
        """检查Ollama服务状态"""
        try:
            response = requests.get(f"{self.base_url}/api/tags", timeout=5)
            if response.status_code == 200:
                self.logger.info("✅ Ollama服务连接成功")
                models = response.json().get('models', [])
                self.logger.info(f"   可用模型: {len(models)}个")
                for model in models:
                    if 'lingjingwanxiang' in model['name']:
                        self.logger.info(f"   🎯 找到目标模型: {model['name']}")
            else:
                self.logger.error(f"❌ Ollama服务连接失败: {response.status_code}")
        except Exception as e:
            self.logger.error(f"❌ 无法连接到Ollama服务: {e}")
            
    def list_models(self) -> List[Dict]:
        """获取可用模型列表"""
        try:
            response = requests.get(f"{self.base_url}/api/tags")
            if response.status_code == 200:
                return response.json().get('models', [])
            return []
        except Exception as e:
            self.logger.error(f"获取模型列表失败: {e}")
            return []
    
    def check_model_exists(self, model_name: str) -> bool:
        """检查模型是否存在"""
        models = self.list_models()
        return any(model['name'] == model_name for model in models)
    
    def create_model_from_template(self, 
                                 base_model: str,
                                 new_model_name: str, 
                                 model_file_content: str) -> bool:
        """
        从模板创建新模型
        
        Args:
            base_model: 基础模型名称
            new_model_name: 新模型名称
            model_file_content: Modelfile内容
            
        Returns:
            是否创建成功
        """
        try:
            data = {
                "name": new_model_name,
                "modelfile": model_file_content
            }
            
            response = requests.post(
                f"{self.base_url}/api/create",
                json=data,
                stream=True
            )
            
            if response.status_code == 200:
                self.logger.info(f"✅ 开始创建模型: {new_model_name}")
                
                # 处理流式响应
                for line in response.iter_lines():
                    if line:
                        try:
                            progress = json.loads(line)
                            if 'status' in progress:
                                self.logger.info(f"   {progress['status']}")
                        except json.JSONDecodeError:
                            continue
                
                self.logger.info(f"✅ 模型创建完成: {new_model_name}")
                return True
            else:
                self.logger.error(f"❌ 模型创建失败: {response.status_code}")
                return False
                
        except Exception as e:
            self.logger.error(f"❌ 创建模型时出错: {e}")
            return False
    
    def generate_response(self, 
                         model: str, 
                         prompt: str, 
                         system_message: str = None,
                         temperature: float = 0.7,
                         top_p: float = 0.9,
                         max_tokens: int = 2048) -> Optional[str]:
        """
        生成模型响应
        
        Args:
            model: 模型名称
            prompt: 输入提示
            system_message: 系统消息
            temperature: 温度参数
            top_p: top_p参数
            max_tokens: 最大token数
            
        Returns:
            模型响应文本
        """
        try:
            messages = []
            if system_message:
                messages.append({"role": "system", "content": system_message})
            messages.append({"role": "user", "content": prompt})
            
            data = {
                "model": model,
                "messages": messages,
                "options": {
                    "temperature": temperature,
                    "top_p": top_p,
                    "num_predict": max_tokens
                },
                "stream": False
            }
            
            response = requests.post(
                f"{self.base_url}/api/chat",
                json=data,
                timeout=120
            )
            
            if response.status_code == 200:
                result = response.json()
                return result.get('message', {}).get('content', '')
            else:
                self.logger.error(f"❌ 生成响应失败: {response.status_code}")
                return None
                
        except Exception as e:
            self.logger.error(f"❌ 生成响应时出错: {e}")
            return None
    
    def fine_tune_with_training_data(self, 
                                   base_model: str,
                                   training_data_file: str,
                                   fine_tuned_model_name: str,
                                   system_prompt: str = None) -> bool:
        """
        使用训练数据对模型进行微调
        
        Args:
            base_model: 基础模型名称
            training_data_file: 训练数据文件路径
            fine_tuned_model_name: 微调后模型名称
            system_prompt: 系统提示
            
        Returns:
            是否微调成功
        """
        try:
            # 检查训练数据文件
            if not os.path.exists(training_data_file):
                self.logger.error(f"❌ 训练数据文件不存在: {training_data_file}")
                return False
            
            # 读取训练数据
            training_examples = self._load_training_data(training_data_file)
            if not training_examples:
                self.logger.error("❌ 训练数据为空")
                return False
            
            self.logger.info(f"📚 加载训练数据: {len(training_examples)}个样本")
            
            # 创建Modelfile
            modelfile_content = self._create_modelfile_with_training_data(
                base_model, training_examples, system_prompt
            )
            
            # 创建微调模型
            return self.create_model_from_template(
                base_model, fine_tuned_model_name, modelfile_content
            )
            
        except Exception as e:
            self.logger.error(f"❌ 微调过程出错: {e}")
            return False
    
    def _load_training_data(self, file_path: str) -> List[Dict]:
        """加载训练数据"""
        training_examples = []
        
        try:
            if file_path.endswith('.jsonl'):
                # JSONL格式
                with open(file_path, 'r', encoding='utf-8') as f:
                    for line in f:
                        if line.strip():
                            example = json.loads(line)
                            training_examples.append(example)
            elif file_path.endswith('.json'):
                # JSON格式
                with open(file_path, 'r', encoding='utf-8') as f:
                    data = json.load(f)
                    if isinstance(data, list):
                        training_examples = data
                    else:
                        training_examples = [data]
            
        except Exception as e:
            self.logger.error(f"❌ 加载训练数据失败: {e}")
            
        return training_examples
    
    def _create_modelfile_with_training_data(self, 
                                           base_model: str, 
                                           training_examples: List[Dict],
                                           system_prompt: str = None) -> str:
        """创建包含训练数据的Modelfile"""
        
        # 默认系统提示
        if not system_prompt:
            system_prompt = """你是一个专业的股票投资分析师，具有丰富的金融市场经验。
你的任务是：
1. 分析股票的技术指标和价格走势
2. 提供专业的投资建议和风险评估
3. 基于历史数据预测价格趋势
4. 给出明确、实用的交易建议

请始终保持客观、理性的分析态度，并提醒用户注意投资风险。"""
        
        # 构建Modelfile
        modelfile_lines = [
            f"FROM {base_model}",
            "",
            f"SYSTEM \"\"\"{system_prompt}\"\"\"",
            ""
        ]
        
        # 添加训练样本作为示例（选择前20个高质量样本）
        selected_examples = training_examples[:20]
        
        for i, example in enumerate(selected_examples):
            instruction = example.get('instruction', '')
            input_text = example.get('input', '')
            output_text = example.get('output', '')
            
            if instruction and input_text and output_text:
                # 组合指令和输入
                user_message = f"{instruction}\n\n{input_text}"
                
                modelfile_lines.extend([
                    f"MESSAGE user \"\"\"{user_message}\"\"\"",
                    f"MESSAGE assistant \"\"\"{output_text}\"\"\"",
                    ""
                ])
        
        # 添加参数设置
        modelfile_lines.extend([
            "PARAMETER temperature 0.7",
            "PARAMETER top_p 0.9",
            "PARAMETER top_k 40",
            "PARAMETER repeat_penalty 1.1",
            "PARAMETER num_predict 2048"
        ])
        
        return "\n".join(modelfile_lines)
    
    def test_fine_tuned_model(self, model_name: str, test_cases: List[Dict]) -> Dict[str, Any]:
        """
        测试微调后的模型
        
        Args:
            model_name: 模型名称
            test_cases: 测试用例列表
            
        Returns:
            测试结果
        """
        results = {
            'model_name': model_name,
            'test_time': datetime.now().isoformat(),
            'test_cases': [],
            'summary': {}
        }
        
        successful_tests = 0
        total_response_time = 0
        
        for i, test_case in enumerate(test_cases):
            self.logger.info(f"🧪 测试用例 {i+1}/{len(test_cases)}")
            
            start_time = time.time()
            response = self.generate_response(
                model=model_name,
                prompt=test_case.get('prompt', ''),
                system_message=test_case.get('system_message')
            )
            response_time = time.time() - start_time
            
            test_result = {
                'input': test_case.get('prompt', ''),
                'expected_type': test_case.get('expected_type', ''),
                'response': response,
                'response_time': response_time,
                'success': response is not None and len(response) > 0
            }
            
            results['test_cases'].append(test_result)
            
            if test_result['success']:
                successful_tests += 1
                total_response_time += response_time
        
        # 计算摘要统计
        results['summary'] = {
            'total_tests': len(test_cases),
            'successful_tests': successful_tests,
            'success_rate': successful_tests / len(test_cases) if test_cases else 0,
            'average_response_time': total_response_time / successful_tests if successful_tests > 0 else 0
        }
        
        self.logger.info(f"✅ 测试完成: 成功率 {results['summary']['success_rate']:.2%}")
        
        return results
    
    def save_test_results(self, results: Dict[str, Any], output_file: str):
        """保存测试结果"""
        os.makedirs(os.path.dirname(output_file), exist_ok=True)
        
        with open(output_file, 'w', encoding='utf-8') as f:
            json.dump(results, f, ensure_ascii=False, indent=2)
        
        self.logger.info(f"✅ 测试结果已保存: {output_file}")


def create_test_cases() -> List[Dict]:
    """创建测试用例"""
    return [
        {
            'prompt': """分析以下股票的技术指标和价格走势，并给出投资建议：

股票代码: 000001.SZ
交易日期: 2024-06-15
开盘价: 12.50
最高价: 12.80
最低价: 12.30
收盘价: 12.75
成交量: 8,500,000

技术指标:
5日均线: 12.60
20日均线: 12.40
RSI: 65.50
MACD: 0.0820
波动率: 0.25""",
            'expected_type': 'investment_advice'
        },
        {
            'prompt': """根据历史价格数据，预测该股票未来的价格趋势：

股票代码: 600000.SH
当前价格: 15.20
5日涨跌幅: +3.50%
20日涨跌幅: +8.20%
RSI: 72.0
MACD: 0.1200""",
            'expected_type': 'price_prediction'
        },
        {
            'prompt': """评估投资该股票的风险等级：

股票代码: 002001.SZ
波动率: 0.45
RSI: 85.0
日涨跌幅: +9.80%
成交量放大: 300%""",
            'expected_type': 'risk_assessment'
        }
    ]


def main():
    """主函数 - 完整的训练流程"""
    logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
    
    # 初始化训练器
    trainer = OllamaTrainer()
    
    # 检查基础模型
    base_model = "lingjingwanxiang:70b"
    if not trainer.check_model_exists(base_model):
        logging.error(f"❌ 基础模型不存在: {base_model}")
        return
    
    logging.info(f"✅ 找到基础模型: {base_model}")
    
    # 查找训练数据文件
    training_data_dir = "data/llm_training"
    if os.path.exists(training_data_dir):
        jsonl_files = [f for f in os.listdir(training_data_dir) if f.endswith('.jsonl')]
        if jsonl_files:
            # 使用最新的训练数据文件
            latest_file = sorted(jsonl_files)[-1]
            training_data_file = os.path.join(training_data_dir, latest_file)
            logging.info(f"📂 使用训练数据: {training_data_file}")
        else:
            logging.error("❌ 未找到训练数据文件")
            return
    else:
        logging.error("❌ 训练数据目录不存在")
        return
    
    # 创建微调模型名称
    timestamp = datetime.now().strftime("%Y%m%d_%H%M")
    fine_tuned_model_name = f"lingjingwanxiang-stock-{timestamp}"
    
    # 执行微调
    logging.info(f"🚀 开始微调模型: {fine_tuned_model_name}")
    success = trainer.fine_tune_with_training_data(
        base_model=base_model,
        training_data_file=training_data_file,
        fine_tuned_model_name=fine_tuned_model_name
    )
    
    if success:
        logging.info(f"✅ 模型微调完成: {fine_tuned_model_name}")
        
        # 测试微调后的模型
        logging.info("🧪 开始测试微调后的模型...")
        test_cases = create_test_cases()
        
        test_results = trainer.test_fine_tuned_model(fine_tuned_model_name, test_cases)
        
        # 保存测试结果
        results_file = f"data/llm_training/test_results_{timestamp}.json"
        trainer.save_test_results(test_results, results_file)
        
        # 显示测试摘要
        summary = test_results['summary']
        print(f"\n🎯 微调模型测试结果:")
        print(f"   模型名称: {fine_tuned_model_name}")
        print(f"   测试用例: {summary['total_tests']}个")
        print(f"   成功率: {summary['success_rate']:.2%}")
        print(f"   平均响应时间: {summary['average_response_time']:.2f}秒")
        print(f"\n✅ 完整训练流程完成！")
        
    else:
        logging.error("❌ 模型微调失败")


if __name__ == "__main__":
    main()