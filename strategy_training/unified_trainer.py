#!/usr/bin/env python3
"""
统一模型训练器
Unified Model Training Pipeline for Strategy-Driven AI Recommendations
"""

import json
import os
import sys
import subprocess
import logging
import pandas as pd
import numpy as np
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Tuple, Any
from pathlib import Path
import uuid

# 添加项目路径
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from strategy_training.dataset_generator import StrategyDatasetGenerator, TrainingSample
from strategy.market_mainstream_strategies import get_all_strategy_ids, MAINSTREAM_STRATEGIES

class UnifiedModelTrainer:
    """统一模型训练器"""
    
    def __init__(self, tushare_token: str = None, base_model: str = "lingjingwanxiang:70b"):
        self.logger = logging.getLogger(__name__)
        self.tushare_token = tushare_token
        self.base_model = base_model
        self.model_name = "ljwx-stock-unified"
        
        # 目录配置
        self.datasets_dir = "strategy_datasets"
        self.training_dir = "unified_training"
        self.models_dir = "trained_models"
        
        # 创建目录
        for dir_path in [self.datasets_dir, self.training_dir, self.models_dir]:
            os.makedirs(dir_path, exist_ok=True)
        
        # 训练配置
        self.training_config = {
            "epochs": 5,
            "batch_size": 16,
            "learning_rate": 1e-5,
            "max_tokens": 4096,
            "temperature": 0.7,
            "validation_split": 0.2
        }
        
        self.dataset_generator = None
    
    def prepare_unified_dataset(self, strategy_ids: List[str] = None, 
                              num_stocks_per_strategy: int = 30,
                              time_range: int = 180) -> str:
        """准备统一训练数据集"""
        self.logger.info("开始准备统一训练数据集")
        
        if strategy_ids is None:
            strategy_ids = get_all_strategy_ids()
        
        # 初始化数据集生成器
        if self.dataset_generator is None:
            self.dataset_generator = StrategyDatasetGenerator(self.tushare_token)
        
        unified_samples = []
        strategy_stats = {}
        
        for i, strategy_id in enumerate(strategy_ids):
            try:
                self.logger.info(f"处理策略 {i+1}/{len(strategy_ids)}: {strategy_id}")
                
                # 检查是否已有数据集文件
                dataset_file = os.path.join(self.datasets_dir, f"{strategy_id}_dataset.jsonl")
                
                if os.path.exists(dataset_file):
                    # 加载已有数据集
                    self.logger.info(f"加载已有数据集: {dataset_file}")
                    strategy_samples = self._load_dataset_from_file(dataset_file)
                else:
                    # 生成新数据集
                    self.logger.info(f"生成新数据集: {strategy_id}")
                    strategy_samples = self.dataset_generator.generate_strategy_dataset(
                        strategy_id, num_stocks_per_strategy, time_range
                    )
                
                # 添加策略标识
                for sample in strategy_samples:
                    sample.input_text = f"[策略:{strategy_id}] {sample.input_text}"
                
                unified_samples.extend(strategy_samples)
                strategy_stats[strategy_id] = len(strategy_samples)
                
                self.logger.info(f"策略 {strategy_id} 贡献 {len(strategy_samples)} 个样本")
                
            except Exception as e:
                self.logger.error(f"处理策略 {strategy_id} 失败: {e}")
                continue
        
        # 保存统一数据集
        unified_dataset_file = self._save_unified_dataset(unified_samples, strategy_stats)
        
        self.logger.info(f"统一数据集准备完成，共 {len(unified_samples)} 个样本")
        self.logger.info(f"数据集已保存到: {unified_dataset_file}")
        
        return unified_dataset_file
    
    def _load_dataset_from_file(self, file_path: str) -> List[TrainingSample]:
        """从文件加载数据集"""
        samples = []
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                for line in f:
                    if line.strip():
                        sample_data = json.loads(line)
                        sample = TrainingSample(**sample_data)
                        samples.append(sample)
            
            self.logger.info(f"从 {file_path} 加载了 {len(samples)} 个样本")
            return samples
            
        except Exception as e:
            self.logger.error(f"加载数据集文件 {file_path} 失败: {e}")
            return []
    
    def _save_unified_dataset(self, samples: List[TrainingSample], 
                            strategy_stats: Dict[str, int]) -> str:
        """保存统一数据集"""
        timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
        
        # 保存JSONL格式的训练数据
        jsonl_file = os.path.join(self.training_dir, f"unified_dataset_{timestamp}.jsonl")
        
        with open(jsonl_file, 'w', encoding='utf-8') as f:
            for sample in samples:
                # 转换为Ollama训练格式
                training_sample = {
                    "prompt": sample.input_text,
                    "response": sample.output_text,
                    "metadata": {
                        "strategy_id": sample.strategy_id,
                        "strategy_type": sample.strategy_type,
                        "stock_code": sample.stock_code,
                        "date": sample.date,
                        "confidence": sample.confidence
                    }
                }
                json.dump(training_sample, f, ensure_ascii=False)
                f.write('\n')
        
        # 保存统计信息
        stats_file = os.path.join(self.training_dir, f"unified_dataset_stats_{timestamp}.json")
        
        stats = {
            "total_samples": len(samples),
            "strategy_distribution": strategy_stats,
            "signal_distribution": self._get_unified_signal_distribution(samples),
            "confidence_stats": self._get_unified_confidence_stats(samples),
            "generated_at": datetime.now().isoformat(),
            "training_config": self.training_config
        }
        
        with open(stats_file, 'w', encoding='utf-8') as f:
            json.dump(stats, f, ensure_ascii=False, indent=2)
        
        self.logger.info(f"统计信息已保存到: {stats_file}")
        
        return jsonl_file
    
    def _get_unified_signal_distribution(self, samples: List[TrainingSample]) -> Dict[str, int]:
        """获取统一信号分布"""
        distribution = {}
        for sample in samples:
            signal_type = sample.signal_type
            distribution[signal_type] = distribution.get(signal_type, 0) + 1
        return distribution
    
    def _get_unified_confidence_stats(self, samples: List[TrainingSample]) -> Dict[str, float]:
        """获取统一置信度统计"""
        confidences = [sample.confidence for sample in samples]
        return {
            "mean": float(np.mean(confidences)),
            "std": float(np.std(confidences)),
            "min": float(np.min(confidences)),
            "max": float(np.max(confidences))
        }
    
    def train_unified_model(self, dataset_file: str) -> str:
        """训练统一模型"""
        self.logger.info(f"开始训练统一模型，使用数据集: {dataset_file}")
        
        try:
            # 检查Ollama是否可用
            if not self._check_ollama_available():
                raise RuntimeError("Ollama不可用，请确保Ollama已启动")
            
            # 创建Modelfile
            modelfile_path = self._create_modelfile(dataset_file)
            
            # 执行模型训练
            model_path = self._execute_ollama_training(modelfile_path)
            
            # 验证模型
            validation_results = self._validate_trained_model(model_path)
            
            # 保存训练记录
            training_record = self._save_training_record(dataset_file, model_path, validation_results)
            
            self.logger.info(f"统一模型训练完成: {model_path}")
            return model_path
            
        except Exception as e:
            self.logger.error(f"统一模型训练失败: {e}")
            raise
    
    def _check_ollama_available(self) -> bool:
        """检查Ollama是否可用"""
        try:
            result = subprocess.run(['ollama', 'list'], 
                                  capture_output=True, text=True, timeout=10)
            return result.returncode == 0
        except Exception as e:
            self.logger.error(f"检查Ollama状态失败: {e}")
            return False
    
    def _create_modelfile(self, dataset_file: str) -> str:
        """创建Modelfile"""
        timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
        modelfile_path = os.path.join(self.training_dir, f"Modelfile_{timestamp}")
        
        modelfile_content = f'''FROM {self.base_model}

# 设置系统提示
SYSTEM """你是ljwx-stock专业股票分析AI助手，专门基于多种投资策略为用户提供股票推荐和分析。

你的核心能力包括：
1. 基于技术分析策略（如RSI、MACD、布林带等）进行股票分析
2. 基于基本面分析策略（如PE、PB、ROE等）评估股票价值
3. 基于量化策略进行数据驱动的投资建议
4. 综合多种策略提供平衡的投资建议

分析原则：
- 始终基于具体的策略框架进行分析
- 提供明确的买入/卖出/持有建议
- 给出具体的目标价位和止损建议
- 说明投资建议的置信度和风险等级
- 建议合适的持有期限

请以专业、客观的态度提供投资建议，并明确说明所有建议仅供参考。"""

# 设置模型参数
PARAMETER temperature {self.training_config['temperature']}
PARAMETER num_predict {self.training_config['max_tokens']}
PARAMETER top_p 0.9
PARAMETER top_k 40

# 模板设置
TEMPLATE """{{{{ .System }}}}

{{{{ .Prompt }}}}

{{{{ .Response }}}}"""
'''
        
        with open(modelfile_path, 'w', encoding='utf-8') as f:
            f.write(modelfile_content)
        
        self.logger.info(f"Modelfile已创建: {modelfile_path}")
        return modelfile_path
    
    def _execute_ollama_training(self, modelfile_path: str) -> str:
        """执行Ollama模型训练"""
        timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
        model_name = f"{self.model_name}:{timestamp}"
        
        try:
            self.logger.info(f"开始创建模型: {model_name}")
            
            # 执行ollama create命令
            cmd = ['ollama', 'create', model_name, '-f', modelfile_path]
            self.logger.info(f"执行命令: {' '.join(cmd)}")
            
            result = subprocess.run(cmd, capture_output=True, text=True, timeout=1800)
            
            if result.returncode == 0:
                self.logger.info(f"模型创建成功: {model_name}")
                
                # 保存模型信息
                model_info_file = os.path.join(self.models_dir, f"{model_name}_info.json")
                model_info = {
                    "model_name": model_name,
                    "base_model": self.base_model,
                    "created_at": datetime.now().isoformat(),
                    "modelfile_path": modelfile_path,
                    "training_config": self.training_config
                }
                
                with open(model_info_file, 'w', encoding='utf-8') as f:
                    json.dump(model_info, f, ensure_ascii=False, indent=2)
                
                return model_name
            else:
                error_msg = f"模型创建失败: {result.stderr}"
                self.logger.error(error_msg)
                raise RuntimeError(error_msg)
                
        except subprocess.TimeoutExpired:
            error_msg = "模型创建超时（30分钟）"
            self.logger.error(error_msg)
            raise RuntimeError(error_msg)
        except Exception as e:
            self.logger.error(f"执行Ollama训练失败: {e}")
            raise
    
    def _validate_trained_model(self, model_name: str) -> Dict[str, Any]:
        """验证训练后的模型"""
        self.logger.info(f"开始验证模型: {model_name}")
        
        # 测试用例
        test_cases = [
            {
                "prompt": "[策略:rsi_mean_reversion] 股票分析报告 - 平安银行(000001.SZ)\n\n交易日期: 20241201\n当前价格: 12.50元\n价格变动: -0.20元 (-1.58%)\n\n技术指标:\n- 5日均线: 12.80元\n- 20日均线: 13.20元\n- RSI(14): 28.5\n- MACD: -0.15\n- 成交量: 45000000股\n- 成交量比: 1.2\n\n基本面数据:\n- 市值: 2500亿\n- PE倍数: 5.2\n- PB倍数: 0.6\n\n请基于RSI均值回归策略进行分析并给出投资建议。",
                "expected_keywords": ["买入", "RSI", "超卖", "建议"]
            },
            {
                "prompt": "[策略:macd_golden_cross] 股票分析报告 - 贵州茅台(600519.SH)\n\n交易日期: 20241201\n当前价格: 1580.00元\n价格变动: +25.00元 (+1.61%)\n\n技术指标:\n- 5日均线: 1560.00元\n- 20日均线: 1540.00元\n- RSI(14): 65.2\n- MACD: 0.25\n- 成交量: 1200000股\n- 成交量比: 1.5\n\n基本面数据:\n- 市值: 19800亿\n- PE倍数: 32.5\n- PB倍数: 8.2\n\n请基于MACD金叉策略进行分析并给出投资建议。",
                "expected_keywords": ["MACD", "金叉", "推荐"]
            }
        ]
        
        validation_results = {
            "model_name": model_name,
            "test_cases": [],
            "success_rate": 0.0,
            "validation_time": datetime.now().isoformat()
        }
        
        successful_tests = 0
        
        for i, test_case in enumerate(test_cases):
            try:
                self.logger.info(f"执行测试用例 {i+1}/{len(test_cases)}")
                
                # 调用模型
                response = self._call_ollama_model(model_name, test_case["prompt"])
                
                # 检查响应质量
                contains_keywords = all(
                    keyword in response.lower() 
                    for keyword in [kw.lower() for kw in test_case["expected_keywords"]]
                )
                
                test_result = {
                    "test_id": i + 1,
                    "prompt_preview": test_case["prompt"][:100] + "...",
                    "response_preview": response[:200] + "..." if len(response) > 200 else response,
                    "contains_expected_keywords": contains_keywords,
                    "response_length": len(response),
                    "success": contains_keywords and len(response) > 50
                }
                
                validation_results["test_cases"].append(test_result)
                
                if test_result["success"]:
                    successful_tests += 1
                
            except Exception as e:
                self.logger.error(f"测试用例 {i+1} 失败: {e}")
                validation_results["test_cases"].append({
                    "test_id": i + 1,
                    "error": str(e),
                    "success": False
                })
        
        validation_results["success_rate"] = successful_tests / len(test_cases)
        
        self.logger.info(f"模型验证完成，成功率: {validation_results['success_rate']:.2%}")
        
        return validation_results
    
    def _call_ollama_model(self, model_name: str, prompt: str) -> str:
        """调用Ollama模型"""
        try:
            cmd = ['ollama', 'run', model_name, prompt]
            result = subprocess.run(cmd, capture_output=True, text=True, timeout=60)
            
            if result.returncode == 0:
                return result.stdout.strip()
            else:
                raise RuntimeError(f"模型调用失败: {result.stderr}")
                
        except subprocess.TimeoutExpired:
            raise RuntimeError("模型调用超时")
        except Exception as e:
            raise RuntimeError(f"模型调用异常: {e}")
    
    def _save_training_record(self, dataset_file: str, model_name: str, 
                            validation_results: Dict[str, Any]) -> str:
        """保存训练记录"""
        timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
        record_file = os.path.join(self.models_dir, f"training_record_{timestamp}.json")
        
        training_record = {
            "model_name": model_name,
            "base_model": self.base_model,
            "dataset_file": dataset_file,
            "training_config": self.training_config,
            "validation_results": validation_results,
            "training_start": timestamp,
            "training_completed": datetime.now().isoformat(),
            "status": "completed" if validation_results["success_rate"] > 0.5 else "failed"
        }
        
        with open(record_file, 'w', encoding='utf-8') as f:
            json.dump(training_record, f, ensure_ascii=False, indent=2)
        
        self.logger.info(f"训练记录已保存: {record_file}")
        return record_file
    
    def full_training_pipeline(self, strategy_ids: List[str] = None,
                             num_stocks_per_strategy: int = 30,
                             time_range: int = 180) -> Dict[str, str]:
        """完整训练流水线"""
        self.logger.info("开始执行完整训练流水线")
        
        try:
            # 1. 准备统一数据集
            self.logger.info("步骤1: 准备统一数据集")
            dataset_file = self.prepare_unified_dataset(
                strategy_ids, num_stocks_per_strategy, time_range
            )
            
            # 2. 训练统一模型
            self.logger.info("步骤2: 训练统一模型")
            model_name = self.train_unified_model(dataset_file)
            
            # 3. 完成
            result = {
                "status": "success",
                "dataset_file": dataset_file,
                "model_name": model_name,
                "completion_time": datetime.now().isoformat()
            }
            
            self.logger.info("完整训练流水线执行成功")
            return result
            
        except Exception as e:
            self.logger.error(f"完整训练流水线执行失败: {e}")
            return {
                "status": "failed",
                "error": str(e),
                "completion_time": datetime.now().isoformat()
            }

def main():
    """主函数"""
    import argparse
    
    # 设置日志
    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
        handlers=[
            logging.FileHandler('logs/unified_training.log'),
            logging.StreamHandler()
        ]
    )
    
    parser = argparse.ArgumentParser(description="统一模型训练器")
    parser.add_argument("--action", type=str, choices=["dataset", "train", "full"], 
                       default="full", help="执行动作")
    parser.add_argument("--strategies", type=str, nargs="*", help="指定策略ID列表")
    parser.add_argument("--stocks", type=int, default=30, help="每个策略使用的股票数量")
    parser.add_argument("--days", type=int, default=180, help="历史数据天数")
    parser.add_argument("--dataset-file", type=str, help="训练数据集文件路径")
    
    args = parser.parse_args()
    
    # 从环境变量获取TuShare token
    tushare_token = os.getenv('TUSHARE_TOKEN')
    if not tushare_token:
        print("警告: 未设置TUSHARE_TOKEN环境变量")
    
    trainer = UnifiedModelTrainer(tushare_token)
    
    if args.action == "dataset":
        dataset_file = trainer.prepare_unified_dataset(
            args.strategies, args.stocks, args.days
        )
        print(f"数据集已生成: {dataset_file}")
        
    elif args.action == "train":
        if not args.dataset_file:
            print("错误: 训练模式需要指定 --dataset-file 参数")
            sys.exit(1)
        
        model_name = trainer.train_unified_model(args.dataset_file)
        print(f"模型已训练完成: {model_name}")
        
    elif args.action == "full":
        result = trainer.full_training_pipeline(
            args.strategies, args.stocks, args.days
        )
        print(f"训练流水线执行结果: {result}")

if __name__ == "__main__":
    main()