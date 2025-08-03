#!/usr/bin/env python3
"""
TuShare数据 + Ollama大模型训练主脚本
完整的端到端训练流程
"""

import os
import sys
import logging
from datetime import datetime, timedelta
import json

# 添加项目路径
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

from llm.tushare_data_extractor import TuShareDataExtractor
from llm.llm_training_data_generator import LLMTrainingDataGenerator
from llm.ollama_trainer import OllamaTrainer, create_test_cases

class TuShareOllamaTrainingPipeline:
    """TuShare + Ollama完整训练流水线"""
    
    def __init__(self, tushare_token: str = None):
        """
        初始化训练流水线
        
        Args:
            tushare_token: TuShare Pro API token
        """
        self.logger = logging.getLogger(__name__)
        
        # 初始化各个组件
        self.data_extractor = TuShareDataExtractor(tushare_token)
        self.data_generator = LLMTrainingDataGenerator()
        self.ollama_trainer = OllamaTrainer()
        
        # 配置
        self.config = {
            'stock_limit': 50,  # 股票数量限制
            'days_back': 180,   # 历史数据天数
            'max_training_examples': 500,  # 最大训练样本数
            'base_model': 'lingjingwanxiang:70b',
            'output_dir': 'data'
        }
        
        self.logger.info("🚀 TuShare + Ollama训练流水线初始化完成")
    
    def step1_extract_stock_data(self) -> str:
        """步骤1: 提取股票数据"""
        self.logger.info("📊 步骤1: 提取TuShare股票数据")
        
        # 获取股票列表
        stocks = self.data_extractor.get_stock_list(limit=self.config['stock_limit'])
        if stocks.empty:
            raise ValueError("无法获取股票列表")
        
        # 提取股票代码
        if 'ts_code' in stocks.columns:
            stock_codes = stocks['ts_code'].tolist()
        else:
            # 处理免费API的情况
            stocks.reset_index(inplace=True)
            stock_codes = [f"{code}.SZ" if code.startswith('0') or code.startswith('3') 
                          else f"{code}.SH" for code in stocks['code'].tolist()]
        
        self.logger.info(f"   选择股票: {len(stock_codes)}只")
        
        # 设置日期范围
        end_date = datetime.now().strftime('%Y%m%d')
        start_date = (datetime.now() - timedelta(days=self.config['days_back'])).strftime('%Y%m%d')
        
        self.logger.info(f"   日期范围: {start_date} ~ {end_date}")
        
        # 提取综合数据集
        dataset = self.data_extractor.extract_comprehensive_dataset(
            stock_codes=stock_codes,
            start_date=start_date,
            end_date=end_date,
            include_financial=False,  # 暂时不包含财务数据
            include_news=False       # 暂时不包含新闻数据
        )
        
        # 保存数据集
        dataset_dir = f"{self.config['output_dir']}/tushare_dataset"
        self.data_extractor.save_dataset(dataset, dataset_dir)
        
        # 返回日线数据文件路径
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        daily_data_file = f"{dataset_dir}/daily_data_{timestamp}.csv"
        
        self.logger.info("✅ 步骤1完成: 股票数据提取成功")
        return daily_data_file
    
    def step2_generate_training_data(self, stock_data_file: str) -> str:
        """步骤2: 生成训练数据"""
        self.logger.info("🧠 步骤2: 生成大模型训练数据")
        
        # 读取股票数据
        import pandas as pd
        stock_data = pd.read_csv(stock_data_file)
        self.logger.info(f"   加载股票数据: {len(stock_data)}条记录")
        
        # 生成训练样本
        training_examples = self.data_generator.create_training_examples(
            stock_data, 
            max_examples=self.config['max_training_examples']
        )
        
        if not training_examples:
            raise ValueError("未能生成训练样本")
        
        self.logger.info(f"   生成训练样本: {len(training_examples)}个")
        
        # 保存训练数据
        training_dir = f"{self.config['output_dir']}/llm_training"
        os.makedirs(training_dir, exist_ok=True)
        
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        training_file = f"{training_dir}/stock_training_data_{timestamp}.jsonl"
        
        self.data_generator.save_training_data(training_examples, training_file, 'jsonl')
        
        self.logger.info("✅ 步骤2完成: 训练数据生成成功")
        return training_file
    
    def step3_fine_tune_model(self, training_data_file: str) -> str:
        """步骤3: 微调模型"""
        self.logger.info("🎯 步骤3: 微调Ollama模型")
        
        # 检查基础模型
        base_model = self.config['base_model']
        if not self.ollama_trainer.check_model_exists(base_model):
            raise ValueError(f"基础模型不存在: {base_model}")
        
        self.logger.info(f"   基础模型: {base_model}")
        
        # 创建微调模型名称
        timestamp = datetime.now().strftime("%Y%m%d_%H%M")
        fine_tuned_model = f"lingjingwanxiang-stock-{timestamp}"
        
        self.logger.info(f"   微调模型: {fine_tuned_model}")
        
        # 执行微调
        success = self.ollama_trainer.fine_tune_with_training_data(
            base_model=base_model,
            training_data_file=training_data_file,
            fine_tuned_model_name=fine_tuned_model
        )
        
        if not success:
            raise ValueError("模型微调失败")
        
        self.logger.info("✅ 步骤3完成: 模型微调成功")
        return fine_tuned_model
    
    def step4_test_model(self, model_name: str) -> dict:
        """步骤4: 测试模型"""
        self.logger.info("🧪 步骤4: 测试微调后的模型")
        
        # 创建测试用例
        test_cases = create_test_cases()
        self.logger.info(f"   测试用例: {len(test_cases)}个")
        
        # 执行测试
        test_results = self.ollama_trainer.test_fine_tuned_model(model_name, test_cases)
        
        # 保存测试结果
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        results_file = f"{self.config['output_dir']}/llm_training/test_results_{timestamp}.json"
        self.ollama_trainer.save_test_results(test_results, results_file)
        
        self.logger.info("✅ 步骤4完成: 模型测试成功")
        return test_results
    
    def run_full_pipeline(self) -> dict:
        """运行完整训练流水线"""
        self.logger.info("🚀 开始完整训练流水线")
        
        pipeline_results = {
            'start_time': datetime.now().isoformat(),
            'config': self.config,
            'steps': {},
            'final_model': None,
            'test_results': None
        }
        
        try:
            # 步骤1: 提取股票数据
            stock_data_file = self.step1_extract_stock_data()
            pipeline_results['steps']['step1_data_extraction'] = {
                'status': 'success',
                'output_file': stock_data_file
            }
            
            # 步骤2: 生成训练数据
            training_data_file = self.step2_generate_training_data(stock_data_file)
            pipeline_results['steps']['step2_training_data'] = {
                'status': 'success',
                'output_file': training_data_file
            }
            
            # 步骤3: 微调模型
            fine_tuned_model = self.step3_fine_tune_model(training_data_file)
            pipeline_results['steps']['step3_fine_tuning'] = {
                'status': 'success',
                'model_name': fine_tuned_model
            }
            pipeline_results['final_model'] = fine_tuned_model
            
            # 步骤4: 测试模型
            test_results = self.step4_test_model(fine_tuned_model)
            pipeline_results['steps']['step4_testing'] = {
                'status': 'success',
                'test_summary': test_results['summary']
            }
            pipeline_results['test_results'] = test_results
            
            pipeline_results['status'] = 'success'
            pipeline_results['end_time'] = datetime.now().isoformat()
            
            self.logger.info("🎉 完整训练流水线执行成功！")
            
        except Exception as e:
            pipeline_results['status'] = 'failed'
            pipeline_results['error'] = str(e)
            pipeline_results['end_time'] = datetime.now().isoformat()
            
            self.logger.error(f"❌ 训练流水线失败: {e}")
            raise
        
        return pipeline_results
    
    def save_pipeline_results(self, results: dict):
        """保存流水线结果"""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        results_file = f"{self.config['output_dir']}/pipeline_results_{timestamp}.json"
        
        os.makedirs(os.path.dirname(results_file), exist_ok=True)
        
        with open(results_file, 'w', encoding='utf-8') as f:
            json.dump(results, f, ensure_ascii=False, indent=2)
        
        self.logger.info(f"✅ 流水线结果已保存: {results_file}")


def main():
    """主函数"""
    # 配置日志
    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
        handlers=[
            logging.StreamHandler(),
            logging.FileHandler('logs/tushare_ollama_training.log')
        ]
    )
    
    # 确保日志目录存在
    os.makedirs('logs', exist_ok=True)
    
    logger = logging.getLogger(__name__)
    
    print("🚀 TuShare + Ollama大模型训练系统")
    print("=" * 60)
    
    try:
        # 获取TuShare token（可选）
        tushare_token = os.getenv('TUSHARE_TOKEN')
        if not tushare_token:
            print("⚠️  未设置TUSHARE_TOKEN环境变量，将使用免费API")
        
        # 初始化训练流水线
        pipeline = TuShareOllamaTrainingPipeline(tushare_token)
        
        # 显示配置信息
        print(f"📋 训练配置:")
        print(f"   股票数量: {pipeline.config['stock_limit']}只")
        print(f"   历史天数: {pipeline.config['days_back']}天")
        print(f"   训练样本: {pipeline.config['max_training_examples']}个")
        print(f"   基础模型: {pipeline.config['base_model']}")
        print()
        
        # 运行完整流水线
        results = pipeline.run_full_pipeline()
        
        # 保存结果
        pipeline.save_pipeline_results(results)
        
        # 显示最终结果
        print("\n🎉 训练完成！")
        print("=" * 60)
        print(f"✅ 最终模型: {results['final_model']}")
        
        if results['test_results']:
            summary = results['test_results']['summary']
            print(f"📊 测试结果:")
            print(f"   测试用例: {summary['total_tests']}个")
            print(f"   成功率: {summary['success_rate']:.2%}")
            print(f"   平均响应时间: {summary['average_response_time']:.2f}秒")
        
        print(f"\n🎯 使用方法:")
        print(f"   ollama run {results['final_model']}")
        
    except KeyboardInterrupt:
        print("\n⚠️ 训练被用户中断")
        
    except Exception as e:
        logger.error(f"❌ 训练失败: {e}")
        print(f"\n❌ 训练失败: {e}")


if __name__ == "__main__":
    main()