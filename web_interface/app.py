#!/usr/bin/env python3
"""
ljwx-stock 训练管理Web界面
提供可视化的训练过程管理、数据选择和模型监控功能
"""

from flask import Flask, render_template, request, jsonify, send_file
from flask_socketio import SocketIO, emit
import os
import sys
import json
import logging
import threading
import time
from datetime import datetime, timedelta
import pandas as pd
import subprocess
from typing import Dict, List, Optional
import uuid

# 添加项目路径
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from llm.tushare_data_extractor import TuShareDataExtractor
from llm.llm_training_data_generator import LLMTrainingDataGenerator
from comprehensive_training import ComprehensiveTrainer

# 策略相关导入
sys.path.append(os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), 'strategy'))
from strategy_models import Strategy, StrategyDatabase, STRATEGY_TEMPLATES, StrategyType
from strategy_engine import BacktestEngine

# 模型评估相关导入
sys.path.append(os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), 'model_evaluation'))
from evaluation_framework import ModelEvaluator

# 推荐回测相关导入
sys.path.append(os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), 'recommendation_backtest'))
from model_recommender import ModelRecommender
from recommendation_tracker import RecommendationTracker

app = Flask(__name__)
app.config['SECRET_KEY'] = 'ljwx-stock-training-interface'
socketio = SocketIO(app, cors_allowed_origins="*")

# 全局状态
training_status = {
    'is_running': False,
    'current_task': '',
    'progress': 0,
    'logs': [],
    'session_id': None
}

class TrainingManager:
    """训练管理器"""
    
    def __init__(self):
        self.logger = logging.getLogger(__name__)
        self.setup_logging()
        
        # 数据目录
        self.data_dir = os.path.join(os.path.dirname(__file__), '..', 'data')
        self.training_dir = os.path.join(self.data_dir, 'llm_training')
        self.models_dir = os.path.join(self.data_dir, 'models')
        
        # 确保目录存在
        os.makedirs(self.training_dir, exist_ok=True)
        os.makedirs(self.models_dir, exist_ok=True)
        
        # 训练器实例
        self.extractor = None
        self.generator = None
        self.trainer = None
        
        # 策略管理器
        self.strategy_db = StrategyDatabase()
        self.backtest_engine = BacktestEngine()
        
        # 模型评估器
        self.model_evaluator = ModelEvaluator()
        
        # 推荐系统
        self.recommendation_tracker = RecommendationTracker()
        self.model_recommender = None  # 延迟初始化
        
    def setup_logging(self):
        """设置日志"""
        logging.basicConfig(
            level=logging.INFO,
            format='%(asctime)s - %(levelname)s - %(message)s'
        )
    
    def emit_log(self, message: str, level: str = 'info'):
        """发送日志到前端"""
        log_entry = {
            'timestamp': datetime.now().isoformat(),
            'level': level,
            'message': message
        }
        training_status['logs'].append(log_entry)
        if len(training_status['logs']) > 1000:  # 限制日志数量
            training_status['logs'] = training_status['logs'][-1000:]
        
        socketio.emit('log_update', log_entry)
        self.logger.info(message)
    
    def emit_progress(self, progress: int, task: str):
        """发送进度更新"""
        training_status['progress'] = progress
        training_status['current_task'] = task
        socketio.emit('progress_update', {
            'progress': progress,
            'task': task
        })
    
    def get_tushare_data_info(self, token: str = None) -> Dict:
        """获取TuShare数据信息"""
        try:
            if not self.extractor:
                self.extractor = TuShareDataExtractor(token)
            
            # 获取股票列表
            stocks_df = self.extractor.get_stock_list(limit=100)
            
            return {
                'status': 'success',
                'stock_count': len(stocks_df),
                'sample_stocks': stocks_df.head(10).to_dict('records') if not stocks_df.empty else [],
                'has_pro_access': token is not None
            }
        except Exception as e:
            return {
                'status': 'error',
                'error': str(e)
            }
    
    def get_training_data_info(self) -> Dict:
        """获取训练数据信息"""
        try:
            training_files = []
            
            for filename in os.listdir(self.training_dir):
                if filename.endswith('.jsonl'):
                    file_path = os.path.join(self.training_dir, filename)
                    file_size = os.path.getsize(file_path)
                    
                    # 计算样本数量
                    sample_count = 0
                    try:
                        with open(file_path, 'r', encoding='utf-8') as f:
                            sample_count = sum(1 for line in f if line.strip())
                    except:
                        sample_count = 0
                    
                    training_files.append({
                        'filename': filename,
                        'size': file_size,
                        'sample_count': sample_count,
                        'created': datetime.fromtimestamp(os.path.getctime(file_path)).isoformat()
                    })
            
            return {
                'status': 'success',
                'files': sorted(training_files, key=lambda x: x['created'], reverse=True)
            }
        except Exception as e:
            return {
                'status': 'error',
                'error': str(e)
            }
    
    def get_models_info(self) -> Dict:
        """获取模型信息"""
        try:
            # 获取Ollama模型列表
            result = subprocess.run(['ollama', 'list'], capture_output=True, text=True)
            
            models = []
            if result.returncode == 0:
                lines = result.stdout.strip().split('\n')[1:]  # 跳过标题行
                for line in lines:
                    if 'ljwx' in line.lower():
                        parts = line.split()
                        if len(parts) >= 3:
                            models.append({
                                'name': parts[0],
                                'id': parts[1],
                                'size': parts[2],
                                'modified': ' '.join(parts[3:]) if len(parts) > 3 else ''
                            })
            
            return {
                'status': 'success',
                'models': models
            }
        except Exception as e:
            return {
                'status': 'error',
                'error': str(e)
            }
    
    def start_data_generation(self, config: Dict):
        """开始数据生成"""
        def generate_data():
            try:
                training_status['is_running'] = True
                training_status['session_id'] = str(uuid.uuid4())
                
                self.emit_log("🚀 开始数据生成任务")
                self.emit_progress(0, "初始化数据提取器")
                
                # 初始化组件
                token = config.get('tushare_token')
                self.extractor = TuShareDataExtractor(token)
                self.generator = LLMTrainingDataGenerator()
                
                self.emit_progress(10, "获取股票列表")
                
                # 获取股票代码
                stock_count = config.get('stock_count', 100)
                stocks_df = self.extractor.get_stock_list(limit=stock_count)
                
                if stocks_df.empty:
                    raise Exception("无法获取股票列表")
                
                stock_codes = stocks_df['ts_code'].tolist() if 'ts_code' in stocks_df.columns else []
                if not stock_codes:
                    stock_codes = [f"{code}.SZ" if code.startswith(('0', '3')) else f"{code}.SH" 
                                 for code in stocks_df['code'].tolist()]
                
                self.emit_log(f"📊 获取到 {len(stock_codes)} 只股票")
                self.emit_progress(20, "提取历史数据")
                
                # 获取历史数据
                days_back = config.get('days_back', 365)
                end_date = datetime.now().strftime('%Y%m%d')
                start_date = (datetime.now() - timedelta(days=days_back)).strftime('%Y%m%d')
                
                dataset = self.extractor.extract_comprehensive_dataset(
                    stock_codes=stock_codes[:stock_count],
                    start_date=start_date,
                    end_date=end_date,
                    include_financial=False,
                    include_news=False
                )
                
                if 'daily_data' not in dataset or dataset['daily_data'].empty:
                    raise Exception("无法获取历史数据")
                
                daily_data = dataset['daily_data']
                self.emit_log(f"📈 获取到 {len(daily_data)} 条历史数据")
                self.emit_progress(60, "生成训练样本")
                
                # 生成训练数据
                max_examples = config.get('max_examples', 1000)
                training_examples = self.generator.create_training_examples(
                    daily_data, max_examples=max_examples
                )
                
                # 转换为字典格式
                training_data = []
                for example in training_examples:
                    if hasattr(example, '__dict__'):
                        example_dict = {
                            'instruction': example.instruction,
                            'input': example.input,
                            'output': example.output,
                            'metadata': example.metadata
                        }
                    else:
                        example_dict = example
                    training_data.append(example_dict)
                
                self.emit_progress(80, "保存训练数据")
                
                # 保存训练数据
                timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
                filename = f"web_generated_training_data_{timestamp}.jsonl"
                filepath = os.path.join(self.training_dir, filename)
                
                with open(filepath, 'w', encoding='utf-8') as f:
                    for data in training_data:
                        f.write(json.dumps(data, ensure_ascii=False) + '\n')
                
                self.emit_progress(100, "数据生成完成")
                self.emit_log(f"✅ 训练数据生成完成：{len(training_data)} 个样本")
                self.emit_log(f"📁 文件保存：{filename}")
                
                socketio.emit('task_completed', {
                    'type': 'data_generation',
                    'filename': filename,
                    'sample_count': len(training_data)
                })
                
            except Exception as e:
                self.emit_log(f"❌ 数据生成失败: {str(e)}", 'error')
                socketio.emit('task_failed', {
                    'type': 'data_generation',
                    'error': str(e)
                })
            finally:
                training_status['is_running'] = False
        
        thread = threading.Thread(target=generate_data)
        thread.daemon = True
        thread.start()
    
    def start_model_training(self, config: Dict):
        """开始模型训练"""
        def train_model():
            try:
                training_status['is_running'] = True
                training_status['session_id'] = str(uuid.uuid4())
                
                self.emit_log("🚀 开始模型训练任务")
                self.emit_progress(0, "准备训练数据")
                
                # 加载训练数据
                training_file = config.get('training_file')
                if not training_file:
                    raise Exception("未选择训练文件")
                
                training_path = os.path.join(self.training_dir, training_file)
                if not os.path.exists(training_path):
                    raise Exception(f"训练文件不存在: {training_file}")
                
                # 读取训练数据
                training_data = []
                with open(training_path, 'r', encoding='utf-8') as f:
                    for line in f:
                        if line.strip():
                            training_data.append(json.loads(line.strip()))
                
                self.emit_log(f"📊 加载训练数据: {len(training_data)} 个样本")
                self.emit_progress(20, "创建模型")
                
                # 准备模型配置
                base_model = config.get('base_model', 'ljwx-stock')
                model_name = config.get('model_name', f"ljwx-stock-web-{datetime.now().strftime('%Y%m%d_%H%M')}")
                sample_count = min(len(training_data), config.get('sample_count', 200))
                
                # 构建系统提示词
                system_prompt = """你是ljwx-stock，一个专业的股票投资分析助手。基于大量A股市场历史数据训练，具备以下专业能力：

🎯 **核心优势**
- 基于真实历史数据的深度学习
- 多维度技术分析和风险评估能力
- 专业的投资决策支持

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

⚠️ **风险提示**：所有分析基于历史数据，投资有风险，请谨慎决策。"""

                self.emit_progress(40, "构建Modelfile")
                
                # 创建Modelfile
                import tempfile
                import random
                
                modelfile_content = f"""FROM {base_model}
SYSTEM "{system_prompt}"
PARAMETER temperature {config.get('temperature', 0.7)}
PARAMETER top_p {config.get('top_p', 0.9)}
PARAMETER top_k {config.get('top_k', 40)}
PARAMETER repeat_penalty {config.get('repeat_penalty', 1.1)}
PARAMETER num_predict {config.get('num_predict', 1024)}

"""
                
                # 添加训练样本
                selected_samples = random.sample(training_data, sample_count)
                
                for i, example in enumerate(selected_samples):
                    instruction = example.get('instruction', '')
                    input_text = example.get('input', '')
                    output_text = example.get('output', '')
                    
                    user_message = f"{instruction}\n\n{input_text}" if input_text else instruction
                    
                    # 清理文本
                    user_message = user_message.replace('"', "'").replace('\n', '\\n')
                    output_text = output_text.replace('"', "'").replace('\n', '\\n')
                    
                    modelfile_content += f'MESSAGE user "{user_message}"\n'
                    modelfile_content += f'MESSAGE assistant "{output_text}"\n\n'
                    
                    if (i + 1) % 50 == 0:
                        progress = 40 + int((i + 1) / sample_count * 40)
                        self.emit_progress(progress, f"添加训练样本 {i+1}/{sample_count}")
                
                self.emit_progress(80, "执行模型训练")
                
                # 创建临时Modelfile并训练
                with tempfile.NamedTemporaryFile(mode='w', suffix='.modelfile', delete=False) as f:
                    f.write(modelfile_content)
                    modelfile_path = f.name
                
                try:
                    self.emit_log(f"🤖 创建模型: {model_name}")
                    
                    result = subprocess.run(
                        ['ollama', 'create', model_name, '-f', modelfile_path],
                        capture_output=True,
                        text=True,
                        timeout=1800  # 30分钟超时
                    )
                    
                    if result.returncode == 0:
                        self.emit_progress(95, "测试模型")
                        self.emit_log(f"✅ 模型创建成功: {model_name}")
                        
                        # 简单测试
                        test_result = subprocess.run(
                            ['ollama', 'run', model_name],
                            input="测试模型是否正常工作",
                            capture_output=True,
                            text=True,
                            timeout=30
                        )
                        
                        if test_result.returncode == 0:
                            self.emit_log("✅ 模型测试通过")
                        else:
                            self.emit_log("⚠️ 模型测试异常", 'warning')
                        
                        self.emit_progress(100, "训练完成")
                        
                        socketio.emit('task_completed', {
                            'type': 'model_training',
                            'model_name': model_name,
                            'sample_count': sample_count
                        })
                    else:
                        raise Exception(f"模型创建失败: {result.stderr}")
                        
                finally:
                    os.unlink(modelfile_path)
                
            except Exception as e:
                self.emit_log(f"❌ 模型训练失败: {str(e)}", 'error')
                socketio.emit('task_failed', {
                    'type': 'model_training',
                    'error': str(e)
                })
            finally:
                training_status['is_running'] = False
        
        thread = threading.Thread(target=train_model)
        thread.daemon = True
        thread.start()
    
    def run_strategy_backtest(self, strategy_id: str, config: Dict):
        """运行策略回测"""
        def backtest():
            try:
                training_status['is_running'] = True
                training_status['session_id'] = str(uuid.uuid4())
                
                self.emit_log("🚀 开始策略回测")
                self.emit_progress(0, "加载策略")
                
                # 获取策略
                strategy = self.strategy_db.get_strategy(strategy_id)
                if not strategy:
                    raise Exception(f"策略不存在: {strategy_id}")
                
                self.emit_progress(20, "获取历史数据")
                
                # 获取回测数据
                token = config.get('tushare_token')
                if not self.extractor:
                    self.extractor = TuShareDataExtractor(token)
                
                stock_code = config.get('stock_code', '000001.SZ')
                start_date = config.get('start_date')
                end_date = config.get('end_date')
                
                # 获取股票数据
                df = self.extractor.get_stock_daily_data(stock_code, start_date, end_date)
                if df.empty:
                    raise Exception("无法获取股票数据")
                
                # 设置索引为日期
                if 'trade_date' in df.columns:
                    df['trade_date'] = pd.to_datetime(df['trade_date'])
                    df.set_index('trade_date', inplace=True)
                
                self.emit_progress(50, "执行回测")
                
                # 运行回测
                result = self.backtest_engine.run_backtest(df, strategy, start_date, end_date)
                
                self.emit_progress(80, "保存结果")
                
                # 保存回测结果
                result_id = self.strategy_db.save_backtest_result(result)
                
                self.emit_progress(100, "回测完成")
                self.emit_log(f"✅ 策略回测完成")
                self.emit_log(f"📊 总收益率: {result.total_return:.2%}")
                self.emit_log(f"📈 夏普比率: {result.sharpe_ratio:.2f}")
                self.emit_log(f"📉 最大回撤: {result.max_drawdown:.2%}")
                
                socketio.emit('backtest_completed', {
                    'strategy_id': strategy_id,
                    'result_id': result_id,
                    'summary': {
                        'total_return': result.total_return,
                        'annual_return': result.annual_return,
                        'max_drawdown': result.max_drawdown,
                        'sharpe_ratio': result.sharpe_ratio,
                        'win_rate': result.win_rate,
                        'total_trades': result.total_trades
                    }
                })
                
            except Exception as e:
                self.emit_log(f"❌ 策略回测失败: {str(e)}", 'error')
                socketio.emit('task_failed', {
                    'type': 'strategy_backtest',
                    'error': str(e)
                })
            finally:
                training_status['is_running'] = False
        
        thread = threading.Thread(target=backtest)
        thread.daemon = True
        thread.start()
    
    def run_model_evaluation(self, model_name: str, config: Dict):
        """运行模型评估"""
        def evaluate():
            try:
                training_status['is_running'] = True
                training_status['session_id'] = str(uuid.uuid4())
                
                self.emit_log(f"🚀 开始评估模型: {model_name}")
                self.emit_progress(0, "初始化评估器")
                
                # 获取测试用例
                category = config.get('category')
                difficulty = config.get('difficulty')
                test_cases = self.model_evaluator.get_test_cases(category=category, difficulty=difficulty)
                
                if not test_cases:
                    raise Exception("没有找到符合条件的测试用例")
                
                self.emit_log(f"📋 加载测试用例: {len(test_cases)}个")
                self.emit_progress(10, f"开始评估 {len(test_cases)} 个测试用例")
                
                # 运行评估
                timeout = config.get('timeout', 30)
                metrics = self.model_evaluator.evaluate_model(model_name, test_cases, timeout)
                
                self.emit_progress(90, "分析评估结果")
                
                # 计算详细统计
                summary = {
                    'model_name': model_name,
                    'total_tests': metrics.total_tests,
                    'success_rate': metrics.success_rate,
                    'avg_overall_score': metrics.avg_overall_score,
                    'avg_response_time': metrics.avg_response_time,
                    'category_scores': metrics.category_scores,
                    'difficulty_scores': metrics.difficulty_scores,
                    'evaluation_date': metrics.evaluation_date
                }
                
                self.emit_progress(100, "评估完成")
                self.emit_log(f"✅ 模型评估完成")
                self.emit_log(f"📊 总体分数: {metrics.avg_overall_score:.2f}")
                self.emit_log(f"📈 成功率: {metrics.success_rate:.2%}")
                self.emit_log(f"⏱️ 平均响应时间: {metrics.avg_response_time:.2f}秒")
                
                socketio.emit('evaluation_completed', {
                    'model_name': model_name,
                    'summary': summary,
                    'detailed_results': [
                        {
                            'test_case_id': r.test_case_id,
                            'overall_score': r.overall_score,
                            'response_time': r.response_time,
                            'error_message': r.error_message
                        } for r in metrics.test_results[:10]  # 只返回前10个结果
                    ]
                })
                
            except Exception as e:
                self.emit_log(f"❌ 模型评估失败: {str(e)}", 'error')
                socketio.emit('task_failed', {
                    'type': 'model_evaluation',
                    'error': str(e)
                })
            finally:
                training_status['is_running'] = False
        
        thread = threading.Thread(target=evaluate)
        thread.daemon = True
        thread.start()
    
    def run_recommendation_generation(self, model_name: str, config: Dict):
        """运行推荐生成"""
        def generate():
            try:
                training_status['is_running'] = True
                training_status['session_id'] = str(uuid.uuid4())
                
                self.emit_log(f"🚀 开始生成股票推荐: {model_name}")
                self.emit_progress(0, "初始化推荐系统")
                
                # 初始化推荐器
                if not self.model_recommender:
                    tushare_token = config.get('tushare_token')
                    self.model_recommender = ModelRecommender(model_name, tushare_token)
                
                self.emit_progress(20, "获取股票候选列表")
                
                # 生成推荐
                num_stocks = config.get('num_stocks', 5)
                analysis_type = config.get('analysis_type', 'comprehensive_analysis')
                
                if config.get('use_daily_mode', True):
                    # 每日推荐模式
                    recommendations = self.model_recommender.generate_daily_recommendations(num_stocks)
                else:
                    # 指定股票模式
                    stock_codes = config.get('stock_codes', [])
                    recommendations = self.model_recommender.generate_stock_recommendations(
                        stock_codes, analysis_type, num_stocks
                    )
                
                self.emit_progress(80, "保存推荐结果")
                
                # 获取推荐详情
                recommendation_details = []
                for rec_id in recommendations:
                    rec = self.recommendation_tracker.get_recommendation_by_id(rec_id)
                    if rec:
                        recommendation_details.append({
                            'id': rec.id,
                            'stock_code': rec.stock_code,
                            'stock_name': rec.stock_name,
                            'recommendation_type': rec.recommendation_type,
                            'confidence_score': rec.confidence_score,
                            'target_price': rec.target_price,
                            'recommend_price': rec.recommend_price
                        })
                
                self.emit_progress(100, "推荐生成完成")
                self.emit_log(f"✅ 生成推荐完成: {len(recommendations)} 个")
                
                socketio.emit('recommendation_generated', {
                    'model_name': model_name,
                    'total_recommendations': len(recommendations),
                    'recommendations': recommendation_details
                })
                
            except Exception as e:
                self.emit_log(f"❌ 推荐生成失败: {str(e)}", 'error')
                socketio.emit('task_failed', {
                    'type': 'recommendation_generation',
                    'error': str(e)
                })
            finally:
                training_status['is_running'] = False
        
        thread = threading.Thread(target=generate)
        thread.daemon = True
        thread.start()
    
    def run_recommendation_validation(self, model_name: str = None):
        """运行推荐验证"""
        def validate():
            try:
                training_status['is_running'] = True
                training_status['session_id'] = str(uuid.uuid4())
                
                self.emit_log("🔍 开始验证推荐结果")
                self.emit_progress(0, "检查待验证推荐")
                
                # 执行验证
                validation_result = self.recommendation_tracker.validate_recommendations(model_name)
                
                self.emit_progress(100, "验证完成")
                self.emit_log(f"✅ 验证完成: {validation_result} 条推荐")
                
                socketio.emit('validation_completed', {
                    'validated_count': validation_result,
                    'model_name': model_name or 'all'
                })
                
            except Exception as e:
                self.emit_log(f"❌ 推荐验证失败: {str(e)}", 'error')
                socketio.emit('task_failed', {
                    'type': 'recommendation_validation',
                    'error': str(e)
                })
            finally:
                training_status['is_running'] = False
        
        thread = threading.Thread(target=validate)
        thread.daemon = True
        thread.start()

# 创建训练管理器实例
training_manager = TrainingManager()

@app.route('/')
def index():
    """主页"""
    return render_template('index.html')

@app.route('/api/status')
def get_status():
    """获取训练状态"""
    return jsonify(training_status)

@app.route('/api/tushare-info')
def get_tushare_info():
    """获取TuShare数据信息"""
    token = request.args.get('token')
    return jsonify(training_manager.get_tushare_data_info(token))

@app.route('/api/training-data-info')
def get_training_data_info():
    """获取训练数据信息"""
    return jsonify(training_manager.get_training_data_info())

@app.route('/api/models-info')
def get_models_info():
    """获取模型信息"""
    return jsonify(training_manager.get_models_info())

@app.route('/api/generate-data', methods=['POST'])
def generate_data():
    """生成训练数据"""
    if training_status['is_running']:
        return jsonify({'error': '训练任务正在进行中'}), 400
    
    config = request.json
    training_manager.start_data_generation(config)
    return jsonify({'status': 'started'})

@app.route('/api/train-model', methods=['POST'])
def train_model():
    """训练模型"""
    if training_status['is_running']:
        return jsonify({'error': '训练任务正在进行中'}), 400
    
    config = request.json
    training_manager.start_model_training(config)
    return jsonify({'status': 'started'})

@app.route('/api/stop-training', methods=['POST'])
def stop_training():
    """停止训练"""
    training_status['is_running'] = False
    return jsonify({'status': 'stopped'})

@app.route('/api/clear-logs', methods=['POST'])
def clear_logs():
    """清除日志"""
    training_status['logs'] = []
    return jsonify({'status': 'cleared'})

# ================== 策略管理API ==================

@app.route('/api/strategies')
def list_strategies():
    """获取策略列表"""
    user_id = request.args.get('user_id', 'default')
    strategy_type = request.args.get('type')
    public_only = request.args.get('public') == 'true'
    
    strategies = training_manager.strategy_db.list_strategies(
        user_id=user_id, 
        strategy_type=strategy_type, 
        public_only=public_only
    )
    
    strategy_list = []
    for strategy in strategies:
        strategy_dict = {
            'id': strategy.id,
            'name': strategy.name,
            'description': strategy.description,
            'strategy_type': strategy.strategy_type,
            'user_id': strategy.user_id,
            'created_at': strategy.created_at,
            'updated_at': strategy.updated_at,
            'is_public': strategy.is_public,
            'tags': strategy.tags,
            'buy_rules_count': len(strategy.buy_rules) if strategy.buy_rules else 0,
            'sell_rules_count': len(strategy.sell_rules) if strategy.sell_rules else 0
        }
        strategy_list.append(strategy_dict)
    
    return jsonify({'strategies': strategy_list})

@app.route('/api/strategies/<strategy_id>')
def get_strategy(strategy_id):
    """获取单个策略"""
    strategy = training_manager.strategy_db.get_strategy(strategy_id)
    if not strategy:
        return jsonify({'error': '策略不存在'}), 404
    
    from dataclasses import asdict
    return jsonify({'strategy': asdict(strategy)})

@app.route('/api/strategies', methods=['POST'])
def create_strategy():
    """创建策略"""
    try:
        data = request.json
        
        # 创建策略对象
        strategy = Strategy(
            name=data.get('name', ''),
            description=data.get('description', ''),
            strategy_type=data.get('strategy_type', StrategyType.TECHNICAL.value),
            user_id=data.get('user_id', 'default'),
            initial_capital=data.get('initial_capital', 100000.0),
            commission=data.get('commission', 0.0003),
            slippage=data.get('slippage', 0.0001),
            is_public=data.get('is_public', False),
            tags=data.get('tags', [])
        )
        
        # 处理买入规则
        if 'buy_rules' in data and data['buy_rules']:
            from strategy_models import StrategyRule, TradingCondition
            buy_rules = []
            for rule_data in data['buy_rules']:
                conditions = []
                for cond_data in rule_data.get('conditions', []):
                    conditions.append(TradingCondition(**cond_data))
                
                rule = StrategyRule(
                    name=rule_data.get('name', ''),
                    conditions=conditions,
                    logic_operator=rule_data.get('logic_operator', 'AND'),
                    signal_type=rule_data.get('signal_type', 'buy'),
                    weight=rule_data.get('weight', 1.0)
                )
                buy_rules.append(rule)
            strategy.buy_rules = buy_rules
        
        # 处理卖出规则
        if 'sell_rules' in data and data['sell_rules']:
            from strategy_models import StrategyRule, TradingCondition
            sell_rules = []
            for rule_data in data['sell_rules']:
                conditions = []
                for cond_data in rule_data.get('conditions', []):
                    conditions.append(TradingCondition(**cond_data))
                
                rule = StrategyRule(
                    name=rule_data.get('name', ''),
                    conditions=conditions,
                    logic_operator=rule_data.get('logic_operator', 'AND'),
                    signal_type=rule_data.get('signal_type', 'sell'),
                    weight=rule_data.get('weight', 1.0)
                )
                sell_rules.append(rule)
            strategy.sell_rules = sell_rules
        
        # 处理风险管理
        if 'risk_management' in data:
            from strategy_models import RiskManagement
            strategy.risk_management = RiskManagement(**data['risk_management'])
        
        # 保存策略
        strategy_id = training_manager.strategy_db.save_strategy(strategy)
        
        return jsonify({'strategy_id': strategy_id, 'status': 'created'})
        
    except Exception as e:
        return jsonify({'error': str(e)}), 400

@app.route('/api/strategies/<strategy_id>', methods=['PUT'])
def update_strategy(strategy_id):
    """更新策略"""
    try:
        strategy = training_manager.strategy_db.get_strategy(strategy_id)
        if not strategy:
            return jsonify({'error': '策略不存在'}), 404
        
        data = request.json
        
        # 更新基本信息
        if 'name' in data:
            strategy.name = data['name']
        if 'description' in data:
            strategy.description = data['description']
        if 'strategy_type' in data:
            strategy.strategy_type = data['strategy_type']
        if 'is_public' in data:
            strategy.is_public = data['is_public']
        if 'tags' in data:
            strategy.tags = data['tags']
        
        # 更新规则和风险管理类似创建逻辑...
        
        # 保存更新
        training_manager.strategy_db.save_strategy(strategy)
        
        return jsonify({'status': 'updated'})
        
    except Exception as e:
        return jsonify({'error': str(e)}), 400

@app.route('/api/strategies/<strategy_id>', methods=['DELETE'])
def delete_strategy(strategy_id):
    """删除策略"""
    success = training_manager.strategy_db.delete_strategy(strategy_id)
    if success:
        return jsonify({'status': 'deleted'})
    else:
        return jsonify({'error': '策略不存在'}), 404

@app.route('/api/strategy-templates')
def get_strategy_templates():
    """获取策略模板"""
    return jsonify({'templates': STRATEGY_TEMPLATES})

@app.route('/api/strategies/<strategy_id>/backtest', methods=['POST'])
def run_backtest(strategy_id):
    """运行策略回测"""
    if training_status['is_running']:
        return jsonify({'error': '任务正在进行中'}), 400
    
    config = request.json
    config['strategy_id'] = strategy_id
    
    training_manager.run_strategy_backtest(strategy_id, config)
    return jsonify({'status': 'started'})

@app.route('/api/strategies/<strategy_id>/backtest-results')
def get_backtest_results(strategy_id):
    """获取回测结果"""
    results = training_manager.strategy_db.get_backtest_results(strategy_id)
    
    result_list = []
    for result in results:
        from dataclasses import asdict
        result_dict = asdict(result)
        # 简化返回数据，避免过大的数组
        if 'equity_curve' in result_dict and len(result_dict['equity_curve']) > 100:
            # 采样显示关键点
            curve = result_dict['equity_curve']
            step = len(curve) // 100
            result_dict['equity_curve'] = curve[::step]
        
        result_list.append(result_dict)
    
    return jsonify({'results': result_list})

# ================== 模型评估API ==================

@app.route('/api/evaluation/test-cases')
def get_test_cases():
    """获取测试用例"""
    category = request.args.get('category')
    difficulty = request.args.get('difficulty')
    
    test_cases = training_manager.model_evaluator.get_test_cases(category=category, difficulty=difficulty)
    
    test_case_list = []
    for test_case in test_cases:
        from dataclasses import asdict
        test_case_dict = asdict(test_case)
        test_case_list.append(test_case_dict)
    
    return jsonify({'test_cases': test_case_list})

@app.route('/api/evaluation/start', methods=['POST'])
def start_model_evaluation():
    """开始模型评估"""
    if training_status['is_running']:
        return jsonify({'error': '任务正在进行中'}), 400
    
    data = request.json
    model_name = data.get('model_name')
    if not model_name:
        return jsonify({'error': '未指定模型名称'}), 400
    
    config = {
        'category': data.get('category'),
        'difficulty': data.get('difficulty'),
        'timeout': data.get('timeout', 30)
    }
    
    training_manager.run_model_evaluation(model_name, config)
    return jsonify({'status': 'started'})

@app.route('/api/evaluation/results/<model_name>')
def get_evaluation_results(model_name):
    """获取评估结果"""
    try:
        history = training_manager.model_evaluator.get_model_performance_history(model_name)
        
        results = []
        for metrics in history:
            from dataclasses import asdict
            metrics_dict = asdict(metrics)
            # 简化数据，移除详细测试结果
            metrics_dict.pop('test_results', None)
            results.append(metrics_dict)
        
        return jsonify({'results': results})
        
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@app.route('/api/evaluation/compare', methods=['POST'])
def compare_models():
    """比较模型性能"""
    data = request.json
    model_names = data.get('model_names', [])
    
    if len(model_names) < 2:
        return jsonify({'error': '至少需要两个模型进行比较'}), 400
    
    try:
        comparison_data = training_manager.model_evaluator.compare_models(model_names)
        return jsonify({'comparison': comparison_data})
        
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@app.route('/api/evaluation/categories')
def get_evaluation_categories():
    """获取评估类别"""
    categories = [
        {'value': '', 'label': '全部类别'},
        {'value': '技术分析', 'label': '技术分析'},
        {'value': '风险评估', 'label': '风险评估'},
        {'value': '市场情绪', 'label': '市场情绪'},
        {'value': '投资策略', 'label': '投资策略'},
        {'value': '基本面分析', 'label': '基本面分析'},
        {'value': '边界测试', 'label': '边界测试'}
    ]
    
    difficulties = [
        {'value': '', 'label': '全部难度'},
        {'value': 'easy', 'label': '简单'},
        {'value': 'medium', 'label': '中等'},
        {'value': 'hard', 'label': '困难'}
    ]
    
    return jsonify({
        'categories': categories,
        'difficulties': difficulties
    })

# ================== 推荐回测API ==================

@app.route('/api/recommendations/generate', methods=['POST'])
def generate_recommendations():
    """生成股票推荐"""
    if training_status['is_running']:
        return jsonify({'error': '任务正在进行中'}), 400
    
    data = request.json
    model_name = data.get('model_name', 'ljwx-stock')
    
    config = {
        'num_stocks': data.get('num_stocks', 5),
        'analysis_type': data.get('analysis_type', 'comprehensive_analysis'),
        'use_daily_mode': data.get('use_daily_mode', True),
        'stock_codes': data.get('stock_codes', []),
        'tushare_token': data.get('tushare_token', '')
    }
    
    training_manager.run_recommendation_generation(model_name, config)
    return jsonify({'status': 'started'})

@app.route('/api/recommendations/validate', methods=['POST'])
def validate_recommendations():
    """验证推荐结果"""
    if training_status['is_running']:
        return jsonify({'error': '任务正在进行中'}), 400
    
    data = request.json
    model_name = data.get('model_name')
    
    training_manager.run_recommendation_validation(model_name)
    return jsonify({'status': 'started'})

@app.route('/api/recommendations/<model_name>')
def get_model_recommendations(model_name):
    """获取模型推荐列表"""
    try:
        limit = request.args.get('limit', 50, type=int)
        recommendations = training_manager.recommendation_tracker.get_model_recommendations(model_name, limit)
        
        recommendation_list = []
        for rec in recommendations:
            from dataclasses import asdict
            rec_dict = asdict(rec)
            recommendation_list.append(rec_dict)
        
        return jsonify({'recommendations': recommendation_list})
        
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@app.route('/api/recommendations/<rec_id>/detail')
def get_recommendation_detail(rec_id):
    """获取推荐详情"""
    try:
        rec = training_manager.recommendation_tracker.get_recommendation_by_id(rec_id)
        if rec:
            from dataclasses import asdict
            return jsonify({'recommendation': asdict(rec)})
        else:
            return jsonify({'error': '推荐不存在'}), 404
            
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@app.route('/api/recommendations/backtest/<model_name>')
def get_backtest_report(model_name):
    """获取回测报告"""
    try:
        start_date = request.args.get('start_date')
        end_date = request.args.get('end_date')
        
        metrics = training_manager.recommendation_tracker.generate_backtest_report(
            model_name, start_date, end_date
        )
        
        from dataclasses import asdict
        # 简化数据，避免返回过大的推荐列表
        metrics_dict = asdict(metrics)
        metrics_dict['recommendations'] = metrics_dict['recommendations'][:10]  # 只返回前10个
        
        return jsonify({'metrics': metrics_dict})
        
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@app.route('/api/recommendations/performance/<model_name>')
def get_model_performance(model_name):
    """获取模型性能摘要"""
    try:
        days = request.args.get('days', 30, type=int)
        
        # 初始化推荐器以获取性能数据
        if not training_manager.model_recommender:
            training_manager.model_recommender = ModelRecommender(model_name)
        
        performance = training_manager.model_recommender.get_model_performance_summary(days)
        
        return jsonify({'performance': performance})
        
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@app.route('/api/recommendations/statistics')
def get_recommendation_statistics():
    """获取推荐统计信息"""
    try:
        with sqlite3.connect(training_manager.recommendation_tracker.db_path) as conn:
            # 总体统计
            cursor = conn.execute('''
                SELECT 
                    COUNT(*) as total,
                    COUNT(CASE WHEN result = 'hit' THEN 1 END) as hits,
                    COUNT(CASE WHEN result = 'miss' THEN 1 END) as misses,
                    COUNT(CASE WHEN result = 'pending' THEN 1 END) as pending,
                    AVG(confidence_score) as avg_confidence,
                    AVG(actual_return) as avg_return
                FROM recommendations
            ''')
            
            total_stats = cursor.fetchone()
            
            # 按模型统计
            cursor = conn.execute('''
                SELECT 
                    model_name,
                    COUNT(*) as total,
                    COUNT(CASE WHEN result = 'hit' THEN 1 END) as hits,
                    COUNT(CASE WHEN result = 'miss' THEN 1 END) as misses,
                    AVG(confidence_score) as avg_confidence
                FROM recommendations
                GROUP BY model_name
                ORDER BY total DESC
            ''')
            
            model_stats = cursor.fetchall()
            
            # 按类型统计
            cursor = conn.execute('''
                SELECT 
                    recommendation_type,
                    COUNT(*) as total,
                    COUNT(CASE WHEN result = 'hit' THEN 1 END) as hits,
                    AVG(actual_return) as avg_return
                FROM recommendations
                WHERE result != 'pending'
                GROUP BY recommendation_type
            ''')
            
            type_stats = cursor.fetchall()
            
            return jsonify({
                'total_statistics': {
                    'total_recommendations': total_stats[0],
                    'total_hits': total_stats[1] or 0,
                    'total_misses': total_stats[2] or 0,
                    'pending_recommendations': total_stats[3] or 0,
                    'overall_hit_rate': (total_stats[1] or 0) / max(total_stats[0] - (total_stats[3] or 0), 1),
                    'avg_confidence': total_stats[4] or 0,
                    'avg_return': total_stats[5] or 0
                },
                'model_statistics': [
                    {
                        'model_name': row[0],
                        'total': row[1],
                        'hits': row[2],
                        'misses': row[3],
                        'hit_rate': row[2] / max(row[1] - (total_stats[3] or 0), 1),
                        'avg_confidence': row[4] or 0
                    } for row in model_stats
                ],
                'type_statistics': [
                    {
                        'type': row[0],
                        'total': row[1],
                        'hits': row[2],
                        'hit_rate': row[2] / max(row[1], 1),
                        'avg_return': row[3] or 0
                    } for row in type_stats
                ]
            })
            
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@socketio.on('connect')
def handle_connect():
    """WebSocket连接"""
    emit('connected', {'status': 'connected'})

@socketio.on('disconnect')
def handle_disconnect():
    """WebSocket断开"""
    pass

if __name__ == '__main__':
    port = 5002
    print("🚀 启动ljwx-stock训练管理界面")
    print(f"📱 访问地址: http://localhost:{port}")
    print("=" * 50)
    
    socketio.run(app, debug=True, host='0.0.0.0', port=port, allow_unsafe_werkzeug=True)