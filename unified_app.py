#!/usr/bin/env python3
"""
ljwx-stock 统一应用服务器
提供管理端和移动端通用的API接口
端口: 5005
"""

import os
import sys
import json
import logging
import asyncio
import threading
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Any
from pathlib import Path
from dotenv import load_dotenv

# 加载环境变量
load_dotenv()

from flask import Flask, render_template, request, jsonify, send_from_directory
from flask_socketio import SocketIO, emit
import gzip
import pandas as pd
import numpy as np

# 添加项目路径
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

# 导入核心组件
try:
    from comprehensive_training import ComprehensiveTrainer
except ImportError:
    ComprehensiveTrainer = None

try:
    from llm.tushare_data_extractor import TuShareDataExtractor
except ImportError:
    TuShareDataExtractor = None

try:
    from strategy.strategy_engine import StrategyEngine
except ImportError:
    StrategyEngine = None

try:
    from strategy.strategy_models import StrategyManager
except ImportError:
    StrategyManager = None

try:
    from recommendation_backtest.recommendation_tracker import RecommendationTracker
except ImportError:
    RecommendationTracker = None

try:
    from recommendation_backtest.model_recommender import ModelRecommender
except ImportError:
    ModelRecommender = None

try:
    from model_evaluation.evaluation_framework import ModelEvaluator
except ImportError:
    ModelEvaluator = None

class UnifiedStockApp:
    """统一股票应用"""
    
    def __init__(self):
        self.app = Flask(__name__, static_folder='static', template_folder='templates')
        self.app.config['SECRET_KEY'] = 'ljwx-stock-unified-2025'
        # Performance optimizations
        self.app.config['SEND_FILE_MAX_AGE_DEFAULT'] = 3600  # Cache static files for 1 hour
        self.app.config['TEMPLATES_AUTO_RELOAD'] = False  # Disable template auto-reload in production
        self.app.config['COMPRESS_MIMETYPES'] = ['text/html', 'text/css', 'text/xml', 'application/json', 'application/javascript']
        self.socketio = SocketIO(self.app, 
                                 cors_allowed_origins="*", 
                                 async_mode='threading',
                                 logger=False,
                                 engineio_logger=False,
                                 ping_timeout=60,
                                 ping_interval=25)
        
        # 配置日志 - 减少日志级别提升性能
        logging.basicConfig(
            level=logging.WARNING,  # 改为WARNING减少日志输出
            format='%(asctime)s - %(levelname)s - %(message)s'
        )
        self.logger = logging.getLogger(__name__)
        
        # 初始化核心组件
        self._init_components()
        
        # 设置路由
        self._setup_routes()
        self._setup_socket_events()
        
        # 应用状态
        self.status = {
            'is_running': False,
            'progress': 0,
            'current_task': '',
            'logs': []
        }
    
    def _init_components(self):
        """初始化核心组件"""
        try:
            # TuShare数据提取器
            tushare_token = os.getenv('TUSHARE_TOKEN', '58cf834df2a4b9a5404f6416248cffa0da78d10e31496385251d7aef')
            
            if TuShareDataExtractor:
                self.data_extractor = TuShareDataExtractor(tushare_token)
                self.logger.info("✅ TuShare数据提取器已初始化")
            else:
                self.data_extractor = None
                self.logger.warning("⚠️ TuShare数据提取器未可用")
            
            # 综合训练器
            if ComprehensiveTrainer:
                self.trainer = ComprehensiveTrainer()
                self.logger.info("✅ 综合训练器已初始化")
            else:
                self.trainer = None
                self.logger.warning("⚠️ 综合训练器未可用")
            
            # 策略引擎和管理器
            if StrategyEngine:
                self.strategy_engine = StrategyEngine()
                self.logger.info("✅ 策略引擎已初始化")
            else:
                self.strategy_engine = None
                self.logger.warning("⚠️ 策略引擎未可用")
                
            if StrategyManager:
                self.strategy_manager = StrategyManager()
                self.logger.info("✅ 策略管理器已初始化")
            else:
                self.strategy_manager = None
                self.logger.warning("⚠️ 策略管理器未可用")
            
            # 推荐系统
            if RecommendationTracker:
                self.recommendation_tracker = RecommendationTracker()
                self.logger.info("✅ 推荐跟踪器已初始化")
            else:
                self.recommendation_tracker = None
                self.logger.warning("⚠️ 推荐跟踪器未可用")
                
            if ModelRecommender and self.data_extractor:
                self.model_recommender = ModelRecommender(tushare_token=tushare_token)
                self.logger.info("✅ 模型推荐器已初始化")
            else:
                self.model_recommender = None
                self.logger.warning("⚠️ 模型推荐器未可用")
            
            # 模型评估器
            if ModelEvaluator:
                self.model_evaluator = ModelEvaluator()
                self.logger.info("✅ 模型评估器已初始化")
            else:
                self.model_evaluator = None
                self.logger.warning("⚠️ 模型评估器未可用")
            
            self.logger.info("核心组件初始化完成")
            
        except Exception as e:
            self.logger.error(f"组件初始化失败: {e}")
            # 不要抛出异常，允许应用继续运行
            pass
    
    def _setup_routes(self):
        """设置所有API路由"""
        
        # ==================== 基础路由 ====================
        
        @self.app.route('/')
        def index():
            """企业级统一首页"""
            return render_template('enterprise_index_minimal.html')
        
        @self.app.route('/full')
        def full_index():
            """完整版首页"""
            return render_template('enterprise_index.html')
        
        @self.app.route('/api/status')
        def get_status():
            """获取系统状态"""
            return jsonify(self.status)
        
        @self.app.route('/api/clear-logs', methods=['POST'])
        def clear_logs():
            """清除日志"""
            self.status['logs'] = []
            return jsonify({'status': 'cleared'})
        
        # ==================== 数据管理API ====================
        
        @self.app.route('/api/tushare-info')
        def tushare_info():
            """TuShare连接信息"""
            if not TuShareDataExtractor:
                return jsonify({
                    'status': 'error',
                    'error': 'TuShare数据提取器未可用'
                })
                
            token = request.args.get('token', '')
            try:
                if token:
                    extractor = TuShareDataExtractor(token)
                else:
                    extractor = self.data_extractor
                
                if not extractor:
                    return jsonify({
                        'status': 'error',
                        'error': '数据提取器未初始化'
                    })
                
                stocks = extractor.get_stock_list(limit=10)
                
                return jsonify({
                    'status': 'success',
                    'stock_count': len(stocks) if not stocks.empty else 0,
                    'has_pro_access': bool(token),
                    'sample_stocks': stocks.head(3).to_dict('records') if not stocks.empty else []
                })
            except Exception as e:
                return jsonify({
                    'status': 'error',
                    'error': str(e)
                })
        
        @self.app.route('/api/training-data-info')
        def training_data_info():
            """训练数据信息"""
            try:
                training_dir = Path('training_data')
                files = []
                
                if training_dir.exists():
                    for file_path in training_dir.glob('*.jsonl'):
                        try:
                            stat = file_path.stat()
                            sample_count = sum(1 for _ in open(file_path, 'r', encoding='utf-8'))
                            
                            files.append({
                                'filename': file_path.name,
                                'size': stat.st_size,
                                'sample_count': sample_count,
                                'created': stat.st_ctime
                            })
                        except Exception as e:
                            self.logger.error(f"读取文件失败 {file_path}: {e}")
                
                return jsonify({'files': files})
            except Exception as e:
                return jsonify({'error': str(e)})
        
        @self.app.route('/api/generate-data', methods=['POST'])
        def generate_data():
            """生成训练数据"""
            try:
                config = request.get_json()
                
                # 在后台线程中执行
                def run_generation():
                    try:
                        self.status['is_running'] = True
                        self.status['current_task'] = '正在生成训练数据...'
                        self._emit_progress(10)
                        
                        # 配置训练器
                        if config.get('tushare_token') and hasattr(self, 'trainer') and self.trainer:
                            self.trainer.data_extractor = TuShareDataExtractor(config['tushare_token'])
                        
                        # 生成数据
                        if hasattr(self, 'trainer') and self.trainer:
                            output_file = self.trainer.generate_comprehensive_dataset(
                                stock_count=config.get('stock_count', 100),
                                days_back=config.get('days_back', 365),
                                max_examples=config.get('max_examples', 1000)
                            )
                        else:
                            raise Exception('训练器不可用，请检查系统配置')
                        
                        self._emit_progress(100, '数据生成完成')
                        self.socketio.emit('task_completed', {
                            'type': 'data_generation',
                            'output_file': output_file,
                            'sample_count': config.get('max_examples', 1000)
                        })
                        
                    except Exception as e:
                        self.logger.error(f"数据生成失败: {e}")
                        self.socketio.emit('task_failed', {'error': str(e)})
                    finally:
                        self.status['is_running'] = False
                
                threading.Thread(target=run_generation, daemon=True).start()
                return jsonify({'status': 'started'})
                
            except Exception as e:
                return jsonify({'error': str(e)})
        
        # ==================== 模型管理API ====================
        
        @self.app.route('/api/models-info')
        def models_info():
            """获取模型信息"""
            try:
                models = []
                
                # 检查训练器是否可用
                if hasattr(self, 'trainer') and self.trainer:
                    try:
                        models = self.trainer.get_available_models()
                    except Exception as e:
                        self.logger.warning(f"获取训练器模型失败: {e}")
                
                # 检查Ollama模型
                try:
                    import subprocess
                    result = subprocess.run(['ollama', 'list'], 
                                          capture_output=True, text=True, timeout=10)
                    if result.returncode == 0:
                        lines = result.stdout.strip().split('\n')[1:]  # 跳过标题行
                        for line in lines:
                            if line.strip():
                                parts = line.split()
                                if len(parts) >= 1:
                                    model_name = parts[0].split(':')[0]  # 移除版本标签
                                    models.append({
                                        'name': model_name,
                                        'id': model_name,
                                        'size': parts[1] if len(parts) > 1 else '未知',
                                        'modified': parts[2] if len(parts) > 2 else '未知',
                                        'type': 'ollama'
                                    })
                except Exception as e:
                    self.logger.warning(f"检查Ollama模型失败: {e}")
                
                # 如果没有找到任何模型，返回默认模型
                if not models:
                    models = [
                        {'name': 'llama3.2', 'id': 'llama3.2', 'size': '2.0GB', 'modified': '系统默认', 'type': 'default'},
                        {'name': 'qwen2', 'id': 'qwen2', 'size': '4.4GB', 'modified': '系统默认', 'type': 'default'},
                        {'name': 'gemma2', 'id': 'gemma2', 'size': '5.4GB', 'modified': '系统默认', 'type': 'default'},
                        {'name': 'phi3', 'id': 'phi3', 'size': '2.3GB', 'modified': '系统默认', 'type': 'default'}
                    ]
                
                return jsonify({'models': models})
            except Exception as e:
                self.logger.error(f"获取模型信息失败: {e}")
                # 返回基础模型列表
                return jsonify({
                    'models': [
                        {'name': 'llama3.2', 'id': 'llama3.2', 'size': '2.0GB', 'modified': '系统默认', 'type': 'default'}
                    ]
                })
        
        @self.app.route('/api/train-model', methods=['POST'])
        def train_model():
            """训练模型"""
            try:
                config = request.get_json()
                
                def run_training():
                    try:
                        self.status['is_running'] = True
                        self.status['current_task'] = '正在训练模型...'
                        self._emit_progress(10)
                        
                        # 执行训练
                        if hasattr(self, 'trainer') and self.trainer:
                            result = self.trainer.fine_tune_model(
                                base_model=config.get('base_model', 'ljwx-stock'),
                                training_file=config['training_file'],
                                model_name=config['model_name'],
                                sample_count=config.get('sample_count', 200)
                            )
                        else:
                            raise Exception('训练器不可用，请检查系统配置')
                        
                        self._emit_progress(100, '模型训练完成')
                        self.socketio.emit('task_completed', {
                            'type': 'model_training',
                            'model_name': config['model_name'],
                            'result': result
                        })
                        
                    except Exception as e:
                        self.logger.error(f"模型训练失败: {e}")
                        self.socketio.emit('task_failed', {'error': str(e)})
                    finally:
                        self.status['is_running'] = False
                
                threading.Thread(target=run_training, daemon=True).start()
                return jsonify({'status': 'started'})
                
            except Exception as e:
                return jsonify({'error': str(e)})
        
        # ==================== 策略管理API ====================
        
        @self.app.route('/api/strategies')
        def get_strategies():
            """获取策略列表"""
            try:
                user_id = request.args.get('user_id', 'default')
                strategies = self.strategy_manager.get_strategies(user_id)
                return jsonify({'strategies': strategies})
            except Exception as e:
                return jsonify({'error': str(e)})
        
        @self.app.route('/api/strategies', methods=['POST'])
        def create_strategy():
            """创建策略"""
            try:
                strategy_data = request.get_json()
                strategy_id = self.strategy_manager.create_strategy(strategy_data)
                return jsonify({'strategy_id': strategy_id})
            except Exception as e:
                return jsonify({'error': str(e)})
        
        @self.app.route('/api/strategies/<strategy_id>')
        def get_strategy(strategy_id):
            """获取策略详情"""
            try:
                strategy = self.strategy_manager.get_strategy(strategy_id)
                return jsonify({'strategy': strategy})
            except Exception as e:
                return jsonify({'error': str(e)})
        
        @self.app.route('/api/strategies/<strategy_id>', methods=['DELETE'])
        def delete_strategy(strategy_id):
            """删除策略"""
            try:
                self.strategy_manager.delete_strategy(strategy_id)
                return jsonify({'status': 'deleted'})
            except Exception as e:
                return jsonify({'error': str(e)})
        
        @self.app.route('/api/strategy-templates')
        def get_strategy_templates():
            """获取策略模板"""
            try:
                templates = self.strategy_manager.get_templates()
                return jsonify({'templates': templates})
            except Exception as e:
                return jsonify({'error': str(e)})
        
        @self.app.route('/api/strategies/<strategy_id>/backtest', methods=['POST'])
        def run_strategy_backtest(strategy_id):
            """运行策略回测"""
            try:
                config = request.get_json()
                
                def run_backtest():
                    try:
                        self.status['is_running'] = True
                        self.status['current_task'] = '正在运行策略回测...'
                        
                        # 执行回测
                        results = self.strategy_engine.run_backtest(
                            strategy_id=strategy_id,
                            stock_code=config['stock_code'],
                            start_date=config['start_date'],
                            end_date=config['end_date'],
                            tushare_token=config.get('tushare_token')
                        )
                        
                        self.socketio.emit('backtest_completed', {
                            'strategy_id': strategy_id,
                            'summary': results
                        })
                        
                    except Exception as e:
                        self.logger.error(f"策略回测失败: {e}")
                        self.socketio.emit('task_failed', {'error': str(e)})
                    finally:
                        self.status['is_running'] = False
                
                threading.Thread(target=run_backtest, daemon=True).start()
                return jsonify({'status': 'started'})
                
            except Exception as e:
                return jsonify({'error': str(e)})
        
        # ==================== 推荐系统API ====================
        
        @self.app.route('/api/recommendations/generate', methods=['POST'])
        def generate_recommendations():
            """生成股票推荐"""
            try:
                config = request.get_json()
                
                def run_generation():
                    try:
                        self.status['is_running'] = True
                        self.status['current_task'] = '正在生成股票推荐...'
                        
                        # 配置推荐器
                        if config.get('tushare_token'):
                            self.model_recommender = ModelRecommender(
                                model_name=config['model_name'],
                                tushare_token=config['tushare_token']
                            )
                        else:
                            self.model_recommender.model_name = config['model_name']
                        
                        # 生成推荐
                        if config.get('use_daily_mode', True):
                            recommendations = self.model_recommender.generate_daily_recommendations(
                                num_stocks=config.get('num_stocks', 5)
                            )
                        else:
                            # 获取指定股票列表生成推荐
                            stock_codes = config.get('stock_codes', [])
                            recommendations = self.model_recommender.generate_stock_recommendations(
                                stock_codes=stock_codes,
                                analysis_type=config.get('analysis_type', 'comprehensive_analysis'),
                                max_stocks=config.get('num_stocks', 5)
                            )
                        
                        self.socketio.emit('recommendation_generated', {
                            'model_name': config['model_name'],
                            'total_recommendations': len(recommendations),
                            'recommendation_ids': recommendations
                        })
                        
                    except Exception as e:
                        self.logger.error(f"推荐生成失败: {e}")
                        self.socketio.emit('task_failed', {'error': str(e)})
                    finally:
                        self.status['is_running'] = False
                
                threading.Thread(target=run_generation, daemon=True).start()
                return jsonify({'status': 'started'})
                
            except Exception as e:
                return jsonify({'error': str(e)})
        
        @self.app.route('/api/recommendations/validate', methods=['POST'])
        def validate_recommendations():
            """验证推荐结果"""
            try:
                config = request.get_json()
                
                def run_validation():
                    try:
                        self.status['is_running'] = True
                        self.status['current_task'] = '正在验证推荐结果...'
                        
                        # 验证推荐
                        model_name = config.get('model_name')
                        if model_name:
                            validated_count = self.recommendation_tracker.validate_recommendations(model_name)
                        else:
                            validated_count = self.recommendation_tracker.validate_recommendations()
                        
                        self.socketio.emit('validation_completed', {
                            'model_name': model_name,
                            'validated_count': validated_count
                        })
                        
                    except Exception as e:
                        self.logger.error(f"推荐验证失败: {e}")
                        self.socketio.emit('task_failed', {'error': str(e)})
                    finally:
                        self.status['is_running'] = False
                
                threading.Thread(target=run_validation, daemon=True).start()
                return jsonify({'status': 'started'})
                
            except Exception as e:
                return jsonify({'error': str(e)})
        
        @self.app.route('/api/recommendations/<model_name>')
        def get_recommendations(model_name):
            """获取推荐列表"""
            try:
                limit = int(request.args.get('limit', 20))
                recommendations = self.recommendation_tracker.get_recommendations_by_model(
                    model_name, limit=limit
                )
                
                return jsonify({
                    'recommendations': [rec.__dict__ for rec in recommendations]
                })
            except Exception as e:
                return jsonify({'error': str(e)})
        
        @self.app.route('/api/recommendations/statistics')
        def get_recommendation_statistics():
            """获取推荐统计"""
            try:
                if self.recommendation_tracker:
                    stats = self.recommendation_tracker.get_statistics()
                    return jsonify(stats)
                else:
                    # 返回模拟数据
                    return jsonify({
                        'total_statistics': {
                            'total_recommendations': 0,
                            'overall_hit_rate': 0.0,
                            'total_hits': 0,
                            'pending_recommendations': 0,
                            'avg_return': 0.0
                        }
                    })
            except Exception as e:
                return jsonify({'error': str(e)})
        
        @self.app.route('/api/recommendations/backtest/<model_name>')
        def get_recommendation_backtest(model_name):
            """获取推荐回测报告"""
            try:
                start_date = request.args.get('start_date')
                end_date = request.args.get('end_date')
                
                if not end_date:
                    end_date = datetime.now().strftime('%Y-%m-%d')
                if not start_date:
                    start_date = (datetime.now() - timedelta(days=30)).strftime('%Y-%m-%d')
                
                metrics = self.recommendation_tracker.generate_backtest_report(
                    model_name, start_date, end_date
                )
                
                return jsonify({
                    'metrics': metrics.__dict__ if metrics else None
                })
            except Exception as e:
                return jsonify({'error': str(e)})
        
        # ==================== 模型评估API ====================
        
        @self.app.route('/api/evaluation/categories')
        def get_evaluation_categories():
            """获取评估类别"""
            try:
                categories = [
                    {'value': '', 'label': '全部类别'},
                    {'value': 'technical', 'label': '技术分析'},
                    {'value': 'fundamental', 'label': '基本面分析'},
                    {'value': 'risk', 'label': '风险评估'}
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
            except Exception as e:
                return jsonify({'error': str(e)})
        
        @self.app.route('/api/evaluation/start', methods=['POST'])
        def start_evaluation():
            """开始模型评估"""
            try:
                config = request.get_json()
                
                def run_evaluation():
                    try:
                        self.status['is_running'] = True
                        self.status['current_task'] = '正在评估模型...'
                        
                        # 运行评估
                        results = self.model_evaluator.evaluate_model(
                            model_name=config['model_name'],
                            category=config.get('category'),
                            difficulty=config.get('difficulty'),
                            timeout=config.get('timeout', 30)
                        )
                        
                        self.socketio.emit('evaluation_completed', {
                            'model_name': config['model_name'],
                            'summary': results
                        })
                        
                    except Exception as e:
                        self.logger.error(f"模型评估失败: {e}")
                        self.socketio.emit('task_failed', {'error': str(e)})
                    finally:
                        self.status['is_running'] = False
                
                threading.Thread(target=run_evaluation, daemon=True).start()
                return jsonify({'status': 'started'})
                
            except Exception as e:
                return jsonify({'error': str(e)})
        
        # ==================== 移动端API ====================
        
        @self.app.route('/api/mobile/dashboard')
        def mobile_dashboard():
            """移动端仪表板数据"""
            try:
                # 获取系统概览数据
                dashboard_data = {
                    'system_status': {
                        'status': '正常运行' if not self.status['is_running'] else '处理中',
                        'last_update': datetime.now().isoformat()
                    },
                    'training_summary': {
                        'total_models': len(self.trainer.get_available_models()) if hasattr(self, 'trainer') and self.trainer else 0,
                        'total_samples': self._get_total_training_samples(),
                        'last_training': self._get_last_training_time()
                    },
                    'recommendation_summary': {
                        'total_recommendations': self._get_total_recommendations(),
                        'hit_rate': self._get_overall_hit_rate(),
                        'active_models': self._get_active_models_count()
                    },
                    'strategy_summary': {
                        'total_strategies': len(self.strategy_manager.get_strategies('default')),
                        'active_backtests': 0,  # 可以后续实现
                        'best_performance': self._get_best_strategy_performance()
                    }
                }
                
                return jsonify(dashboard_data)
            except Exception as e:
                return jsonify({'error': str(e)})
        
        @self.app.route('/api/mobile/recommendations/latest')
        def mobile_latest_recommendations():
            """移动端最新推荐"""
            try:
                limit = int(request.args.get('limit', 10))
                
                # 检查推荐跟踪器是否可用
                if not self.recommendation_tracker:
                    return jsonify({
                        'recommendations': [],
                        'total': 0,
                        'message': '推荐系统暂不可用'
                    })
                
                # 检查方法是否存在
                if not hasattr(self.recommendation_tracker, 'get_latest_recommendations'):
                    return jsonify({
                        'recommendations': [],
                        'total': 0,
                        'message': '推荐功能正在初始化'
                    })
                
                latest_recs = self.recommendation_tracker.get_latest_recommendations(limit)
                
                mobile_recs = []
                for rec in latest_recs:
                    mobile_recs.append({
                        'id': rec.id,
                        'stock_code': rec.stock_code,
                        'stock_name': rec.stock_name,
                        'recommendation_type': rec.recommendation_type,
                        'confidence_score': rec.confidence_score,
                        'target_price': rec.target_price,
                        'actual_return': rec.actual_return,
                        'result': rec.result,
                        'created_time': rec.created_time,
                        'model_name': rec.model_name
                    })
                
                return jsonify({'recommendations': mobile_recs})
            except Exception as e:
                return jsonify({'error': str(e)})
        
        @self.app.route('/api/mobile/quick-analysis', methods=['POST'])
        def mobile_quick_analysis():
            """移动端快速分析"""
            try:
                data = request.get_json()
                stock_code = data.get('stock_code')
                analysis_type = data.get('analysis_type', 'quick')
                
                if not stock_code:
                    return jsonify({'error': '股票代码不能为空'})
                
                # 获取股票基本信息和简单分析
                analysis_result = self._quick_stock_analysis(stock_code)
                
                return jsonify({
                    'stock_code': stock_code,
                    'analysis': analysis_result,
                    'timestamp': datetime.now().isoformat()
                })
                
            except Exception as e:
                return jsonify({'error': str(e)})
    
    def _setup_socket_events(self):
        """设置Socket.IO事件"""
        
        @self.socketio.on('connect')
        def handle_connect():
            emit('connected', {'status': 'connected'})
            self.logger.info('客户端已连接')
        
        @self.socketio.on('disconnect')
        def handle_disconnect():
            self.logger.info('客户端已断开连接')
    
    def _emit_progress(self, progress: int, task: str = ''):
        """发送进度更新"""
        self.status['progress'] = progress
        if task:
            self.status['current_task'] = task
        
        self.socketio.emit('progress_update', {
            'progress': progress,
            'task': task
        })
    
    def _add_log(self, message: str, level: str = 'info'):
        """添加日志"""
        log_entry = {
            'timestamp': datetime.now().isoformat(),
            'message': message,
            'level': level
        }
        
        self.status['logs'].append(log_entry)
        
        # 限制日志数量
        if len(self.status['logs']) > 1000:
            self.status['logs'] = self.status['logs'][-500:]
        
        self.socketio.emit('log_update', log_entry)
    
    # ==================== 辅助方法 ====================
    
    def _get_total_training_samples(self) -> int:
        """获取总训练样本数"""
        try:
            training_dir = Path('training_data')
            total = 0
            if training_dir.exists():
                for file_path in training_dir.glob('*.jsonl'):
                    total += sum(1 for _ in open(file_path, 'r', encoding='utf-8'))
            return total
        except:
            return 0
    
    def _get_last_training_time(self) -> str:
        """获取最后训练时间"""
        try:
            # 简单实现，可以后续优化
            return datetime.now().strftime('%Y-%m-%d %H:%M')
        except:
            return '未知'
    
    def _get_total_recommendations(self) -> int:
        """获取总推荐数"""
        try:
            stats = self.recommendation_tracker.get_statistics()
            return stats.get('total_statistics', {}).get('total_recommendations', 0)
        except:
            return 0
    
    def _get_overall_hit_rate(self) -> float:
        """获取总体命中率"""
        try:
            stats = self.recommendation_tracker.get_statistics()
            return stats.get('total_statistics', {}).get('overall_hit_rate', 0.0)
        except:
            return 0.0
    
    def _get_active_models_count(self) -> int:
        """获取活跃模型数量"""
        try:
            if hasattr(self, 'trainer') and self.trainer:
                return len(self.trainer.get_available_models())
            return 0
        except:
            return 0
    
    def _get_best_strategy_performance(self) -> float:
        """获取最佳策略表现"""
        try:
            # 简单实现，返回模拟数据
            return 0.15  # 15%收益率
        except:
            return 0.0
    
    def _quick_stock_analysis(self, stock_code: str) -> Dict:
        """快速股票分析"""
        try:
            # 检查数据提取器是否可用
            if not hasattr(self, 'data_extractor') or self.data_extractor is None:
                # 尝试重新初始化数据提取器
                try:
                    from llm.tushare_data_extractor import TuShareDataExtractor
                    tushare_token = os.getenv('TUSHARE_TOKEN', '58cf834df2a4b9a5404f6416248cffa0da78d10e31496385251d7aef')
                    self.data_extractor = TuShareDataExtractor(tushare_token)
                    self.logger.info("重新初始化数据提取器成功")
                except Exception as e:
                    self.logger.error(f"重新初始化数据提取器失败: {e}")
                    return {
                        'error': '数据服务不可用',
                        'suggestion': '请检查TuShare连接或联系管理员',
                        'code': stock_code
                    }
            
            # 获取基本数据
            end_date = datetime.now().strftime('%Y%m%d')
            start_date = (datetime.now() - timedelta(days=30)).strftime('%Y%m%d')
            
            daily_data = self.data_extractor.get_stock_daily_data(stock_code, start_date, end_date)
            
            if daily_data is None or daily_data.empty:
                return {
                    'error': f'无法获取股票 {stock_code} 的数据',
                    'suggestion': '请检查股票代码是否正确（如：000001.SZ 或 600000.SH）',
                    'code': stock_code
                }
            
            latest = daily_data.iloc[-1]
            
            # 计算简单指标
            ma5 = daily_data['close'].tail(5).mean()
            ma20 = daily_data['close'].tail(20).mean() if len(daily_data) >= 20 else ma5
            
            # 价格趋势
            trend = '上涨' if latest['close'] > ma20 else '下跌'
            
            return {
                'current_price': float(latest['close']),
                'price_change': float(latest['pct_chg']),
                'volume': float(latest['vol']),
                'ma5': float(ma5),
                'ma20': float(ma20),
                'trend': trend,
                'analysis_time': datetime.now().strftime('%Y-%m-%d %H:%M:%S'),
                'stock_code': stock_code,
                'data_date': latest['trade_date'] if 'trade_date' in latest else 'N/A'
            }
            
        except Exception as e:
            self.logger.error(f"股票分析失败 {stock_code}: {e}")
            return {
                'error': f'分析失败: {str(e)}',
                'suggestion': '请稍后重试或检查网络连接',
                'code': stock_code
            }
    
    def run(self, host='0.0.0.0', port=5005, debug=False):
        """启动应用"""
        self.logger.info(f"启动ljwx-stock统一应用 - http://{host}:{port}")
        self.socketio.run(self.app, host=host, port=port, debug=debug, allow_unsafe_werkzeug=True)

def app_factory():
    """应用工厂函数，用于Gunicorn"""
    app_instance = UnifiedStockApp()
    return app_instance.app

def main():
    """主函数"""
    print("🚀 ljwx-stock 统一应用启动器")
    print("=" * 50)
    
    try:
        # 创建应用实例
        app = UnifiedStockApp()
        
        print("✅ 应用初始化完成")
        print(f"🌐 Web服务: http://localhost:5005")
        print(f"📱 移动API: http://localhost:5005/api/mobile/")
        print(f"🛑 按 Ctrl+C 停止服务")
        print("=" * 50)
        
        # 启动服务
        app.run(port=5005, debug=False)
        
    except KeyboardInterrupt:
        print("\n👋 服务已停止")
    except Exception as e:
        print(f"❌ 启动失败: {e}")
        sys.exit(1)

if __name__ == '__main__':
    main()