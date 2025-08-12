#!/usr/bin/env python3
"""
策略训练API接口
Strategy Training API Endpoints for AI Stock Recommendations
"""

import os
import sys
import json
import logging
from datetime import datetime
from flask import Blueprint, request, jsonify, current_app
from typing import Dict, List, Optional, Any

# 添加项目路径
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from strategy_training.dataset_generator import StrategyDatasetGenerator
from strategy_training.unified_trainer import UnifiedModelTrainer
from strategy_training.recommendation_scheduler import RecommendationScheduler
from strategy.market_mainstream_strategies import get_all_strategy_ids, MAINSTREAM_STRATEGIES

# 创建蓝图
strategy_training_bp = Blueprint('strategy_training', __name__, url_prefix='/api/strategy-training')

# 全局变量
dataset_generator = None
unified_trainer = None
recommendation_scheduler = None
logger = logging.getLogger(__name__)

def get_dataset_generator():
    """获取数据集生成器实例"""
    global dataset_generator
    if dataset_generator is None:
        tushare_token = os.getenv('TUSHARE_TOKEN')
        dataset_generator = StrategyDatasetGenerator(tushare_token)
        logger.info("✅ 数据集生成器已初始化")
    return dataset_generator

def get_unified_trainer():
    """获取统一训练器实例"""
    global unified_trainer
    if unified_trainer is None:
        tushare_token = os.getenv('TUSHARE_TOKEN')
        unified_trainer = UnifiedModelTrainer(tushare_token)
        logger.info("✅ 统一训练器已初始化")
    return unified_trainer

def get_recommendation_scheduler():
    """获取推荐调度器实例"""
    global recommendation_scheduler
    if recommendation_scheduler is None:
        tushare_token = os.getenv('TUSHARE_TOKEN')
        recommendation_scheduler = RecommendationScheduler(tushare_token)
        recommendation_scheduler.initialize()
        logger.info("✅ 推荐调度器已初始化")
    return recommendation_scheduler

@strategy_training_bp.route('/health', methods=['GET'])
def health_check():
    """健康检查"""
    try:
        return jsonify({
            "success": True,
            "status": "healthy",
            "timestamp": datetime.now().isoformat(),
            "components": {
                "dataset_generator": "ready",
                "unified_trainer": "ready", 
                "recommendation_scheduler": "ready"
            }
        })
    except Exception as e:
        logger.error(f"健康检查失败: {e}")
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500

@strategy_training_bp.route('/strategies', methods=['GET'])
def get_available_strategies():
    """获取可用策略列表"""
    try:
        strategy_ids = get_all_strategy_ids()
        strategies_info = []
        
        for strategy_id in strategy_ids:
            if strategy_id in MAINSTREAM_STRATEGIES:
                strategy = MAINSTREAM_STRATEGIES[strategy_id]
                strategies_info.append({
                    "id": strategy_id,
                    "name": strategy.get("name", strategy_id),
                    "description": strategy.get("description", ""),
                    "strategy_type": strategy.get("strategy_type", "technical"),
                    "parameters": strategy.get("parameters", {})
                })
        
        return jsonify({
            "success": True,
            "strategies": strategies_info,
            "total_count": len(strategies_info)
        })
        
    except Exception as e:
        logger.error(f"获取策略列表失败: {e}")
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500

@strategy_training_bp.route('/dataset/generate', methods=['POST'])
def generate_dataset():
    """生成训练数据集"""
    try:
        data = request.get_json() or {}
        
        # 参数提取
        strategy_id = data.get('strategy_id')
        num_stocks = data.get('num_stocks', 30)
        time_range = data.get('time_range', 180)
        
        if not strategy_id:
            return jsonify({
                "success": False,
                "error": "缺少必需参数: strategy_id"
            }), 400
        
        if strategy_id not in MAINSTREAM_STRATEGIES:
            return jsonify({
                "success": False,
                "error": f"未知策略ID: {strategy_id}"
            }), 400
        
        # 生成数据集
        generator = get_dataset_generator()
        samples = generator.generate_strategy_dataset(strategy_id, num_stocks, time_range)
        
        return jsonify({
            "success": True,
            "strategy_id": strategy_id,
            "samples_generated": len(samples),
            "parameters": {
                "num_stocks": num_stocks,
                "time_range": time_range
            },
            "generated_at": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"生成数据集失败: {e}")
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500

@strategy_training_bp.route('/dataset/generate-all', methods=['POST'])
def generate_all_datasets():
    """生成所有策略的训练数据集"""
    try:
        data = request.get_json() or {}
        
        # 参数提取
        num_stocks = data.get('num_stocks', 30)
        time_range = data.get('time_range', 180)
        strategy_ids = data.get('strategy_ids')  # 可选，指定特定策略
        
        # 生成所有数据集
        generator = get_dataset_generator()
        
        if strategy_ids:
            # 验证策略ID
            invalid_strategies = [sid for sid in strategy_ids if sid not in MAINSTREAM_STRATEGIES]
            if invalid_strategies:
                return jsonify({
                    "success": False,
                    "error": f"未知策略ID: {', '.join(invalid_strategies)}"
                }), 400
        
        # 异步执行生成任务
        import threading
        
        def generate_task():
            try:
                generator.generate_all_strategy_datasets(num_stocks, time_range)
                logger.info("所有数据集生成完成")
            except Exception as e:
                logger.error(f"生成所有数据集失败: {e}")
        
        thread = threading.Thread(target=generate_task, daemon=True)
        thread.start()
        
        return jsonify({
            "success": True,
            "message": "数据集生成任务已启动",
            "parameters": {
                "num_stocks": num_stocks,
                "time_range": time_range,
                "strategy_count": len(strategy_ids) if strategy_ids else len(get_all_strategy_ids())
            },
            "started_at": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"启动数据集生成失败: {e}")
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500

@strategy_training_bp.route('/model/train', methods=['POST'])
def train_unified_model():
    """训练统一模型"""
    try:
        data = request.get_json() or {}
        
        # 参数提取
        dataset_file = data.get('dataset_file')
        strategy_ids = data.get('strategy_ids')
        num_stocks_per_strategy = data.get('num_stocks_per_strategy', 30)
        time_range = data.get('time_range', 180)
        
        trainer = get_unified_trainer()
        
        if dataset_file:
            # 使用指定的数据集文件训练
            model_name = trainer.train_unified_model(dataset_file)
            
            return jsonify({
                "success": True,
                "model_name": model_name,
                "dataset_file": dataset_file,
                "trained_at": datetime.now().isoformat()
            })
        else:
            # 执行完整训练流水线
            import threading
            
            def training_task():
                try:
                    result = trainer.full_training_pipeline(
                        strategy_ids, num_stocks_per_strategy, time_range
                    )
                    logger.info(f"训练流水线完成: {result}")
                except Exception as e:
                    logger.error(f"训练流水线失败: {e}")
            
            thread = threading.Thread(target=training_task, daemon=True)
            thread.start()
            
            return jsonify({
                "success": True,
                "message": "模型训练任务已启动",
                "parameters": {
                    "strategy_ids": strategy_ids,
                    "num_stocks_per_strategy": num_stocks_per_strategy,
                    "time_range": time_range
                },
                "started_at": datetime.now().isoformat()
            })
        
    except Exception as e:
        logger.error(f"训练统一模型失败: {e}")
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500

@strategy_training_bp.route('/recommendations/generate', methods=['POST'])
def generate_recommendations():
    """生成推荐"""
    try:
        data = request.get_json() or {}
        
        # 参数提取
        category = data.get('category', 'general')
        force_generate = data.get('force_generate', False)
        
        scheduler = get_recommendation_scheduler()
        
        if force_generate:
            # 强制生成新推荐
            result = scheduler.force_generate_recommendations(category)
        else:
            # 检查缓存
            cached_result = scheduler.get_cached_recommendations(category)
            if cached_result:
                return jsonify({
                    "success": True,
                    "source": "cache",
                    "category": category,
                    "recommendations": cached_result["recommendations"],
                    "generated_at": cached_result["generated_at"],
                    "count": len(cached_result["recommendations"])
                })
            else:
                # 生成新推荐
                result = scheduler.force_generate_recommendations(category)
        
        return jsonify({
            "success": result["success"],
            "source": "generated",
            "category": category,
            "recommendations": result.get("recommendations", []),
            "count": result.get("count", 0),
            "generated_at": datetime.now().isoformat(),
            "error": result.get("error")
        })
        
    except Exception as e:
        logger.error(f"生成推荐失败: {e}")
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500

@strategy_training_bp.route('/recommendations/personalized', methods=['POST'])
def generate_personalized_recommendations():
    """生成个性化推荐"""
    try:
        data = request.get_json() or {}
        
        # 参数提取
        user_id = data.get('user_id')
        force_generate = data.get('force_generate', False)
        
        if not user_id:
            return jsonify({
                "success": False,
                "error": "缺少必需参数: user_id"
            }), 400
        
        scheduler = get_recommendation_scheduler()
        
        if not force_generate:
            # 检查缓存
            cached_result = scheduler.get_cached_recommendations(user_id=user_id)
            if cached_result:
                return jsonify({
                    "success": True,
                    "source": "cache",
                    "user_id": user_id,
                    "recommendations": cached_result["recommendations"],
                    "generated_at": cached_result["generated_at"],
                    "count": len(cached_result["recommendations"])
                })
        
        # 生成个性化推荐
        result = scheduler._generate_personalized_recommendations()
        
        if result["success"] and user_id in result["user_results"]:
            user_result = result["user_results"][user_id]
            if user_result["success"]:
                cached_result = scheduler.get_cached_recommendations(user_id=user_id)
                return jsonify({
                    "success": True,
                    "source": "generated",
                    "user_id": user_id,
                    "recommendations": cached_result["recommendations"] if cached_result else [],
                    "generated_at": datetime.now().isoformat(),
                    "count": user_result["count"]
                })
        
        return jsonify({
            "success": False,
            "error": "生成个性化推荐失败"
        }), 500
        
    except Exception as e:
        logger.error(f"生成个性化推荐失败: {e}")
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500

@strategy_training_bp.route('/scheduler/start', methods=['POST'])
def start_scheduler():
    """启动推荐调度器"""
    try:
        scheduler = get_recommendation_scheduler()
        
        if scheduler.is_running:
            return jsonify({
                "success": True,
                "message": "推荐调度器已在运行",
                "status": "running"
            })
        
        scheduler.start_scheduler()
        
        return jsonify({
            "success": True,
            "message": "推荐调度器已启动",
            "status": "started",
            "started_at": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"启动推荐调度器失败: {e}")
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500

@strategy_training_bp.route('/scheduler/stop', methods=['POST'])
def stop_scheduler():
    """停止推荐调度器"""
    try:
        scheduler = get_recommendation_scheduler()
        
        if not scheduler.is_running:
            return jsonify({
                "success": True,
                "message": "推荐调度器未在运行",
                "status": "stopped"
            })
        
        scheduler.stop_scheduler()
        
        return jsonify({
            "success": True,
            "message": "推荐调度器已停止",
            "status": "stopped",
            "stopped_at": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"停止推荐调度器失败: {e}")
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500

@strategy_training_bp.route('/scheduler/status', methods=['GET'])
def scheduler_status():
    """获取调度器状态"""
    try:
        scheduler = get_recommendation_scheduler()
        
        return jsonify({
            "success": True,
            "status": {
                "is_running": scheduler.is_running,
                "config": scheduler.config,
                "cache_info": {
                    "general": "available" if "general" in scheduler.recommendation_cache else "empty",
                    "realtime": "available" if "realtime" in scheduler.recommendation_cache else "empty",
                    "user_count": len([k for k in scheduler.recommendation_cache.keys() if k.startswith("user_")])
                }
            },
            "checked_at": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"获取调度器状态失败: {e}")
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500

@strategy_training_bp.route('/status', methods=['GET'])
def system_status():
    """获取系统状态"""
    try:
        # 检查各组件状态
        components_status = {}
        
        try:
            generator = get_dataset_generator()
            components_status["dataset_generator"] = "ready"
        except Exception as e:
            components_status["dataset_generator"] = f"error: {str(e)}"
        
        try:
            trainer = get_unified_trainer()
            components_status["unified_trainer"] = "ready"
        except Exception as e:
            components_status["unified_trainer"] = f"error: {str(e)}"
        
        try:
            scheduler = get_recommendation_scheduler()
            components_status["recommendation_scheduler"] = "ready" if scheduler.is_running else "stopped"
        except Exception as e:
            components_status["recommendation_scheduler"] = f"error: {str(e)}"
        
        # 检查Ollama状态
        import subprocess
        try:
            result = subprocess.run(['ollama', 'list'], capture_output=True, text=True, timeout=5)
            ollama_status = "available" if result.returncode == 0 else "error"
        except:
            ollama_status = "unavailable"
        
        return jsonify({
            "success": True,
            "system_status": {
                "components": components_status,
                "ollama": ollama_status,
                "tushare_token": "configured" if os.getenv('TUSHARE_TOKEN') else "missing"
            },
            "checked_at": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"获取系统状态失败: {e}")
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500

@strategy_training_bp.route('/models/list', methods=['GET'])
def list_trained_models():
    """列出已训练的模型"""
    try:
        models_dir = "trained_models"
        models = []
        
        if os.path.exists(models_dir):
            for filename in os.listdir(models_dir):
                if filename.endswith("_info.json"):
                    try:
                        with open(os.path.join(models_dir, filename), 'r', encoding='utf-8') as f:
                            model_info = json.load(f)
                            models.append(model_info)
                    except Exception as e:
                        logger.error(f"读取模型信息文件 {filename} 失败: {e}")
        
        # 按创建时间排序
        models.sort(key=lambda x: x.get("created_at", ""), reverse=True)
        
        return jsonify({
            "success": True,
            "models": models,
            "total_count": len(models)
        })
        
    except Exception as e:
        logger.error(f"列出训练模型失败: {e}")
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500

# 错误处理
@strategy_training_bp.errorhandler(404)
def not_found(error):
    return jsonify({
        "success": False,
        "error": "API端点未找到"
    }), 404

@strategy_training_bp.errorhandler(405)
def method_not_allowed(error):
    return jsonify({
        "success": False,
        "error": "HTTP方法不允许"
    }), 405

@strategy_training_bp.errorhandler(500)
def internal_error(error):
    return jsonify({
        "success": False,
        "error": "内部服务器错误"
    }), 500