#!/usr/bin/env python3
"""
个人股票推荐API模块
提供个性化推荐、筛选、历史记录等功能
"""

from flask import Blueprint, request, jsonify, current_app
from datetime import datetime, timedelta
import logging
import json
from typing import Dict, List, Optional, Any
import sys
import os

# 添加项目路径
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

# 创建蓝图和日志
recommendations_bp = Blueprint('recommendations', __name__)
logger = logging.getLogger(__name__)

try:
    from llm.intelligent_recommender import intelligent_recommender
except ImportError as e:
    logger.warning(f"智能推荐模块导入失败: {e}")
    # 创建一个模拟的智能推荐器
    class MockIntelligentRecommender:
        async def get_latest_intelligent_recommendations(self, limit=20):
            return []
        async def generate_intelligent_recommendations(self, limit=20):
            return []
        async def save_intelligent_recommendations(self, recommendations):
            return True
    intelligent_recommender = MockIntelligentRecommender()

try:
    from recommendation_backtest.model_recommender import ModelRecommender
except ImportError as e:
    logger.warning(f"模型推荐器导入失败: {e}")
    # 创建一个模拟的模型推荐器
    class MockModelRecommender:
        def __init__(self):
            pass
    ModelRecommender = MockModelRecommender

class PersonalRecommendationAPI:
    """个人推荐API处理器"""
    
    def __init__(self):
        self.intelligent_recommender = intelligent_recommender
        self.model_recommender = ModelRecommender()
        
    def get_personalized_recommendations(self, user_id: str = 'default', 
                                             limit: int = 20,
                                             recommendation_type: str = None,
                                             risk_level: str = None,
                                             sector: str = None,
                                             confidence_level: str = None) -> Dict[str, Any]:
        """获取个性化推荐（同步版本）"""
        try:
            import asyncio
            
            # 创建新的事件循环
            loop = asyncio.new_event_loop()
            asyncio.set_event_loop(loop)
            
            try:
                # 获取智能推荐
                recommendations = loop.run_until_complete(
                    self.intelligent_recommender.get_latest_intelligent_recommendations(limit * 2)
                )
                
                # 如果没有数据，尝试生成新推荐
                if not recommendations:
                    logger.info("没有找到现有推荐，开始生成新推荐...")
                    new_recommendations = loop.run_until_complete(
                        self.intelligent_recommender.generate_intelligent_recommendations(limit)
                    )
                    if new_recommendations:
                        loop.run_until_complete(
                            self.intelligent_recommender.save_intelligent_recommendations(new_recommendations)
                        )
                        recommendations = [self._convert_to_dict(rec) for rec in new_recommendations]
                
                # 应用筛选条件
                filtered_recommendations = self._apply_filters(
                    recommendations, recommendation_type, risk_level, sector, confidence_level
                )
                
                # 限制返回数量
                final_recommendations = filtered_recommendations[:limit]
                
                # 计算统计信息
                stats = self._calculate_statistics(recommendations)
                
                return {
                    'success': True,
                    'recommendations': final_recommendations,
                    'total_count': len(recommendations),
                    'filtered_count': len(filtered_recommendations),
                    'returned_count': len(final_recommendations),
                    'statistics': stats,
                    'timestamp': datetime.now().isoformat()
                }
            finally:
                loop.close()
                
        except Exception as e:
            logger.error(f"获取个性化推荐失败: {e}")
            return {
                'success': False,
                'error': str(e),
                'recommendations': [],
                'timestamp': datetime.now().isoformat()
            }
    
    def _convert_to_dict(self, recommendation) -> Dict[str, Any]:
        """将推荐对象转换为字典"""
        if hasattr(recommendation, '__dict__'):
            rec_dict = recommendation.__dict__.copy()
        else:
            rec_dict = recommendation
            
        # 确保关键字段存在
        rec_dict.setdefault('ts_code', rec_dict.get('stock_code', ''))
        rec_dict.setdefault('stock_code', rec_dict.get('ts_code', ''))
        rec_dict.setdefault('stock_name', rec_dict.get('name', ''))
        rec_dict.setdefault('name', rec_dict.get('stock_name', ''))
        rec_dict.setdefault('recommendation_type', rec_dict.get('recommendation', 'hold'))
        rec_dict.setdefault('confidence_score', rec_dict.get('confidence', 0.5))
        rec_dict.setdefault('current_price', 0.0)
        rec_dict.setdefault('target_price', None)
        rec_dict.setdefault('stop_loss', None)
        rec_dict.setdefault('risk_level', 'medium')
        rec_dict.setdefault('sector', '其他')
        
        # 处理JSON字段
        if isinstance(rec_dict.get('key_factors'), str):
            try:
                rec_dict['key_factors'] = json.loads(rec_dict['key_factors'])
            except:
                rec_dict['key_factors'] = []
                
        if isinstance(rec_dict.get('technical_signals'), str):
            try:
                rec_dict['technical_signals'] = json.loads(rec_dict['technical_signals'])
            except:
                rec_dict['technical_signals'] = {}
        
        return rec_dict
    
    def _apply_filters(self, recommendations: List[Dict], 
                      recommendation_type: str = None,
                      risk_level: str = None, 
                      sector: str = None,
                      confidence_level: str = None) -> List[Dict]:
        """应用筛选条件"""
        filtered = recommendations
        
        # 推荐类型筛选
        if recommendation_type:
            filtered = [r for r in filtered 
                       if r.get('recommendation_type') == recommendation_type or 
                          r.get('recommendation') == recommendation_type]
        
        # 风险等级筛选
        if risk_level:
            filtered = [r for r in filtered 
                       if risk_level in str(r.get('risk_level', '')).lower()]
        
        # 行业板块筛选
        if sector:
            filtered = [r for r in filtered 
                       if sector.lower() in str(r.get('sector', '')).lower()]
        
        # 置信度筛选
        if confidence_level:
            confidence_threshold = {
                'high': 0.8,
                'medium': 0.6, 
                'low': 0.0
            }.get(confidence_level, 0.0)
            
            if confidence_level == 'high':
                filtered = [r for r in filtered 
                           if (r.get('confidence_score') or r.get('confidence', 0)) >= 0.8]
            elif confidence_level == 'medium':
                filtered = [r for r in filtered 
                           if 0.6 <= (r.get('confidence_score') or r.get('confidence', 0)) < 0.8]
            elif confidence_level == 'low':
                filtered = [r for r in filtered 
                           if (r.get('confidence_score') or r.get('confidence', 0)) < 0.6]
        
        return filtered
    
    def _calculate_statistics(self, recommendations: List[Dict]) -> Dict[str, Any]:
        """计算推荐统计信息"""
        if not recommendations:
            return {
                'total_recommendations': 0,
                'buy_recommendations': 0,
                'hold_recommendations': 0,
                'sell_recommendations': 0,
                'average_confidence': 0.0,
                'risk_distribution': {}
            }
        
        total = len(recommendations)
        buy_count = len([r for r in recommendations 
                        if (r.get('recommendation_type') or r.get('recommendation')) == 'buy'])
        hold_count = len([r for r in recommendations 
                         if (r.get('recommendation_type') or r.get('recommendation')) == 'hold'])
        sell_count = len([r for r in recommendations 
                         if (r.get('recommendation_type') or r.get('recommendation')) == 'sell'])
        
        confidences = [(r.get('confidence_score') or r.get('confidence', 0)) 
                      for r in recommendations]
        avg_confidence = sum(confidences) / len(confidences) if confidences else 0.0
        
        # 风险分布
        risk_distribution = {}
        for rec in recommendations:
            risk = rec.get('risk_level', 'medium')
            risk_distribution[risk] = risk_distribution.get(risk, 0) + 1
        
        return {
            'total_recommendations': total,
            'buy_recommendations': buy_count,
            'hold_recommendations': hold_count,
            'sell_recommendations': sell_count,
            'average_confidence': avg_confidence,
            'risk_distribution': risk_distribution
        }

# 创建API处理器实例
personal_rec_api = PersonalRecommendationAPI()

@recommendations_bp.route('/api/mobile/recommendations/intelligent', methods=['GET'])
def get_intelligent_recommendations():
    """获取智能推荐"""
    try:
        # 获取查询参数
        user_id = request.args.get('user_id', 'default')
        limit = min(int(request.args.get('limit', 20)), 100)  # 限制最大50条
        recommendation_type = request.args.get('type')
        risk_level = request.args.get('risk_level')
        sector = request.args.get('sector')
        confidence_level = request.args.get('confidence')
        
        # 使用同步方式获取推荐
        result = personal_rec_api.get_personalized_recommendations(
            user_id=user_id,
            limit=limit,
            recommendation_type=recommendation_type,
            risk_level=risk_level,
            sector=sector,
            confidence_level=confidence_level
        )
        
        return jsonify(result)
        
    except Exception as e:
        logger.error(f"获取智能推荐API失败: {e}")
        return jsonify({
            'success': False,
            'error': f'服务器错误: {str(e)}',
            'recommendations': [],
            'timestamp': datetime.now().isoformat()
        }), 500

@recommendations_bp.route('/api/mobile/recommendations/refresh', methods=['POST'])
def refresh_recommendations():
    """刷新推荐数据"""
    try:
        user_id = request.json.get('user_id', 'default') if request.json else 'default'
        max_stocks = min(int(request.json.get('max_stocks', 20) if request.json else 20), 50)
        
        # 使用同步方式生成新推荐
        import asyncio
        try:
            loop = asyncio.new_event_loop()
            asyncio.set_event_loop(loop)
            new_recommendations = loop.run_until_complete(
                intelligent_recommender.generate_intelligent_recommendations(max_stocks)
            )
            
            if new_recommendations:
                # 保存推荐结果
                success = loop.run_until_complete(
                    intelligent_recommender.save_intelligent_recommendations(new_recommendations)
                )
                loop.close()
                
                if success:
                    # 转换为字典格式
                    recommendations_dict = [personal_rec_api._convert_to_dict(rec) for rec in new_recommendations]
                    stats = personal_rec_api._calculate_statistics(recommendations_dict)
                    
                    return jsonify({
                        'success': True,
                        'message': '推荐已刷新',
                        'recommendations': recommendations_dict,
                        'count': len(recommendations_dict),
                        'statistics': stats,
                        'timestamp': datetime.now().isoformat()
                    })
                else:
                    return jsonify({
                        'success': False,
                        'error': '保存推荐失败',
                        'timestamp': datetime.now().isoformat()
                    }), 500
            else:
                loop.close()
                return jsonify({
                    'success': False,
                    'error': '生成推荐失败',
                    'timestamp': datetime.now().isoformat()
                }), 500
        except Exception as e:
            logger.error(f"异步执行失败: {e}")
            return jsonify({
                'success': False,
                'error': '推荐服务暂时不可用',
                'timestamp': datetime.now().isoformat()
            }), 500
            
    except Exception as e:
        logger.error(f"刷新推荐API失败: {e}")
        return jsonify({
            'success': False,
            'error': f'服务器错误: {str(e)}',
            'timestamp': datetime.now().isoformat()
        }), 500

@recommendations_bp.route('/api/mobile/recommendations/detail/<stock_code>', methods=['GET'])
def get_recommendation_detail(stock_code):
    """获取推荐详情"""
    try:
        # 这里可以从数据库查询详细信息
        # 目前返回模拟数据
        detail = {
            'success': True,
            'stock_code': stock_code,
            'stock_name': '示例股票',
            'recommendation_type': 'buy',
            'confidence_score': 0.85,
            'current_price': 25.68,
            'target_price': 30.00,
            'stop_loss': 23.00,
            'risk_level': 'medium',
            'reasoning': '基于技术分析和AI模型综合判断，该股票具有较好的上涨潜力。',
            'key_factors': ['技术指标良好', '均线支撑', 'RSI正常'],
            'technical_signals': {
                'trend': '上升',
                'ma_signal': '多头',
                'rsi_signal': '正常'
            },
            'sector': '科技',
            'market_position': '成长',
            'holding_period': '中期(1-3个月)',
            'timestamp': datetime.now().isoformat()
        }
        
        return jsonify(detail)
        
    except Exception as e:
        logger.error(f"获取推荐详情失败: {e}")
        return jsonify({
            'success': False,
            'error': f'获取详情失败: {str(e)}',
            'timestamp': datetime.now().isoformat()
        }), 500

@recommendations_bp.route('/api/mobile/recommendations/history', methods=['GET'])
def get_recommendation_history():
    """获取推荐历史"""
    try:
        user_id = request.args.get('user_id', 'default')
        days = min(int(request.args.get('days', 30)), 90)  # 限制最多90天
        limit = min(int(request.args.get('limit', 100)), 500)  # 限制最多500条
        
        # 这里应该从数据库查询历史记录
        # 目前返回模拟数据
        history = {
            'success': True,
            'user_id': user_id,
            'period_days': days,
            'recommendations': [],  # 历史推荐数据
            'performance_summary': {
                'total_recommendations': 0,
                'successful_recommendations': 0,
                'success_rate': 0.0,
                'average_return': 0.0
            },
            'timestamp': datetime.now().isoformat()
        }
        
        return jsonify(history)
        
    except Exception as e:
        logger.error(f"获取推荐历史失败: {e}")
        return jsonify({
            'success': False,
            'error': f'获取历史失败: {str(e)}',
            'timestamp': datetime.now().isoformat()
        }), 500

@recommendations_bp.route('/api/mobile/recommendations/settings', methods=['GET', 'POST'])
def recommendation_settings():
    """推荐设置"""
    try:
        user_id = request.args.get('user_id') or (request.json.get('user_id') if request.json else 'default')
        
        if request.method == 'GET':
            # 获取用户设置
            settings = {
                'success': True,
                'user_id': user_id,
                'settings': {
                    'risk_tolerance': 'medium',  # low, medium, high
                    'preferred_sectors': ['technology', 'healthcare'],
                    'investment_horizon': 'medium_term',  # short_term, medium_term, long_term
                    'notification_enabled': True,
                    'max_recommendations': 20,
                    'min_confidence_threshold': 0.6
                },
                'timestamp': datetime.now().isoformat()
            }
            return jsonify(settings)
        
        elif request.method == 'POST':
            # 更新用户设置
            new_settings = request.json.get('settings', {})
            
            # 这里应该保存到数据库
            # 目前返回确认信息
            return jsonify({
                'success': True,
                'message': '设置已更新',
                'user_id': user_id,
                'settings': new_settings,
                'timestamp': datetime.now().isoformat()
            })
            
    except Exception as e:
        logger.error(f"推荐设置API失败: {e}")
        return jsonify({
            'success': False,
            'error': f'设置操作失败: {str(e)}',
            'timestamp': datetime.now().isoformat()
        }), 500

@recommendations_bp.route('/api/mobile/recommendations/watchlist', methods=['GET', 'POST', 'DELETE'])
def manage_watchlist():
    """管理观察列表"""
    try:
        user_id = request.args.get('user_id') or (request.json.get('user_id') if request.json else 'default')
        
        if request.method == 'GET':
            # 获取观察列表
            watchlist = {
                'success': True,
                'user_id': user_id,
                'watchlist': [],  # 观察列表中的股票
                'count': 0,
                'timestamp': datetime.now().isoformat()
            }
            return jsonify(watchlist)
        
        elif request.method == 'POST':
            # 添加到观察列表
            stock_code = request.json.get('stock_code')
            if not stock_code:
                return jsonify({
                    'success': False,
                    'error': '股票代码不能为空',
                    'timestamp': datetime.now().isoformat()
                }), 400
            
            # 这里应该保存到数据库
            return jsonify({
                'success': True,
                'message': f'已添加 {stock_code} 到观察列表',
                'stock_code': stock_code,
                'timestamp': datetime.now().isoformat()
            })
        
        elif request.method == 'DELETE':
            # 从观察列表移除
            stock_code = request.json.get('stock_code')
            if not stock_code:
                return jsonify({
                    'success': False,
                    'error': '股票代码不能为空',
                    'timestamp': datetime.now().isoformat()
                }), 400
            
            # 这里应该从数据库删除
            return jsonify({
                'success': True,
                'message': f'已从观察列表移除 {stock_code}',
                'stock_code': stock_code,
                'timestamp': datetime.now().isoformat()
            })
            
    except Exception as e:
        logger.error(f"观察列表API失败: {e}")
        return jsonify({
            'success': False,
            'error': f'观察列表操作失败: {str(e)}',
            'timestamp': datetime.now().isoformat()
        }), 500

# 错误处理
@recommendations_bp.errorhandler(404)
def not_found(error):
    return jsonify({
        'success': False,
        'error': '接口不存在',
        'timestamp': datetime.now().isoformat()
    }), 404

@recommendations_bp.errorhandler(500)
def internal_error(error):
    return jsonify({
        'success': False,
        'error': '服务器内部错误',
        'timestamp': datetime.now().isoformat()
    }), 500