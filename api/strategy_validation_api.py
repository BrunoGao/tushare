#!/usr/bin/env python3
"""
策略验证API
提供策略回测、评估和分析的RESTful接口
"""

import sys
import os
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import json
import logging
from datetime import datetime, timedelta
from flask import Blueprint, request, jsonify
from typing import Dict, List, Optional, Any

# 导入策略验证组件
try:
    from strategy_validation.advanced_validator import (
        AdvancedStrategyValidator, 
        StrategyConfig, 
        ValidationResult,
        rsi_mean_reversion_strategy,
        macd_trend_strategy
    )
    from database.db_manager import DatabaseManager
    VALIDATION_AVAILABLE = True
except ImportError as e:
    VALIDATION_AVAILABLE = False
    print(f"策略验证组件不可用: {e}")

# 创建蓝图
validation_bp = Blueprint('strategy_validation', __name__)

# 设置日志
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# 预定义策略
PREDEFINED_STRATEGIES = {
    'rsi_mean_reversion': {
        'name': 'RSI均值回归策略',
        'description': '基于RSI指标的超买超卖交易策略',
        'function': rsi_mean_reversion_strategy,
        'default_params': {
            'rsi_oversold': 30,
            'rsi_overbought': 70,
            'max_holding_days': 25
        },
        'default_risk_params': {
            'max_position_size': 0.15,
            'stop_loss': 0.08,
            'take_profit': 0.15
        }
    },
    'macd_trend': {
        'name': 'MACD趋势跟踪策略',
        'description': '基于MACD指标的趋势跟踪策略',
        'function': macd_trend_strategy,
        'default_params': {
            'fast': 12,
            'slow': 26,
            'signal': 9
        },
        'default_risk_params': {
            'max_position_size': 0.12,
            'stop_loss': 0.06,
            'take_profit': 0.12
        }
    }
}

class StrategyValidationAPI:
    """策略验证API类"""
    
    def __init__(self):
        self.db_manager = None
        self._init_database()
    
    def _init_database(self):
        """初始化数据库连接"""
        try:
            self.db_manager = DatabaseManager()
        except Exception as e:
            logger.error(f"数据库初始化失败: {e}")

@validation_bp.route('/health', methods=['GET'])
def health_check():
    """健康检查"""
    return jsonify({
        'status': 'healthy',
        'validation_available': VALIDATION_AVAILABLE,
        'timestamp': datetime.now().isoformat(),
        'available_strategies': list(PREDEFINED_STRATEGIES.keys()) if VALIDATION_AVAILABLE else []
    })

@validation_bp.route('/strategies', methods=['GET'])
def get_available_strategies():
    """获取可用策略列表"""
    if not VALIDATION_AVAILABLE:
        return jsonify({'error': '策略验证功能不可用'}), 503
    
    strategies = {}
    for key, strategy in PREDEFINED_STRATEGIES.items():
        strategies[key] = {
            'name': strategy['name'],
            'description': strategy['description'],
            'default_params': strategy['default_params'],
            'default_risk_params': strategy['default_risk_params']
        }
    
    return jsonify({'strategies': strategies})

@validation_bp.route('/validate', methods=['POST'])
def validate_strategy():
    """执行策略验证"""
    if not VALIDATION_AVAILABLE:
        return jsonify({'error': '策略验证功能不可用'}), 503
    
    try:
        data = request.get_json()
        if not data:
            return jsonify({'error': '请求数据不能为空'}), 400
        
        # 验证必需参数
        required_fields = ['strategy_id', 'stock_codes', 'start_date', 'end_date']
        for field in required_fields:
            if field not in data:
                return jsonify({'error': f'缺少必需字段: {field}'}), 400
        
        strategy_id = data['strategy_id']
        if strategy_id not in PREDEFINED_STRATEGIES:
            return jsonify({'error': f'不支持的策略ID: {strategy_id}'}), 400
        
        # 获取策略配置
        strategy_info = PREDEFINED_STRATEGIES[strategy_id]
        
        # 合并用户参数和默认参数
        strategy_params = strategy_info['default_params'].copy()
        if 'parameters' in data:
            strategy_params.update(data['parameters'])
        
        risk_params = strategy_info['default_risk_params'].copy()
        if 'risk_params' in data:
            risk_params.update(data['risk_params'])
        
        # 创建策略配置
        config = StrategyConfig(
            name=f"{strategy_info['name']} (API)",
            description=strategy_info['description'],
            parameters=strategy_params,
            risk_params=risk_params,
            benchmark=data.get('benchmark', '000300.SH')
        )
        
        # 创建验证器
        validator = AdvancedStrategyValidator(config)
        
        # 执行验证
        logger.info(f"开始验证策略: {strategy_id}")
        result = validator.comprehensive_validation(
            stock_codes=data['stock_codes'],
            start_date=data['start_date'],
            end_date=data['end_date'],
            strategy_func=strategy_info['function'],
            save_results=data.get('save_results', True)
        )
        
        # 转换结果为JSON可序列化格式
        result_dict = _serialize_validation_result(result)
        
        return jsonify({
            'success': True,
            'validation_id': result.validation_id,
            'result': result_dict
        })
        
    except Exception as e:
        logger.error(f"策略验证失败: {e}")
        return jsonify({'error': f'验证过程中发生错误: {str(e)}'}), 500

@validation_bp.route('/results/<validation_id>', methods=['GET'])
def get_validation_result(validation_id: str):
    """获取验证结果"""
    if not VALIDATION_AVAILABLE:
        return jsonify({'error': '策略验证功能不可用'}), 503
    
    try:
        api = StrategyValidationAPI()
        if not api.db_manager:
            return jsonify({'error': '数据库连接不可用'}), 503
        
        # 查询数据库
        sql = """
        SELECT * FROM strategy_validations WHERE id = %s
        """
        result = api.db_manager.fetch_data_with_params(sql, (validation_id,))
        
        if result.empty:
            return jsonify({'error': '未找到指定的验证结果'}), 404
        
        # 转换结果
        row = result.iloc[0]
        validation_result = {
            'validation_id': row['id'],
            'strategy_name': row['strategy_name'],
            'test_period': row['test_period'],
            'total_return': float(row['total_return']) if row['total_return'] else 0,
            'annualized_return': float(row['annualized_return']) if row['annualized_return'] else 0,
            'volatility': float(row['volatility']) if row['volatility'] else 0,
            'sharpe_ratio': float(row['sharpe_ratio']) if row['sharpe_ratio'] else 0,
            'max_drawdown': float(row['max_drawdown']) if row['max_drawdown'] else 0,
            'calmar_ratio': float(row['calmar_ratio']) if row['calmar_ratio'] else 0,
            'win_rate': float(row['win_rate']) if row['win_rate'] else 0,
            'profit_loss_ratio': float(row['profit_loss_ratio']) if row['profit_loss_ratio'] else 0,
            'total_trades': int(row['total_trades']) if row['total_trades'] else 0,
            'benchmark_return': float(row['benchmark_return']) if row['benchmark_return'] else 0,
            'excess_return': float(row['excess_return']) if row['excess_return'] else 0,
            'information_ratio': float(row['information_ratio']) if row['information_ratio'] else 0,
            'created_at': row['created_at'].isoformat() if row['created_at'] else None
        }
        
        # 解析详细结果
        if row['detailed_results']:
            try:
                detailed_results = json.loads(row['detailed_results'])
                validation_result['detailed_results'] = detailed_results
            except json.JSONDecodeError:
                logger.warning(f"解析详细结果失败: {validation_id}")
        
        return jsonify({'result': validation_result})
        
    except Exception as e:
        logger.error(f"获取验证结果失败: {e}")
        return jsonify({'error': f'获取结果时发生错误: {str(e)}'}), 500

@validation_bp.route('/results', methods=['GET'])
def list_validation_results():
    """获取验证结果列表"""
    if not VALIDATION_AVAILABLE:
        return jsonify({'error': '策略验证功能不可用'}), 503
    
    try:
        api = StrategyValidationAPI()
        if not api.db_manager:
            return jsonify({'error': '数据库连接不可用'}), 503
        
        # 获取查询参数
        page = int(request.args.get('page', 1))
        limit = min(int(request.args.get('limit', 20)), 100)  # 最多100条
        offset = (page - 1) * limit
        
        strategy_name = request.args.get('strategy_name')
        
        # 构建查询SQL
        where_clause = ""
        params = []
        
        if strategy_name:
            where_clause = "WHERE strategy_name LIKE %s"
            params.append(f"%{strategy_name}%")
        
        # 查询总数
        count_sql = f"SELECT COUNT(*) as total FROM strategy_validations {where_clause}"
        total_result = api.db_manager.fetch_data_with_params(count_sql, params) if params else api.db_manager.fetch_data(count_sql)
        total_count = int(total_result.iloc[0]['total']) if not total_result.empty else 0
        
        # 查询数据
        sql = f"""
        SELECT id, strategy_name, test_period, total_return, annualized_return,
               sharpe_ratio, max_drawdown, total_trades, created_at
        FROM strategy_validations 
        {where_clause}
        ORDER BY created_at DESC 
        LIMIT %s OFFSET %s
        """
        params.extend([limit, offset])
        
        results = api.db_manager.fetch_data_with_params(sql, params)
        
        # 转换结果
        validations = []
        for _, row in results.iterrows():
            validations.append({
                'validation_id': row['id'],
                'strategy_name': row['strategy_name'],
                'test_period': row['test_period'],
                'total_return': float(row['total_return']) if row['total_return'] else 0,
                'annualized_return': float(row['annualized_return']) if row['annualized_return'] else 0,
                'sharpe_ratio': float(row['sharpe_ratio']) if row['sharpe_ratio'] else 0,
                'max_drawdown': float(row['max_drawdown']) if row['max_drawdown'] else 0,
                'total_trades': int(row['total_trades']) if row['total_trades'] else 0,
                'created_at': row['created_at'].isoformat() if row['created_at'] else None
            })
        
        return jsonify({
            'validations': validations,
            'pagination': {
                'page': page,
                'limit': limit,
                'total': total_count,
                'pages': (total_count + limit - 1) // limit
            }
        })
        
    except Exception as e:
        logger.error(f"获取验证结果列表失败: {e}")
        return jsonify({'error': f'获取列表时发生错误: {str(e)}'}), 500

@validation_bp.route('/quick-validate', methods=['POST'])
def quick_validate():
    """快速验证（预设配置）"""
    if not VALIDATION_AVAILABLE:
        return jsonify({'error': '策略验证功能不可用'}), 503
    
    try:
        data = request.get_json() or {}
        
        # 使用默认配置
        strategy_id = data.get('strategy_id', 'rsi_mean_reversion')
        if strategy_id not in PREDEFINED_STRATEGIES:
            return jsonify({'error': f'不支持的策略ID: {strategy_id}'}), 400
        
        # 默认测试股票池
        default_stocks = [
            '000001.SZ', '000002.SZ', '600036.SH',
            '600519.SH', '000858.SZ'
        ]
        
        # 默认测试期间（最近1年）
        end_date = datetime.now()
        start_date = end_date - timedelta(days=365)
        
        validation_request = {
            'strategy_id': strategy_id,
            'stock_codes': data.get('stock_codes', default_stocks),
            'start_date': data.get('start_date', start_date.strftime('%Y-%m-%d')),
            'end_date': data.get('end_date', end_date.strftime('%Y-%m-%d')),
            'save_results': data.get('save_results', True)
        }
        
        # 直接执行验证
        try:
            strategy_id = validation_request['strategy_id']
            strategy_info = PREDEFINED_STRATEGIES[strategy_id]
            
            # 创建策略配置
            config = StrategyConfig(
                name=f"{strategy_info['name']} (快速验证)",
                description=strategy_info['description'],
                parameters=strategy_info['default_params'].copy(),
                risk_params=strategy_info['default_risk_params'].copy(),
                benchmark='000300.SH'
            )
            
            # 创建验证器
            validator = AdvancedStrategyValidator(config)
            
            # 执行验证
            result = validator.comprehensive_validation(
                stock_codes=validation_request['stock_codes'],
                start_date=validation_request['start_date'],
                end_date=validation_request['end_date'],
                strategy_func=strategy_info['function'],
                save_results=validation_request['save_results']
            )
            
            # 转换结果
            result_dict = _serialize_validation_result(result)
            
            return jsonify({
                'success': True,
                'validation_id': result.validation_id,
                'result': result_dict
            })
            
        except Exception as validation_error:
            raise validation_error
        
    except Exception as e:
        logger.error(f"快速验证失败: {e}")
        return jsonify({'error': f'快速验证时发生错误: {str(e)}'}), 500

def _serialize_validation_result(result: ValidationResult) -> Dict[str, Any]:
    """序列化验证结果为JSON格式"""
    try:
        # 提取基础指标
        result_dict = {
            'validation_id': result.validation_id,
            'strategy_name': result.strategy_name,
            'test_period': result.test_period,
            'created_at': result.created_at,
            
            # 收益指标
            'total_return': round(result.total_return, 6),
            'annualized_return': round(result.annualized_return, 6),
            'benchmark_return': round(result.benchmark_return, 6),
            'excess_return': round(result.excess_return, 6),
            
            # 风险指标
            'volatility': round(result.volatility, 6),
            'max_drawdown': round(result.max_drawdown, 6),
            'sharpe_ratio': round(result.sharpe_ratio, 4),
            'calmar_ratio': round(result.calmar_ratio, 4),
            'information_ratio': round(result.information_ratio, 4),
            
            # 交易指标
            'total_trades': result.total_trades,
            'win_rate': round(result.win_rate, 6),
            'profit_loss_ratio': round(result.profit_loss_ratio, 4),
            
            # 时间序列数据
            'monthly_returns': [round(r, 4) for r in result.monthly_returns],
            'drawdown_series': [round(d, 4) for d in result.drawdown_series],
            
            # 交易明细（限制数量避免响应过大，并序列化）
            'detailed_trades': _serialize_trades(result.detailed_trades[:100]) if result.detailed_trades else [],
            'total_detailed_trades': len(result.detailed_trades) if result.detailed_trades else 0
        }
        
        # 添加性能评级
        result_dict['performance_rating'] = _get_performance_rating(result)
        
        # 添加关键指标摘要
        result_dict['summary'] = {
            'is_profitable': result.total_return > 0,
            'beats_benchmark': result.excess_return > 0,
            'good_sharpe': result.sharpe_ratio > 1.0,
            'acceptable_drawdown': result.max_drawdown < 0.20,
            'sufficient_trades': result.total_trades >= 10
        }
        
        return result_dict
        
    except Exception as e:
        logger.error(f"序列化验证结果失败: {e}")
        # 返回基础结果
        return {
            'validation_id': getattr(result, 'validation_id', 'unknown'),
            'strategy_name': getattr(result, 'strategy_name', 'unknown'),
            'error': '结果序列化失败'
        }

def _get_performance_rating(result: ValidationResult) -> Dict[str, Any]:
    """获取性能评级"""
    score = 0
    
    # 收益评分 (40%)
    if result.annualized_return >= 0.20:
        score += 40
    elif result.annualized_return >= 0.15:
        score += 32
    elif result.annualized_return >= 0.10:
        score += 24
    elif result.annualized_return >= 0.05:
        score += 16
    elif result.annualized_return >= 0:
        score += 8
    
    # 风险控制评分 (30%)
    if result.max_drawdown <= 0.10:
        score += 30
    elif result.max_drawdown <= 0.15:
        score += 24
    elif result.max_drawdown <= 0.20:
        score += 18
    elif result.max_drawdown <= 0.25:
        score += 12
    elif result.max_drawdown <= 0.30:
        score += 6
    
    # 夏普比率评分 (20%)
    if result.sharpe_ratio >= 2.0:
        score += 20
    elif result.sharpe_ratio >= 1.5:
        score += 16
    elif result.sharpe_ratio >= 1.0:
        score += 12
    elif result.sharpe_ratio >= 0.5:
        score += 8
    elif result.sharpe_ratio >= 0:
        score += 4
    
    # 交易效率评分 (10%)
    if result.win_rate >= 0.65 and result.profit_loss_ratio >= 1.5:
        score += 10
    elif result.win_rate >= 0.60 and result.profit_loss_ratio >= 1.2:
        score += 8
    elif result.win_rate >= 0.55:
        score += 6
    elif result.win_rate >= 0.50:
        score += 4
    elif result.win_rate >= 0.45:
        score += 2
    
    # 评级映射
    if score >= 85:
        rating = "A+"
        rating_text = "优秀"
    elif score >= 75:
        rating = "A"
        rating_text = "良好"
    elif score >= 65:
        rating = "B+"
        rating_text = "一般"
    elif score >= 55:
        rating = "B"
        rating_text = "及格"
    elif score >= 45:
        rating = "C"
        rating_text = "偏弱"
    else:
        rating = "D"
        rating_text = "不佳"
    
    return {
        'score': score,
        'rating': rating,
        'rating_text': rating_text,
        'description': f"{rating} ({rating_text})"
    }

def _serialize_trades(trades):
    """序列化交易记录"""
    if not trades:
        return []
    
    serialized_trades = []
    for trade in trades:
        serialized_trade = {}
        for key, value in trade.items():
            if hasattr(value, 'isoformat'):  # datetime对象
                serialized_trade[key] = value.isoformat()
            elif str(type(value)).find('pandas') != -1:  # pandas类型
                if hasattr(value, 'isoformat'):
                    serialized_trade[key] = value.isoformat()
                else:
                    serialized_trade[key] = str(value)
            elif isinstance(value, (bool, int, float, str, type(None))):
                serialized_trade[key] = value
            else:
                serialized_trade[key] = str(value)
        serialized_trades.append(serialized_trade)
    return serialized_trades

# 错误处理
@validation_bp.errorhandler(400)
def bad_request(error):
    return jsonify({'error': '请求参数错误'}), 400

@validation_bp.errorhandler(404)
def not_found(error):
    return jsonify({'error': '资源未找到'}), 404

@validation_bp.errorhandler(500)
def internal_error(error):
    logger.error(f"内部服务器错误: {error}")
    return jsonify({'error': '内部服务器错误'}), 500