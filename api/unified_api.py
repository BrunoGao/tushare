"""
灵境万象股票分析系统 - 统一API接口
为不同客户端(Web、鸿蒙、小程序等)提供标准化的RESTful API
"""
from flask import Flask, request, jsonify, Blueprint
from flask_cors import CORS
from datetime import datetime, timedelta
import logging
from typing import Dict, List, Any, Optional
import sys, os

# 添加项目路径
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from database.db_manager import DatabaseManager
from utils.response_formatter import ResponseFormatter
from utils.auth_middleware import AuthMiddleware
from utils.rate_limiter import RateLimiter

# 日志配置
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# 创建统一API蓝图
unified_api = Blueprint('unified_api', __name__, url_prefix='/api/v1')

# 初始化工具类
db_manager = DatabaseManager()
response_formatter = ResponseFormatter()
auth_middleware = AuthMiddleware()
rate_limiter = RateLimiter()

# ============ 认证和中间件装饰器 ============

def require_auth(f):
    """需要认证的装饰器"""
    def decorated(*args, **kwargs):
        if not auth_middleware.validate_request(request):
            return response_formatter.error("认证失败", 401)
        return f(*args, **kwargs)
    decorated.__name__ = f.__name__
    return decorated

def rate_limit(max_requests: int = 100, window: int = 60):
    """请求限流装饰器"""
    def decorator(f):
        def decorated(*args, **kwargs):
            if not rate_limiter.is_allowed(request.remote_addr, max_requests, window):
                return response_formatter.error("请求过于频繁", 429)
            return f(*args, **kwargs)
        decorated.__name__ = f.__name__
        return decorated
    return decorator

# ============ 市场数据API ============

@unified_api.route('/market/overview', methods=['GET'])
@rate_limit(100, 60)
def get_market_overview():
    """获取市场概览数据"""
    try:
        # 获取主要指数
        indices = _get_major_indices()
        
        # 获取市场统计
        market_stats = _get_market_statistics()
        
        # 获取热门板块
        hot_sectors = _get_hot_sectors()
        
        # 获取涨跌家数
        advance_decline = _get_advance_decline_data()
        
        data = {
            "indices": indices,
            "market_stats": market_stats,
            "hot_sectors": hot_sectors,
            "advance_decline": advance_decline,
            "update_time": datetime.now().isoformat()
        }
        
        return response_formatter.success(data, "市场概览获取成功")
        
    except Exception as e:
        logger.error(f"获取市场概览失败: {e}")
        return response_formatter.error(str(e), 500)

@unified_api.route('/market/stocks/hot', methods=['GET'])
@rate_limit(200, 60)
def get_hot_stocks():
    """获取热门股票"""
    try:
        limit = request.args.get('limit', 20, type=int)
        sort_by = request.args.get('sort_by', 'pct_chg')  # pct_chg, volume, amount
        
        # 获取热门股票数据
        hot_stocks = db_manager.fetch_data(f"""
            SELECT 
                sd.ts_code, sb.name, sd.close, sd.pct_chg, 
                sd.vol, sd.amount, sb.industry, sb.market,
                sd.trade_date
            FROM stock_daily_202507 sd
            JOIN stock_basic sb ON sd.ts_code = sb.ts_code
            WHERE sd.trade_date = (SELECT MAX(trade_date) FROM stock_daily_202507)
            ORDER BY sd.{sort_by} DESC
            LIMIT {min(limit, 100)}
        """)
        
        stocks_list = []
        for _, row in hot_stocks.iterrows():
            stocks_list.append({
                "ts_code": row['ts_code'],
                "name": row['name'],
                "close": float(row['close']),
                "pct_chg": round(float(row['pct_chg']), 2),
                "volume": int(row['vol']),
                "amount": float(row['amount']),
                "industry": row['industry'],
                "market": row['market'],
                "trade_date": row['trade_date'].strftime('%Y-%m-%d') if hasattr(row['trade_date'], 'strftime') else str(row['trade_date'])
            })
        
        return response_formatter.success(stocks_list, f"获取{len(stocks_list)}只热门股票")
        
    except Exception as e:
        logger.error(f"获取热门股票失败: {e}")
        return response_formatter.error(str(e), 500)

# ============ 股票数据API ============

@unified_api.route('/stocks/<ts_code>/basic', methods=['GET'])
@rate_limit(500, 60)
def get_stock_basic_info(ts_code: str):
    """获取股票基本信息"""
    try:
        # 股票基本信息
        basic_info = db_manager.fetch_data(f"""
            SELECT ts_code, name, industry, market, list_date, 
                   exchange, curr_type, list_status
            FROM stock_basic 
            WHERE ts_code = '{ts_code}'
        """)
        
        if basic_info.empty:
            return response_formatter.error("股票不存在", 404)
        
        stock_info = basic_info.iloc[0].to_dict()
        
        # 最新行情
        latest_price = db_manager.fetch_data(f"""
            SELECT close, pct_chg, vol, amount, trade_date,
                   open, high, low, pre_close
            FROM stock_daily_202507
            WHERE ts_code = '{ts_code}'
            ORDER BY trade_date DESC
            LIMIT 1
        """)
        
        if not latest_price.empty:
            price_data = latest_price.iloc[0].to_dict()
            stock_info.update({
                "latest_price": {
                    "close": float(price_data['close']),
                    "pct_chg": round(float(price_data['pct_chg']), 2),
                    "volume": int(price_data['vol']),
                    "amount": float(price_data['amount']),
                    "open": float(price_data['open']),
                    "high": float(price_data['high']),
                    "low": float(price_data['low']),
                    "pre_close": float(price_data['pre_close']),
                    "trade_date": price_data['trade_date'].strftime('%Y-%m-%d') if hasattr(price_data['trade_date'], 'strftime') else str(price_data['trade_date'])
                }
            })
        
        return response_formatter.success(stock_info, "股票信息获取成功")
        
    except Exception as e:
        logger.error(f"获取股票基本信息失败: {e}")
        return response_formatter.error(str(e), 500)

@unified_api.route('/stocks/<ts_code>/kline', methods=['GET'])
@rate_limit(300, 60)
def get_stock_kline(ts_code: str):
    """获取股票K线数据"""
    try:
        days = request.args.get('days', 120, type=int)
        period = request.args.get('period', 'daily')  # daily, weekly, monthly
        
        # 限制查询天数
        days = min(days, 500)
        
        end_date = datetime.now().strftime('%Y%m%d')
        start_date = (datetime.now() - timedelta(days=days)).strftime('%Y%m%d')
        
        kline_data = db_manager.fetch_data(f"""
            SELECT trade_date, open, close, high, low, vol, amount, pct_chg
            FROM stock_daily_202507
            WHERE ts_code = '{ts_code}'
                AND trade_date BETWEEN '{start_date}' AND '{end_date}'
            ORDER BY trade_date
        """)
        
        if kline_data.empty:
            return response_formatter.error("无K线数据", 404)
        
        # 格式化K线数据
        klines = []
        for _, row in kline_data.iterrows():
            klines.append({
                "date": row['trade_date'].strftime('%Y-%m-%d') if hasattr(row['trade_date'], 'strftime') else str(row['trade_date']),
                "open": float(row['open']),
                "close": float(row['close']),
                "high": float(row['high']),
                "low": float(row['low']),
                "volume": int(row['vol']),
                "amount": float(row['amount']),
                "pct_chg": round(float(row['pct_chg']), 2)
            })
        
        return response_formatter.success({
            "ts_code": ts_code,
            "period": period,
            "klines": klines,
            "count": len(klines)
        }, f"获取{len(klines)}条K线数据")
        
    except Exception as e:
        logger.error(f"获取K线数据失败: {e}")
        return response_formatter.error(str(e), 500)

# ============ 技术分析API ============

@unified_api.route('/analysis/technical/<ts_code>', methods=['GET'])
@rate_limit(200, 60)
def get_technical_analysis(ts_code: str):
    """获取技术分析结果"""
    try:
        days = request.args.get('days', 60, type=int)
        
        # 获取技术指标
        technical_data = db_manager.fetch_data(f"""
            SELECT ti.*, sd.close, sd.trade_date
            FROM technical_indicators ti
            JOIN stock_daily_202507 sd ON ti.ts_code = sd.ts_code AND ti.trade_date = sd.trade_date
            WHERE ti.ts_code = '{ts_code}'
            ORDER BY ti.trade_date DESC
            LIMIT {min(days, 200)}
        """)
        
        if technical_data.empty:
            return response_formatter.error("无技术指标数据", 404)
        
        # 最新指标
        latest = technical_data.iloc[0]
        
        # 生成技术分析信号
        signals = _generate_technical_signals(technical_data)
        
        analysis_result = {
            "ts_code": ts_code,
            "latest_indicators": {
                "ma5": float(latest.get('ma5', 0)),
                "ma10": float(latest.get('ma10', 0)),
                "ma20": float(latest.get('ma20', 0)),
                "ma60": float(latest.get('ma60', 0)),
                "rsi6": float(latest.get('rsi6', 50)),
                "rsi12": float(latest.get('rsi12', 50)),
                "macd_dif": float(latest.get('macd_dif', 0)),
                "macd_dea": float(latest.get('macd_dea', 0)),
                "kdj_k": float(latest.get('kdj_k', 50)),
                "kdj_d": float(latest.get('kdj_d', 50)),
                "vol_ratio": float(latest.get('vol_ratio', 1)),
                "turn_rate": float(latest.get('turn_rate', 0))
            },
            "signals": signals,
            "analysis_time": datetime.now().isoformat()
        }
        
        return response_formatter.success(analysis_result, "技术分析获取成功")
        
    except Exception as e:
        logger.error(f"技术分析失败: {e}")
        return response_formatter.error(str(e), 500)

# ============ AI智能分析API ============

@unified_api.route('/ai/predict/<ts_code>', methods=['GET'])
@rate_limit(50, 60)
def get_ai_prediction(ts_code: str):
    """获取AI预测结果"""
    try:
        model_name = request.args.get('model', 'random_forest')
        
        # 调用AI预测
        from ai.ml_trainer import ml_trainer
        
        # 确保模型已加载
        if not ml_trainer.models:
            ml_trainer.load_models()
        
        if not ml_trainer.models:
            return response_formatter.error("AI模型未训练", 503)
        
        prediction = ml_trainer.predict_single_stock(ts_code, model_name)
        
        return response_formatter.success(prediction, "AI预测获取成功")
        
    except Exception as e:
        logger.error(f"AI预测失败: {e}")
        return response_formatter.error(str(e), 500)

@unified_api.route('/ai/recommendations', methods=['GET'])
@rate_limit(20, 60)
def get_ai_recommendations():
    """获取AI智能推荐"""
    try:
        limit = request.args.get('limit', 20, type=int)
        strategy = request.args.get('strategy', 'multi_strategy')
        min_score = request.args.get('min_score', 60, type=int)
        
        # 调用智能推荐系统
        from llm.intelligent_recommender import intelligent_recommender
        import asyncio
        
        async def get_recommendations():
            return await intelligent_recommender.get_latest_intelligent_recommendations(limit)
        
        # 在新的事件循环中运行
        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        recommendations = loop.run_until_complete(get_recommendations())
        loop.close()
        
        # 过滤推荐结果
        filtered_recommendations = [
            rec for rec in recommendations 
            if rec.get('score', 0) >= min_score
        ][:limit]
        
        return response_formatter.success({
            "recommendations": filtered_recommendations,
            "count": len(filtered_recommendations),
            "strategy": strategy,
            "min_score": min_score
        }, f"获取{len(filtered_recommendations)}条AI推荐")
        
    except Exception as e:
        logger.error(f"AI推荐失败: {e}")
        return response_formatter.error(str(e), 500)

# ============ 搜索API ============

@unified_api.route('/search/stocks', methods=['GET'])
@rate_limit(500, 60)
def search_stocks():
    """股票搜索"""
    try:
        query = request.args.get('q', '').strip()
        limit = request.args.get('limit', 10, type=int)
        
        if not query or len(query) < 2:
            return response_formatter.error("搜索关键词至少2个字符", 400)
        
        # 搜索股票
        search_results = db_manager.fetch_data(f"""
            SELECT ts_code, name, industry, market
            FROM stock_basic
            WHERE (
                ts_code LIKE '%{query}%' OR
                name LIKE '%{query}%'
            )
            ORDER BY
                CASE
                    WHEN ts_code = '{query}' THEN 1
                    WHEN ts_code LIKE '{query}%' THEN 2
                    WHEN name LIKE '{query}%' THEN 3
                    ELSE 4
                END,
                ts_code
            LIMIT {min(limit, 50)}
        """)
        
        stocks = []
        for _, row in search_results.iterrows():
            stocks.append({
                "ts_code": row['ts_code'],
                "name": row['name'],
                "industry": row['industry'],
                "market": row['market'],
                "display_name": f"{row['name']}({row['ts_code']})"
            })
        
        return response_formatter.success({
            "query": query,
            "results": stocks,
            "count": len(stocks)
        }, f"找到{len(stocks)}只股票")
        
    except Exception as e:
        logger.error(f"股票搜索失败: {e}")
        return response_formatter.error(str(e), 500)

# ============ 系统API ============

@unified_api.route('/system/health', methods=['GET'])
def get_system_health():
    """系统健康检查"""
    try:
        # 数据库连接检查
        db_status = "healthy"
        try:
            test_query = db_manager.fetch_data("SELECT 1 as test")
            if test_query.empty:
                db_status = "warning"
        except:
            db_status = "error"
        
        # AI系统检查
        ai_status = "healthy"
        try:
            from ai.health_check import get_ai_system_health
            ai_health = get_ai_system_health()
            ai_status = ai_health.get('overall_status', 'unknown')
        except:
            ai_status = "warning"
        
        # 系统统计
        stock_count = 0
        try:
            stock_stats = db_manager.fetch_data("SELECT COUNT(*) as count FROM stock_basic")
            stock_count = int(stock_stats.iloc[0]['count']) if not stock_stats.empty else 0
        except:
            pass
        
        health_data = {
            "system_status": "healthy" if db_status == "healthy" and ai_status in ["healthy", "warning"] else "warning",
            "components": {
                "database": db_status,
                "ai_system": ai_status
            },
            "statistics": {
                "total_stocks": stock_count,
                "api_version": "v1.0.0",
                "server_time": datetime.now().isoformat()
            }
        }
        
        return response_formatter.success(health_data, "系统健康检查完成")
        
    except Exception as e:
        logger.error(f"系统健康检查失败: {e}")
        return response_formatter.error(str(e), 500)

# ============ 辅助函数 ============

def _get_major_indices():
    """获取主要指数数据"""
    # 模拟指数数据，实际应从数据库获取
    return [
        {"code": "000001.SH", "name": "上证指数", "close": 3234.56, "pct_chg": 0.87},
        {"code": "399001.SZ", "name": "深证成指", "close": 12456.78, "pct_chg": -0.34},
        {"code": "399006.SZ", "name": "创业板指", "close": 2876.45, "pct_chg": 1.23}
    ]

def _get_market_statistics():
    """获取市场统计数据"""
    try:
        stats = db_manager.fetch_data("""
            SELECT 
                COUNT(*) as total_stocks,
                SUM(CASE WHEN pct_chg > 0 THEN 1 ELSE 0 END) as rising_count,
                SUM(CASE WHEN pct_chg < 0 THEN 1 ELSE 0 END) as falling_count,
                AVG(pct_chg) as avg_change
            FROM stock_daily_202507
            WHERE trade_date = (SELECT MAX(trade_date) FROM stock_daily_202507)
        """)
        
        if not stats.empty:
            row = stats.iloc[0]
            return {
                "total_stocks": int(row['total_stocks']),
                "rising_count": int(row['rising_count']),
                "falling_count": int(row['falling_count']),
                "avg_change": round(float(row['avg_change']), 2)
            }
    except:
        pass
    
    return {"total_stocks": 0, "rising_count": 0, "falling_count": 0, "avg_change": 0}

def _get_hot_sectors():
    """获取热门板块"""
    # 简化实现，返回模拟数据
    return [
        {"name": "人工智能", "change": 5.67, "stock_count": 128},
        {"name": "新能源汽车", "change": 3.24, "stock_count": 89},
        {"name": "半导体", "change": -2.11, "stock_count": 156}
    ]

def _get_advance_decline_data():
    """获取涨跌家数数据"""
    try:
        ad_data = db_manager.fetch_data("""
            SELECT 
                SUM(CASE WHEN pct_chg > 0 THEN 1 ELSE 0 END) as advance,
                SUM(CASE WHEN pct_chg < 0 THEN 1 ELSE 0 END) as decline,
                SUM(CASE WHEN pct_chg = 0 THEN 1 ELSE 0 END) as unchanged
            FROM stock_daily_202507
            WHERE trade_date = (SELECT MAX(trade_date) FROM stock_daily_202507)
        """)
        
        if not ad_data.empty:
            row = ad_data.iloc[0]
            return {
                "advance": int(row['advance']),
                "decline": int(row['decline']),
                "unchanged": int(row['unchanged'])
            }
    except:
        pass
    
    return {"advance": 0, "decline": 0, "unchanged": 0}

def _generate_technical_signals(df):
    """生成技术分析信号"""
    if df.empty:
        return {}
    
    latest = df.iloc[0]
    signals = {}
    
    # MA信号
    ma5 = latest.get('ma5', 0)
    ma20 = latest.get('ma20', 0)
    close = latest.get('close', 0)
    
    if ma5 and ma20 and close:
        if ma5 > ma20 and close > ma5:
            signals['ma_signal'] = "多头排列"
        elif ma5 < ma20 and close < ma5:
            signals['ma_signal'] = "空头排列"
        else:
            signals['ma_signal'] = "震荡整理"
    
    # RSI信号
    rsi = latest.get('rsi6', 50)
    if rsi > 70:
        signals['rsi_signal'] = "超买"
    elif rsi < 30:
        signals['rsi_signal'] = "超卖"
    else:
        signals['rsi_signal'] = "正常"
    
    # MACD信号
    macd_dif = latest.get('macd_dif', 0)
    macd_dea = latest.get('macd_dea', 0)
    if macd_dif > macd_dea:
        signals['macd_signal'] = "金叉"
    else:
        signals['macd_signal'] = "死叉"
    
    return signals

if __name__ == '__main__':
    # 测试运行
    app = Flask(__name__)
    CORS(app)
    app.register_blueprint(unified_api)
    app.run(host='0.0.0.0', port=5006, debug=True)