#!/usr/bin/env python3
"""
测试本地化UI的简单Flask应用
"""
from flask import Flask, render_template, jsonify, request
import random
import datetime

app = Flask(__name__)

# 模拟数据
def generate_mock_stocks():
    """生成模拟股票数据"""
    stocks = [
        {'ts_code': '000001.SZ', 'name': '平安银行', 'industry': '银行'},
        {'ts_code': '000002.SZ', 'name': '万科A', 'industry': '房地产'},
        {'ts_code': '600000.SH', 'name': '浦发银行', 'industry': '银行'},
        {'ts_code': '600036.SH', 'name': '招商银行', 'industry': '银行'},
        {'ts_code': '000858.SZ', 'name': '五粮液', 'industry': '食品饮料'},
        {'ts_code': '600519.SH', 'name': '贵州茅台', 'industry': '食品饮料'},
        {'ts_code': '000858.SZ', 'name': '五粮液', 'industry': '食品饮料'},
        {'ts_code': '300015.SZ', 'name': '爱尔眼科', 'industry': '医疗保健'},
        {'ts_code': '002415.SZ', 'name': '海康威视', 'industry': '电子设备'},
        {'ts_code': '600276.SH', 'name': '恒瑞医药', 'industry': '医药生物'}
    ]
    
    for stock in stocks:
        base_price = random.uniform(10, 200)
        change = random.uniform(-5, 5)
        stock.update({
            'close': round(base_price, 2),
            'change': round(change, 2),
            'pct_chg': round((change / base_price) * 100, 2),
            'vol': random.randint(50000000, 500000000)
        })
    
    return stocks

@app.route('/')
def index():
    """主页路由"""
    return render_template('index_local.html')

@app.route('/test-local')
def test_local():
    """测试本地化页面"""
    return render_template('index_local.html')

@app.route('/original')
def original():
    """原始页面（使用CDN）"""
    return render_template('index.html')

@app.route('/api/stocks/search')
def search_stocks():
    """搜索股票API"""
    query = request.args.get('q', '').lower()
    limit = int(request.args.get('limit', 10))
    
    all_stocks = generate_mock_stocks()
    
    # 简单的搜索过滤
    filtered_stocks = [
        stock for stock in all_stocks 
        if query in stock['name'].lower() or query in stock['ts_code'].lower()
    ]
    
    return jsonify({
        'success': True,
        'data': filtered_stocks[:limit]
    })

@app.route('/api/stocks/hot')
def hot_stocks():
    """热门股票API"""
    limit = int(request.args.get('limit', 10))
    stocks = generate_mock_stocks()[:limit]
    
    return jsonify({
        'success': True,
        'data': stocks
    })

@app.route('/api/indexes/major')
def major_indexes():
    """主要指数API"""
    indexes = [
        {
            'code': '000001.SH',
            'name': '上证指数',
            'close': round(random.uniform(3000, 3500), 2),
            'pct_chg': round(random.uniform(-2, 2), 2)
        },
        {
            'code': '399001.SZ',
            'name': '深证成指',
            'close': round(random.uniform(10000, 12000), 2),
            'pct_chg': round(random.uniform(-2, 2), 2)
        },
        {
            'code': '399006.SZ',
            'name': '创业板指',
            'close': round(random.uniform(2000, 2800), 2),
            'pct_chg': round(random.uniform(-2, 2), 2)
        },
        {
            'code': '000300.SH',
            'name': '沪深300',
            'close': round(random.uniform(3800, 4200), 2),
            'pct_chg': round(random.uniform(-2, 2), 2)
        }
    ]
    
    return jsonify({
        'success': True,
        'data': indexes
    })

@app.route('/api/system/status')
def system_status():
    """系统状态API"""
    return jsonify({
        'success': True,
        'data': {
            'last_update': datetime.datetime.now().isoformat(),
            'monitored_stocks': random.randint(50, 200),
            'today_signals': random.randint(5, 30),
            'ws_connected': True
        }
    })

@app.route('/api/recommendations')
def recommendations():
    """推荐API"""
    return jsonify({
        'success': True,
        'message': '推荐功能正在开发中',
        'data': []
    })

# 其他页面路由（简单重定向到主页）
@app.route('/analysis')
def analysis():
    return render_template('index_local.html')

@app.route('/market-dashboard')
def market_dashboard():
    return render_template('index_local.html')

@app.route('/intelligent-recommendations')
def intelligent_recommendations():
    return render_template('index_local.html')

@app.route('/watchlist')
def watchlist():
    return render_template('index_local.html')

@app.route('/signals')
def signals():
    return render_template('index_local.html')

@app.route('/ai-analysis')
def ai_analysis():
    return render_template('index_local.html')

if __name__ == '__main__':
    print("🚀 启动本地化UI测试服务器...")
    print("📋 访问地址:")
    print("   本地化版本: http://localhost:5001/")
    print("   原始版本:   http://localhost:5001/original")
    print("   对比测试:   http://localhost:5001/test-local")
    print("=" * 50)
    
    app.run(
        host='0.0.0.0',
        port=5001,
        debug=True,
        use_reloader=True
    )