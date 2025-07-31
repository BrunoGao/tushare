#!/usr/bin/env python3
"""
市场数据仪表板API - 提供技术指标、板块信息、龙虎榜、资金流等数据
"""

import sys
import os
from flask import Flask, render_template, jsonify, request
from datetime import datetime, timedelta
import pandas as pd
import numpy as np

# 添加项目路径
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import config
from database.db_manager import DatabaseManager

app = Flask(__name__, template_folder='../templates')
db = DatabaseManager()

@app.route('/market-dashboard')
def market_dashboard():
    """市场数据仪表板主页"""
    return render_template('market_dashboard.html')

@app.route('/api/market/stats')
def api_market_stats():
    """获取市场统计数据"""
    try:
        # 获取活跃股票数量
        active_stocks = db.fetch_data("""
            SELECT COUNT(DISTINCT ts_code) as count 
            FROM technical_indicators
        """).iloc[0]['count']
        
        # 获取技术指标覆盖
        indicator_coverage = db.fetch_data("""
            SELECT COUNT(*) as count 
            FROM technical_indicators 
            WHERE ma5 IS NOT NULL
        """).iloc[0]['count']
        
        # 获取板块数量
        sector_count = db.fetch_data("""
            SELECT COUNT(*) as count FROM t_concept
        """).iloc[0]['count']
        
        # 计算资金净流入（模拟）
        net_money_flow = db.fetch_data("""
            SELECT SUM(amount) as total 
            FROM stock_daily_202507 
            WHERE trade_date = (SELECT MAX(trade_date) FROM stock_daily_202507)
        """).iloc[0]['total'] or 0
        
        return jsonify({
            'active_stocks': f"{active_stocks:,}",
            'indicator_coverage': f"{indicator_coverage:,}",
            'sector_count': f"{sector_count:,}",
            'net_money_flow': int(net_money_flow / 100)  # 转换为合理数值
        })
        
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@app.route('/api/market/hot-sectors')
def api_hot_sectors():
    """获取热门板块数据"""
    try:
        # 获取概念板块及其成分股
        sectors = db.fetch_data("""
            SELECT c.name, COUNT(cd.ts_code) as stock_count
            FROM t_concept c
            LEFT JOIN t_concept_detail cd ON c.concept_name = cd.concept_name
            GROUP BY c.name
            ORDER BY stock_count DESC
            LIMIT 10
        """)
        
        hot_sectors = []
        for _, row in sectors.iterrows():
            # 模拟涨跌幅数据
            change = np.random.normal(0, 3)  # 正态分布，均值0，标准差3
            hot_sectors.append({
                'name': row['name'],
                'change': round(change, 2),
                'stock_count': int(row['stock_count'] or 0)
            })
        
        return jsonify(hot_sectors)
        
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@app.route('/api/market/dragon-tiger')
def api_dragon_tiger():
    """获取龙虎榜数据"""
    try:
        # 获取龙虎榜数据
        dragon_tiger = db.fetch_data("""
            SELECT dt.ts_code, dt.name, dt.close, dt.pct_chg, dt.amount,
                   dt.buy_amount, dt.sell_amount, dt.reason
            FROM t_dragon_tiger_list dt
            ORDER BY dt.amount DESC
            LIMIT 20
        """)
        
        dragon_tiger_list = []
        for _, row in dragon_tiger.iterrows():
            dragon_tiger_list.append({
                'ts_code': row['ts_code'],
                'name': row['name'],
                'change': round(float(row['pct_chg'] or 0), 2),
                'buy_amount': int(row['buy_amount'] or 0),
                'sell_amount': int(row['sell_amount'] or 0),
                'reason': row['reason'] or '数据异常'
            })
        
        return jsonify(dragon_tiger_list)
        
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@app.route('/api/market/money-flow')
def api_money_flow():
    """获取资金流向数据"""
    try:
        # 获取资金流数据
        money_flow = db.fetch_data("""
            SELECT SUM(buy_amount) as total_buy, SUM(sell_amount) as total_sell
            FROM t_money_flow
            WHERE trade_date = (SELECT MAX(trade_date) FROM t_money_flow)
        """)
        
        if not money_flow.empty and money_flow.iloc[0]['total_buy']:
            total_buy = int(money_flow.iloc[0]['total_buy'])
            total_sell = int(money_flow.iloc[0]['total_sell'] or 0)
            
            return jsonify({
                'main_inflow': total_buy - total_sell,
                'retail_outflow': -(total_sell - total_buy) // 2
            })
        else:
            # 模拟数据
            return jsonify({
                'main_inflow': 2340000000,
                'retail_outflow': -890000000
            })
        
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@app.route('/api/market/stocks')
def api_stocks():
    """获取股票技术指标数据"""
    try:
        limit = request.args.get('limit', 100, type=int)
        search = request.args.get('search', '')
        
        # 构建查询条件
        where_clause = ""
        if search:
            where_clause = f"WHERE (b.name LIKE '%{search}%' OR t.ts_code LIKE '%{search}%')"
        
        # 获取最新的技术指标数据
        stocks = db.fetch_data(f"""
            SELECT t.ts_code, b.name, t.close, d.pct_chg,
                   t.ma5, t.ma10, t.ma20, t.rsi6, t.rsi12, t.vol_ratio
            FROM technical_indicators t
            JOIN stock_basic b ON t.ts_code = b.ts_code
            LEFT JOIN stock_daily_202507 d ON t.ts_code = d.ts_code 
                AND t.trade_date = d.trade_date
            {where_clause}
            ORDER BY t.trade_date DESC, t.ts_code
            LIMIT {limit}
        """)
        
        stocks_list = []
        for _, row in stocks.iterrows():
            # 生成简单的交易信号
            signal = None
            if row['ma5'] and row['ma20']:
                if row['ma5'] > row['ma20'] and (row['rsi6'] or 0) < 30:
                    signal = '买入'
                elif row['ma5'] < row['ma20'] and (row['rsi6'] or 0) > 70:
                    signal = '卖出'
            
            stocks_list.append({
                'ts_code': row['ts_code'],
                'name': row['name'],
                'close': float(row['close'] or 0),
                'pct_chg': float(row['pct_chg'] or 0),
                'ma5': float(row['ma5'] or 0),
                'ma10': float(row['ma10'] or 0),
                'ma20': float(row['ma20'] or 0),
                'rsi6': float(row['rsi6'] or 0),
                'rsi12': float(row['rsi12'] or 0),
                'vol_ratio': float(row['vol_ratio'] or 0),
                'signal': signal
            })
        
        return jsonify(stocks_list)
        
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@app.route('/api/market/heatmap/<indicator_type>')
def api_heatmap_data(indicator_type):
    """获取技术指标热力图数据"""
    try:
        # 根据指标类型获取不同的数据
        if indicator_type == 'macd':
            field = 'macd_dif'
        elif indicator_type == 'kdj':
            field = 'kdj_k'
        elif indicator_type == 'rsi':
            field = 'rsi6'
        else:
            field = 'rsi6'
        
        # 获取指标分布数据
        heatmap_data = db.fetch_data(f"""
            SELECT 
                CASE 
                    WHEN b.industry LIKE '%科技%' OR b.industry LIKE '%软件%' OR b.industry LIKE '%互联网%' THEN '科技'
                    WHEN b.industry LIKE '%银行%' OR b.industry LIKE '%保险%' OR b.industry LIKE '%证券%' THEN '金融'
                    WHEN b.industry LIKE '%医药%' OR b.industry LIKE '%生物%' OR b.industry LIKE '%医疗%' THEN '医药'
                    WHEN b.industry LIKE '%食品%' OR b.industry LIKE '%饮料%' OR b.industry LIKE '%零售%' THEN '消费'
                    WHEN b.industry LIKE '%机械%' OR b.industry LIKE '%制造%' OR b.industry LIKE '%工程%' THEN '工业'
                    ELSE '其他'
                END as sector,
                CASE 
                    WHEN t.{field} > 80 THEN '强买入'
                    WHEN t.{field} > 60 THEN '买入'
                    WHEN t.{field} > 40 THEN '中性'
                    WHEN t.{field} > 20 THEN '卖出'
                    ELSE '强卖出'
                END as signal_level,
                COUNT(*) as count
            FROM technical_indicators t
            JOIN stock_basic b ON t.ts_code = b.ts_code
            WHERE t.{field} IS NOT NULL
            GROUP BY sector, signal_level
        """)
        
        # 转换为热力图格式
        sectors = ['科技', '金融', '医药', '消费', '工业', '其他']
        signals = ['强买入', '买入', '中性', '卖出', '强卖出']
        
        heatmap_result = []
        for i, sector in enumerate(sectors):
            for j, signal in enumerate(signals):
                matching = heatmap_data[
                    (heatmap_data['sector'] == sector) & 
                    (heatmap_data['signal_level'] == signal)
                ]
                count = int(matching['count'].sum()) if not matching.empty else 0
                heatmap_result.append([i, j, count, count])
        
        return jsonify(heatmap_result)
        
    except Exception as e:
        return jsonify({'error': str(e)}), 500

if __name__ == '__main__':
    app.run(debug=True, host='0.0.0.0', port=5001)