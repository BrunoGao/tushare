from flask import Flask, request, jsonify, render_template_string
from flask_cors import CORS
import logging
from datetime import datetime, timedelta
import sys, os
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import config
from utils.db_helper import db
from analysis.recommender import recommender
from llm.interface import llm

# 初始化Flask应用
app = Flask(__name__)
CORS(app)  # 允许跨域请求
logging.basicConfig(level=getattr(logging, config.LOG_LEVEL), format=config.LOG_FORMAT)
logger = logging.getLogger(__name__)

# 简单的前端页面模板
HTML_TEMPLATE = '''
<!DOCTYPE html>
<html><head><meta charset="utf-8"><title>股票推荐系统</title>
<style>body{font-family:Arial;margin:20px}table{border-collapse:collapse;width:100%}th,td{border:1px solid #ddd;padding:8px;text-align:left}th{background-color:#f2f2f2}.btn{background:#007bff;color:white;padding:8px 16px;border:none;cursor:pointer;margin:5px}.btn:hover{background:#0056b3}.answer{background:#f8f9fa;padding:15px;border-left:4px solid #007bff;margin:10px 0}</style></head>
<body><h1>🚀 A股推荐系统</h1>
<div><button class="btn" onclick="loadRecommend('ma_crossover')">均线策略</button>
<button class="btn" onclick="loadRecommend('momentum')">动量策略</button>
<button class="btn" onclick="loadStock()">股票查询</button>
<button class="btn" onclick="askLLM()">AI问答</button></div>
<div id="result"></div>
<script>
async function loadRecommend(strategy){
  const response = await fetch(`/api/recommend?strategy=${strategy}&limit=20`);
  const data = await response.json();
  if(data.success){
    let html = `<h2>${strategy}策略推荐 (${data.data.length}只)</h2><table><tr><th>排名</th><th>代码</th><th>名称</th><th>分数</th><th>理由</th></tr>`;
    data.data.forEach(item => html += `<tr><td>${item.rank_no}</td><td>${item.ts_code}</td><td>${item.name}</td><td>${item.score}</td><td>${item.reason}</td></tr>`);
    document.getElementById('result').innerHTML = html + '</table>';
  }
}
async function loadStock(){
  const code = prompt('请输入股票代码(如000001.SZ):');
  if(code){
    const response = await fetch(`/api/stock/${code}/history?days=30`);
    const data = await response.json();
    if(data.success){
      let html = `<h2>${code} 历史数据</h2><table><tr><th>日期</th><th>开盘</th><th>收盘</th><th>最高</th><th>最低</th><th>成交量</th></tr>`;
      data.data.forEach(item => html += `<tr><td>${item.trade_date}</td><td>${item.open}</td><td>${item.close}</td><td>${item.high}</td><td>${item.low}</td><td>${item.vol}</td></tr>`);
      document.getElementById('result').innerHTML = html + '</table>';
    }
  }
}
async function askLLM(){
  const question = prompt('请输入您的问题:');
  if(question){
    const response = await fetch('/api/llm/ask', {
      method: 'POST',
      headers: {'Content-Type': 'application/json'},
      body: JSON.stringify({question: question})
    });
    const data = await response.json();
    if(data.success){
      document.getElementById('result').innerHTML = `<h2>AI回答</h2><div class="answer">${data.data.answer}</div>`;
    }
  }
}
</script></body></html>
'''

@app.route('/')
def index(): return render_template_string(HTML_TEMPLATE)  # 主页

@app.route('/api/health')
def health(): return jsonify({"status": "healthy", "timestamp": datetime.now().isoformat()})  # 健康检查

@app.route('/api/recommend')  # 获取推荐
def get_recommendations():
    try:
        strategy = request.args.get('strategy', 'ma_crossover')
        limit = int(request.args.get('limit', config.MAX_RECOMMEND))
        date_filter = request.args.get('date')
        
        recommendations = recommender.get_recommendations(date_filter, strategy, limit)
        
        return jsonify({
            "success": True,
            "count": len(recommendations),
            "data": recommendations,
            "timestamp": datetime.now().isoformat()
        })
    except Exception as e:
        logger.error(f"获取推荐失败: {e}")
        return jsonify({"success": False, "error": str(e)}), 500

@app.route('/api/recommend/generate')  # 生成推荐
def generate_recommendations():
    try:
        strategy = request.args.get('strategy', 'ma_crossover')
        limit = int(request.args.get('limit', config.MAX_RECOMMEND))
        
        recommendations = recommender.generate_recommendations(strategy, limit)
        
        return jsonify({
            "success": True,
            "count": len(recommendations),
            "message": f"成功生成{len(recommendations)}只推荐股票",
            "timestamp": datetime.now().isoformat()
        })
    except Exception as e:
        logger.error(f"生成推荐失败: {e}")
        return jsonify({"success": False, "error": str(e)}), 500

@app.route('/api/stock/<ts_code>')  # 获取股票基本信息
def get_stock_info(ts_code):
    try:
        with db.engine.connect() as conn:
            sql = "SELECT * FROM stock_basic WHERE ts_code = :ts_code"
            result = conn.execute(db.text(sql), {'ts_code': ts_code})
            row = result.fetchone()
            
        if row:
            return jsonify({
                "success": True,
                "data": dict(row._mapping),
                "timestamp": datetime.now().isoformat()
            })
        else:
            return jsonify({"success": False, "error": "股票不存在"}), 404
            
    except Exception as e:
        logger.error(f"获取股票信息失败: {e}")
        return jsonify({"success": False, "error": str(e)}), 500

@app.route('/api/stock/<ts_code>/history')  # 获取股票历史数据
def get_stock_history(ts_code):
    try:
        days = int(request.args.get('days', 30))
        limit = int(request.args.get('limit', 100))
        
        end_date = datetime.now().strftime('%Y%m%d')
        start_date = (datetime.now() - timedelta(days=days)).strftime('%Y%m%d')
        
        df = db.query_stock_data(ts_code, start_date, end_date, limit)
        
        if not df.empty:
            data = df.to_dict('records')
            # 转换日期格式
            for item in data:
                if 'trade_date' in item:
                    item['trade_date'] = item['trade_date'].strftime('%Y-%m-%d') if hasattr(item['trade_date'], 'strftime') else str(item['trade_date'])
            
            return jsonify({
                "success": True,
                "count": len(data),
                "data": data,
                "timestamp": datetime.now().isoformat()
            })
        else:
            return jsonify({"success": False, "error": "无历史数据"}), 404
            
    except Exception as e:
        logger.error(f"获取历史数据失败: {e}")
        return jsonify({"success": False, "error": str(e)}), 500

@app.route('/api/stocks')  # 获取股票列表
def get_stock_list():
    try:
        exchange = request.args.get('exchange')
        industry = request.args.get('industry')
        limit = int(request.args.get('limit', 100))
        
        sql = "SELECT ts_code, name, industry, exchange, list_date FROM stock_basic WHERE list_status='L'"
        params = {}
        
        if exchange:
            sql += " AND exchange = :exchange"
            params['exchange'] = exchange
        if industry:
            sql += " AND industry = :industry"
            params['industry'] = industry
            
        sql += " ORDER BY ts_code"
        if limit:
            sql += f" LIMIT {limit}"
            
        with db.engine.connect() as conn:
            result = conn.execute(db.text(sql), params)
            data = [dict(row._mapping) for row in result]
            
        return jsonify({
            "success": True,
            "count": len(data),
            "data": data,
            "timestamp": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"获取股票列表失败: {e}")
        return jsonify({"success": False, "error": str(e)}), 500

@app.route('/api/llm/ask', methods=['POST'])  # 大模型问答接口
def llm_ask():
    try:
        data = request.get_json()
        question = data.get('question', '')
        ts_code = data.get('ts_code')
        
        if not question:
            return jsonify({"success": False, "error": "问题不能为空"}), 400
            
        # 调用LLM接口
        result = llm.ask_about_stock(question, ts_code)
        
        return jsonify({
            "success": result.get("success", False),
            "data": result,
            "timestamp": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"LLM问答失败: {e}")
        return jsonify({"success": False, "error": str(e)}), 500

@app.route('/api/llm/trend')  # 市场趋势分析
def llm_trend():
    try:
        industry = request.args.get('industry')
        days = int(request.args.get('days', 7))
        
        result = llm.analyze_market_trend(industry, days)
        
        return jsonify({
            "success": result.get("success", False),
            "data": result,
            "timestamp": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"趋势分析失败: {e}")
        return jsonify({"success": False, "error": str(e)}), 500

@app.route('/api/llm/advice')  # 投资建议
def llm_advice():
    try:
        risk_level = request.args.get('risk_level', 'medium')
        amount = float(request.args.get('amount', 10000))
        
        result = llm.generate_investment_advice(risk_level, amount)
        
        return jsonify({
            "success": result.get("success", False),
            "data": result,
            "timestamp": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"投资建议失败: {e}")
        return jsonify({"success": False, "error": str(e)}), 500

@app.route('/api/llm/explain/<ts_code>/<strategy>')  # 推荐解释
def llm_explain(ts_code, strategy):
    try:
        result = llm.explain_recommendation(ts_code, strategy)
        
        return jsonify({
            "success": result.get("success", False),
            "data": result,
            "timestamp": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"推荐解释失败: {e}")
        return jsonify({"success": False, "error": str(e)}), 500

@app.route('/api/stats')  # 系统统计信息
def get_stats():
    try:
        with db.engine.connect() as conn:
            # 获取统计数据
            stats = {}
            stats['stock_count'] = conn.execute(db.text("SELECT COUNT(*) FROM stock_basic WHERE list_status='L'")).scalar()
            stats['recommend_count'] = conn.execute(db.text("SELECT COUNT(*) FROM recommend_result WHERE recommend_date = CURDATE()")).scalar()
            stats['last_update'] = conn.execute(db.text("SELECT MAX(created_at) FROM system_log")).scalar()
            stats['llm_available'] = llm.health_check()
            
        return jsonify({
            "success": True,
            "data": stats,
            "timestamp": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"获取统计信息失败: {e}")
        return jsonify({"success": False, "error": str(e)}), 500

@app.errorhandler(404)
def not_found(error): return jsonify({"success": False, "error": "接口不存在"}), 404

@app.errorhandler(500)
def internal_error(error): return jsonify({"success": False, "error": "服务器内部错误"}), 500

if __name__ == '__main__':
    logger.info(f"启动API服务器 {config.API_HOST}:{config.API_PORT}")
    app.run(host=config.API_HOST, port=config.API_PORT, debug=config.API_DEBUG) 