from flask import Flask, request, jsonify, render_template, send_file, session
from flask_cors import CORS
import logging
import pandas as pd
from datetime import datetime, timedelta
import sys, os
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import config
from utils.db_helper import db
from analysis.recommender import recommender
from utils.user_manager import user_manager, UserRole

# 初始化日志
logging.basicConfig(level=getattr(logging, config.LOG_LEVEL), format=config.LOG_FORMAT)
logger = logging.getLogger(__name__)

# 导入我们的综合训练系统
try:
    from ai.unified_trainer import unified_trainer
    UNIFIED_TRAINER_AVAILABLE = True
    logger.info("✅ 综合训练系统已加载")
except ImportError as e:
    UNIFIED_TRAINER_AVAILABLE = False
    logger.warning(f"⚠️ 综合训练系统不可用: {e}")

try:
    from llm.enhanced_feature_engineering import enhanced_feature_engineer
    ENHANCED_FEATURES_AVAILABLE = True
    logger.info("✅ 增强特征工程已加载")
except ImportError as e:
    ENHANCED_FEATURES_AVAILABLE = False
    logger.warning(f"⚠️ 增强特征工程不可用: {e}")

try:
    from ai.model_evaluation import evaluation_system
    EVALUATION_SYSTEM_AVAILABLE = True
    logger.info("✅ 模型评估系统已加载")
except ImportError as e:
    EVALUATION_SYSTEM_AVAILABLE = False
    logger.warning(f"⚠️ 模型评估系统不可用: {e}")

try:
    from ai.model_optimizer import model_optimizer
    MODEL_OPTIMIZER_AVAILABLE = True
    logger.info("✅ 模型优化器已加载")
except ImportError as e:
    MODEL_OPTIMIZER_AVAILABLE = False
    logger.warning(f"⚠️ 模型优化器不可用: {e}")
try:
    from analysis.technical_indicators import tech_indicator
except ImportError:
    # 如果talib有问题，使用简化版技术指标
    from utils.technical_indicators_simple import tech_indicator
    print("⚠️ 使用简化版技术指标（talib不可用）")
from llm.stock_analyzer import stock_analyzer
try:
    from frontend.chart_generator import chart_generator
except ImportError:
    # 如果原图表生成器有问题，使用简化版
    from frontend.simple_chart_generator import simple_chart_generator as chart_generator
    print("⚠️ 使用简化版图表生成器")
from sqlalchemy import text
import tempfile
import uuid
import numpy as np
from database.db_manager import DatabaseManager
from dataclasses import asdict

# 初始化数据库管理器
db_manager = DatabaseManager()

# 初始化Flask应用
app = Flask(__name__, template_folder='../templates')
app.secret_key = 'ljwx_stock_secret_2024'  # 用于session加密
CORS(app, supports_credentials=True)  # 允许跨域请求，支持认证

# 导入并注册个人推荐API蓝图
try:
    from api.personal_recommendations_api import recommendations_bp
    app.register_blueprint(recommendations_bp)
    logger.info("✅ 个人推荐API已注册")
except ImportError as e:
    logger.warning(f"⚠️ 个人推荐API加载失败: {e}")

# 权限检查装饰器
def require_permission(permission):
    """需要特定权限的装饰器"""
    def decorator(f):
        def decorated(*args, **kwargs):
            # 检查session token
            session_token = request.headers.get('Authorization')
            if session_token and session_token.startswith('Bearer '):
                session_token = session_token.split(' ')[1]
            else:
                session_token = request.headers.get('X-Session-Token')
            
            if not session_token:
                return jsonify({"success": False, "error": "需要登录", "code": 401}), 401
            
            # 验证session
            user_info = user_manager.validate_session(session_token)
            if not user_info:
                return jsonify({"success": False, "error": "登录已过期", "code": 401}), 401
            
            # 检查权限
            if permission not in user_info['permissions']:
                return jsonify({"success": False, "error": "权限不足", "code": 403}), 403
            
            # 将用户信息传递给视图函数
            request.current_user = user_info
            return f(*args, **kwargs)
        
        decorated.__name__ = f.__name__
        return decorated
    return decorator

def require_admin(f):
    """需要管理员权限的装饰器"""
    def decorated(*args, **kwargs):
        session_token = request.headers.get('Authorization')
        if session_token and session_token.startswith('Bearer '):
            session_token = session_token.split(' ')[1]
        else:
            session_token = request.headers.get('X-Session-Token')
        
        if not session_token:
            return jsonify({"success": False, "error": "需要登录", "code": 401}), 401
        
        user_info = user_manager.validate_session(session_token)
        if not user_info:
            return jsonify({"success": False, "error": "登录已过期", "code": 401}), 401
        
        if user_info['role'] != 'admin':
            return jsonify({"success": False, "error": "需要管理员权限", "code": 403}), 403
        
        request.current_user = user_info
        return f(*args, **kwargs)
    
    decorated.__name__ = f.__name__
    return decorated

# 增强的前端页面模板
HTML_TEMPLATE = '''
<!DOCTYPE html>
<html><head><meta charset="utf-8"><title>灵境万象 - 实时股票分析系统</title>
<script src="https://cdn.jsdelivr.net/npm/echarts@5.4.3/dist/echarts.min.js"></script>
<style>
body{font-family:'Microsoft YaHei',Arial;margin:0;padding:20px;background:#1a1a1a;color:#fff}
.header{text-align:center;margin-bottom:30px}
.nav{display:flex;justify-content:center;gap:15px;margin-bottom:30px}
.btn{background:#007bff;color:white;padding:10px 20px;border:none;border-radius:5px;cursor:pointer;margin:5px;font-size:14px}
.btn:hover{background:#0056b3}
.btn.danger{background:#dc3545}
.btn.success{background:#28a745}
.container{max-width:1200px;margin:0 auto}
.row{display:flex;gap:20px;margin-bottom:20px}
.col{flex:1;background:#2d2d2d;padding:20px;border-radius:8px}
.result{background:#f8f9fa;color:#333;padding:15px;border-left:4px solid #007bff;margin:10px 0;border-radius:5px}
.analysis-box{background:#2d2d2d;padding:20px;margin:10px 0;border-radius:8px;border:1px solid #444}
.signal-box{padding:10px;margin:5px;border-radius:5px;text-align:center;font-weight:bold}
.signal-buy{background:#28a745;color:white}
.signal-sell{background:#dc3545;color:white}
.signal-hold{background:#ffc107;color:#333}
.chart-container{width:100%;height:600px;margin:20px 0;border:1px solid #444;border-radius:8px}
.loading{text-align:center;padding:20px;color:#007bff}
table{width:100%;border-collapse:collapse;margin:10px 0}
th,td{padding:8px;text-align:left;border:1px solid #444}
th{background:#333;color:#fff}
.input-group{margin:10px 0}
.input-group label{display:block;margin-bottom:5px;font-weight:bold}
.input-group input,select{width:100%;padding:8px;border:1px solid #444;border-radius:4px;background:#333;color:#fff}
.realtime-panel{display:grid;grid-template-columns:1fr 2fr;gap:20px;margin:20px 0}
.price-board{background:#333;padding:15px;border-radius:8px}
.price-item{display:flex;justify-content:space-between;margin:8px 0}
.price-up{color:#0f0}.price-down{color:#f00}
.ws-status{position:fixed;top:10px;right:10px;padding:5px 10px;border-radius:5px;font-size:12px}
.ws-connected{background:#28a745}.ws-disconnected{background:#dc3545}
.mini-chart{height:200px;margin:10px 0}
.indicator-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(120px,1fr));gap:10px;margin:15px 0}
.indicator-item{background:#444;padding:10px;border-radius:5px;text-align:center}
</style></head>
<body>
<div class="ws-status" id="ws-status">🔴 未连接</div>
<div class="container">
<div class="header">
<h1>🚀 灵境万象 - 实时股票分析系统</h1>
<p>WebSocket实时推送 | 技术分析 | AI分析 | 实时信号监控</p>
</div>

<div class="nav">
<button class="btn" onclick="showSection('analysis')">📊 股票分析</button>
<button class="btn" onclick="showSection('signals')">🎯 实时信号</button>
<button class="btn" onclick="showSection('recommend')">⭐ 智能推荐</button>
<button class="btn" onclick="showSection('llm')">🤖 AI问答</button>
<button class="btn" onclick="showSection('portfolio')">📈 组合监控</button>
</div>

<!-- 股票分析页面 -->
<div id="analysis" class="section" style="display:none;">
<div class="row">
<div class="col">
<h3>📊 技术分析</h3>
<div class="input-group">
<label>股票代码:</label>
<div style="position:relative;">
<input type="text" id="analysis-code" placeholder="输入股票代码或名称搜索" value="000001.SZ" autocomplete="off">
<div id="search-suggestions" style="display:none;position:absolute;top:100%;left:0;right:0;background:#333;border:1px solid #555;border-top:none;max-height:200px;overflow-y:auto;z-index:1000;"></div>
</div>
<div style="margin-top:10px;">
<button class="btn" style="font-size:12px;padding:5px 10px;" onclick="loadPopularStocks('all')">热门股票</button>
<button class="btn" style="font-size:12px;padding:5px 10px;" onclick="loadPopularStocks('bank')">银行股</button>
<button class="btn" style="font-size:12px;padding:5px 10px;" onclick="loadPopularStocks('tech')">科技股</button>
<button class="btn" style="font-size:12px;padding:5px 10px;" onclick="loadPopularStocks('consumer')">消费股</button>
</div>
</div>
<div class="input-group">
<label>分析周期:</label>
<select id="analysis-days">
<option value="30">30天</option>
<option value="60">60天</option>
<option value="120" selected>120天</option>
<option value="250">250天</option>
</select>
</div>
<button class="btn success" onclick="loadTechAnalysis()">开始分析</button>
<button class="btn" onclick="loadChart()">生成图表</button>
<button class="btn" onclick="subscribeRealtime()" id="subscribe-btn">🔔 订阅实时</button>
</div>

<!-- 实时数据面板 -->
<div class="realtime-panel" id="realtime-panel" style="display:none;">
<div class="price-board">
<h4>💰 实时行情</h4>
<div class="price-item"><span>当前价:</span><span id="current-price">--</span></div>
<div class="price-item"><span>涨跌额:</span><span id="price-change">--</span></div>
<div class="price-item"><span>涨跌幅:</span><span id="price-change-pct">--</span></div>
<div class="price-item"><span>成交量:</span><span id="volume">--</span></div>
<div class="indicator-grid">
<div class="indicator-item"><div>MA5</div><div id="ma5">--</div></div>
<div class="indicator-item"><div>MA20</div><div id="ma20">--</div></div>
<div class="indicator-item"><div>RSI</div><div id="rsi">--</div></div>
<div class="indicator-item"><div>MACD</div><div id="macd">--</div></div>
</div>
</div>
<div><h4>📈 实时走势</h4><div class="mini-chart" id="mini-chart"></div></div>
</div>
</div>
<div class="col">
<h3>📈 技术指标</h3>
<div id="tech-indicators"></div>
</div>
</div>
<div class="chart-container" id="analysis-chart"></div>
</div>

<!-- 实时信号页面 -->
<div id="signals" class="section" style="display:none;">
<div class="row">
<div class="col">
<h3>🎯 实时交易信号</h3>
<div class="input-group">
<label>监控股票:</label>
<div style="position:relative;">
<input type="text" id="signal-codes" placeholder="输入股票代码，用逗号分隔" value="000001.SZ" autocomplete="off">
<div id="signal-suggestions" style="display:none;position:absolute;top:100%;left:0;right:0;background:#333;border:1px solid #555;border-top:none;max-height:200px;overflow-y:auto;z-index:1000;"></div>
</div>
</div>
<button class="btn success" onclick="loadRealTimeSignals()">开始监控</button>
<button class="btn" onclick="loadSignalChart()">信号图表</button>
</div>
<div class="col">
<h3>📊 信号统计</h3>
<div id="signal-stats"></div>
</div>
</div>
<div id="signals-result"></div>
</div>

<!-- 智能推荐页面 -->
<div id="recommend" class="section" style="display:none;">
<div class="row">
<div class="col">
<h3>⭐ 智能推荐</h3>
<div class="input-group">
<label>推荐策略:</label>
<select id="recommend-strategy">
<option value="ma_crossover">均线策略</option>
<option value="momentum">动量策略</option>
</select>
</div>
<div class="input-group">
<label>推荐数量:</label>
<select id="recommend-limit">
<option value="10">10只</option>
<option value="20" selected>20只</option>
<option value="50">50只</option>
</select>
</div>
<button class="btn success" onclick="loadRecommend()">获取推荐</button>
<button class="btn" onclick="generateRecommend()">生成新推荐</button>
</div>
</div>
<div id="recommend-result"></div>
</div>

<!-- AI问答页面 -->
<div id="llm" class="section" style="display:none;">
<div class="row">
<div class="col">
<h3>🤖 AI股票分析师</h3>
<div class="input-group">
<label>请输入您的问题:</label>
<input type="text" id="llm-question" placeholder="分析一下000001的走势" style="width:70%;display:inline-block;">
<button class="btn" onclick="askLLM()" style="width:25%;display:inline-block;">提问</button>
</div>
<div class="input-group">
<label>快速问题:</label>
<button class="btn" onclick="quickAsk('推荐几只好股票')">推荐股票</button>
<button class="btn" onclick="quickAsk('银行股怎么样')">行业分析</button>
<button class="btn" onclick="quickAsk('市场趋势如何')">市场趋势</button>
</div>
</div>
</div>
<div id="llm-result"></div>
</div>

<!-- 组合监控页面 -->
<div id="portfolio" class="section" style="display:none;">
<div class="row">
<div class="col">
<h3>📈 投资组合监控</h3>
<div class="input-group">
<label>股票组合:</label>
<div style="position:relative;">
<input type="text" id="portfolio-codes" placeholder="输入股票代码，用逗号分隔" value="000001.SZ,000002.SZ,600000.SH" autocomplete="off">
<div id="portfolio-suggestions" style="display:none;position:absolute;top:100%;left:0;right:0;background:#333;border:1px solid #555;border-top:none;max-height:200px;overflow-y:auto;z-index:1000;"></div>
</div>
</div>
<button class="btn success" onclick="loadPortfolio()">监控组合</button>
<button class="btn" onclick="loadPortfolioDashboard()">仪表板</button>
</div>
<div class="col">
<h3>📊 组合统计</h3>
<div id="portfolio-stats"></div>
</div>
</div>
<div id="portfolio-result"></div>
</div>

<div id="main-result"></div>
</div>

<script>
let currentSection = 'analysis';
let ws = null;
let isSubscribed = false;
let miniChart = null;
let currentStockCode = '';

// WebSocket连接管理
function connectWebSocket() {
    const wsUrl = `ws://${window.location.hostname}:8765`;
    ws = new WebSocket(wsUrl);
    
    ws.onopen = () => {
        updateWSStatus('🟢 已连接', 'ws-connected');
        console.log('WebSocket连接成功');
    };
    
    ws.onmessage = (event) => {
        const data = JSON.parse(event.data);
        handleWebSocketMessage(data);
    };
    
    ws.onclose = () => {
        updateWSStatus('🔴 已断开', 'ws-disconnected');
        setTimeout(connectWebSocket, 3000); // 自动重连
    };
    
    ws.onerror = (error) => {
        console.error('WebSocket错误:', error);
        updateWSStatus('🔴 连接错误', 'ws-disconnected');
    };
}

function updateWSStatus(text, className) {
    const status = document.getElementById('ws-status');
    status.textContent = text;
    status.className = `ws-status ${className}`;
}

function handleWebSocketMessage(data) {
    console.log('收到数据:', data);
    if (data.type === 'realtime_data' || data.type === 'realtime_update') {
        updateRealtimeDisplay(data.data);
        updateMiniChart(data.data.trend_data);
    }
}

function subscribeRealtime() {
    const code = document.getElementById('analysis-code').value;
    if (!code) {
        alert('请先输入股票代码');
        return;
    }
    
    if (!ws || ws.readyState !== WebSocket.OPEN) {
        alert('WebSocket未连接，正在尝试连接...');
        connectWebSocket();
        return;
    }
    
    if (isSubscribed && currentStockCode === code) {
        // 取消订阅
        ws.send(JSON.stringify({type: 'unsubscribe', code: code}));
        isSubscribed = false;
        currentStockCode = '';
        document.getElementById('subscribe-btn').textContent = '🔔 订阅实时';
        document.getElementById('realtime-panel').style.display = 'none';
    } else {
        // 订阅
        ws.send(JSON.stringify({type: 'subscribe', code: code}));
        isSubscribed = true;
        currentStockCode = code;
        document.getElementById('subscribe-btn').textContent = '🔕 取消订阅';
        document.getElementById('realtime-panel').style.display = 'grid';
        initMiniChart();
    }
}

function updateRealtimeDisplay(data) {
    if (data.error) {
        console.error('数据错误:', data.error);
        return;
    }
    
    const price = data.price;
    const indicators = data.indicators;
    
    // 更新价格信息
    document.getElementById('current-price').textContent = price.current.toFixed(2);
    
    const changeEl = document.getElementById('price-change');
    const changePctEl = document.getElementById('price-change-pct');
    
    changeEl.textContent = (price.change >= 0 ? '+' : '') + price.change.toFixed(2);
    changePctEl.textContent = (price.change_pct >= 0 ? '+' : '') + price.change_pct.toFixed(2) + '%';
    
    // 设置颜色
    const colorClass = price.change >= 0 ? 'price-up' : 'price-down';
    changeEl.className = colorClass;
    changePctEl.className = colorClass;
    
    document.getElementById('volume').textContent = (price.volume / 10000).toFixed(0) + '万';
    
    // 更新技术指标
    document.getElementById('ma5').textContent = indicators.ma5.toFixed(2);
    document.getElementById('ma20').textContent = indicators.ma20.toFixed(2);
    document.getElementById('rsi').textContent = indicators.rsi.toFixed(1);
    document.getElementById('macd').textContent = indicators.macd.toFixed(4);
}

function initMiniChart() {
    if (!miniChart) {
        miniChart = echarts.init(document.getElementById('mini-chart'));
    }
}

function updateMiniChart(trendData) {
    if (!miniChart || !trendData) return;
    
    const dates = trendData.map(d => d.date.substr(4,2) + '/' + d.date.substr(6,2));
    const prices = trendData.map(d => d.close);
    const ma5 = trendData.map(d => d.ma5);
    const ma20 = trendData.map(d => d.ma20);
    
    const option = {
        backgroundColor: 'transparent',
        grid: { top: 10, bottom: 30, left: 40, right: 20 },
        xAxis: { type: 'category', data: dates, axisLine: { lineStyle: { color: '#666' } } },
        yAxis: { type: 'value', axisLine: { lineStyle: { color: '#666' } } },
        series: [
            { name: '收盘价', type: 'line', data: prices, lineStyle: { color: '#fff' } },
            { name: 'MA5', type: 'line', data: ma5, lineStyle: { color: '#0f0' } },
            { name: 'MA20', type: 'line', data: ma20, lineStyle: { color: '#f90' } }
        ]
    };
    
    miniChart.setOption(option);
}

// 页面加载时连接WebSocket
window.addEventListener('load', connectWebSocket);

function showSection(section) {
    document.querySelectorAll('.section').forEach(s => s.style.display = 'none');
    document.getElementById(section).style.display = 'block';
    currentSection = section;
}

// 显示第一个页面
showSection('analysis');

async function loadTechAnalysis() {
    const code = document.getElementById('analysis-code').value;
    const days = document.getElementById('analysis-days').value;
    
    if (!code) {
        alert('请输入股票代码');
        return;
    }
    
    showLoading('tech-indicators');
    
    try {
        const response = await fetch(`/api/technical/indicators/${code}?days=${days}`);
        const data = await response.json();
        
        if (data.success) {
            displayTechIndicators(data.data);
        } else {
            document.getElementById('tech-indicators').innerHTML = `<div class="result">错误: ${data.error}</div>`;
        }
    } catch (error) {
        document.getElementById('tech-indicators').innerHTML = `<div class="result">请求失败: ${error}</div>`;
    }
}

async function loadChart() {
    const code = document.getElementById('analysis-code').value;
    const days = document.getElementById('analysis-days').value;
    
    showLoading('analysis-chart');
    
    try {
        const response = await fetch(`/api/chart/comprehensive/${code}?days=${days}`);
        const data = await response.json();
        
        if (data.success) {
            document.getElementById('analysis-chart').innerHTML = data.chart_html;
        } else {
            document.getElementById('analysis-chart').innerHTML = `<div class="result">图表生成失败: ${data.error}</div>`;
        }
    } catch (error) {
        document.getElementById('analysis-chart').innerHTML = `<div class="result">请求失败: ${error}</div>`;
    }
}

async function loadRealTimeSignals() {
    const codes = document.getElementById('signal-codes').value.split(',');
    
    showLoading('signals-result');
    
    try {
        const response = await fetch('/api/signals/realtime', {
            method: 'POST',
            headers: {'Content-Type': 'application/json'},
            body: JSON.stringify({stock_codes: codes})
        });
        const data = await response.json();
        
        if (data.success) {
            displaySignals(data.data);
        } else {
            document.getElementById('signals-result').innerHTML = `<div class="result">错误: ${data.error}</div>`;
        }
    } catch (error) {
        document.getElementById('signals-result').innerHTML = `<div class="result">请求失败: ${error}</div>`;
    }
}

async function loadRecommend() {
    const strategy = document.getElementById('recommend-strategy').value;
    const limit = document.getElementById('recommend-limit').value;
    
    showLoading('recommend-result');
    
    try {
        const response = await fetch(`/api/recommend?strategy=${strategy}&limit=${limit}`);
        const data = await response.json();
        
        if (data.success) {
            displayRecommendations(data.data);
        } else {
            document.getElementById('recommend-result').innerHTML = `<div class="result">错误: ${data.error}</div>`;
        }
    } catch (error) {
        document.getElementById('recommend-result').innerHTML = `<div class="result">请求失败: ${error}</div>`;
    }
}

async function askLLM() {
    const question = document.getElementById('llm-question').value;
    
    if (!question) {
        alert('请输入问题');
        return;
    }
    
    showLoading('llm-result');
    
    try {
        const response = await fetch('/api/llm/intelligent', {
            method: 'POST',
            headers: {'Content-Type': 'application/json'},
            body: JSON.stringify({question: question})
        });
        const data = await response.json();
        
        if (data.success) {
            displayLLMResult(data.data);
        } else {
            document.getElementById('llm-result').innerHTML = `<div class="result">错误: ${data.error}</div>`;
        }
    } catch (error) {
        document.getElementById('llm-result').innerHTML = `<div class="result">请求失败: ${error}</div>`;
    }
}

function quickAsk(question) {
    document.getElementById('llm-question').value = question;
    askLLM();
}

async function loadPortfolio() {
    const codes = document.getElementById('portfolio-codes').value.split(',');
    
    showLoading('portfolio-result');
    
    try {
        const response = await fetch('/api/portfolio/monitor', {
            method: 'POST',
            headers: {'Content-Type': 'application/json'},
            body: JSON.stringify({stock_codes: codes})
        });
        const data = await response.json();
        
        if (data.success) {
            displayPortfolio(data.data);
        } else {
            document.getElementById('portfolio-result').innerHTML = `<div class="result">错误: ${data.error}</div>`;
        }
    } catch (error) {
        document.getElementById('portfolio-result').innerHTML = `<div class="result">请求失败: ${error}</div>`;
    }
}

function displayTechIndicators(data) {
    const html = `
        <div class="analysis-box">
            <h4>📊 最新技术指标</h4>
            <table>
                <tr><th>指标</th><th>数值</th><th>状态</th></tr>
                <tr><td>RSI(14)</td><td>${data.rsi14.toFixed(2)}</td><td>${data.rsi14 > 70 ? '超买' : data.rsi14 < 30 ? '超卖' : '正常'}</td></tr>
                <tr><td>MACD</td><td>${data.macd.toFixed(4)}</td><td>${data.macd > data.macd_signal ? '多头' : '空头'}</td></tr>
                <tr><td>强弱度</td><td>${data.strength_score.toFixed(2)}</td><td>${data.strength_score > 60 ? '强势' : data.strength_score < 40 ? '弱势' : '中性'}</td></tr>
                <tr><td>MA5</td><td>${data.ma5.toFixed(2)}</td><td>${data.close > data.ma5 ? '上方' : '下方'}</td></tr>
                <tr><td>MA20</td><td>${data.ma20.toFixed(2)}</td><td>${data.close > data.ma20 ? '上方' : '下方'}</td></tr>
            </table>
            
            <h4>🎯 交易信号</h4>
            <div class="signal-box ${data.buy_signal ? 'signal-buy' : data.sell_signal ? 'signal-sell' : 'signal-hold'}">
                ${data.buy_signal ? '🟢 买入信号' : data.sell_signal ? '🔴 卖出信号' : '🟡 持有观望'}
            </div>
        </div>
    `;
    document.getElementById('tech-indicators').innerHTML = html;
}

function displaySignals(signals) {
    let html = '<div class="analysis-box"><h4>🎯 实时交易信号</h4>';
    
    signals.forEach(signal => {
        html += `
            <div class="signal-box ${signal.buy_signal ? 'signal-buy' : signal.sell_signal ? 'signal-sell' : 'signal-hold'}">
                <strong>${signal.stock_name}(${signal.stock_code})</strong><br>
                价格: ${signal.current_price}元 | 强弱度: ${signal.strength_score.toFixed(2)}<br>
                ${signal.signal_summary}
            </div>
        `;
    });
    
    html += '</div>';
    document.getElementById('signals-result').innerHTML = html;
}

function displayRecommendations(recommendations) {
    let html = '<div class="analysis-box"><h4>⭐ 智能推荐结果</h4><table><tr><th>排名</th><th>股票</th><th>分数</th><th>策略</th><th>理由</th></tr>';
    
    recommendations.forEach(rec => {
        html += `<tr>
            <td>${rec.rank_no}</td>
            <td>${rec.name}(${rec.ts_code})</td>
            <td>${rec.score.toFixed(3)}</td>
            <td>${rec.strategy}</td>
            <td>${rec.reason}</td>
        </tr>`;
    });
    
    html += '</table></div>';
    document.getElementById('recommend-result').innerHTML = html;
}

function displayLLMResult(result) {
    const html = `
        <div class="analysis-box">
            <h4>🤖 AI分析师回答</h4>
            <div class="result">${result.analysis}</div>
            <small>分析时间: ${new Date(result.timestamp).toLocaleString()}</small>
        </div>
    `;
    document.getElementById('llm-result').innerHTML = html;
}

function displayPortfolio(portfolio) {
    let html = '<div class="analysis-box"><h4>📈 投资组合监控</h4>';
    
    // 组合统计
    html += `
        <div class="row">
            <div class="col">
                <h5>📊 组合统计</h5>
                <p>总股票数: ${portfolio.summary.total_stocks}</p>
                <p>平均强弱度: ${portfolio.summary.avg_strength.toFixed(2)}</p>
                <p>平均涨跌幅: ${portfolio.summary.avg_change.toFixed(2)}%</p>
            </div>
        </div>
    `;
    
    // 股票详情
    html += '<table><tr><th>股票</th><th>价格</th><th>涨跌幅</th><th>强弱度</th><th>信号</th></tr>';
    
    portfolio.stocks.forEach(stock => {
        html += `<tr>
            <td>${stock.stock_name}(${stock.stock_code})</td>
            <td>${stock.current_price.toFixed(2)}</td>
            <td style="color: ${stock.change_pct >= 0 ? 'green' : 'red'}">${stock.change_pct.toFixed(2)}%</td>
            <td>${stock.strength_score.toFixed(2)}</td>
            <td>${stock.buy_signal ? '🟢买入' : stock.sell_signal ? '🔴卖出' : '🟡持有'}</td>
        </tr>`;
    });
    
    html += '</table></div>';
    document.getElementById('portfolio-result').innerHTML = html;
}

function showLoading(elementId) {
    document.getElementById(elementId).innerHTML = '<div class="loading">🔄 加载中...</div>';
}

// 自动刷新功能
setInterval(() => {
    if (currentSection === 'signals') {
        loadRealTimeSignals();
    } else if (currentSection === 'portfolio') {
        loadPortfolio();
    }
}, 30000); // 30秒刷新一次

// 股票搜索功能
let searchTimeout = null;
let currentSuggestionBox = null;

function setupStockSearch() {
    // 为所有股票输入框添加搜索功能
    const inputs = [
        {id: 'analysis-code', suggestions: 'search-suggestions'},
        {id: 'signal-codes', suggestions: 'signal-suggestions'},
        {id: 'portfolio-codes', suggestions: 'portfolio-suggestions'}
    ];
    
    inputs.forEach(config => {
        const input = document.getElementById(config.id);
        const suggestions = document.getElementById(config.suggestions);
        
        if (!input || !suggestions) return;
        
        input.addEventListener('input', (e) => {
            clearTimeout(searchTimeout);
            const query = e.target.value.trim();
            
            if (query.length < 2) {
                suggestions.style.display = 'none';
                return;
            }
            
            searchTimeout = setTimeout(() => {
                searchStocks(query, suggestions, input);
            }, 300);
        });
        
        input.addEventListener('focus', (e) => {
            currentSuggestionBox = suggestions;
        });
        
        input.addEventListener('blur', (e) => {
            // 延迟隐藏，以便点击建议项
            setTimeout(() => {
                if (suggestions) suggestions.style.display = 'none';
            }, 200);
        });
    });
    
    // 点击其他地方隐藏建议框
    document.addEventListener('click', (e) => {
        if (!e.target.closest('.input-group')) {
            document.querySelectorAll('[id$=\"-suggestions\"]').forEach(s => {
                s.style.display = 'none';
            });
        }
    });
}

async function searchStocks(query, suggestionsEl, inputEl) {
    try {
        const response = await fetch(`/api/stocks/search?q=${encodeURIComponent(query)}&limit=8`);
        const data = await response.json();
        
        if (data.success && data.data.length > 0) {
            displaySearchSuggestions(data.data, suggestionsEl, inputEl);
        } else {
            suggestionsEl.style.display = 'none';
        }
    } catch (error) {
        console.error('搜索失败:', error);
        suggestionsEl.style.display = 'none';
    }
}

function displaySearchSuggestions(stocks, suggestionsEl, inputEl) {
    let html = '';
    stocks.forEach(stock => {
        html += `
            <div style="padding:8px 12px;cursor:pointer;border-bottom:1px solid #555;"
                 onmouseover="this.style.background='#444'"
                 onmouseout="this.style.background='transparent'"
                 onclick="selectStock('${stock.ts_code}', '${stock.display_name}', '${inputEl.id}')">
                <div style="font-weight:bold;">${stock.display_name}</div>
                <div style="font-size:12px;color:#999;">${stock.industry} | ${stock.market}</div>
            </div>
        `;
    });
    
    suggestionsEl.innerHTML = html;
    suggestionsEl.style.display = 'block';
}

function selectStock(tsCode, displayName, inputId) {
    const input = document.getElementById(inputId);
    
    if (inputId === 'analysis-code') {
        // 单个股票输入
        input.value = tsCode;
    } else {
        // 多个股票输入（信号监控、投资组合）
        const currentValue = input.value.trim();
        const codes = currentValue ? currentValue.split(',').map(s => s.trim()) : [];
        
        // 避免重复添加
        if (!codes.includes(tsCode)) {
            codes.push(tsCode);
            input.value = codes.join(',');
        }
    }
    
    // 隐藏建议框
    document.querySelectorAll('[id$="-suggestions"]').forEach(s => s.style.display = 'none');
}

function selectPopularStock(tsCode, displayName) {
    const input = document.getElementById('analysis-code');
    input.value = tsCode;
    closePopularStocks();
}

function closePopularStocks() {
    const modal = document.getElementById('popular-stocks-modal');
    if (modal) {
        modal.remove();
    }
}

// 页面加载完成后初始化搜索功能
window.addEventListener('load', () => {
    setupStockSearch();
});

</script>
</body></html>
'''

# ============ 用户认证API ============

@app.route('/api/auth/login', methods=['POST'])
def login():
    """用户登录"""
    try:
        data = request.get_json()
        username = data.get('username', '').strip()
        password = data.get('password', '').strip()
        
        if not username or not password:
            return jsonify({"success": False, "error": "用户名和密码不能为空"}), 400
        
        # 用户认证
        user_info = user_manager.authenticate(username, password)
        if not user_info:
            return jsonify({"success": False, "error": "用户名或密码错误"}), 401
        
        logger.info(f"用户登录成功: {username}, 角色: {user_info['role']}")
        
        return jsonify({
            "success": True,
            "data": {
                "user": {
                    "username": user_info['username'],
                    "role": user_info['role'],
                    "permissions": user_info['permissions']
                },
                "session_token": user_info['session_token'],
                "login_time": user_info['login_time']
            },
            "message": f"欢迎 {user_info['role']} 用户 {username}",
            "timestamp": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"用户登录失败: {e}")
        return jsonify({"success": False, "error": str(e)}), 500

@app.route('/api/auth/logout', methods=['POST'])
def logout():
    """用户登出"""
    try:
        session_token = request.headers.get('Authorization')
        if session_token and session_token.startswith('Bearer '):
            session_token = session_token.split(' ')[1]
        else:
            session_token = request.headers.get('X-Session-Token')
        
        if not session_token:
            return jsonify({"success": False, "error": "未找到会话令牌"}), 400
        
        success = user_manager.logout(session_token)
        if success:
            return jsonify({
                "success": True,
                "message": "登出成功",
                "timestamp": datetime.now().isoformat()
            })
        else:
            return jsonify({"success": False, "error": "登出失败"}), 400
            
    except Exception as e:
        logger.error(f"用户登出失败: {e}")
        return jsonify({"success": False, "error": str(e)}), 500

@app.route('/api/auth/user-info')
def get_current_user():
    """获取当前用户信息"""
    try:
        session_token = request.headers.get('Authorization')
        if session_token and session_token.startswith('Bearer '):
            session_token = session_token.split(' ')[1]
        else:
            session_token = request.headers.get('X-Session-Token')
        
        if not session_token:
            return jsonify({"success": False, "error": "需要登录"}), 401
        
        user_info = user_manager.validate_session(session_token)
        if not user_info:
            return jsonify({"success": False, "error": "登录已过期"}), 401
        
        return jsonify({
            "success": True,
            "data": {
                "username": user_info['username'],
                "role": user_info['role'],
                "permissions": user_info['permissions']
            },
            "timestamp": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"获取用户信息失败: {e}")
        return jsonify({"success": False, "error": str(e)}), 500

@app.route('/api/auth/change-password', methods=['POST'])
@require_permission('user:update')
def change_password():
    """修改密码"""
    try:
        data = request.get_json()
        current_password = data.get('current_password', '').strip()
        new_password = data.get('new_password', '').strip()
        
        if not current_password or not new_password:
            return jsonify({"success": False, "error": "当前密码和新密码不能为空"}), 400
        
        if len(new_password) < 6:
            return jsonify({"success": False, "error": "新密码长度不能少于6位"}), 400
        
        # 这里需要实现密码修改逻辑
        # 由于当前用户管理器是内存存储，暂时返回提示
        return jsonify({
            "success": False,
            "error": "密码修改功能需要数据库支持，当前为演示版本"
        }), 501
        
    except Exception as e:
        logger.error(f"修改密码失败: {e}")
        return jsonify({"success": False, "error": str(e)}), 500

# ============ 管理员专用API ============

@app.route('/api/admin/users', methods=['GET'])
@require_admin
def list_users():
    """获取用户列表（管理员专用）"""
    try:
        users = user_manager.list_users()
        return jsonify({
            "success": True,
            "data": {
                "users": users,
                "count": len(users)
            },
            "timestamp": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"获取用户列表失败: {e}")
        return jsonify({"success": False, "error": str(e)}), 500

@app.route('/api/admin/users', methods=['POST'])
@require_admin
def create_user():
    """创建用户（管理员专用）"""
    try:
        data = request.get_json()
        username = data.get('username', '').strip()
        password = data.get('password', '').strip()
        role = data.get('role', 'user').strip()
        
        if not username or not password:
            return jsonify({"success": False, "error": "用户名和密码不能为空"}), 400
        
        if len(password) < 6:
            return jsonify({"success": False, "error": "密码长度不能少于6位"}), 400
        
        if role not in ['admin', 'user']:
            return jsonify({"success": False, "error": "角色只能是admin或user"}), 400
        
        user_role = UserRole.ADMIN if role == 'admin' else UserRole.USER
        success = user_manager.create_user(username, password, user_role)
        
        if success:
            return jsonify({
                "success": True,
                "message": f"用户 {username} 创建成功",
                "timestamp": datetime.now().isoformat()
            })
        else:
            return jsonify({"success": False, "error": "用户创建失败，可能用户已存在"}), 400
            
    except Exception as e:
        logger.error(f"创建用户失败: {e}")
        return jsonify({"success": False, "error": str(e)}), 500

@app.route('/api/admin/system/status')
@require_admin
def admin_system_status():
    """系统状态（管理员专用）"""
    try:
        # 获取系统统计信息
        status_info = {
            "active_sessions": len(user_manager.sessions),
            "total_users": len(user_manager.users),
            "system_uptime": "运行中",
            "database_status": "正常",
            "api_endpoints": len([rule for rule in app.url_map.iter_rules()]),
            "server_time": datetime.now().isoformat()
        }
        
        return jsonify({
            "success": True,
            "data": status_info,
            "timestamp": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"获取系统状态失败: {e}")
        return jsonify({"success": False, "error": str(e)}), 500


# 前端页面路由
@app.route('/')
def index():
    """主页 - 重定向到登录页面"""
    from flask import redirect
    return redirect('/login')

@app.route('/login')
def login_page():
    """用户登录页面"""
    try:
        from pathlib import Path
        frontend_path = Path(__file__).parent.parent / "frontend" / "login.html"
        if frontend_path.exists():
            return send_file(str(frontend_path))
        else:
            return "登录页面不存在", 404
    except Exception as e:
        logger.error(f"加载登录页面失败: {e}")
        return f"加载登录页面失败: {e}", 500

@app.route('/analysis')
def analysis_page():
    """技术分析页面"""
    return render_template('analysis.html')

@app.route('/watchlist')
def watchlist_page():
    """自选股页面"""
    return render_template('watchlist.html')

@app.route('/signals')
def signals_page():
    """信号监控页面"""
    return render_template('signals.html')

@app.route('/ai-analysis')
def ai_analysis_page():
    """AI分析页面"""
    return render_template('ai_analysis.html')

@app.route('/recommendations')
def recommendations_page():
    """推荐页面"""
    return render_template('recommendations.html')

@app.route('/intelligent-recommendations')
def intelligent_recommendations_page():
    """智能推荐页面"""
    return render_template('intelligent_recommendations.html')

@app.route('/api/health')
def health(): return jsonify({"status": "healthy", "timestamp": datetime.now().isoformat()})  # 健康检查

# 技术分析相关接口
@app.route('/api/technical/indicators/<ts_code>')
def get_technical_indicators(ts_code):
    """获取技术指标"""
    try:
        days = int(request.args.get('days', 60))
        
        # 获取历史数据
        end_date = datetime.now().strftime('%Y%m%d')
        start_date = (datetime.now() - timedelta(days=days)).strftime('%Y%m%d')
        df = db.query_stock_data(ts_code, start_date, end_date)
        
        if df.empty:
            return jsonify({"success": False, "error": f"{ts_code} 无数据"}), 404
            
        # 计算技术指标
        df = tech_indicator.calculate_all_indicators(df)
        signals = tech_indicator.generate_signals(df)
        
        # 获取最新数据
        latest = df.iloc[-1]
        latest_signals = signals.iloc[-1] if not signals.empty else {}
        
        # 安全转换函数，处理NaN值
        def safe_float(val, default=0):
            try:
                if pd.isna(val) or val is None:
                    return default
                return float(val)
            except (ValueError, TypeError):
                return default
        
        return jsonify({
            "success": True,
            "data": {
                "ts_code": ts_code,
                "trade_date": latest['trade_date'].strftime('%Y-%m-%d'),
                "close": safe_float(latest['close']),
                "ma5": safe_float(latest.get('ma5'), 0),
                "ma20": safe_float(latest.get('ma20'), 0),
                "ma60": safe_float(latest.get('ma60'), 0),
                "rsi14": safe_float(latest.get('rsi14'), 50),
                "macd": safe_float(latest.get('macd'), 0),
                "macd_signal": safe_float(latest.get('macd_signal'), 0),
                "k": safe_float(latest.get('k'), 50),
                "d": safe_float(latest.get('d'), 50),
                "strength_score": safe_float(latest.get('strength_score'), 50),
                "buy_signal": int(latest_signals.get('buy_signal', 0)) if latest_signals.get('buy_signal') is not None else 0,
                "sell_signal": int(latest_signals.get('sell_signal', 0)) if latest_signals.get('sell_signal') is not None else 0,
                "support": safe_float(latest.get('support'), 0),
                "resistance": safe_float(latest.get('resistance'), 0)
            },
            "timestamp": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"获取技术指标失败: {e}")
        return jsonify({"success": False, "error": str(e)}), 500

# 图表生成接口
@app.route('/api/chart/comprehensive/<ts_code>')
def get_comprehensive_chart(ts_code):
    """获取综合技术分析图表"""
    try:
        days = int(request.args.get('days', 120))
        
        # 获取股票数据并生成结构化图表数据
        from datetime import datetime, timedelta
        end_date = datetime.now().strftime('%Y%m%d')
        start_date = (datetime.now() - timedelta(days=days)).strftime('%Y%m%d')
        df = db.query_stock_data(ts_code, start_date, end_date)
        
        if df.empty:
            return jsonify({
                "success": False,
                "error": f"未找到股票 {ts_code} 的数据"
            })
        
        # 计算技术指标
        df_with_indicators = tech_indicator.calculate_all_indicators(df)
        
        # 转换日期格式
        if hasattr(df_with_indicators['trade_date'].iloc[0], 'strftime'):
            df_with_indicators['date_str'] = df_with_indicators['trade_date'].apply(lambda x: x.strftime('%Y-%m-%d'))
        else:
            df_with_indicators['date_str'] = df_with_indicators['trade_date'].astype(str)
        
        # 准备图表数据
        chart_data = {
            "dates": df_with_indicators['date_str'].tolist(),
            "kline": [[row['date_str'], float(row['open']), float(row['close']), 
                      float(row['low']), float(row['high'])] for _, row in df_with_indicators.iterrows()],
            "volume": [[row['date_str'], float(row.get('vol', 0))] for _, row in df_with_indicators.iterrows()],
            "close": [float(row['close']) for _, row in df_with_indicators.iterrows()],
            "ma5": [float(row.get('ma5', 0)) if pd.notna(row.get('ma5')) else None for _, row in df_with_indicators.iterrows()],
            "ma10": [float(row.get('ma10', 0)) if pd.notna(row.get('ma10')) else None for _, row in df_with_indicators.iterrows()],
            "ma20": [float(row.get('ma20', 0)) if pd.notna(row.get('ma20')) else None for _, row in df_with_indicators.iterrows()]
        }
        
        # 股票基本信息
        latest_data = df_with_indicators.iloc[-1]
        stock_info = {
            "code": ts_code,
            "latest_price": float(latest_data['close']),
            "change": float(latest_data.get('change', 0)),
            "pct_chg": float(latest_data.get('pct_chg', 0)),
            "volume": float(latest_data.get('vol', 0)),
            "pe": 15.2,  # 模拟数据
            "pb": 1.8,   # 模拟数据
            "data_points": len(df_with_indicators)
        }
        
        return jsonify({
            "success": True,
            "chart_data": chart_data,
            "stock_info": stock_info,
            "timestamp": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"获取图表数据失败: {e}")
        return jsonify({
            "success": False,
            "error": str(e)
        })

@app.route('/api/chart/realtime/<ts_code>')
def get_realtime_chart(ts_code):
    """获取实时信号图表"""
    try:
        result = chart_generator.create_realtime_signal_chart(ts_code)
        return jsonify(result)
        
    except Exception as e:
        logger.error(f"生成实时图表失败: {e}")
        return jsonify({"success": False, "error": str(e)}), 500

# 实时信号接口
@app.route('/api/signals/realtime', methods=['POST'])
def get_realtime_signals():
    """获取实时交易信号"""
    try:
        data = request.get_json()
        stock_codes = data.get('stock_codes', [])
        
        if not stock_codes:
            return jsonify({"success": False, "error": "未提供股票代码"}), 400
            
        signals_data = []
        
        for code in stock_codes[:10]:  # 最多10只股票
            # 获取最新数据和信号
            end_date = datetime.now().strftime('%Y%m%d')
            start_date = (datetime.now() - timedelta(days=30)).strftime('%Y%m%d')
            df = db.query_stock_data(code, start_date, end_date)
            
            if not df.empty:
                # 计算指标和信号
                df = tech_indicator.calculate_all_indicators(df)
                signals = tech_indicator.generate_signals(df)
                
                latest = df.iloc[-1]
                latest_signals = signals.iloc[-1] if not signals.empty else {}
                
                # 获取股票名称
                with db.engine.connect() as conn:
                    sql = "SELECT name FROM stock_basic WHERE ts_code = :ts_code"
                    result = conn.execute(text(sql), {'ts_code': code})
                    stock_name = result.scalar() or code
                
                # 生成信号摘要
                current_signals = {
                    'buy_signal': int(latest_signals.get('buy_signal', 0)),
                    'sell_signal': int(latest_signals.get('sell_signal', 0)),
                    'ma_golden_cross': int(latest_signals.get('ma_golden_cross', 0)),
                    'macd_golden_cross': int(latest_signals.get('macd_golden_cross', 0)),
                    'strength_score': float(latest.get('strength_score', 50)),
                    'rsi': float(latest.get('rsi14', 50)),
                    'current_price': float(latest['close'])
                }
                
                signal_summary = chart_generator._generate_signal_summary(current_signals)
                
                signals_data.append({
                    "stock_code": code,
                    "stock_name": stock_name,
                    "current_price": current_signals['current_price'],
                    "strength_score": current_signals['strength_score'],
                    "buy_signal": current_signals['buy_signal'],
                    "sell_signal": current_signals['sell_signal'],
                    "signal_summary": signal_summary,
                    "rsi": current_signals['rsi']
                })
                
        return jsonify({
            "success": True,
            "data": signals_data,
            "count": len(signals_data),
            "timestamp": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"获取实时信号失败: {e}")
        return jsonify({"success": False, "error": str(e)}), 500

# LLM智能分析接口
@app.route('/api/llm/intelligent', methods=['POST'])
def llm_intelligent_analysis():
    """LLM智能分析"""
    try:
        data = request.get_json()
        question = data.get('question', '')
        
        if not question:
            return jsonify({"success": False, "error": "问题不能为空"}), 400
            
        result = stock_analyzer.intelligent_qa(question)
        return jsonify(result)
        
    except Exception as e:
        logger.error(f"LLM分析失败: {e}")
        return jsonify({"success": False, "error": str(e)}), 500

@app.route('/api/llm/stock/<ts_code>')
def llm_stock_analysis(ts_code):
    """单只股票LLM分析"""
    try:
        query = request.args.get('query', '')
        result = stock_analyzer.analyze_single_stock(ts_code, query)
        return jsonify(result)
        
    except Exception as e:
        logger.error(f"股票分析失败: {e}")
        return jsonify({"success": False, "error": str(e)}), 500

# 投资组合监控接口
@app.route('/api/portfolio/monitor', methods=['POST'])
def portfolio_monitor():
    """投资组合监控"""
    try:
        data = request.get_json()
        stock_codes = data.get('stock_codes', [])
        
        if not stock_codes:
            return jsonify({"success": False, "error": "未提供股票代码"}), 400
            
        portfolio_data = []
        
        for code in stock_codes[:10]:  # 最多10只股票
            # 获取最新数据
            end_date = datetime.now().strftime('%Y%m%d')
            start_date = (datetime.now() - timedelta(days=30)).strftime('%Y%m%d')
            df = db.query_stock_data(code, start_date, end_date)
            
            if not df.empty:
                # 计算指标
                df = tech_indicator.calculate_all_indicators(df)
                signals = tech_indicator.generate_signals(df)
                
                latest = df.iloc[-1]
                latest_signals = signals.iloc[-1] if not signals.empty else {}
                
                # 获取股票名称
                with db.engine.connect() as conn:
                    sql = "SELECT name FROM stock_basic WHERE ts_code = :ts_code"
                    result = conn.execute(text(sql), {'ts_code': code})
                    stock_name = result.scalar() or code
                
                portfolio_data.append({
                    'stock_code': code,
                    'stock_name': stock_name,
                    'current_price': float(latest['close']),
                    'change_pct': float(latest.get('change_pct', 0)),
                    'strength_score': float(latest.get('strength_score', 50)),
                    'buy_signal': int(latest_signals.get('buy_signal', 0)),
                    'sell_signal': int(latest_signals.get('sell_signal', 0)),
                    'rsi': float(latest.get('rsi14', 50))
                })
                
        # 计算组合统计
        if portfolio_data:
            summary = {
                'total_stocks': len(portfolio_data),
                'avg_strength': sum(d['strength_score'] for d in portfolio_data) / len(portfolio_data),
                'avg_change': sum(d['change_pct'] for d in portfolio_data) / len(portfolio_data),
                'buy_signals': sum(1 for d in portfolio_data if d['buy_signal']),
                'sell_signals': sum(1 for d in portfolio_data if d['sell_signal'])
            }
            
            return jsonify({
                "success": True,
                "data": {
                    "stocks": portfolio_data,
                    "summary": summary
                },
                "timestamp": datetime.now().isoformat()
            })
        else:
            return jsonify({"success": False, "error": "投资组合无有效数据"}), 400
            
    except Exception as e:
        logger.error(f"投资组合监控失败: {e}")
        return jsonify({"success": False, "error": str(e)}), 500

@app.route('/api/portfolio/dashboard', methods=['POST'])
def portfolio_dashboard():
    """投资组合仪表板"""
    try:
        data = request.get_json()
        stock_codes = data.get('stock_codes', [])
        
        result = chart_generator.create_portfolio_dashboard(stock_codes)
        return jsonify(result)
        
    except Exception as e:
        logger.error(f"组合仪表板失败: {e}")
        return jsonify({"success": False, "error": str(e)}), 500

# 多策略推荐接口
@app.route('/api/recommendations/generate', methods=['POST'])
@require_permission('recommendations:generate')
def generate_multi_strategy_recommendations():
    """生成多策略推荐（需要权限）"""
    try:
        from utils.multi_strategy_recommender import multi_strategy_recommender
        
        data = request.get_json()
        strategy = data.get('strategy', 'multi_strategy')
        limit = int(data.get('limit', 20))
        min_score = float(data.get('min_score', 60))
        
        # 根据策略类型生成推荐
        if strategy == 'multi_strategy':
            # 综合推荐
            result = multi_strategy_recommender.generate_enhanced_recommendations(
                strategy_weights=None,
                limit=limit,
                min_score=min_score
            )
            recommendations = result.get('recommendations', [])
        else:
            # 单一策略推荐
            recommendations = multi_strategy_recommender.get_strategy_recommendations(
                strategy_name=strategy,
                limit=limit
            )
        
        return jsonify({
            "success": True,
            "data": recommendations,
            "count": len(recommendations),
            "strategy": strategy,
            "timestamp": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"生成多策略推荐失败: {e}")
        return jsonify({"success": False, "error": str(e)}), 500

@app.route('/api/recommendations/performance')
def get_recommendation_performance():
    """获取推荐表现统计"""
    try:
        from utils.multi_strategy_recommender import multi_strategy_recommender
        
        days_back = int(request.args.get('days', 30))
        performance = multi_strategy_recommender.get_recommendation_performance(days_back)
        
        return jsonify({
            "success": True,
            "data": performance,
            "timestamp": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"获取推荐表现失败: {e}")
        return jsonify({"success": False, "error": str(e)}), 500

@app.route('/api/system/health')
def get_system_health():
    """获取系统健康状态"""
    try:
        from utils.data_health_checker import data_health_checker
        from utils.redis_cache_manager import cache_manager
        from utils.system_monitor import system_monitor
        
        # 数据健康检查
        data_health = data_health_checker.check_system_health()
        
        # 缓存健康检查
        cache_health = cache_manager.health_check()
        
        # 系统监控状态
        system_status = system_monitor.get_current_status()
        
        return jsonify({
            "success": True,
            "data": {
                "data_health": data_health,
                "cache_health": cache_health,
                "system_status": system_status,
                "overall_status": "healthy" if all([
                    data_health.get('overall_status') == 'HEALTHY',
                    cache_health.get('status') == 'healthy',
                    system_status.get('status') in ['healthy', 'warning']
                ]) else "warning"
            },
            "timestamp": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"获取系统健康状态失败: {e}")
        return jsonify({"success": False, "error": str(e)}), 500

# 保留原有接口
@app.route('/api/recommend')
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

@app.route('/api/recommend/generate')
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

@app.route('/api/stats')
def get_stats():
    try:
        with db.engine.connect() as conn:
            stats = {}
            stats['stock_count'] = conn.execute(text("SELECT COUNT(*) FROM stock_basic")).scalar()
            stats['recommend_count'] = conn.execute(text("SELECT COUNT(*) FROM recommend_result WHERE recommend_date = CURDATE()")).scalar()
            stats['last_update'] = conn.execute(text("SELECT MAX(created_at) FROM system_log")).scalar()
            
            # 检查LLM可用性
            try:
                from llm.stock_analyzer import stock_analyzer
                stats['llm_available'] = stock_analyzer.health_check() if hasattr(stock_analyzer, 'health_check') else True
            except:
                stats['llm_available'] = False
            
        return jsonify({
            "success": True,
            "data": stats,
            "timestamp": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"获取统计信息失败: {e}")
        return jsonify({"success": False, "error": str(e)}), 500

# 股票搜索接口 - 改善用户体验
@app.route('/api/stocks/search')
def search_stocks():
    """股票代码和名称搜索"""
    try:
        query = request.args.get('q', '').strip()
        limit = int(request.args.get('limit', 10))
        
        if not query:
            return jsonify({"success": False, "error": "搜索关键词不能为空"}), 400
        
        if len(query) < 2:
            return jsonify({"success": False, "error": "搜索关键词至少2个字符"}), 400
        
        with db.engine.connect() as conn:
            # 搜索股票代码或名称
            sql = """
                SELECT ts_code, name, industry, market
                FROM stock_basic
                WHERE (
                    ts_code LIKE :query_code OR
                    name LIKE :query_name OR
                    ts_code LIKE :query_simple
                )
                ORDER BY
                    CASE
                        WHEN ts_code = :exact_code THEN 1
                        WHEN ts_code LIKE :start_code THEN 2
                        WHEN name LIKE :start_name THEN 3
                        ELSE 4
                    END,
                    ts_code
                LIMIT :limit
            """
            
            result = conn.execute(text(sql), {
                'query_code': f'%{query}%',
                'query_name': f'%{query}%',
                'query_simple': f'{query}%',
                'exact_code': query,
                'start_code': f'{query}%',
                'start_name': f'{query}%',
                'limit': limit
            })
            
            stocks = []
            for row in result:
                stocks.append({
                    'ts_code': row.ts_code,
                    'name': row.name,
                    'industry': row.industry or '未知',
                    'market': row.market or '未知',
                    'display_name': f"{row.name}({row.ts_code})"
                })
        
        return jsonify({
            "success": True,
            "data": stocks,
            "count": len(stocks),
            "query": query,
            "timestamp": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"股票搜索失败: {e}")
        return jsonify({"success": False, "error": str(e)}), 500

@app.route('/api/stocks/popular')
def get_popular_stocks():
    """获取热门股票建议"""
    try:
        category = request.args.get('category', 'all')  # all, bank, tech, consumer
        limit = int(request.args.get('limit', 20))
        
        with db.engine.connect() as conn:
            # 根据分类获取热门股票
            if category == 'bank':
                sql = """
                    SELECT ts_code, name, industry, market
                    FROM stock_basic
                    WHERE (industry LIKE '%银行%' OR industry LIKE '%金融%')
                    ORDER BY ts_code
                    LIMIT :limit
                """
            elif category == 'tech':
                sql = """
                    SELECT ts_code, name, industry, market
                    FROM stock_basic
                    WHERE (industry LIKE '%软件%' OR industry LIKE '%互联网%' OR industry LIKE '%电子%' OR industry LIKE '%通信%')
                    ORDER BY ts_code
                    LIMIT :limit
                """
            elif category == 'consumer':
                sql = """
                    SELECT ts_code, name, industry, market
                    FROM stock_basic
                    WHERE (industry LIKE '%消费%' OR industry LIKE '%食品%' OR industry LIKE '%饮料%' OR industry LIKE '%家电%')
                    ORDER BY ts_code
                    LIMIT :limit
                """
            else:
                # 获取一些知名的大盘股
                popular_codes = [
                    '000001.SZ', '000002.SZ', '000858.SZ', '000725.SZ',  # 平安银行、万科、五粮液、京东方
                    '600000.SH', '600036.SH', '600519.SH', '600887.SH',  # 浦发银行、招商银行、贵州茅台、伊利股份
                    '002415.SZ', '002594.SZ', '300015.SZ', '300059.SZ'   # 海康威视、比亚迪、爱尔眼科、东方财富
                ]
                
                placeholders = ','.join([f':code{i}' for i in range(len(popular_codes))])
                sql = f"""
                    SELECT ts_code, name, industry, market
                    FROM stock_basic
                    WHERE ts_code IN ({placeholders})
                    ORDER BY ts_code
                """
                
                params = {f'code{i}': code for i, code in enumerate(popular_codes)}
                result = conn.execute(text(sql), params)
            
            if category != 'all':
                result = conn.execute(text(sql), {'limit': limit})
            
            stocks = []
            for row in result:
                stocks.append({
                    'ts_code': row.ts_code,
                    'name': row.name,
                    'industry': row.industry or '未知',
                    'market': row.market or '未知',
                    'display_name': f"{row.name}({row.ts_code})"
                })
        
        return jsonify({
            "success": True,
            "data": stocks,
            "count": len(stocks),
            "category": category,
            "timestamp": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"获取热门股票失败: {e}")
        return jsonify({"success": False, "error": str(e)}), 500

@app.errorhandler(404)
def not_found(error): return jsonify({"success": False, "error": "接口不存在"}), 404

@app.errorhandler(500)
def internal_error(error): return jsonify({"success": False, "error": "服务器内部错误"}), 500

# 前端页面路由
@app.route('/frontend/realtime_dashboard.html')
def realtime_dashboard():
    """实时分析前端页面"""
    try:
        from pathlib import Path
        frontend_path = Path(__file__).parent.parent / "frontend" / "realtime_dashboard.html"
        if frontend_path.exists():
            return send_file(str(frontend_path))
        else:
            return "前端页面文件不存在", 404
    except Exception as e:
        logger.error(f"加载前端页面失败: {e}")
        return f"加载前端页面失败: {e}", 500

@app.route('/frontend/recommendation_dashboard.html')
def recommendation_dashboard():
    """推荐系统前端页面"""
    try:
        from pathlib import Path
        frontend_path = Path(__file__).parent.parent / "frontend" / "recommendation_dashboard.html"
        if frontend_path.exists():
            return send_file(str(frontend_path))
        else:
            return "推荐系统页面文件不存在", 404
    except Exception as e:
        logger.error(f"加载推荐系统页面失败: {e}")
        return f"加载推荐系统页面失败: {e}", 500

# 新增API端点支持现代前端
@app.route('/api/stocks/hot')
def get_hot_stocks():
    """获取热门股票"""
    try:
        limit = int(request.args.get('limit', 10))
        
        # 模拟热门股票数据（实际应从数据库获取）
        hot_stocks = [
            {"ts_code": "000001.SZ", "name": "平安银行", "close": 12.35, "change": 0.15, "pct_chg": 1.23, "vol": 12500000},
            {"ts_code": "000002.SZ", "name": "万科A", "close": 18.56, "change": -0.22, "pct_chg": -1.17, "vol": 8900000},
            {"ts_code": "600036.SH", "name": "招商银行", "close": 45.78, "change": 0.89, "pct_chg": 1.98, "vol": 15600000}
        ]
        
        return jsonify({
            "success": True,
            "data": hot_stocks[:limit],
            "timestamp": datetime.now().isoformat()
        })
    except Exception as e:
        return jsonify({"success": False, "error": str(e)})

@app.route('/api/indexes/major')
def get_major_indexes():
    """获取主要指数"""
    try:
        # 模拟指数数据（实际应从数据源获取）
        indexes = [
            {"code": "000001.SH", "name": "上证指数", "close": 3234.56, "pct_chg": 0.87},
            {"code": "399001.SZ", "name": "深证成指", "close": 12456.78, "pct_chg": -0.34},
            {"code": "399006.SZ", "name": "创业板指", "close": 2876.45, "pct_chg": 1.23}
        ]
        
        return jsonify({
            "success": True,
            "data": indexes,
            "timestamp": datetime.now().isoformat()
        })
    except Exception as e:
        return jsonify({"success": False, "error": str(e)})

@app.route('/api/system/status')
def get_system_status():
    """获取系统状态"""
    try:
        # 检测WebSocket服务器是否运行
        import socket
        ws_connected = False
        try:
            sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            sock.settimeout(1)
            result = sock.connect_ex(('localhost', 8765))
            ws_connected = result == 0
            sock.close()
        except:
            ws_connected = False
        
        status = {
            "last_update": datetime.now().strftime("%H:%M:%S"),
            "monitored_stocks": 25,
            "today_signals": 8,
            "ws_connected": ws_connected
        }
        
        return jsonify({
            "success": True,
            "data": status,
            "timestamp": datetime.now().isoformat()
        })
    except Exception as e:
        return jsonify({"success": False, "error": str(e)})

@app.route('/api/indicators/<ts_code>')
def get_indicators_summary(ts_code):
    """获取技术指标摘要"""
    try:
        days = int(request.args.get('days', 120))
        
        # 模拟指标数据（实际应计算真实指标）
        trend_indicators = [
            {"name": "MA5", "description": "5日均线", "value": 12.45, "signal": "buy", "signal_text": "多头"},
            {"name": "MA20", "description": "20日均线", "value": 12.12, "signal": "buy", "signal_text": "多头"},
            {"name": "MACD", "description": "指数平滑移动平均", "value": 0.15, "signal": "buy", "signal_text": "金叉"}
        ]
        
        momentum_indicators = [
            {"name": "RSI", "description": "相对强弱指标", "value": 65.4, "signal": "hold", "signal_text": "中性"},
            {"name": "KDJ_K", "description": "随机指标K", "value": 72.8, "signal": "sell", "signal_text": "超买"},
            {"name": "CCI", "description": "顺势指标", "value": 125.6, "signal": "sell", "signal_text": "超买"}
        ]
        
        return jsonify({
            "success": True,
            "data": {
                "trend": trend_indicators,
                "momentum": momentum_indicators
            },
            "timestamp": datetime.now().isoformat()
        })
    except Exception as e:
        return jsonify({"success": False, "error": str(e)})

@app.route('/api/ai/analysis/<ts_code>')
def get_ai_analysis(ts_code):
    """获取AI分析结果"""
    try:
        # 模拟AI分析结果（实际应调用LLM模块）
        analysis = {
            "overall_score": 7.5,
            "recommendation": "buy",
            "recommendation_text": "建议买入",
            "risk_level": "medium",
            "risk_level_text": "中等风险",
            "analysis_text": "基于技术分析和基面数据，该股票目前处于上升趋势中，多项技术指标显示买入信号。建议适量配置，注意风险控制。"
        }
        
        return jsonify({
            "success": True,
            "data": analysis,
            "timestamp": datetime.now().isoformat()
        })
    except Exception as e:
        return jsonify({"success": False, "error": str(e)})

@app.route('/api/stocks/money-flow/<ts_code>')
def get_stock_money_flow(ts_code):
    """获取个股资金流向数据"""
    try:
        # 获取最近30天的资金流向数据
        money_flow = db_manager.fetch_data(f"""
            SELECT trade_date, 
                   buy_sm_amount, buy_md_amount, buy_lg_amount, buy_elg_amount,
                   sell_sm_amount, sell_md_amount, sell_lg_amount, sell_elg_amount
            FROM t_money_flow 
            WHERE ts_code = '{ts_code}'
            ORDER BY trade_date DESC 
            LIMIT 30
        """)
        
        if money_flow.empty:
            # 返回模拟数据
            return jsonify({
                "success": True,
                "data": {
                    "main_inflow": 12500000,
                    "retail_outflow": -5600000,
                    "net_inflow": 6900000,
                    "inflow_ratio": 68.5,
                    "recent_trend": "主力资金净流入",
                    "flow_details": [
                        {"type": "超大单", "amount": 8900000, "ratio": 45.2},
                        {"type": "大单", "amount": 3600000, "ratio": 18.3},
                        {"type": "中单", "amount": -2100000, "ratio": -10.7},
                        {"type": "小单", "amount": -3500000, "ratio": -17.8}
                    ]
                },
                "timestamp": datetime.now().isoformat()
            })
        
        # 计算最新资金流向
        latest = money_flow.iloc[0]
        main_inflow = (latest.get('buy_lg_amount', 0) or 0) + (latest.get('buy_elg_amount', 0) or 0)
        main_outflow = (latest.get('sell_lg_amount', 0) or 0) + (latest.get('sell_elg_amount', 0) or 0)
        retail_inflow = (latest.get('buy_sm_amount', 0) or 0) + (latest.get('buy_md_amount', 0) or 0)
        retail_outflow = (latest.get('sell_sm_amount', 0) or 0) + (latest.get('sell_md_amount', 0) or 0)
        
        net_main = main_inflow - main_outflow
        net_retail = retail_inflow - retail_outflow
        total_amount = main_inflow + main_outflow + retail_inflow + retail_outflow
        
        flow_details = [
            {"type": "超大单", "amount": int(latest.get('buy_elg_amount', 0) or 0 - latest.get('sell_elg_amount', 0) or 0), "ratio": 0},
            {"type": "大单", "amount": int(latest.get('buy_lg_amount', 0) or 0 - latest.get('sell_lg_amount', 0) or 0), "ratio": 0},
            {"type": "中单", "amount": int(latest.get('buy_md_amount', 0) or 0 - latest.get('sell_md_amount', 0) or 0), "ratio": 0},
            {"type": "小单", "amount": int(latest.get('buy_sm_amount', 0) or 0 - latest.get('sell_sm_amount', 0) or 0), "ratio": 0}
        ]
        
        # 计算比例
        for detail in flow_details:
            if total_amount > 0:
                detail["ratio"] = round((abs(detail["amount"]) / total_amount) * 100, 1)
        
        return jsonify({
            "success": True,
            "data": {
                "main_inflow": int(net_main),
                "retail_outflow": int(net_retail),
                "net_inflow": int(net_main + net_retail),
                "inflow_ratio": round((net_main / (net_main + abs(net_retail))) * 100, 1) if (net_main + abs(net_retail)) > 0 else 0,
                "recent_trend": "主力资金净流入" if net_main > 0 else "主力资金净流出",
                "flow_details": flow_details
            },
            "timestamp": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"获取资金流向失败: {e}")
        return jsonify({"success": False, "error": str(e)})

@app.route('/api/stocks/dragon-tiger/<ts_code>')
def get_stock_dragon_tiger(ts_code):
    """获取个股龙虎榜数据"""
    try:
        # 获取该股票最近的龙虎榜数据
        dragon_tiger = db_manager.fetch_data(f"""
            SELECT trade_date, close, pct_chg, amount, l_buy, l_sell, reason,
                   buy1, buy2, buy3, buy4, buy5,
                   sell1, sell2, sell3, sell4, sell5
            FROM t_dragon_tiger_list 
            WHERE ts_code = '{ts_code}'
            ORDER BY trade_date DESC 
            LIMIT 10
        """)
        
        if dragon_tiger.empty:
            # 返回模拟数据
            return jsonify({
                "success": True,
                "data": {
                    "is_on_list": False,
                    "latest_date": None,
                    "reason": "该股票近期未上龙虎榜",
                    "buy_list": [],
                    "sell_list": [],
                    "summary": {
                        "total_buy": 0,
                        "total_sell": 0,
                        "net_buy": 0
                    }
                },
                "timestamp": datetime.now().isoformat()
            })
        
        latest = dragon_tiger.iloc[0]
        
        # 解析买卖榜数据
        buy_list = []
        sell_list = []
        
        for i in range(1, 6):
            buy_col = f'buy{i}'
            sell_col = f'sell{i}'
            
            if latest.get(buy_col):
                buy_list.append({
                    "rank": i,
                    "name": f"买入席位{i}",
                    "amount": int(latest.get(buy_col, 0) or 0)
                })
            
            if latest.get(sell_col):
                sell_list.append({
                    "rank": i,
                    "name": f"卖出席位{i}",
                    "amount": int(latest.get(sell_col, 0) or 0)
                })
        
        total_buy = int(latest.get('l_buy', 0) or 0)
        total_sell = int(latest.get('l_sell', 0) or 0)
        
        return jsonify({
            "success": True,
            "data": {
                "is_on_list": True,
                "latest_date": latest['trade_date'].strftime('%Y-%m-%d') if hasattr(latest['trade_date'], 'strftime') else str(latest['trade_date']),
                "reason": latest.get('reason', '数据异常'),
                "close_price": float(latest.get('close', 0) or 0),
                "pct_change": float(latest.get('pct_chg', 0) or 0),
                "buy_list": buy_list,
                "sell_list": sell_list,
                "summary": {
                    "total_buy": total_buy,
                    "total_sell": total_sell,
                    "net_buy": total_buy - total_sell
                }
            },
            "timestamp": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"获取龙虎榜数据失败: {e}")
        return jsonify({"success": False, "error": str(e)})

@app.route('/api/stocks/quote/<ts_code>')
def get_stock_quote(ts_code):
    """获取股票实时报价"""
    try:
        # 从数据库获取最新数据
        df = db.query_stock_data(ts_code, '', '', limit=1)
        if df.empty:
            return jsonify({"success": False, "error": "未找到股票数据"})
        
        latest = df.iloc[0]
        quote_data = {
            "ts_code": ts_code,
            "close": float(latest['close']),
            "change": float(latest.get('change', 0)),
            "pct_chg": float(latest.get('pct_chg', 0)),
            "vol": float(latest.get('vol', 0)),
            "amount": float(latest.get('amount', 0)),
            "trade_date": latest['trade_date'].strftime('%Y-%m-%d') if hasattr(latest['trade_date'], 'strftime') else str(latest['trade_date'])
        }
        
        return jsonify({
            "success": True,
            "data": quote_data,
            "timestamp": datetime.now().isoformat()
        })
    except Exception as e:
        return jsonify({"success": False, "error": str(e)})

# ============ 市场数据仪表板路由 ============

@app.route('/market-dashboard')
def market_dashboard():
    """市场数据仪表板主页"""
    return render_template('market_dashboard.html')

@app.route('/api/market/stats')
def api_market_stats():
    """获取市场统计数据"""
    try:
        # 获取活跃股票数量
        active_stocks = db_manager.fetch_data("""
            SELECT COUNT(DISTINCT ts_code) as count 
            FROM technical_indicators
        """).iloc[0]['count']
        
        # 获取技术指标覆盖
        indicator_coverage = db_manager.fetch_data("""
            SELECT COUNT(*) as count 
            FROM technical_indicators 
            WHERE ma5 IS NOT NULL
        """).iloc[0]['count']
        
        # 获取板块数量
        sector_count = db_manager.fetch_data("""
            SELECT COUNT(*) as count FROM t_concept
        """).iloc[0]['count']
        
        # 计算资金净流入（模拟）
        net_money_flow_query = db_manager.fetch_data("""
            SELECT SUM(amount) as total 
            FROM stock_daily_202507 
            WHERE trade_date = (SELECT MAX(trade_date) FROM stock_daily_202507)
        """)
        net_money_flow = int((net_money_flow_query.iloc[0]['total'] or 0) / 100)
        
        return jsonify({
            'active_stocks': f"{active_stocks:,}",
            'indicator_coverage': f"{indicator_coverage:,}",
            'sector_count': f"{sector_count:,}",
            'net_money_flow': net_money_flow
        })
        
    except Exception as e:
        logger.error(f"获取市场统计失败: {e}")
        return jsonify({'error': str(e)}), 500

@app.route('/api/market/hot-sectors')
def api_hot_sectors():
    """获取热门板块数据"""
    try:
        # 获取概念板块及其成分股 - 修复字段名
        sectors = db_manager.fetch_data("""
            SELECT c.name, COUNT(cd.ts_code) as stock_count
            FROM t_concept c
            LEFT JOIN t_concept_detail cd ON c.name = cd.concept_name
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
        logger.error(f"获取热门板块失败: {e}")
        # 返回模拟数据作为后备
        return jsonify([
            { 'name': '人工智能', 'change': 5.67, 'stock_count': 128 },
            { 'name': '新能源汽车', 'change': 3.24, 'stock_count': 89 },
            { 'name': '半导体', 'change': -2.11, 'stock_count': 156 },
            { 'name': '医疗器械', 'change': 1.89, 'stock_count': 93 },
            { 'name': '5G通信', 'change': -0.78, 'stock_count': 67 }
        ])

@app.route('/api/market/dragon-tiger')
def api_dragon_tiger():
    """获取龙虎榜数据"""
    try:
        # 获取龙虎榜数据 - 修复字段名
        dragon_tiger = db_manager.fetch_data("""
            SELECT dt.ts_code, dt.name, dt.close, dt.pct_chg, dt.amount,
                   dt.l_buy, dt.l_sell, dt.reason
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
                'buy_amount': int(row['l_buy'] or 0),
                'sell_amount': int(row['l_sell'] or 0),
                'reason': row['reason'] or '数据异常'
            })
        
        return jsonify(dragon_tiger_list)
        
    except Exception as e:
        logger.error(f"获取龙虎榜失败: {e}")
        # 返回模拟数据作为后备
        return jsonify([
            {
                'ts_code': '000001.SZ',
                'name': '平安银行',
                'change': 4.56,
                'buy_amount': 890000000,
                'sell_amount': 234000000,
                'reason': '日涨幅偏离值达7%'
            }
        ])

@app.route('/api/market/money-flow')
def api_money_flow():
    """获取资金流向数据"""
    try:
        # 获取资金流数据 - 修复字段名
        money_flow = db_manager.fetch_data("""
            SELECT 
                SUM(buy_lg_amount + buy_elg_amount) as total_buy, 
                SUM(sell_lg_amount + sell_elg_amount) as total_sell
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
        logger.error(f"获取资金流向失败: {e}")
        # 返回模拟数据作为后备
        return jsonify({
            'main_inflow': 2340000000,
            'retail_outflow': -890000000
        })

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
        stocks = db_manager.fetch_data(f"""
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
        logger.error(f"获取股票数据失败: {e}")
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
        
        # 获取指标分布数据 - 修复中文字符问题
        heatmap_data = db_manager.fetch_data(f"""
            SELECT 
                CASE 
                    WHEN b.industry LIKE '%科技%' OR b.industry LIKE '%软件%' THEN '科技'
                    WHEN b.industry LIKE '%银行%' OR b.industry LIKE '%保险%' THEN '金融'  
                    WHEN b.industry LIKE '%医药%' OR b.industry LIKE '%生物%' THEN '医药'
                    WHEN b.industry LIKE '%食品%' OR b.industry LIKE '%饮料%' THEN '消费'
                    WHEN b.industry LIKE '%机械%' OR b.industry LIKE '%制造%' THEN '工业'
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
        logger.error(f"获取热力图数据失败: {e}")
        # 返回模拟数据作为后备
        heatmap_result = []
        for i in range(6):
            for j in range(5):
                count = np.random.randint(0, 100)
                heatmap_result.append([i, j, count, count])
        return jsonify(heatmap_result)

@app.route('/api/ai/health')
def ai_health_check():
    """AI系统健康检查"""
    try:
        from ai.health_check import get_ai_system_health
        health_status = get_ai_system_health()
        
        return jsonify({
            "success": True,
            "data": health_status,
            "timestamp": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"AI健康检查失败: {e}")
        return jsonify({
            "success": False,
            "error": str(e),
            "fallback_info": {
                "message": "AI健康检查模块不可用",
                "suggestions": [
                    "检查ai/health_check.py文件是否存在",
                    "确保所有依赖包已正确安装",
                    "运行: pip install -r requirements_ai.txt"
                ]
            }
        }), 500

# ============ AI机器学习模型接口 ============

@app.route('/api/ai/models/train', methods=['POST'])
@require_admin
def train_ai_models():
    """训练AI模型（管理员专用）"""
    try:
        data = request.get_json() or {}
        days_back = int(data.get('days_back', 180))
        stock_limit = int(data.get('stock_limit', 500))
        
        # 导入AI训练模块
        try:
            from ai.ml_trainer import ml_trainer
        except ImportError as e:
            return jsonify({
                "success": False,
                "error": f"AI训练模块不可用: {e}"
            })
        
        # 异步运行训练
        import asyncio
        
        async def run_training():
            try:
                # 准备训练数据
                training_data, X, y = ml_trainer.prepare_training_data(days_back, stock_limit)
                
                # 训练所有模型
                performance = ml_trainer.train_all_models(X, y)
                
                # 保存模型
                save_success = ml_trainer.save_models()
                
                return {
                    "training_samples": X.shape[0],
                    "feature_count": X.shape[1],
                    "models_trained": len(performance),
                    "performance": performance,
                    "models_saved": save_success
                }
            except Exception as e:
                raise e
        
        # 在新的事件循环中运行
        try:
            loop = asyncio.new_event_loop()
            asyncio.set_event_loop(loop)
            result = loop.run_until_complete(run_training())
            loop.close()
        except Exception as e:
            logger.error(f"AI模型训练失败: {e}")
            return jsonify({
                "success": False,
                "error": f"模型训练失败: {str(e)}"
            })
        
        return jsonify({
            "success": True,
            "data": result,
            "timestamp": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"AI模型训练接口失败: {e}")
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500

@app.route('/api/ai/models/predict/<ts_code>')
def predict_stock_ai(ts_code):
    """AI预测股票"""
    try:
        model_name = request.args.get('model', None)
        
        # 导入AI训练模块
        try:
            from ai.ml_trainer import ml_trainer
        except ImportError:
            return jsonify({
                "success": False,
                "error": "AI预测模块不可用"
            })
        
        # 尝试加载模型
        if not ml_trainer.models:
            ml_trainer.load_models()
        
        if not ml_trainer.models:
            return jsonify({
                "success": False,
                "error": "没有可用的训练模型，请先训练模型"
            })
        
        # 预测
        prediction = ml_trainer.predict_single_stock(ts_code, model_name)
        
        return jsonify({
            "success": True,
            "data": prediction,
            "timestamp": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"AI预测失败: {e}")
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500

@app.route('/api/ai/models/performance')
def get_model_performance():
    """获取模型性能"""
    try:
        # 首先检查AI系统健康状态
        try:
            from ai.health_check import get_ai_system_health
            health_status = get_ai_system_health()
            
            if health_status['overall_status'] == 'error':
                return jsonify({
                    "success": False,
                    "error": "AI系统不可用",
                    "health_status": health_status,
                    "recommendations": health_status['recommendations']
                })
        except ImportError:
            return jsonify({
                "success": False,
                "error": "AI健康检查模块不可用"
            })
        
        # 导入AI训练模块
        try:
            from ai.ml_trainer import ml_trainer
        except ImportError as e:
            return jsonify({
                "success": False,
                "error": f"AI训练模块不可用: {e}",
                "health_status": health_status if 'health_status' in locals() else None
            })
        
        # 尝试加载模型
        if not ml_trainer.model_performance:
            ml_trainer.load_models()
        
        return jsonify({
            "success": True,
            "data": {
                "performance": ml_trainer.model_performance,
                "available_models": list(ml_trainer.available_models.keys()),
                "trained_models": list(ml_trainer.models.keys()),
                "feature_count": len(ml_trainer.feature_names),
                "health_status": health_status if 'health_status' in locals() else None
            },
            "timestamp": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"获取模型性能失败: {e}")
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500

@app.route('/api/ai/models/features/<model_name>')
def get_feature_importance(model_name):
    """获取特征重要性"""
    try:
        # 导入AI训练模块
        try:
            from ai.ml_trainer import ml_trainer
        except ImportError:
            return jsonify({
                "success": False,
                "error": "AI模块不可用"
            })
        
        # 尝试加载模型
        if not ml_trainer.models:
            ml_trainer.load_models()
        
        if model_name not in ml_trainer.models:
            return jsonify({
                "success": False,
                "error": f"模型{model_name}不存在"
            })
        
        # 获取特征重要性
        feature_importance = ml_trainer.get_feature_importance(model_name)
        
        return jsonify({
            "success": True,
            "data": {
                "model_name": model_name,
                "feature_importance": feature_importance
            },
            "timestamp": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"获取特征重要性失败: {e}")
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500

@app.route('/api/ai/batch-predict', methods=['POST'])
def batch_predict_stocks():
    """批量预测股票"""
    try:
        data = request.get_json()
        stock_codes = data.get('stock_codes', [])
        model_name = data.get('model_name', None)
        
        if not stock_codes:
            return jsonify({
                "success": False,
                "error": "未提供股票代码列表"
            })
        
        # 导入AI训练模块
        try:
            from ai.ml_trainer import ml_trainer
        except ImportError:
            return jsonify({
                "success": False,
                "error": "AI预测模块不可用"
            })
        
        # 尝试加载模型
        if not ml_trainer.models:
            ml_trainer.load_models()
        
        if not ml_trainer.models:
            return jsonify({
                "success": False,
                "error": "没有可用的训练模型"
            })
        
        # 批量预测
        predictions = []
        for ts_code in stock_codes[:50]:  # 限制最多50只股票
            try:
                prediction = ml_trainer.predict_single_stock(ts_code, model_name)
                predictions.append(prediction)
            except Exception as e:
                logger.error(f"预测股票{ts_code}失败: {e}")
                continue
        
        return jsonify({
            "success": True,
            "data": {
                "predictions": predictions,
                "total_count": len(predictions),
                "requested_count": len(stock_codes)
            },
            "timestamp": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"批量预测失败: {e}")
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500

# ============ 综合预测推荐系统接口 ============

@app.route('/api/unified/models/train', methods=['POST'])
@require_admin
def train_unified_models():
    """训练综合模型系统（管理员专用）"""
    try:
        if not UNIFIED_TRAINER_AVAILABLE:
            return jsonify({
                "success": False,
                "error": "综合训练系统不可用，请检查依赖安装"
            }), 503
        
        data = request.get_json() or {}
        stock_limit = int(data.get('stock_limit', 200))
        days_back = int(data.get('days_back', 120))
        train_traditional = data.get('train_traditional', True)
        train_deep = data.get('train_deep', True)
        epochs = int(data.get('epochs', 30))
        
        logger.info(f"开始训练综合模型: {stock_limit}只股票, {days_back}天历史数据")
        
        # 训练所有模型
        performance = unified_trainer.train_all_models(
            stock_limit=stock_limit,
            days_back=days_back,
            train_traditional=train_traditional,
            train_deep=train_deep,
            epochs=epochs
        )
        
        if not performance:
            return jsonify({
                "success": False,
                "error": "模型训练失败，没有成功训练的模型"
            }), 500
        
        # 统计信息
        traditional_models = [k for k in performance.keys() if not k.startswith(('mlp', 'lstm', 'multimodal'))]
        deep_models = [k for k in performance.keys() if k.startswith(('mlp', 'lstm', 'multimodal'))]
        
        best_model = max(performance.keys(), key=lambda x: performance[x].get('accuracy', 0))
        best_accuracy = performance[best_model].get('accuracy', 0)
        
        return jsonify({
            "success": True,
            "data": {
                "performance": performance,
                "summary": {
                    "total_models": len(performance),
                    "traditional_models": len(traditional_models),
                    "deep_learning_models": len(deep_models),
                    "best_model": best_model,
                    "best_accuracy": best_accuracy,
                    "training_params": {
                        "stock_limit": stock_limit,
                        "days_back": days_back,
                        "epochs": epochs
                    }
                }
            },
            "message": f"成功训练{len(performance)}个模型，最佳模型: {best_model} (准确率: {best_accuracy:.4f})",
            "timestamp": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"综合模型训练失败: {e}")
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500

@app.route('/api/unified/predict/<ts_code>')
def unified_predict_single_stock(ts_code):
    """综合模型预测单只股票"""
    try:
        if not UNIFIED_TRAINER_AVAILABLE:
            return jsonify({
                "success": False,
                "error": "综合训练系统不可用"
            }), 503
        
        model_name = request.args.get('model')
        
        # 进行预测
        prediction = unified_trainer.predict_stock(ts_code, model_name)
        
        if 'error' in prediction:
            return jsonify({
                "success": False,
                "error": prediction['error']
            }), 400
        
        # 记录预测到评估系统
        if EVALUATION_SYSTEM_AVAILABLE:
            try:
                pred_id = evaluation_system.record_prediction(
                    ts_code=ts_code,
                    model_name=prediction.get('model_name', 'unknown'),
                    prediction=prediction.get('prediction', 0),
                    confidence=prediction.get('confidence', 0.5),
                    metadata={
                        'api_endpoint': 'unified_predict_single',
                        'request_timestamp': datetime.now().isoformat(),
                        'probability': prediction.get('probability')
                    }
                )
                prediction['evaluation_id'] = pred_id
            except Exception as e:
                logger.warning(f"记录预测失败: {e}")
        
        return jsonify({
            "success": True,
            "data": prediction,
            "timestamp": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"综合预测失败 {ts_code}: {e}")
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500

@app.route('/api/unified/predict/batch', methods=['POST'])
def unified_predict_batch():
    """综合模型批量预测"""
    try:
        if not UNIFIED_TRAINER_AVAILABLE:
            return jsonify({
                "success": False,
                "error": "综合训练系统不可用"
            }), 503
        
        data = request.get_json()
        stock_codes = data.get('stock_codes', [])
        model_name = data.get('model_name')
        
        if not stock_codes:
            return jsonify({
                "success": False,
                "error": "未提供股票代码列表"
            }), 400
        
        # 限制批量预测数量
        if len(stock_codes) > 100:
            return jsonify({
                "success": False,
                "error": "批量预测最多支持100只股票"
            }), 400
        
        # 批量预测
        predictions = []
        failed_predictions = []
        
        for ts_code in stock_codes:
            try:
                prediction = unified_trainer.predict_stock(ts_code, model_name)
                if 'error' in prediction:
                    failed_predictions.append({
                        'ts_code': ts_code,
                        'error': prediction['error']
                    })
                else:
                    predictions.append(prediction)
            except Exception as e:
                failed_predictions.append({
                    'ts_code': ts_code,
                    'error': str(e)
                })
        
        return jsonify({
            "success": True,
            "data": {
                "predictions": predictions,
                "failed_predictions": failed_predictions,
                "success_count": len(predictions),
                "failed_count": len(failed_predictions),
                "total_requested": len(stock_codes)
            },
            "timestamp": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"批量预测失败: {e}")
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500

@app.route('/api/unified/recommendations/generate', methods=['POST'])
def generate_unified_recommendations():
    """生成综合AI推荐"""
    try:
        if not UNIFIED_TRAINER_AVAILABLE:
            return jsonify({
                "success": False,
                "error": "综合训练系统不可用"
            }), 503
        
        data = request.get_json() or {}
        stock_limit = int(data.get('stock_limit', 50))
        model_name = data.get('model_name')
        min_confidence = float(data.get('min_confidence', 0.6))
        recommendation_type = data.get('type', 'buy')  # buy, sell, all
        
        # 获取候选股票列表
        if ENHANCED_FEATURES_AVAILABLE:
            candidate_stocks = enhanced_feature_engineer.base_engineer.get_stock_list_for_training(stock_limit * 2)
        else:
            # 回退到基础方法
            candidate_stocks = []
            with db_manager.engine.connect() as conn:
                result = conn.execute(text(f"""
                    SELECT ts_code FROM stock_basic 
                    ORDER BY RAND() 
                    LIMIT {stock_limit * 2}
                """))
                candidate_stocks = [row.ts_code for row in result]
        
        if not candidate_stocks:
            return jsonify({
                "success": False,
                "error": "无法获取候选股票列表"
            }), 500
        
        # 批量预测
        recommendations = []
        
        for ts_code in candidate_stocks[:stock_limit]:
            try:
                prediction = unified_trainer.predict_stock(ts_code, model_name)
                
                if 'error' not in prediction:
                    confidence = prediction.get('confidence', 0)
                    pred_value = prediction.get('prediction', 0)
                    
                    # 根据推荐类型筛选
                    include_prediction = False
                    if recommendation_type == 'all':
                        include_prediction = confidence >= min_confidence
                    elif recommendation_type == 'buy' and pred_value == 1:
                        include_prediction = confidence >= min_confidence
                    elif recommendation_type == 'sell' and pred_value == -1:
                        include_prediction = confidence >= min_confidence
                    
                    if include_prediction:
                        # 获取股票基本信息
                        try:
                            with db_manager.engine.connect() as conn:
                                stock_info = conn.execute(text("""
                                    SELECT name, industry FROM stock_basic WHERE ts_code = :ts_code
                                """), {'ts_code': ts_code}).first()
                                
                                prediction['stock_name'] = stock_info.name if stock_info else ts_code
                                prediction['industry'] = stock_info.industry if stock_info else '未知'
                        except:
                            prediction['stock_name'] = ts_code
                            prediction['industry'] = '未知'
                        
                        recommendations.append(prediction)
                        
            except Exception as e:
                logger.warning(f"预测股票{ts_code}失败: {e}")
                continue
        
        # 按置信度排序
        recommendations.sort(key=lambda x: x.get('confidence', 0), reverse=True)
        
        # 限制返回数量
        final_recommendations = recommendations[:min(len(recommendations), 20)]
        
        return jsonify({
            "success": True,
            "data": {
                "recommendations": final_recommendations,
                "total_count": len(final_recommendations),
                "candidate_count": len(candidate_stocks),
                "filters": {
                    "min_confidence": min_confidence,
                    "recommendation_type": recommendation_type,
                    "model_name": model_name or "auto"
                }
            },
            "timestamp": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"生成综合推荐失败: {e}")
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500

@app.route('/api/unified/models/status')
def unified_models_status():
    """获取综合模型系统状态"""
    try:
        status = {
            "unified_trainer_available": UNIFIED_TRAINER_AVAILABLE,
            "enhanced_features_available": ENHANCED_FEATURES_AVAILABLE,
            "system_status": "healthy" if UNIFIED_TRAINER_AVAILABLE else "limited"
        }
        
        if UNIFIED_TRAINER_AVAILABLE:
            # 检查已训练的模型
            try:
                model_count = len(unified_trainer.models)
                performance_history = unified_trainer.performance_history
                
                status.update({
                    "trained_models": model_count,
                    "performance_history": performance_history,
                    "available_model_types": list(unified_trainer.models.keys()) if unified_trainer.models else [],
                })
                
                # 检查模型文件
                import os
                model_files = []
                if os.path.exists(unified_trainer.model_save_path):
                    model_files = [f for f in os.listdir(unified_trainer.model_save_path) 
                                 if f.endswith(('.joblib', '.pth', '.json'))]
                
                status['saved_model_files'] = model_files
                
            except Exception as e:
                status['model_check_error'] = str(e)
        
        return jsonify({
            "success": True,
            "data": status,
            "timestamp": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"获取系统状态失败: {e}")
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500

@app.route('/api/unified/features/analyze/<ts_code>')
def analyze_stock_features(ts_code):
    """分析股票特征"""
    try:
        if not ENHANCED_FEATURES_AVAILABLE:
            return jsonify({
                "success": False,
                "error": "增强特征工程不可用"
            }), 503
        
        days_back = int(request.args.get('days', 90))
        
        # 获取股票数据
        end_date = datetime.now().strftime('%Y%m%d')
        start_date = (datetime.now() - timedelta(days=days_back)).strftime('%Y%m%d')
        
        df = enhanced_feature_engineer.base_engineer.fetch_raw_stock_data(ts_code, start_date, end_date)
        
        if df.empty:
            return jsonify({
                "success": False,
                "error": f"无法获取股票{ts_code}的数据"
            }), 404
        
        # 应用所有特征工程
        df = enhanced_feature_engineer.base_engineer.calculate_technical_features(df)
        df = enhanced_feature_engineer.base_engineer.calculate_fundamental_features(df)
        df = enhanced_feature_engineer.extract_statistical_features(df)
        df = enhanced_feature_engineer.create_momentum_features(df)
        df = enhanced_feature_engineer.create_regime_features(df)
        
        # 获取最新特征
        latest = df.iloc[-1] if not df.empty else None
        
        if latest is None:
            return jsonify({
                "success": False,
                "error": "无法计算特征"
            }), 500
        
        # 特征分析
        numeric_features = df.select_dtypes(include=[np.number]).columns
        feature_analysis = {}
        
        for feature in numeric_features:
            if not feature.startswith('future_'):
                values = df[feature].dropna()
                if len(values) > 0:
                    feature_analysis[feature] = {
                        'current_value': float(latest[feature]) if pd.notna(latest[feature]) else None,
                        'mean': float(values.mean()),
                        'std': float(values.std()),
                        'min': float(values.min()),
                        'max': float(values.max()),
                        'percentile_rank': float((values < latest[feature]).mean() * 100) if pd.notna(latest[feature]) else None
                    }
        
        # 特征分类
        feature_categories = enhanced_feature_engineer._categorize_features(list(feature_analysis.keys()))
        
        return jsonify({
            "success": True,
            "data": {
                "ts_code": ts_code,
                "analysis_period": f"{start_date} - {end_date}",
                "total_features": len(feature_analysis),
                "feature_analysis": feature_analysis,
                "feature_categories": feature_categories,
                "data_points": len(df)
            },
            "timestamp": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"特征分析失败 {ts_code}: {e}")
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500

# ============ 模型评估和反馈系统接口 ============

@app.route('/api/evaluation/run', methods=['POST'])
@require_admin
def run_model_evaluation():
    """运行模型评估（管理员专用）"""
    try:
        if not EVALUATION_SYSTEM_AVAILABLE:
            return jsonify({
                "success": False,
                "error": "模型评估系统不可用"
            }), 503
        
        data = request.get_json() or {}
        evaluation_days = int(data.get('evaluation_days', 5))
        
        logger.info(f"开始运行模型评估，评估周期: {evaluation_days}天")
        
        # 运行评估
        evaluation_result = evaluation_system.evaluate_predictions(evaluation_days)
        
        if 'error' in evaluation_result:
            return jsonify({
                "success": False,
                "error": evaluation_result['error']
            }), 500
        
        return jsonify({
            "success": True,
            "data": evaluation_result,
            "message": f"模型评估完成，评估了{evaluation_result.get('evaluated_count', 0)}条预测记录",
            "timestamp": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"运行模型评估失败: {e}")
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500

@app.route('/api/evaluation/feedback', methods=['POST'])
def add_prediction_feedback():
    """添加预测反馈"""
    try:
        if not EVALUATION_SYSTEM_AVAILABLE:
            return jsonify({
                "success": False,
                "error": "模型评估系统不可用"
            }), 503
        
        data = request.get_json()
        prediction_id = data.get('prediction_id')
        feedback_type = data.get('feedback_type')  # accuracy, usefulness, satisfaction
        feedback_value = data.get('feedback_value')  # correct/incorrect, useful/not_useful, 1-5
        user_id = data.get('user_id')
        comments = data.get('comments')
        
        if not all([prediction_id, feedback_type, feedback_value]):
            return jsonify({
                "success": False,
                "error": "缺少必需参数: prediction_id, feedback_type, feedback_value"
            }), 400
        
        # 添加反馈
        success = evaluation_system.add_user_feedback(
            prediction_id=prediction_id,
            feedback_type=feedback_type,
            feedback_value=feedback_value,
            user_id=user_id,
            comments=comments
        )
        
        if success:
            return jsonify({
                "success": True,
                "message": "反馈已记录",
                "timestamp": datetime.now().isoformat()
            })
        else:
            return jsonify({
                "success": False,
                "error": "反馈记录失败"
            }), 500
        
    except Exception as e:
        logger.error(f"添加预测反馈失败: {e}")
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500

@app.route('/api/evaluation/report')
def get_evaluation_report():
    """获取模型评估报告"""
    try:
        if not EVALUATION_SYSTEM_AVAILABLE:
            return jsonify({
                "success": False,
                "error": "模型评估系统不可用"
            }), 503
        
        model_name = request.args.get('model_name')
        days_back = int(request.args.get('days', 30))
        
        # 获取性能报告
        report = evaluation_system.get_model_performance_report(model_name, days_back)
        
        if 'error' in report:
            return jsonify({
                "success": False,
                "error": report['error']
            }), 500
        
        return jsonify({
            "success": True,
            "data": report,
            "timestamp": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"获取评估报告失败: {e}")
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500

@app.route('/api/evaluation/statistics')
def get_evaluation_statistics():
    """获取评估统计信息"""
    try:
        if not EVALUATION_SYSTEM_AVAILABLE:
            return jsonify({
                "success": False,
                "error": "模型评估系统不可用"
            }), 503
        
        days_back = int(request.args.get('days', 30))
        
        # 获取统计信息
        stats = evaluation_system.get_feedback_statistics(days_back)
        
        if 'error' in stats:
            return jsonify({
                "success": False,
                "error": stats['error']
            }), 500
        
        return jsonify({
            "success": True,
            "data": stats,
            "timestamp": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"获取评估统计失败: {e}")
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500

@app.route('/api/evaluation/dashboard')
def get_evaluation_dashboard():
    """获取评估仪表板数据"""
    try:
        if not EVALUATION_SYSTEM_AVAILABLE:
            return jsonify({
                "success": False,
                "error": "模型评估系统不可用"
            }), 503
        
        days_back = int(request.args.get('days', 7))
        
        # 获取多种统计信息
        stats = evaluation_system.get_feedback_statistics(days_back)
        report = evaluation_system.get_model_performance_report(None, days_back)
        
        # 组合仪表板数据
        dashboard_data = {
            "period": f"最近{days_back}天",
            "overview": {
                "total_predictions": 0,
                "evaluated_predictions": 0,
                "overall_accuracy": 0,
                "best_model": "N/A",
                "worst_model": "N/A"
            },
            "model_performance": [],
            "recent_trends": [],
            "user_feedback": {
                "total_feedback": 0,
                "feedback_distribution": {}
            },
            "recommendations": []
        }
        
        # 填充数据
        if 'error' not in stats:
            predictions_by_model = stats.get('predictions_by_model', [])
            if predictions_by_model:
                total_preds = sum(p['total_predictions'] for p in predictions_by_model)
                total_evaluated = sum(p['evaluated_predictions'] for p in predictions_by_model)
                
                dashboard_data['overview']['total_predictions'] = total_preds
                dashboard_data['overview']['evaluated_predictions'] = total_evaluated
                
                # 计算整体准确率
                if total_evaluated > 0:
                    total_correct = sum(p['correct_predictions'] for p in predictions_by_model)
                    dashboard_data['overview']['overall_accuracy'] = total_correct / total_evaluated
                
                # 找最佳和最差模型
                if predictions_by_model:
                    models_with_accuracy = [(p['model_name'], p['correct_predictions'] / max(p['evaluated_predictions'], 1)) 
                                          for p in predictions_by_model if p['evaluated_predictions'] > 0]
                    
                    if models_with_accuracy:
                        best = max(models_with_accuracy, key=lambda x: x[1])
                        worst = min(models_with_accuracy, key=lambda x: x[1])
                        dashboard_data['overview']['best_model'] = f"{best[0]} ({best[1]:.2%})"
                        dashboard_data['overview']['worst_model'] = f"{worst[0]} ({worst[1]:.2%})"
                
                dashboard_data['model_performance'] = predictions_by_model
            
            # 用户反馈统计
            feedback_by_type = stats.get('feedback_by_type', [])
            if feedback_by_type:
                total_feedback = sum(f['count'] for f in feedback_by_type)
                dashboard_data['user_feedback']['total_feedback'] = total_feedback
                dashboard_data['user_feedback']['feedback_distribution'] = {
                    f['feedback_type']: f['count'] for f in feedback_by_type
                }
        
        # 添加建议
        if 'error' not in report:
            dashboard_data['recommendations'] = report.get('recommendations', [])
        
        return jsonify({
            "success": True,
            "data": dashboard_data,
            "timestamp": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"获取评估仪表板失败: {e}")
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500

# ============ 投资策略管理接口 ============

@app.route('/dashboard')
def user_dashboard():
    """用户投资策略仪表板"""
    return render_template('user_dashboard.html')

@app.route('/api/strategies', methods=['GET'])
def get_user_strategies():
    """获取用户策略列表"""
    try:
        # 从session或header获取用户ID
        user_id = request.headers.get('X-User-ID', 'default')
        strategy_type = request.args.get('type')  # 策略类型过滤
        risk_level = request.args.get('risk_level')  # 风险等级过滤
        
        # 导入策略管理器
        from strategy.strategy_models import StrategyManager
        from strategy.market_mainstream_strategies import (
            MAINSTREAM_STRATEGIES, 
            get_strategies_by_risk_level,
            get_strategies_by_type
        )
        
        strategy_manager = StrategyManager()
        
        # 获取用户自定义策略
        user_strategies = strategy_manager.get_strategies(user_id)
        
        # 获取主流策略模板
        mainstream_strategies = []
        for strategy_id, strategy_template in MAINSTREAM_STRATEGIES.items():
            strategy_info = {
                'id': strategy_id,
                'name': strategy_template['name'],
                'description': strategy_template['description'],
                'strategy_type': strategy_template['strategy_type'],
                'tags': strategy_template.get('tags', []),
                'is_template': True,
                'risk_level': _get_strategy_risk_level(strategy_id),
                'buy_rules_count': len(strategy_template.get('buy_rules', [])),
                'sell_rules_count': len(strategy_template.get('sell_rules', []))
            }
            mainstream_strategies.append(strategy_info)
        
        # 合并策略列表
        all_strategies = user_strategies + mainstream_strategies
        
        # 应用过滤器
        if strategy_type:
            all_strategies = [s for s in all_strategies if s.get('strategy_type') == strategy_type]
        
        if risk_level:
            risk_strategy_ids = get_strategies_by_risk_level(risk_level)
            all_strategies = [s for s in all_strategies if s.get('id') in risk_strategy_ids or not s.get('is_template')]
        
        return jsonify({
            "success": True,
            "data": {
                "strategies": all_strategies,
                "total": len(all_strategies),
                "user_strategies_count": len(user_strategies),
                "template_strategies_count": len(mainstream_strategies)
            },
            "timestamp": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"获取策略列表失败: {e}")
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500

@app.route('/api/strategies/<strategy_id>', methods=['GET'])
def get_strategy_detail(strategy_id):
    """获取策略详情"""
    try:
        from strategy.strategy_models import StrategyManager
        from strategy.market_mainstream_strategies import get_strategy_template
        
        # 先尝试从用户策略中获取
        strategy_manager = StrategyManager()
        user_id = request.headers.get('X-User-ID', 'default')
        
        strategy = strategy_manager.db.get_strategy(strategy_id)
        
        if strategy:
            # 用户自定义策略
            strategy_data = {
                'id': strategy.id,
                'name': strategy.name,
                'description': strategy.description,
                'strategy_type': strategy.strategy_type,
                'user_id': strategy.user_id,
                'buy_rules': [asdict(rule) for rule in strategy.buy_rules],
                'sell_rules': [asdict(rule) for rule in strategy.sell_rules],
                'risk_management': asdict(strategy.risk_management),
                'tags': strategy.tags,
                'created_at': strategy.created_at,
                'updated_at': strategy.updated_at,
                'is_template': False
            }
        else:
            # 主流策略模板
            template = get_strategy_template(strategy_id)
            if not template:
                return jsonify({
                    "success": False,
                    "error": "策略不存在"
                }), 404
            
            strategy_data = {
                'id': strategy_id,
                'name': template['name'],
                'description': template['description'],
                'strategy_type': template['strategy_type'],
                'buy_rules': [asdict(rule) for rule in template.get('buy_rules', [])],
                'sell_rules': [asdict(rule) for rule in template.get('sell_rules', [])],
                'risk_management': asdict(template.get('risk_management', {})),
                'tags': template.get('tags', []),
                'is_template': True,
                'risk_level': _get_strategy_risk_level(strategy_id)
            }
        
        return jsonify({
            "success": True,
            "data": strategy_data,
            "timestamp": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"获取策略详情失败: {e}")
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500

@app.route('/api/strategies', methods=['POST'])
def create_user_strategy():
    """创建用户策略"""
    try:
        data = request.get_json()
        user_id = request.headers.get('X-User-ID', 'default')
        
        from strategy.strategy_models import Strategy, StrategyManager, StrategyRule, TradingCondition, RiskManagement
        
        # 创建策略对象
        strategy = Strategy(
            name=data.get('name', ''),
            description=data.get('description', ''),
            strategy_type=data.get('strategy_type', 'technical'),
            user_id=user_id
        )
        
        # 解析买入规则
        if 'buy_rules' in data:
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
                strategy.buy_rules.append(rule)
        
        # 解析卖出规则
        if 'sell_rules' in data:
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
                strategy.sell_rules.append(rule)
        
        # 解析风险管理
        if 'risk_management' in data:
            strategy.risk_management = RiskManagement(**data['risk_management'])
        
        # 标签
        strategy.tags = data.get('tags', [])
        
        # 保存策略
        strategy_manager = StrategyManager()
        strategy_id = strategy_manager.db.save_strategy(strategy)
        
        return jsonify({
            "success": True,
            "data": {
                "strategy_id": strategy_id,
                "message": "策略创建成功"
            },
            "timestamp": datetime.now().isoformat()
        }), 201
        
    except Exception as e:
        logger.error(f"创建策略失败: {e}")
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500

@app.route('/api/strategies/<strategy_id>/clone', methods=['POST'])
def clone_strategy(strategy_id):
    """克隆策略（从模板创建用户策略）"""
    try:
        data = request.get_json() or {}
        user_id = request.headers.get('X-User-ID', 'default')
        
        from strategy.strategy_models import Strategy, StrategyManager
        from strategy.market_mainstream_strategies import get_strategy_template
        
        # 获取模板策略
        template = get_strategy_template(strategy_id)
        if not template:
            return jsonify({
                "success": False,
                "error": "策略模板不存在"
            }), 404
        
        # 创建用户策略
        strategy = Strategy(
            name=data.get('name', f"{template['name']} - 我的副本"),
            description=data.get('description', template['description']),
            strategy_type=template['strategy_type'],
            user_id=user_id,
            buy_rules=template.get('buy_rules', []),
            sell_rules=template.get('sell_rules', []),
            risk_management=template.get('risk_management'),
            tags=template.get('tags', []) + ['克隆策略']
        )
        
        # 保存策略
        strategy_manager = StrategyManager()
        strategy_id = strategy_manager.db.save_strategy(strategy)
        
        return jsonify({
            "success": True,
            "data": {
                "strategy_id": strategy_id,
                "message": "策略克隆成功"
            },
            "timestamp": datetime.now().isoformat()
        }), 201
        
    except Exception as e:
        logger.error(f"克隆策略失败: {e}")
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500

@app.route('/api/strategies/templates', methods=['GET'])
def get_strategy_templates():
    """获取策略模板库"""
    try:
        from strategy.market_mainstream_strategies import (
            MAINSTREAM_STRATEGIES, 
            STRATEGY_RISK_LEVELS,
            MARKET_ENVIRONMENT_STRATEGIES
        )
        
        category = request.args.get('category')  # 技术分析、基本面分析、量化等
        risk_level = request.args.get('risk_level')  # 保守、稳健、激进、投机
        market_env = request.args.get('market_env')  # 牛市、熊市、震荡市等
        
        templates = []
        for strategy_id, template in MAINSTREAM_STRATEGIES.items():
            template_info = {
                'id': strategy_id,
                'name': template['name'],
                'description': template['description'],
                'strategy_type': template['strategy_type'],
                'tags': template.get('tags', []),
                'risk_level': _get_strategy_risk_level(strategy_id),
                'market_suitability': _get_market_suitability(strategy_id),
                'complexity': _get_strategy_complexity(template),
                'expected_return': _get_expected_return_range(strategy_id),
                'max_drawdown': getattr(template.get('risk_management'), 'max_drawdown', 0.15) if template.get('risk_management') else 0.15,
                'buy_rules_count': len(template.get('buy_rules', [])),
                'sell_rules_count': len(template.get('sell_rules', []))
            }
            templates.append(template_info)
        
        # 应用过滤器
        if category:
            templates = [t for t in templates if t['strategy_type'] == category]
        
        if risk_level:
            risk_strategies = STRATEGY_RISK_LEVELS.get(risk_level, {}).get('strategies', [])
            templates = [t for t in templates if t['id'] in risk_strategies]
        
        if market_env:
            market_strategies = MARKET_ENVIRONMENT_STRATEGIES.get(market_env, [])
            templates = [t for t in templates if t['id'] in market_strategies]
        
        # 按风险等级分组
        templates_by_risk = {
            'conservative': [],
            'moderate': [],
            'aggressive': [],
            'speculative': []
        }
        
        for template in templates:
            risk = template['risk_level']
            if risk in templates_by_risk:
                templates_by_risk[risk].append(template)
        
        return jsonify({
            "success": True,
            "data": {
                "templates": templates,
                "templates_by_risk": templates_by_risk,
                "total": len(templates),
                "risk_levels": list(STRATEGY_RISK_LEVELS.keys()),
                "market_environments": list(MARKET_ENVIRONMENT_STRATEGIES.keys())
            },
            "timestamp": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"获取策略模板失败: {e}")
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500

def _get_strategy_risk_level(strategy_id):
    """获取策略风险等级"""
    from strategy.market_mainstream_strategies import STRATEGY_RISK_LEVELS
    for risk_level, config in STRATEGY_RISK_LEVELS.items():
        if strategy_id in config.get('strategies', []):
            return risk_level
    return 'moderate'

def _get_market_suitability(strategy_id):
    """获取策略市场适应性"""
    from strategy.market_mainstream_strategies import MARKET_ENVIRONMENT_STRATEGIES
    suitable_markets = []
    for market_env, strategies in MARKET_ENVIRONMENT_STRATEGIES.items():
        if strategy_id in strategies:
            suitable_markets.append(market_env)
    return suitable_markets

def _get_strategy_complexity(template):
    """计算策略复杂度"""
    buy_rules_count = len(template.get('buy_rules', []))
    sell_rules_count = len(template.get('sell_rules', []))
    
    total_conditions = 0
    for rule in template.get('buy_rules', []):
        total_conditions += len(rule.conditions) if hasattr(rule, 'conditions') else 0
    for rule in template.get('sell_rules', []):
        total_conditions += len(rule.conditions) if hasattr(rule, 'conditions') else 0
    
    if total_conditions <= 3:
        return '简单'
    elif total_conditions <= 6:
        return '中等'
    else:
        return '复杂'

def _get_expected_return_range(strategy_id):
    """获取预期收益范围"""
    risk_level = _get_strategy_risk_level(strategy_id)
    return {
        'conservative': '5-12%',
        'moderate': '10-20%', 
        'aggressive': '15-35%',
        'speculative': '8-15%'  # 虽然风险高但配对交易等策略收益相对稳定
    }.get(risk_level, '10-20%')

# ============ 模型优化和重训练接口 ============

@app.route('/api/optimization/analyze', methods=['POST'])
@require_admin
def analyze_model_performance():
    """分析模型性能（管理员专用）"""
    try:
        if not MODEL_OPTIMIZER_AVAILABLE:
            return jsonify({
                "success": False,
                "error": "模型优化器不可用"
            }), 503
        
        data = request.get_json() or {}
        days_back = int(data.get('days_back', 30))
        
        logger.info(f"开始分析模型性能，回溯{days_back}天")
        
        # 分析模型性能
        analysis_result = model_optimizer.analyze_model_performance(days_back)
        
        if 'error' in analysis_result:
            return jsonify({
                "success": False,
                "error": analysis_result['error']
            }), 500
        
        return jsonify({
            "success": True,
            "data": analysis_result,
            "message": f"模型性能分析完成，分析了{len(analysis_result.get('models_analyzed', {}))}个模型",
            "timestamp": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"模型性能分析失败: {e}")
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500

@app.route('/api/optimization/optimize', methods=['POST'])
@require_admin
def optimize_model_parameters():
    """优化模型参数（管理员专用）"""
    try:
        if not MODEL_OPTIMIZER_AVAILABLE:
            return jsonify({
                "success": False,
                "error": "模型优化器不可用"
            }), 503
        
        data = request.get_json()
        model_name = data.get('model_name')
        optimization_type = data.get('optimization_type', 'hyperparameter')
        
        if not model_name:
            return jsonify({
                "success": False,
                "error": "缺少必需参数: model_name"
            }), 400
        
        logger.info(f"开始优化模型 {model_name}，优化类型: {optimization_type}")
        
        # 优化模型参数
        optimization_result = model_optimizer.optimize_model_parameters(model_name, optimization_type)
        
        if 'error' in optimization_result:
            return jsonify({
                "success": False,
                "error": optimization_result['error']
            }), 500
        
        return jsonify({
            "success": True,
            "data": optimization_result,
            "message": f"模型{model_name}优化完成",
            "timestamp": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"模型参数优化失败: {e}")
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500

@app.route('/api/optimization/retrain', methods=['POST'])
@require_admin
def retrain_models():
    """重新训练模型（管理员专用）"""
    try:
        if not MODEL_OPTIMIZER_AVAILABLE:
            return jsonify({
                "success": False,
                "error": "模型优化器不可用"
            }), 503
        
        data = request.get_json() or {}
        model_names = data.get('model_names')  # 可选，为空则重训练所有模型
        retrain_config = data.get('config', {
            'stock_limit': 400,
            'days_back': 150,
            'epochs': 40,
            'train_traditional': True,
            'train_deep': True
        })
        
        logger.info(f"开始重新训练模型: {model_names or 'ALL'}")
        
        # 重新训练模型
        retrain_result = model_optimizer.retrain_models(model_names, retrain_config)
        
        if 'error' in retrain_result:
            return jsonify({
                "success": False,
                "error": retrain_result['error']
            }), 500
        
        return jsonify({
            "success": True,
            "data": retrain_result,
            "message": f"模型重训练完成，训练了{len(retrain_result.get('retrained_models', []))}个模型",
            "timestamp": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"模型重训练失败: {e}")
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500

@app.route('/api/optimization/auto-check', methods=['POST'])
@require_admin
def auto_optimization_check():
    """自动优化检查（管理员专用）"""
    try:
        if not MODEL_OPTIMIZER_AVAILABLE:
            return jsonify({
                "success": False,
                "error": "模型优化器不可用"
            }), 503
        
        logger.info("执行自动优化检查...")
        
        # 执行自动优化检查
        optimization_actions = model_optimizer.auto_optimization_check()
        
        if 'error' in optimization_actions:
            return jsonify({
                "success": False,
                "error": optimization_actions['error']
            }), 500
        
        actions_count = len(optimization_actions.get('actions_taken', []))
        
        return jsonify({
            "success": True,
            "data": optimization_actions,
            "message": f"自动优化检查完成，执行了{actions_count}个优化动作",
            "timestamp": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"自动优化检查失败: {e}")
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500

@app.route('/api/optimization/scheduler/start', methods=['POST'])
@require_admin
def start_optimization_scheduler():
    """启动优化调度器（管理员专用）"""
    try:
        if not MODEL_OPTIMIZER_AVAILABLE:
            return jsonify({
                "success": False,
                "error": "模型优化器不可用"
            }), 503
        
        # 启动调度器
        model_optimizer.start_scheduler()
        
        return jsonify({
            "success": True,
            "message": "优化调度器已启动",
            "status": "running",
            "timestamp": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"启动优化调度器失败: {e}")
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500

@app.route('/api/optimization/scheduler/stop', methods=['POST'])
@require_admin
def stop_optimization_scheduler():
    """停止优化调度器（管理员专用）"""
    try:
        if not MODEL_OPTIMIZER_AVAILABLE:
            return jsonify({
                "success": False,
                "error": "模型优化器不可用"
            }), 503
        
        # 停止调度器
        model_optimizer.stop_scheduler()
        
        return jsonify({
            "success": True,
            "message": "优化调度器已停止",
            "status": "stopped",
            "timestamp": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"停止优化调度器失败: {e}")
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500

@app.route('/api/optimization/status')
@require_admin
def get_optimization_status():
    """获取优化状态（管理员专用）"""
    try:
        if not MODEL_OPTIMIZER_AVAILABLE:
            return jsonify({
                "success": False,
                "error": "模型优化器不可用"
            }), 503
        
        # 获取优化状态
        status = model_optimizer.get_optimization_status()
        
        if 'error' in status:
            return jsonify({
                "success": False,
                "error": status['error']
            }), 500
        
        return jsonify({
            "success": True,
            "data": status,
            "timestamp": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"获取优化状态失败: {e}")
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500

@app.route('/api/optimization/config', methods=['GET', 'POST'])
@require_admin
def optimization_config():
    """获取或更新优化配置（管理员专用）"""
    try:
        if not MODEL_OPTIMIZER_AVAILABLE:
            return jsonify({
                "success": False,
                "error": "模型优化器不可用"
            }), 503
        
        if request.method == 'GET':
            # 获取当前配置
            return jsonify({
                "success": True,
                "data": model_optimizer.optimization_config,
                "timestamp": datetime.now().isoformat()
            })
        
        elif request.method == 'POST':
            # 更新配置
            data = request.get_json()
            
            if not data:
                return jsonify({
                    "success": False,
                    "error": "缺少配置数据"
                }), 400
            
            # 更新配置
            for key, value in data.items():
                if key in model_optimizer.optimization_config:
                    model_optimizer.optimization_config[key] = value
            
            logger.info(f"优化配置已更新: {data}")
            
            return jsonify({
                "success": True,
                "data": model_optimizer.optimization_config,
                "message": "优化配置已更新",
                "timestamp": datetime.now().isoformat()
            })
        
    except Exception as e:
        logger.error(f"优化配置操作失败: {e}")
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500

# ============ 智能推荐系统接口 (基于Ollama) ============

@app.route('/api/intelligent/recommendations/generate', methods=['POST'])
def generate_intelligent_recommendations():
    """生成智能推荐"""
    try:
        data = request.get_json() or {}
        max_stocks = int(data.get('max_stocks', 20))
        max_concurrent = int(data.get('max_concurrent', 5))
        
        # 导入智能推荐模块
        try:
            from llm.intelligent_recommender import intelligent_recommender
        except ImportError as e:
            return jsonify({
                "success": False, 
                "error": f"智能推荐模块不可用: {e}",
                "fallback": "traditional"
            })
        
        # 异步运行推荐生成
        import asyncio
        
        async def run_recommendation():
            return await intelligent_recommender.generate_intelligent_recommendations(
                max_stocks=max_stocks, 
                max_concurrent=max_concurrent
            )
        
        # 在新的事件循环中运行
        try:
            loop = asyncio.new_event_loop()
            asyncio.set_event_loop(loop)
            recommendations = loop.run_until_complete(run_recommendation())
            loop.close()
        except Exception as e:
            logger.error(f"异步推荐生成失败: {e}")
            return jsonify({
                "success": False,
                "error": f"推荐生成失败: {str(e)}"
            })
        
        if not recommendations:
            return jsonify({
                "success": True,
                "data": [],
                "message": "暂无推荐结果",
                "timestamp": datetime.now().isoformat()
            })
        
        # 转换为API响应格式
        result_data = []
        for rec in recommendations:
            result_data.append({
                "ts_code": rec.ts_code,
                "name": rec.name,
                "recommendation": rec.recommendation,
                "confidence": round(rec.confidence, 3),
                "score": round(rec.score, 3),
                "technical_score": round(rec.technical_score, 3),
                "ai_score": round(rec.ai_score, 3),
                "target_price": rec.target_price,
                "stop_loss": rec.stop_loss,
                "reasoning": rec.reasoning,
                "risk_level": rec.risk_level,
                "key_factors": rec.key_factors,
                "technical_signals": rec.technical_signals,
                "market_position": rec.market_position,
                "sector": rec.sector,
                "holding_period": rec.holding_period,
                "recommendation_date": rec.recommendation_date.isoformat()
            })
        
        # 保存推荐结果
        asyncio.set_event_loop(asyncio.new_event_loop())
        save_success = asyncio.get_event_loop().run_until_complete(
            intelligent_recommender.save_intelligent_recommendations(recommendations)
        )
        
        return jsonify({
            "success": True,
            "data": result_data,
            "count": len(result_data),
            "saved": save_success,
            "generation_info": {
                "max_stocks": max_stocks,
                "max_concurrent": max_concurrent,
                "buy_count": len([r for r in result_data if r["recommendation"] == "buy"]),
                "hold_count": len([r for r in result_data if r["recommendation"] == "hold"]),
                "avg_confidence": round(sum(r["confidence"] for r in result_data) / len(result_data), 3) if result_data else 0
            },
            "timestamp": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"智能推荐生成失败: {e}")
        return jsonify({
            "success": False, 
            "error": str(e)
        }), 500

@app.route('/api/intelligent/recommendations/latest')
def get_latest_intelligent_recommendations():
    """获取最新的智能推荐"""
    try:
        limit = int(request.args.get('limit', 20))
        
        # 导入智能推荐模块
        try:
            from llm.intelligent_recommender import intelligent_recommender
        except ImportError:
            return jsonify({
                "success": False, 
                "error": "智能推荐模块不可用"
            })
        
        # 异步获取推荐
        import asyncio
        
        async def get_recommendations():
            return await intelligent_recommender.get_latest_intelligent_recommendations(limit)
        
        try:
            loop = asyncio.new_event_loop()
            asyncio.set_event_loop(loop)
            recommendations = loop.run_until_complete(get_recommendations())
            loop.close()
        except Exception as e:
            logger.error(f"获取智能推荐失败: {e}")
            return jsonify({
                "success": False,
                "error": f"获取推荐失败: {str(e)}"
            })
        
        return jsonify({
            "success": True,
            "data": recommendations,
            "count": len(recommendations),
            "timestamp": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"获取智能推荐失败: {e}")
        return jsonify({
            "success": False, 
            "error": str(e)
        }), 500

@app.route('/api/intelligent/health')
def intelligent_system_health():
    """智能推荐系统健康检查"""
    try:
        # 检查Ollama服务
        try:
            from llm.ollama_analyzer import ollama_analyzer
            import asyncio
            
            async def check_health():
                return await ollama_analyzer.health_check()
            
            loop = asyncio.new_event_loop()
            asyncio.set_event_loop(loop)
            ollama_health = loop.run_until_complete(check_health())
            loop.close()
            
        except Exception as e:
            ollama_health = {
                "status": "error",
                "error": str(e)
            }
        
        # 检查数据库连接
        try:
            test_query = db_manager.fetch_data("SELECT COUNT(*) as count FROM stock_basic LIMIT 1")
            db_health = {
                "status": "healthy",
                "stock_count": int(test_query.iloc[0]['count']) if not test_query.empty else 0
            }
        except Exception as e:
            db_health = {
                "status": "error",
                "error": str(e)
            }
        
        # 检查智能推荐表
        try:
            recommendations_count = db_manager.fetch_data("""
                SELECT COUNT(*) as count 
                FROM intelligent_recommendations 
                WHERE recommendation_date >= DATE_SUB(CURDATE(), INTERVAL 7 DAY)
            """)
            recent_recommendations = int(recommendations_count.iloc[0]['count']) if not recommendations_count.empty else 0
        except:
            recent_recommendations = 0
        
        overall_status = "healthy"
        if ollama_health["status"] != "healthy" or db_health["status"] != "healthy":
            overall_status = "warning"
        
        return jsonify({
            "success": True,
            "overall_status": overall_status,
            "components": {
                "ollama": ollama_health,
                "database": db_health,
                "recent_recommendations": recent_recommendations
            },
            "timestamp": datetime.now().isoformat()
        })
        
    except Exception as e:
        logger.error(f"健康检查失败: {e}")
        return jsonify({
            "success": False,
            "error": str(e)
        }), 500

if __name__ == '__main__':
    import threading
    import asyncio
    import sys
    import os
    
    # 添加项目根目录到Python路径
    project_root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    if project_root not in sys.path:
        sys.path.insert(0, project_root)
    
    try:
        from websocket.realtime_server import RealtimeDataServer
        websocket_available = True
    except ImportError as e:
        logger.warning(f"WebSocket模块导入失败: {e}")
        websocket_available = False
    
    # 启动WebSocket服务器
    def start_websocket_server():
        try:
            logger.info(f"启动WebSocket服务器 {config.WS_HOST}:{config.WS_PORT}")
            server = RealtimeDataServer()
            
            # 创建新的事件循环
            loop = asyncio.new_event_loop()
            asyncio.set_event_loop(loop)
            
            # 启动服务器
            start_server = server.start_server(config.WS_HOST, config.WS_PORT)
            loop.run_until_complete(start_server)
            loop.run_forever()
        except Exception as e:
            logger.error(f"WebSocket服务器启动失败: {e}")
    
    # 如果WebSocket可用，在后台线程启动
    if websocket_available:
        ws_thread = threading.Thread(target=start_websocket_server, daemon=True)
        ws_thread.start()
        logger.info(f"WebSocket服务器运行在 ws://{config.WS_HOST}:{config.WS_PORT}")
    else:
        logger.warning("WebSocket服务器未启动（模块导入失败）")
    
    # 启动API服务器
    logger.info(f"启动增强版API服务器 {config.API_HOST}:{config.API_PORT}")
    app.run(host=config.API_HOST, port=config.API_PORT, debug=config.API_DEBUG) 