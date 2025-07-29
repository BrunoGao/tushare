from flask import Flask, request, jsonify, render_template_string, send_file
from flask_cors import CORS
import logging
import pandas as pd
from datetime import datetime, timedelta
import sys, os
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import config
from utils.db_helper import db
from analysis.recommender import recommender
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

# 初始化Flask应用
app = Flask(__name__)
CORS(app)  # 允许跨域请求
logging.basicConfig(level=getattr(logging, config.LOG_LEVEL), format=config.LOG_FORMAT)
logger = logging.getLogger(__name__)

# 增强的前端页面模板
HTML_TEMPLATE = '''
<!DOCTYPE html>
<html><head><meta charset="utf-8"><title>领京万象 - 实时股票分析系统</title>
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
<h1>🚀 领京万象 - 实时股票分析系统</h1>
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
    document.querySelectorAll('[id$=\"-suggestions\"]').forEach(s => {
        s.style.display = 'none';
    });
}

async function loadPopularStocks(category) {
    try {
        const response = await fetch(`/api/stocks/popular?category=${category}&limit=12`);
        const data = await response.json();
        
        if (data.success) {
            displayPopularStocks(data.data, category);
        } else {
            alert('加载热门股票失败');
        }
    } catch (error) {
        console.error('加载热门股票失败:', error);
        alert('加载热门股票失败');
    }
}

function displayPopularStocks(stocks, category) {
    const categoryNames = {
        'all': '热门股票',
        'bank': '银行股',
        'tech': '科技股',
        'consumer': '消费股'
    };
    
    let html = `
        <div style="position:fixed;top:50%;left:50%;transform:translate(-50%,-50%);
                    background:#2d2d2d;border:1px solid #555;border-radius:8px;
                    padding:20px;max-width:600px;max-height:500px;overflow-y:auto;z-index:2000;">
            <div style="display:flex;justify-content:space-between;align-items:center;margin-bottom:15px;">
                <h4>${categoryNames[category] || '股票列表'}</h4>
                <button onclick="closePopularStocks()" style="background:none;border:none;color:#fff;font-size:20px;cursor:pointer;">×</button>
            </div>
            <div style="display:grid;grid-template-columns:repeat(auto-fill,minmax(180px,1fr));gap:10px;">
    `;
    
    stocks.forEach(stock => {
        html += `
            <div style="background:#333;padding:10px;border-radius:5px;cursor:pointer;text-align:center;"
                 onclick="selectPopularStock('${stock.ts_code}', '${stock.display_name}')">
                <div style="font-weight:bold;font-size:14px;">${stock.name}</div>
                <div style="font-size:12px;color:#999;margin:2px 0;">${stock.ts_code}</div>
                <div style="font-size:11px;color:#666;">${stock.industry}</div>
            </div>
        `;
    });
    
    html += `
            </div>
        </div>
        <div id="popular-overlay" style="position:fixed;top:0;left:0;width:100%;height:100%;background:rgba(0,0,0,0.7);z-index:1999;" onclick="closePopularStocks()"></div>
    `;
    
    // 添加到页面
    const overlay = document.createElement('div');
    overlay.id = 'popular-stocks-modal';
    overlay.innerHTML = html;
    document.body.appendChild(overlay);
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

@app.route('/')
def index(): return render_template_string(HTML_TEMPLATE)  # 主页

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
        
        # 使用简化图表生成器
        if hasattr(chart_generator, 'create_basic_chart'):
            result = chart_generator.create_basic_chart(ts_code, days)
        else:
            result = chart_generator.create_comprehensive_chart(ts_code, days)
        
        if result['success']:
            return jsonify({
                "success": True,
                "chart_html": result['chart_html'],
                "stock_info": {
                    "code": result.get('stock_code', ts_code),
                    "latest_price": result.get('latest_price', 0),
                    "data_points": result.get('data_points', 0)
                },
                "timestamp": datetime.now().isoformat()
            })
        else:
            return jsonify(result), 400
            
    except Exception as e:
        logger.error(f"生成图表失败: {e}")
        return jsonify({"success": False, "error": str(e)}), 500

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
def generate_multi_strategy_recommendations():
    """生成多策略推荐"""
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
            stats['stock_count'] = conn.execute(text("SELECT COUNT(*) FROM stock_basic WHERE list_status='L'")).scalar()
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
                WHERE list_status = 'L'
                AND (
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
                    WHERE list_status = 'L'
                    AND (industry LIKE '%银行%' OR industry LIKE '%金融%')
                    ORDER BY ts_code
                    LIMIT :limit
                """
            elif category == 'tech':
                sql = """
                    SELECT ts_code, name, industry, market
                    FROM stock_basic
                    WHERE list_status = 'L'
                    AND (industry LIKE '%软件%' OR industry LIKE '%互联网%' OR industry LIKE '%电子%' OR industry LIKE '%通信%')
                    ORDER BY ts_code
                    LIMIT :limit
                """
            elif category == 'consumer':
                sql = """
                    SELECT ts_code, name, industry, market
                    FROM stock_basic
                    WHERE list_status = 'L'
                    AND (industry LIKE '%消费%' OR industry LIKE '%食品%' OR industry LIKE '%饮料%' OR industry LIKE '%家电%')
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
                    WHERE list_status = 'L'
                    AND ts_code IN ({placeholders})
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

if __name__ == '__main__':
    logger.info(f"启动增强版API服务器 {config.API_HOST}:{config.API_PORT}")
    app.run(host=config.API_HOST, port=config.API_PORT, debug=config.API_DEBUG) 