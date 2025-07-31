#!/bin/bash
# TuShare 市场数据仪表板停止脚本

echo "🛑 停止TuShare市场数据仪表板系统"
echo "======================================"

# 读取进程ID
if [ -f .dashboard_pids ]; then
    echo "📋 读取进程列表..."
    while read pid; do
        if ps -p $pid > /dev/null 2>&1; then
            echo "🔄 停止进程: $pid"
            kill $pid 2>/dev/null
        fi
    done < .dashboard_pids
    
    # 等待进程结束
    sleep 3
    
    # 强制结束残留进程
    while read pid; do
        if ps -p $pid > /dev/null 2>&1; then
            echo "💥 强制停止进程: $pid"
            kill -9 $pid 2>/dev/null
        fi
    done < .dashboard_pids
    
    rm -f .dashboard_pids
    echo "✅ 进程列表已清理"
else
    echo "⚠️ 未找到进程列表文件，尝试按名称停止..."
fi

# 按进程名停止相关进程
echo "🔍 查找并停止相关进程..."

# 停止历史数据同步
pids=$(ps aux | grep "historical_data_sync.py" | grep -v grep | awk '{print $2}')
if [ ! -z "$pids" ]; then
    echo "🔄 停止历史数据同步进程: $pids"
    echo $pids | xargs kill 2>/dev/null
fi

# 停止技术指标修复
pids=$(ps aux | grep "fix_technical_indicators.py" | grep -v grep | awk '{print $2}')
if [ ! -z "$pids" ]; then
    echo "🔄 停止技术指标修复进程: $pids"
    echo $pids | xargs kill 2>/dev/null
fi

# 停止API服务器
pids=$(ps aux | grep "api/app.py" | grep -v grep | awk '{print $2}')
if [ ! -z "$pids" ]; then
    echo "🔄 停止API服务器进程: $pids"
    echo $pids | xargs kill 2>/dev/null
fi

# 释放端口
echo "🌐 释放端口 5005..."
lsof -ti:5005 | xargs kill -9 2>/dev/null

sleep 2

echo ""
echo "✅ 系统已停止"
echo "======================================"