#!/bin/bash

# 股票数据系统启动脚本

echo "🚀 启动股票数据系统"

# 检查并创建日志目录
mkdir -p logs

# 1. 启动数据同步（如果需要）
if [ "$1" = "sync" ]; then
    echo "📊 执行数据同步..."
    python scheduler/quick_data_sync.py --days 7
fi

# 2. 启动API服务器
echo "🌐 启动API服务器..."
python api/app.py &
API_PID=$!

# 3. 启动WebSocket实时服务器
echo "🔌 启动WebSocket实时服务器..."
python websocket/realtime_server.py &
WEBSOCKET_PID=$!

# 4. 启动定时调度器（可选）
if [ "$1" = "full" ]; then
    echo "⏰ 启动定时调度器..."
    python scheduler/stock_data_scheduler.py &
    SCHEDULER_PID=$!
fi

echo "✅ 系统启动完成！"
echo "📱 前端访问: http://localhost:5005"
echo "📊 技术分析: http://localhost:5005/analysis"
echo "🔌 WebSocket服务: ws://localhost:8765"
echo ""
echo "按 Ctrl+C 停止系统"

# 等待中断信号
trap 'echo "🛑 正在停止系统..."; kill $API_PID 2>/dev/null; kill $WEBSOCKET_PID 2>/dev/null; [ ! -z "$SCHEDULER_PID" ] && kill $SCHEDULER_PID 2>/dev/null; echo "✅ 系统已停止"; exit' INT

# 保持脚本运行
wait