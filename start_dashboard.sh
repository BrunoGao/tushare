#!/bin/bash
# TuShare 市场数据仪表板启动脚本

echo "🚀 启动TuShare市场数据仪表板系统"
echo "======================================"

# 检查Python环境
if ! command -v python &> /dev/null; then
    echo "❌ Python未找到，请先安装Python"
    exit 1
fi

# 检查数据库连接
echo "🔍 检查数据库连接..."
python -c "from database.db_manager import DatabaseManager; db = DatabaseManager(); print('✅ 数据库连接成功')" 2>/dev/null
if [ $? -ne 0 ]; then
    echo "❌ 数据库连接失败，请检查配置"
    exit 1
fi

# 检查端口占用
PORT=5005
if lsof -ti:$PORT > /dev/null 2>&1; then
    echo "⚠️ 端口 $PORT 被占用，尝试释放..."
    lsof -ti:$PORT | xargs kill -9 2>/dev/null
    sleep 2
fi

# 创建必要的目录
mkdir -p logs

echo "📊 启动历史数据同步进程（后台运行）..."
nohup python scheduler/historical_data_sync.py > logs/historical_sync.log 2>&1 &
HISTORICAL_PID=$!
echo "✅ 历史数据同步进程启动: PID $HISTORICAL_PID"

echo "🔧 启动技术指标修复进程（后台运行）..."
nohup python scheduler/fix_technical_indicators.py > logs/fix_indicators.log 2>&1 &
INDICATORS_PID=$!
echo "✅ 技术指标修复进程启动: PID $INDICATORS_PID"

echo "🌐 启动Web API服务器..."
python api/app.py &
API_PID=$!

echo ""
echo "🎉 系统启动完成！"
echo "======================================"
echo "📊 市场仪表板: http://localhost:5005/market-dashboard"
echo "📈 API服务器: http://localhost:5005"
echo ""
echo "后台进程:"
echo "  - 历史数据同步: PID $HISTORICAL_PID"
echo "  - 技术指标修复: PID $INDICATORS_PID" 
echo "  - Web API服务器: PID $API_PID"
echo ""
echo "📋 查看日志:"
echo "  - tail -f logs/historical_sync.log"
echo "  - tail -f logs/fix_indicators.log"
echo ""
echo "🛑 停止系统: ./stop_dashboard.sh"

# 等待API服务器启动
sleep 3

# 检查服务器状态
if curl -s http://localhost:5005/api/market/stats > /dev/null; then
    echo "✅ API服务器运行正常"
else
    echo "⚠️ API服务器可能启动异常，请检查日志"
fi

# 保存进程ID
echo "$HISTORICAL_PID" > .dashboard_pids
echo "$INDICATORS_PID" >> .dashboard_pids
echo "$API_PID" >> .dashboard_pids

echo ""
echo "系统运行中... 按Ctrl+C退出"

# 捕获退出信号
trap 'echo "正在停止系统..."; kill $HISTORICAL_PID $INDICATORS_PID $API_PID 2>/dev/null; rm -f .dashboard_pids; exit' INT

# 等待进程
wait $API_PID