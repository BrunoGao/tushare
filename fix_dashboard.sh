#!/bin/bash
# 投资策略仪表板修复脚本

echo "🔧 投资策略仪表板修复和启动脚本"
echo "=================================="

# 检查虚拟环境
if [ ! -d "tushare_venv" ]; then
    echo "❌ 虚拟环境不存在，请先创建虚拟环境"
    exit 1
fi

# 激活虚拟环境
source tushare_venv/bin/activate

# 停止现有服务
echo "🛑 停止现有服务..."
pkill -f "python.*api/app.py" 2>/dev/null || true

# 等待端口释放
sleep 2

# 检查数据目录
if [ ! -d "data" ]; then
    echo "📁 创建数据目录..."
    mkdir -p data
fi

echo "🚀 启动API服务..."
# 启动API服务（后台运行）
nohup ./tushare_venv/bin/python api/app.py > logs/api_server.log 2>&1 &

# 等待服务启动
echo "⏳ 等待服务启动..."
sleep 5

# 检查服务状态
if curl -s "http://localhost:5005/dashboard" > /dev/null; then
    echo "✅ 投资策略仪表板已启动成功！"
    echo ""
    echo "📊 访问地址："
    echo "   仪表板: http://localhost:5005/dashboard"
    echo "   API文档: http://localhost:5005/api/strategies/templates"
    echo ""
    echo "🎯 功能特性："
    echo "   • 18个主流投资策略模板"
    echo "   • 技术分析、基本面、量化、混合策略"
    echo "   • 4种风险等级：保守、稳健、激进、投机"
    echo "   • 策略克隆和自定义功能"
    echo "   • 实时策略表现监控"
    echo ""
    echo "🔍 如果仍然看到空页面，请："
    echo "   1. 清除浏览器缓存 (Ctrl+F5)"
    echo "   2. 检查浏览器控制台错误信息"
    echo "   3. 确保数据库服务正常运行"
    echo ""
    echo "📋 日志文件: logs/api_server.log"
else
    echo "❌ 服务启动失败，请检查日志文件"
    cat logs/api_server.log 2>/dev/null || echo "日志文件不存在"
fi