#!/bin/bash

# 股票全量数据同步启动脚本

echo "======================================"
echo "🚀 股票全量数据同步程序"
echo "======================================"

# 检查Python环境
if ! command -v python3 &> /dev/null; then
    echo "❌ Python3 未安装"
    exit 1
fi

# 检查依赖
echo "📋 检查依赖..."
python3 -c "import tushare, pandas, asyncio, aiohttp, tqdm" 2>/dev/null
if [ $? -ne 0 ]; then
    echo "❌ 缺少必要依赖，请安装: pip install tushare pandas aiohttp tqdm"
    exit 1
fi

# 创建日志目录
mkdir -p logs

# 提供选项菜单
echo ""
echo "请选择运行模式:"
echo "1. 启动异步全量同步"
echo "2. 监控同步进度"
echo "3. 查看同步汇总"
echo "4. 清除进度重新开始"
echo "5. 退出"
echo ""

read -p "请输入选项 (1-5): " choice

case $choice in
    1)
        echo "🚀 启动异步全量数据同步..."
        echo "💡 提示: 可以打开新终端运行 'python monitor_progress.py --monitor' 监控进度"
        echo ""
        python3 fetch_full_data_async.py
        ;;
    2)
        echo "📊 启动进度监控..."
        python3 monitor_progress.py --monitor
        ;;
    3)
        echo "📋 显示同步汇总..."
        python3 monitor_progress.py --summary
        ;;
    4)
        read -p "⚠️  确定要清除进度重新开始吗? (y/N): " confirm
        if [[ $confirm == [yY] || $confirm == [yY][eE][sS] ]]; then
            rm -f logs/fetch_progress.json
            echo "✅ 进度已清除"
        else
            echo "❌ 操作已取消"
        fi
        ;;
    5)
        echo "👋 退出"
        exit 0
        ;;
    *)
        echo "❌ 无效选项"
        exit 1
        ;;
esac