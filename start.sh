#!/bin/bash
# ljwx-stock 统一应用启动脚本

echo "🚀 ljwx-stock 统一应用启动"
echo "================================"

# 加载环境变量
if [ -f .env ]; then
    echo "📝 加载环境变量..."
    export $(cat .env | grep -v '^#' | xargs)
    echo "   ✅ 环境变量已加载"
else
    echo "   ⚠️  未找到.env文件，使用默认配置"
fi

# 启动应用
echo ""
echo "🌐 启动Web应用..."
python start_unified.py