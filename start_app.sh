#!/bin/bash

# ljwx-stock 应用启动脚本

# 获取脚本所在目录
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# 检查虚拟环境
if [ ! -f "./tushare_venv/bin/python" ]; then
    echo "❌ 虚拟环境未找到，请先创建虚拟环境"
    exit 1
fi

# 检查.env文件
if [ ! -f ".env" ]; then
    echo "⚠️ .env文件未找到，请确保已配置环境变量"
    echo "📋 参考env.example创建.env文件"
fi

# 安装依赖包
echo "📦 安装/更新依赖包..."
./tushare_venv/bin/pip install -r requirements.txt

echo "🚀 启动 ljwx-stock 应用..."
echo "📁 工作目录: $SCRIPT_DIR"
echo "🐍 Python路径: ./tushare_venv/bin/python"
echo "=========================="

# 启动应用
./tushare_venv/bin/python wsgi_app.py