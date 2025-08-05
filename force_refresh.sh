#!/bin/bash

echo "🔄 强制刷新应用..."

# 停止现有进程
pkill -f "python.*wsgi_app" 2>/dev/null || true
pkill -f "python.*unified_app" 2>/dev/null || true

sleep 2

# 清理Python缓存
find . -name "*.pyc" -delete 2>/dev/null || true
find . -name "__pycache__" -type d -exec rm -rf {} + 2>/dev/null || true

echo "✅ 缓存已清理"

# 重新启动应用
echo "🚀 重新启动应用..."
./start_app.sh
