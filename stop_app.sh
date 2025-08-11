#!/bin/bash

# ljwx-stock 应用停止脚本

# 获取脚本所在目录
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

echo -e "${BLUE}======================================${NC}"
echo -e "${BLUE}    停止 ljwx-stock 系统服务${NC}"
echo -e "${BLUE}======================================${NC}"

# 停止任务调度器
if [ -f "data/scheduler.pid" ]; then
    SCHEDULER_PID=$(cat data/scheduler.pid)
    if kill -0 $SCHEDULER_PID 2>/dev/null; then
        echo -e "${CYAN}🛑 停止任务调度器 (PID: $SCHEDULER_PID)${NC}"
        kill $SCHEDULER_PID
        sleep 2
        
        # 强制停止（如果需要）
        if kill -0 $SCHEDULER_PID 2>/dev/null; then
            echo -e "${YELLOW}⚠️ 强制停止任务调度器${NC}"
            kill -9 $SCHEDULER_PID
        fi
        
        rm -f data/scheduler.pid
        echo -e "${GREEN}✅ 任务调度器已停止${NC}"
    else
        echo -e "${YELLOW}⚠️ 任务调度器进程不存在${NC}"
        rm -f data/scheduler.pid
    fi
else
    echo -e "${CYAN}ℹ️ 未找到任务调度器PID文件${NC}"
fi

# 停止主应用（通过进程名查找）
echo -e "${CYAN}🔍 查找并停止主应用进程...${NC}"
MAIN_PIDS=$(pgrep -f "python.*wsgi_app.py")
if [ ! -z "$MAIN_PIDS" ]; then
    echo -e "${CYAN}🛑 停止主应用进程: $MAIN_PIDS${NC}"
    kill $MAIN_PIDS
    sleep 3
    
    # 检查是否还有进程运行
    REMAINING_PIDS=$(pgrep -f "python.*wsgi_app.py")
    if [ ! -z "$REMAINING_PIDS" ]; then
        echo -e "${YELLOW}⚠️ 强制停止剩余进程: $REMAINING_PIDS${NC}"
        kill -9 $REMAINING_PIDS
    fi
    
    echo -e "${GREEN}✅ 主应用已停止${NC}"
else
    echo -e "${CYAN}ℹ️ 未找到运行中的主应用进程${NC}"
fi

# 停止数据库系统相关进程
DB_PIDS=$(pgrep -f "python.*database_system.py")
if [ ! -z "$DB_PIDS" ]; then
    echo -e "${CYAN}🛑 停止数据库系统进程: $DB_PIDS${NC}"
    kill $DB_PIDS
    sleep 2
    
    # 强制停止（如果需要）
    REMAINING_DB_PIDS=$(pgrep -f "python.*database_system.py")
    if [ ! -z "$REMAINING_DB_PIDS" ]; then
        echo -e "${YELLOW}⚠️ 强制停止数据库系统进程${NC}"
        kill -9 $REMAINING_DB_PIDS
    fi
    
    echo -e "${GREEN}✅ 数据库系统进程已停止${NC}"
fi

# 清理临时文件
echo -e "${CYAN}🧹 清理临时文件...${NC}"
rm -f data/scheduler.pid
rm -f server.pid

echo -e "${BLUE}======================================${NC}"
echo -e "${GREEN}✅ ljwx-stock 系统已完全停止${NC}"
echo -e "${BLUE}======================================${NC}"