#!/bin/bash

# ljwx-stock 应用启动脚本 - 集成数据库优化系统

# 获取脚本所在目录
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

echo -e "${BLUE}======================================${NC}"
echo -e "${BLUE}    ljwx-stock 专业投资策略系统${NC}"
echo -e "${BLUE}======================================${NC}"

# 检查虚拟环境
if [ ! -f "./tushare_venv/bin/python" ]; then
    echo -e "${RED}❌ 虚拟环境未找到，请先创建虚拟环境${NC}"
    exit 1
fi

# 检查配置文件
CONFIG_FILE="database_config_sqlite.json"
if [ -f "database_config.json" ]; then
    CONFIG_FILE="database_config.json"
fi

if [ ! -f "$CONFIG_FILE" ]; then
    echo -e "${RED}❌ 配置文件未找到: $CONFIG_FILE${NC}"
    exit 1
fi

# 检查.env文件
if [ ! -f ".env" ]; then
    echo -e "${YELLOW}⚠️ .env文件未找到，使用配置文件中的设置${NC}"
    echo -e "${CYAN}📋 参考env.example创建.env文件以覆盖默认配置${NC}"
fi

# 安装依赖包
echo -e "${CYAN}📦 检查和安装依赖包...${NC}"
./tushare_venv/bin/pip install -q -r requirements.txt
./tushare_venv/bin/pip install -q mysql-connector-python tushare ratelimit apscheduler redis psutil numba

# 创建必要目录
echo -e "${CYAN}📁 创建必要目录结构...${NC}"
mkdir -p data logs temp cache

# 数据库优化系统初始化
echo -e "${PURPLE}🔧 初始化数据库优化系统...${NC}"

# 1. 初始化数据库结构
echo -e "${CYAN}  ✅ 步骤1: 初始化数据库结构${NC}"
./tushare_venv/bin/python database/database_system.py -c "$CONFIG_FILE" --command init --create-db

if [ $? -eq 0 ]; then
    echo -e "${GREEN}  ✅ 数据库结构初始化完成${NC}"
else
    echo -e "${YELLOW}  ⚠️ 数据库可能已存在，继续启动${NC}"
fi

# 2. 检查系统状态
echo -e "${CYAN}  ✅ 步骤2: 检查系统组件状态${NC}"
./tushare_venv/bin/python database/database_system.py -c "$CONFIG_FILE" --command status

# 3. 可选：同步基础数据（首次启动时）
FIRST_RUN_FLAG="data/.first_run_completed"
if [ ! -f "$FIRST_RUN_FLAG" ]; then
    echo -e "${CYAN}  ✅ 步骤3: 首次运行 - 同步股票基础信息${NC}"
    echo -e "${YELLOW}  📊 这可能需要几分钟时间...${NC}"
    
    timeout 300 ./tushare_venv/bin/python database/database_system.py -c "$CONFIG_FILE" --command sync-basic
    
    if [ $? -eq 0 ]; then
        echo -e "${GREEN}  ✅ 股票基础信息同步完成${NC}"
        touch "$FIRST_RUN_FLAG"
    else
        echo -e "${YELLOW}  ⚠️ 基础数据同步超时或失败，系统仍可启动${NC}"
    fi
else
    echo -e "${GREEN}  ✅ 步骤3: 基础数据已存在，跳过同步${NC}"
fi

# 4. 启动后台任务调度器
echo -e "${CYAN}  ✅ 步骤4: 启动数据同步调度器${NC}"
nohup ./tushare_venv/bin/python database/database_system.py -c "$CONFIG_FILE" --command start-scheduler > logs/scheduler.log 2>&1 &
SCHEDULER_PID=$!

if [ $? -eq 0 ]; then
    echo -e "${GREEN}  ✅ 任务调度器已启动 (PID: $SCHEDULER_PID)${NC}"
    echo $SCHEDULER_PID > data/scheduler.pid
else
    echo -e "${YELLOW}  ⚠️ 任务调度器启动失败，手动模式运行${NC}"
fi

sleep 2

echo -e "${PURPLE}🚀 启动主应用服务...${NC}"
echo -e "${CYAN}📁 工作目录: $SCRIPT_DIR${NC}"
echo -e "${CYAN}🐍 Python路径: ./tushare_venv/bin/python${NC}"
echo -e "${CYAN}⚙️ 配置文件: $CONFIG_FILE${NC}"
echo -e "${BLUE}======================================${NC}"

# 定义清理函数
cleanup() {
    echo -e "\n${YELLOW}🛑 正在停止服务...${NC}"
    if [ -f "data/scheduler.pid" ]; then
        SCHEDULER_PID=$(cat data/scheduler.pid)
        if kill -0 $SCHEDULER_PID 2>/dev/null; then
            echo -e "${CYAN}  停止任务调度器 (PID: $SCHEDULER_PID)${NC}"
            kill $SCHEDULER_PID
            rm -f data/scheduler.pid
        fi
    fi
    echo -e "${GREEN}✅ 服务已停止${NC}"
    exit 0
}

# 设置信号处理
trap cleanup SIGINT SIGTERM

# 启动主应用
./tushare_venv/bin/python wsgi_app.py