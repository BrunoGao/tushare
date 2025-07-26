#!/usr/bin/env python3
import sys, os, argparse, subprocess
from datetime import datetime
from sqlalchemy import text

def print_banner():
    print("""
🚀 A股智能推荐系统
====================================
基于TuShare + MySQL + LLM的股票分析系统
支持历史数据获取、技术分析、智能推荐
====================================
""")

def run_command(cmd, description):
    print(f"\n📋 {description}")
    print(f"🔄 执行命令: {' '.join(cmd)}")
    print(f"⏰ 开始时间: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    try:
        result = subprocess.run(cmd, check=True, capture_output=True, text=True, encoding='utf-8')
        print(f"✅ 成功: {description}")
        if result.stdout: 
            print("📄 输出日志:")
            print(result.stdout)
        return True
    except subprocess.CalledProcessError as e:
        print(f"❌ 失败: {description}")
        print(f"🔍 错误详情:")
        if e.stdout: print(f"标准输出: {e.stdout}")
        if e.stderr: print(f"错误输出: {e.stderr}")
        return False
    finally:
        print(f"⏰ 结束时间: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")

def setup_project():  # 项目初始化
    print("🔧 初始化项目...")
    
    # 创建必要目录
    dirs = ['logs', 'data', 'sql']
    for d in dirs:
        os.makedirs(d, exist_ok=True)
        print(f"📁 创建目录: {d}")
        
    # 复制环境配置
    if not os.path.exists('.env'):
        if os.path.exists('env.example'):
            import shutil
            shutil.copy('env.example', '.env')
            print("📝 已创建 .env 文件，请编辑配置")
        else:
            print("⚠️  请手动创建 .env 文件")
            
    print("✅ 项目初始化完成")

def fetch_basic_data():  # 获取基础数据
    return run_command([sys.executable, 'scripts/fetch_stock_basic.py'], '获取股票基本信息')

def fetch_historical_data(mode='10years'):  # 获取历史数据
    if mode == 'plan':
        return run_command([sys.executable, 'scripts/fetch_stock_daily.py', 'plan'], '执行3天历史数据获取计划')
    elif mode == 'update':
        return run_command([sys.executable, 'scripts/fetch_stock_daily.py', 'update'], '每日数据更新')
    elif mode == '10years':
        return run_command([sys.executable, 'scripts/fetch_stock_daily.py', '10years'], '一次性获取所有股票最近10年数据')
    else:
        print("❌ 无效的获取模式")
        return False

def generate_recommendations(strategy='ma_crossover'):  # 生成推荐
    return run_command([sys.executable, 'analysis/recommender.py', strategy, '50'], f'生成{strategy}策略推荐')

def start_api_server():  # 启动API服务
    print("🌐 启动API服务器...")
    print("📱 访问地址: http://localhost:5000")
    print("📊 API文档: http://localhost:5000/api/health")
    print("⌨️  按 Ctrl+C 停止服务")
    
    try:
        subprocess.run([sys.executable, 'api/app.py'], check=True)
    except KeyboardInterrupt:
        print("\n🛑 API服务已停止")

def start_scheduler():  # 启动定时任务
    print("⏰ 启动定时任务调度器...")
    print("📅 任务计划: 交易日17:00数据更新, 18:00推荐更新")
    print("⌨️  按 Ctrl+C 停止调度器")
    
    try:
        subprocess.run([sys.executable, 'scheduler/daily_tasks.py'], check=True)
    except KeyboardInterrupt:
        print("\n🛑 定时任务已停止")

def run_manual_task(task):  # 手动执行任务
    if task == 'update':
        return run_command([sys.executable, 'scheduler/daily_tasks.py', 'update'], '手动数据更新')
    elif task == 'recommend':
        return run_command([sys.executable, 'scheduler/daily_tasks.py', 'recommend'], '手动推荐更新')
    elif task == 'cleanup':
        return run_command([sys.executable, 'scheduler/daily_tasks.py', 'cleanup'], '手动清理任务')
    else:
        print("❌ 无效的任务类型")
        return False

def test_llm(test_type='ask'):  # 测试LLM功能
    if test_type == 'ask':
        return run_command([sys.executable, 'llm/interface.py', 'ask', '今日推荐哪些股票？'], 'LLM问答测试')
    elif test_type == 'trend':
        return run_command([sys.executable, 'llm/interface.py', 'trend', '银行'], 'LLM趋势分析测试')
    elif test_type == 'advice':
        return run_command([sys.executable, 'llm/interface.py', 'advice', 'medium', '10000'], 'LLM投资建议测试')
    else:
        print("❌ 无效的测试类型")
        return False

def docker_deploy():  # Docker部署
    print("🐳 Docker容器化部署...")
    
    # 检查Docker
    if not run_command(['docker', '--version'], '检查Docker'):
        return False
        
    if not run_command(['docker-compose', '--version'], '检查Docker Compose'):
        return False
        
    # 启动服务
    return run_command(['docker-compose', 'up', '-d'], '启动Docker服务')

def show_status():  # 显示系统状态
    print("📊 系统状态检查")
    
    # 检查API服务
    try:
        import requests
        response = requests.get('http://localhost:5000/api/health', timeout=3)
        if response.status_code == 200:
            print("✅ API服务: 运行中")
        else:
            print("❌ API服务: 异常")
    except:
        print("❌ API服务: 未启动")
        
    # 检查数据库连接
    try:
        from utils.db_helper import db
        with db.engine.connect() as conn:
            result = conn.execute(text("SELECT COUNT(*) FROM stock_basic")).scalar()
            print(f"✅ 数据库: 已连接，股票数量: {result}")
    except Exception as e:
        print(f"❌ 数据库: 连接失败 - {e}")
        
    # 检查LLM服务
    try:
        from llm.interface import llm
        if llm.health_check():
            print("✅ LLM服务: 可用")
        else:
            print("❌ LLM服务: 不可用")
    except:
        print("❌ LLM服务: 未配置")

def main():
    print_banner()
    
    parser = argparse.ArgumentParser(description='A股智能推荐系统管理工具')
    parser.add_argument('command', choices=[
        'fetch-history', 'fetch-10years', 'serve-api', 'recommend', 
        'fetch-comprehensive', 'scheduler', 'init-schema', 'health-check'
    ], help='要执行的命令')
    parser.add_argument('--category', choices=['basic', 'financial', 'money_flow', 
                                              'shareholder', 'announcement', 'market_ext', 'macro', 'all'],
                       default='all', help='数据类别 (用于fetch-comprehensive)')
    parser.add_argument('--mode', choices=['full', 'incremental'], default='incremental',
                       help='获取模式 (用于fetch-comprehensive)')
    parser.add_argument('--task', help='任务名称 (用于scheduler)')
    parser.add_argument('--scheduler-cmd', choices=['start', 'status', 'run', 'health'], 
                       help='调度器命令')
    
    args = parser.parse_args()
    
    # 路由执行
    if args.command == 'init':
        setup_project()
        
    elif args.command == 'fetch-basic':
        fetch_basic_data()
        
    elif args.command == 'fetch-history':
        fetch_historical_data('plan')
        
    elif args.command == 'fetch-update':
        fetch_historical_data('update')
        
    elif args.command == 'fetch-10years':
        print("🚀 开始获取所有股票最近10年历史数据...")
        fetch_historical_data('10years')
        
    elif args.command == 'serve-api':
        print("🌐 启动API服务...")
        run_command(['python3', 'api/app.py'], "启动API服务")
        
    elif args.command == 'recommend':
        print("💡 生成股票推荐...")
        run_command(['python3', '-c', 'from analysis.recommender import recommender; print(recommender.get_recommendations())'], 
                   "生成股票推荐")
        
    elif args.command == 'fetch-comprehensive':
        print(f"📊 开始获取A股全维度数据 - 类别: {args.category}, 模式: {args.mode}")
        cmd = ['python3', 'scripts/fetch_comprehensive_data.py', 
               '--category', args.category, '--mode', args.mode]
        run_command(cmd, f"获取{args.category}数据")
        
    elif args.command == 'scheduler':
        if not args.scheduler_cmd:
            print("❌ 请指定调度器命令 --scheduler-cmd")
            return
            
        print(f"⏰ 执行调度器命令: {args.scheduler_cmd}")
        cmd = ['python3', 'utils/data_scheduler.py', args.scheduler_cmd]
        
        if args.scheduler_cmd == 'run' and args.task:
            cmd.extend(['--task', args.task, '--mode', args.mode])
            
        run_command(cmd, f"调度器{args.scheduler_cmd}")
        
    elif args.command == 'init-schema':
        print("🔧 初始化全维度数据库表结构...")
        run_command(['python3', '-c', 
                    'from utils.comprehensive_data_schema import ComprehensiveDataSchema; '
                    'from utils.db_helper import db; '
                    'ComprehensiveDataSchema.create_all_tables(db)'], 
                   "初始化数据库表结构")
        
    elif args.command == 'health-check':
        print("🏥 执行系统健康检查...")
        run_command(['python3', 'utils/data_scheduler.py', 'health'], "系统健康检查")
        
    elif args.command == 'llm-test':
        test_llm(args.llm_test)
        
    elif args.command == 'docker':
        docker_deploy()
        
    elif args.command == 'status':
        show_status()
        
    else:
        parser.print_help()

if __name__ == "__main__":
    main() 