#!/usr/bin/env python3
"""
A股全维度数据获取系统演示脚本
展示基于TuShare Pro的多维度数据获取和调度管理功能
"""
import logging
from datetime import datetime
import sys
import os
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

# 配置日志
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)

def demo_system_overview():
    """演示系统概览"""
    print("🎯 A股全维度数据获取系统演示")
    print("=" * 80)
    print()
    
    print("📊 支持的数据维度:")
    print("  1. 基本面信息 - 股票基本信息、行业分类、指数成分、股本结构、公司高管")
    print("  2. 财务类信息 - 资产负债表、利润表、现金流量表、财务指标、业绩预告/快报")
    print("  3. 资金流向   - 个股资金流、龙虎榜数据、主力资金监控")
    print("  4. 股东及股权 - 十大股东、流通股东、股东户数、限售解禁")
    print("  5. 公告类信息 - 分红送股、股票回购、重要公告")
    print("  6. 行情扩展   - 停复牌、涨跌停、概念板块分类")
    print("  7. 宏观经济   - 沪深港通、融资融券、CPI/PPI/GDP等宏观指标")
    print()
    
    print("⏰ 智能调度功能:")
    print("  • 定时任务调度 (基本信息周更新、财务季更新、资金流日更新)")
    print("  • 错误重试机制 (指数退避策略、超时控制)")
    print("  • 健康监控检查 (数据库连接、API状态、磁盘空间)")
    print("  • 手动任务执行 (支持全量/增量模式)")
    print()
    
def demo_database_tables():
    """演示数据库表结构"""
    print("🗂️ 数据库表结构预览:")
    print("-" * 80)
    
    tables_info = [
        ("t_stock_basic", "股票基本信息表", "ts_code, name, industry, area, list_date等"),
        ("t_balance_sheet", "资产负债表", "total_assets, total_liab, total_hldr_eqy等"),
        ("t_income_statement", "利润表", "total_revenue, operate_profit, n_income等"),
        ("t_cashflow_statement", "现金流量表", "n_cashflow_act, free_cashflow等"),
        ("t_financial_indicators", "财务指标表", "roe, roa, eps, bps等关键指标"),
        ("t_money_flow", "个股资金流向表", "net_mf_amount, buy_lg_amount等"),
        ("t_top10_holders", "十大股东表", "holder_name, hold_amount, hold_ratio等"),
        ("t_dividend", "分红送股表", "cash_div, stk_div, ex_date等"),
        ("t_money_flow_hsgt", "沪深港通资金流向表", "north_money, south_money等"),
    ]
    
    for table_name, description, fields in tables_info:
        print(f"📋 {table_name:<25} {description:<15} 主要字段: {fields}")
    
    print(f"\n✅ 总计创建 30+ 张专业数据表，覆盖A股数据全维度")
    print()

def demo_command_usage():
    """演示命令行使用方法"""
    print("🛠️ 命令行使用示例:")
    print("-" * 80)
    
    commands = [
        ("python3 run.py init-schema", "初始化全维度数据库表结构"),
        ("python3 run.py fetch-comprehensive --mode full", "全量获取所有维度数据"),
        ("python3 run.py fetch-comprehensive --category basic", "增量更新基本面数据"),
        ("python3 run.py fetch-comprehensive --category financial", "增量更新财务数据"),
        ("python3 run.py scheduler --scheduler-cmd start", "启动定时调度器"),
        ("python3 run.py scheduler --scheduler-cmd status", "查看调度状态"),
        ("python3 run.py scheduler --scheduler-cmd run --task money_flow", "手动执行资金流任务"),
        ("python3 run.py health-check", "系统健康检查"),
    ]
    
    for command, description in commands:
        print(f"💻 {command:<55} # {description}")
    
    print()

def demo_data_categories():
    """演示数据类别说明"""
    print("📊 数据类别详细说明:")
    print("-" * 80)
    
    categories = {
        "basic": {
            "name": "基本面信息",
            "update_freq": "每周更新",
            "tables": ["t_stock_basic", "t_industry_classification", "t_index_components", "t_share_structure", "t_company_managers"],
            "description": "股票基础信息、行业分类、指数成分等静态数据"
        },
        "financial": {
            "name": "财务类信息", 
            "update_freq": "季度更新",
            "tables": ["t_balance_sheet", "t_income_statement", "t_cashflow_statement", "t_financial_indicators", "t_performance_forecast"],
            "description": "三大报表、财务指标、业绩预告等财务数据"
        },
        "money_flow": {
            "name": "资金流向",
            "update_freq": "每日更新",
            "tables": ["t_money_flow", "t_dragon_tiger_list"],
            "description": "个股资金流向、龙虎榜等资金监控数据"
        },
        "shareholder": {
            "name": "股东及股权",
            "update_freq": "季度更新", 
            "tables": ["t_top10_holders", "t_top10_float_holders", "t_holder_number", "t_share_float"],
            "description": "股东结构、持股变化、解禁信息等"
        },
        "announcement": {
            "name": "公告类信息",
            "update_freq": "每日更新",
            "tables": ["t_dividend", "t_repurchase"],
            "description": "分红送股、股票回购等重要公告"
        },
        "market_ext": {
            "name": "行情扩展", 
            "update_freq": "每日更新",
            "tables": ["t_suspend", "t_limit_price", "t_concept", "t_concept_detail"],
            "description": "停复牌、涨跌停、概念板块等行情扩展数据"
        },
        "macro": {
            "name": "宏观经济",
            "update_freq": "每月更新",
            "tables": ["t_money_flow_hsgt", "t_margin_detail", "t_macro_indicators"],
            "description": "沪深港通、融资融券、宏观经济指标等"
        }
    }
    
    for category, info in categories.items():
        print(f"🔹 {category:<15} {info['name']:<12} ({info['update_freq']})")
        print(f"   📋 涉及表: {', '.join(info['tables'][:3])}{'...' if len(info['tables']) > 3 else ''}")
        print(f"   📖 说明: {info['description']}")
        print()

def demo_scheduler_features():
    """演示调度器功能"""
    print("⏰ 智能调度系统功能:")
    print("-" * 80)
    
    print("🕒 定时任务配置:")
    schedule_config = [
        ("stock_basic", "基本信息", "每周一 09:00", "1小时", "优先级1"),
        ("financial_data", "财务数据", "每月15日 10:00", "2小时", "优先级2"),
        ("money_flow", "资金流向", "每日 18:30", "30分钟", "优先级3"),
        ("shareholder_data", "股东数据", "每月20日 11:00", "1小时", "优先级4"),
        ("announcement_data", "公告数据", "每日 19:00", "15分钟", "优先级5"),
        ("market_extension", "行情扩展", "每日 19:30", "15分钟", "优先级6"),
        ("macro_data", "宏观数据", "每月5日 12:00", "30分钟", "优先级7"),
    ]
    
    for task, name, schedule, timeout, priority in schedule_config:
        print(f"  📅 {task:<18} {name:<8} {schedule:<15} 超时:{timeout:<8} {priority}")
    
    print()
    print("🔧 错误处理机制:")
    print("  • 自动重试: 失败任务最多重试3次")
    print("  • 指数退避: 重试间隔递增 (60秒、120秒、180秒)")
    print("  • 超时控制: 各任务设置不同超时时间")
    print("  • 状态持久化: 任务状态保存到JSON文件")
    print()
    
    print("🏥 健康监控:")
    print("  • 数据库连接检查")
    print("  • TuShare API连通性检查")
    print("  • 磁盘空间检查 (>1GB)")
    print("  • 最近成功任务统计")
    print()

def demo_technical_features():
    """演示技术特性"""
    print("🛠️ 技术特性与优化:")
    print("-" * 80)
    
    print("📊 数据处理优化:")
    print("  • 批量upsert操作 - 高效插入/更新大量数据")
    print("  • 断点续传机制 - 避免重复获取，支持增量更新")
    print("  • 数据类型优化 - 自动处理NaN值转换")
    print("  • 分批处理 - 避免内存溢出和API限制")
    print()
    
    print("⚡ 性能优化:")
    print("  • 多线程并发 - 配置并发线程数提升效率")
    print("  • 速率限制控制 - 遵循TuShare API调用限制")
    print("  • 连接池管理 - 数据库连接复用")
    print("  • 索引优化 - 关键字段建立索引提升查询性能")
    print()
    
    print("🔒 稳定性保障:")
    print("  • 异常捕获 - 完善的错误处理机制")
    print("  • 日志记录 - 详细的操作日志和错误追踪")
    print("  • 事务管理 - 保证数据一致性")
    print("  • 资源清理 - 自动释放连接和内存")
    print()

def demo_integration_potential():
    """演示集成潜力"""
    print("🚀 后续扩展与集成潜力:")
    print("-" * 80)
    
    print("🤖 LLM分析集成:")
    print("  • 基于全维度数据的智能股票分析")
    print("  • 财务健康度评估和风险预警")
    print("  • 个性化投资建议生成")
    print("  • 自然语言查询和问答系统")
    print()
    
    print("📈 推荐算法扩展:")
    print("  • 多因子量化选股模型")
    print("  • 基于资金流的主力追踪")
    print("  • 行业轮动和板块分析")
    print("  • 事件驱动的投资机会发现")
    print()
    
    print("📊 可视化与前端:")
    print("  • 实时数据大屏展示")
    print("  • 交互式财务报表分析")
    print("  • 股东变化可视化")
    print("  • 资金流向动态图表")
    print()
    
    print("🔗 外部系统集成:")
    print("  • 交易系统接口对接")
    print("  • 风控系统数据源")
    print("  • 研报生成系统")
    print("  • 监管报告自动化")
    print()

def main():
    """主演示函数"""
    print(f"🎬 A股全维度数据获取系统演示")
    print(f"📅 演示时间: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    print("=" * 100)
    print()
    
    try:
        demo_system_overview()
        input("按回车键继续...")
        print()
        
        demo_database_tables()
        input("按回车键继续...")
        print()
        
        demo_data_categories()
        input("按回车键继续...")
        print()
        
        demo_command_usage()
        input("按回车键继续...")
        print()
        
        demo_scheduler_features()
        input("按回车键继续...")
        print()
        
        demo_technical_features()
        input("按回车键继续...")
        print()
        
        demo_integration_potential()
        
        print("🎉 演示完成！")
        print()
        print("💡 快速开始:")
        print("1. python3 run.py init-schema")
        print("2. python3 run.py fetch-comprehensive --category basic --mode incremental")
        print("3. python3 run.py scheduler --scheduler-cmd status")
        print("4. python3 run.py health-check")
        
    except KeyboardInterrupt:
        print("\n🛑 演示被用户中断")
    except Exception as e:
        logger.error(f"❌ 演示过程中出现错误: {e}")

if __name__ == "__main__":
    main() 