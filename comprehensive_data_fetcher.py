#!/usr/bin/env python3
"""
统一的综合数据抓取管理系统
整合所有数据抓取模块，提供统一的管理界面
"""

import os
import sys
import time
import json
import subprocess
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Tuple
import logging
import argparse
from concurrent.futures import ThreadPoolExecutor, as_completed
import threading

# 添加项目根目录到路径
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

# 配置日志
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler('logs/comprehensive_fetch.log'),
        logging.StreamHandler()
    ]
)
logger = logging.getLogger(__name__)

class ComprehensiveDataFetcher:
    """综合数据抓取管理器"""
    
    def __init__(self):
        self.progress_file = 'logs/comprehensive_fetch_progress.json'
        self.stats = {
            'start_time': datetime.now().isoformat(),
            'completed_modules': [],
            'failed_modules': [],
            'total_records': 0,
            'duration': 0
        }
        self.lock = threading.Lock()
        
        # 定义数据抓取模块
        self.fetch_modules = {
            'sector': {
                'name': '板块数据',
                'script': 'fetch_sector_data_simple.py',
                'priority': 1,
                'description': '行业分类、概念板块(简化版)',
                'estimated_time': 10  # 预估耗时(秒)
            },
            'moneyflow': {
                'name': '资金流数据',
                'script': 'fetch_moneyflow_data.py',
                'priority': 2,
                'description': '个股资金流、板块资金流、沪深港通',
                'estimated_time': 1800
            },
            'holder': {
                'name': '股东数据',
                'script': 'fetch_holder_data.py',
                'priority': 3,
                'description': '股东人数、前十大股东、增减持',
                'estimated_time': 1200
            },
            'top_list': {
                'name': '龙虎榜数据',
                'script': 'fetch_toplist_data.py',
                'priority': 4,
                'description': '龙虎榜明细、机构席位',
                'estimated_time': 600
            },
            'fund': {
                'name': '基金数据',
                'script': 'fetch_fund_data.py',
                'priority': 5,
                'description': '基金基本信息、净值、持仓',
                'estimated_time': 2400
            },
            'financial': {
                'name': '财务报表',
                'script': 'fetch_financial_data.py',
                'priority': 6,
                'description': '利润表、资产负债表、现金流量表',
                'estimated_time': 3000
            },
            'dividend': {
                'name': '分红配股',
                'script': 'fetch_dividend_data.py',
                'priority': 7,
                'description': '分红送股、配股、股本变动',
                'estimated_time': 900
            }
        }
    
    def display_menu(self):
        """显示菜单"""
        print("\n" + "="*80)
        print("🚀 TuShare 综合数据抓取系统")
        print("="*80)
        
        print("\n📊 可用数据模块:")
        for key, module in self.fetch_modules.items():
            status = "✅" if key in self.stats['completed_modules'] else "⏳"
            est_time = f"{module['estimated_time']//60}分{module['estimated_time']%60}秒"
            print(f"  {status} [{key}] {module['name']} - {module['description']} (预估: {est_time})")
        
        print(f"\n📈 执行选项:")
        print(f"  [all]     抓取所有数据 (预估总时间: {sum(m['estimated_time'] for m in self.fetch_modules.values())//60}分钟)")
        print(f"  [basic]   抓取基础数据 (sector + moneyflow + holder)")
        print(f"  [recent]  抓取近期数据 (moneyflow + top_list)")
        print(f"  [status]  查看抓取状态")
        print(f"  [clean]   清理日志文件")
        print(f"  [exit]    退出程序")
        
    def execute_module(self, module_key: str, args: List[str] = None) -> Tuple[bool, str]:
        """执行单个数据抓取模块"""
        module = self.fetch_modules.get(module_key)
        if not module:
            return False, f"未找到模块: {module_key}"
        
        script_path = module['script']
        if not os.path.exists(script_path):
            return False, f"脚本文件不存在: {script_path}"
        
        logger.info(f"🚀 开始执行: {module['name']}")
        
        try:
            # 构建命令
            cmd = ['python', script_path]
            if args:
                cmd.extend(args)
            
            # 执行脚本
            start_time = time.time()
            result = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                timeout=module['estimated_time'] * 2  # 超时时间为预估时间的2倍
            )
            
            duration = time.time() - start_time
            
            if result.returncode == 0:
                with self.lock:
                    if module_key not in self.stats['completed_modules']:
                        self.stats['completed_modules'].append(module_key)
                
                logger.info(f"✅ {module['name']} 执行成功 (耗时: {duration:.1f}秒)")
                return True, f"执行成功，耗时: {duration:.1f}秒"
            else:
                error_msg = result.stderr or result.stdout or "未知错误"
                with self.lock:
                    self.stats['failed_modules'].append({
                        'module': module_key,
                        'error': error_msg,
                        'time': datetime.now().isoformat()
                    })
                
                logger.error(f"❌ {module['name']} 执行失败: {error_msg}")
                return False, f"执行失败: {error_msg}"
                
        except subprocess.TimeoutExpired:
            error_msg = f"执行超时 (>{module['estimated_time'] * 2}秒)"
            logger.error(f"❌ {module['name']} {error_msg}")
            return False, error_msg
            
        except Exception as e:
            error_msg = str(e)
            logger.error(f"❌ {module['name']} 执行异常: {error_msg}")
            return False, error_msg
    
    def execute_batch(self, module_keys: List[str], parallel: bool = False) -> Dict[str, Tuple[bool, str]]:
        """批量执行数据抓取模块"""
        results = {}
        
        if parallel:
            # 并行执行
            with ThreadPoolExecutor(max_workers=3) as executor:
                future_to_module = {
                    executor.submit(self.execute_module, key): key 
                    for key in module_keys
                }
                
                for future in as_completed(future_to_module):
                    module_key = future_to_module[future]
                    try:
                        success, message = future.result()
                        results[module_key] = (success, message)
                    except Exception as e:
                        results[module_key] = (False, str(e))
        else:
            # 串行执行
            for module_key in module_keys:
                success, message = self.execute_module(module_key)
                results[module_key] = (success, message)
                
                # 保存进度
                self.save_progress()
                
                # 如果失败，询问是否继续
                if not success:
                    logger.warning(f"⚠️ {self.fetch_modules[module_key]['name']} 执行失败")
                    response = input("是否继续执行其他模块? (y/n): ").lower()
                    if response != 'y':
                        break
        
        return results
    
    def show_status(self):
        """显示抓取状态"""
        print("\n" + "="*60)
        print("📊 数据抓取状态报告")
        print("="*60)
        
        print(f"\n✅ 已完成模块 ({len(self.stats['completed_modules'])}):")
        for module_key in self.stats['completed_modules']:
            module = self.fetch_modules[module_key]
            print(f"  ✓ {module['name']} - {module['description']}")
        
        print(f"\n❌ 失败模块 ({len(self.stats['failed_modules'])}):")
        for failed in self.stats['failed_modules']:
            module_key = failed['module']
            module = self.fetch_modules[module_key]
            print(f"  ✗ {module['name']} - {failed['error'][:100]}...")
        
        remaining = set(self.fetch_modules.keys()) - set(self.stats['completed_modules']) - {f['module'] for f in self.stats['failed_modules']}
        print(f"\n⏳ 待处理模块 ({len(remaining)}):")
        for module_key in remaining:
            module = self.fetch_modules[module_key]
            print(f"  ○ {module['name']} - {module['description']}")
        
        if self.stats.get('start_time'):
            start_time = datetime.fromisoformat(self.stats['start_time'])
            duration = (datetime.now() - start_time).total_seconds()
            print(f"\n⏱️ 总耗时: {duration//60:.0f}分{duration%60:.0f}秒")
    
    def clean_logs(self):
        """清理日志文件"""
        try:
            log_dir = 'logs'
            if os.path.exists(log_dir):
                log_files = [f for f in os.listdir(log_dir) if f.endswith('.log')]
                for log_file in log_files:
                    file_path = os.path.join(log_dir, log_file)
                    # 只保留最近的日志
                    if os.path.getmtime(file_path) < time.time() - 7*24*3600:  # 7天前
                        os.remove(file_path)
                        print(f"🗑️ 删除旧日志: {log_file}")
                
                print("✅ 日志清理完成")
            else:
                print("📁 日志目录不存在")
                
        except Exception as e:
            logger.error(f"❌ 清理日志失败: {e}")
    
    def save_progress(self):
        """保存进度"""
        try:
            os.makedirs('logs', exist_ok=True)
            self.stats['last_update'] = datetime.now().isoformat()
            with open(self.progress_file, 'w') as f:
                json.dump(self.stats, f, indent=2, ensure_ascii=False)
        except Exception as e:
            logger.error(f"❌ 保存进度失败: {e}")
    
    def load_progress(self):
        """加载进度"""
        try:
            if os.path.exists(self.progress_file):
                with open(self.progress_file, 'r') as f:
                    saved_stats = json.load(f)
                    self.stats.update(saved_stats)
                logger.info("📖 已加载历史进度")
        except Exception as e:
            logger.warning(f"⚠️ 加载进度失败: {e}")
    
    def interactive_mode(self):
        """交互模式"""
        self.load_progress()
        
        while True:
            self.display_menu()
            
            choice = input(f"\n请选择操作: ").strip().lower()
            
            if choice == 'exit':
                print("👋 退出程序")
                break
                
            elif choice == 'status':
                self.show_status()
                input("\n按回车键继续...")
                
            elif choice == 'clean':
                self.clean_logs()
                input("\n按回车键继续...")
                
            elif choice == 'all':
                print("🚀 开始抓取所有数据...")
                module_keys = list(self.fetch_modules.keys())
                results = self.execute_batch(module_keys)
                
                print(f"\n📊 执行结果:")
                for key, (success, message) in results.items():
                    status = "✅" if success else "❌"
                    print(f"  {status} {self.fetch_modules[key]['name']}: {message}")
                
                input("\n按回车键继续...")
                
            elif choice == 'basic':
                print("🚀 开始抓取基础数据...")
                basic_modules = ['sector', 'moneyflow', 'holder']
                results = self.execute_batch(basic_modules)
                
                print(f"\n📊 执行结果:")
                for key, (success, message) in results.items():
                    status = "✅" if success else "❌"
                    print(f"  {status} {self.fetch_modules[key]['name']}: {message}")
                
                input("\n按回车键继续...")
                
            elif choice == 'recent':
                print("🚀 开始抓取近期数据...")
                recent_modules = ['moneyflow', 'top_list']
                
                # 添加日期参数
                today = datetime.now().strftime('%Y%m%d')
                week_ago = (datetime.now() - timedelta(days=7)).strftime('%Y%m%d')
                
                for module_key in recent_modules:
                    success, message = self.execute_module(
                        module_key, 
                        ['--start-date', week_ago, '--end-date', today]
                    )
                    status = "✅" if success else "❌"
                    print(f"  {status} {self.fetch_modules[module_key]['name']}: {message}")
                
                input("\n按回车键继续...")
                
            elif choice in self.fetch_modules:
                print(f"🚀 开始抓取: {self.fetch_modules[choice]['name']}")
                success, message = self.execute_module(choice)
                status = "✅" if success else "❌"
                print(f"{status} 结果: {message}")
                input("\n按回车键继续...")
                
            else:
                print("❌ 无效选择，请重新输入")
                time.sleep(1)
    
    def run(self, modules: List[str] = None, parallel: bool = False):
        """运行数据抓取"""
        if not modules:
            # 如果没有指定模块，进入交互模式
            self.interactive_mode()
        else:
            # 执行指定模块
            self.load_progress()
            results = self.execute_batch(modules, parallel)
            
            print(f"\n📊 执行结果:")
            success_count = 0
            for key, (success, message) in results.items():
                status = "✅" if success else "❌"
                print(f"  {status} {self.fetch_modules[key]['name']}: {message}")
                if success:
                    success_count += 1
            
            print(f"\n📈 总结: {success_count}/{len(modules)} 个模块执行成功")
            
            self.stats['end_time'] = datetime.now().isoformat()
            self.save_progress()

def main():
    """主函数"""
    parser = argparse.ArgumentParser(description='TuShare综合数据抓取系统')
    parser.add_argument('--modules', nargs='+', help='指定要执行的模块')
    parser.add_argument('--parallel', action='store_true', help='并行执行模块')
    parser.add_argument('--list', action='store_true', help='列出所有可用模块')
    
    args = parser.parse_args()
    
    fetcher = ComprehensiveDataFetcher()
    
    if args.list:
        print("\n📊 可用数据模块:")
        for key, module in fetcher.fetch_modules.items():
            print(f"  [{key}] {module['name']} - {module['description']}")
        return 0
    
    try:
        fetcher.run(args.modules, args.parallel)
    except KeyboardInterrupt:
        logger.info("👋 用户中断程序")
    except Exception as e:
        logger.error(f"❌ 程序执行失败: {e}")
        return 1
    
    return 0

if __name__ == "__main__":
    exit(main())