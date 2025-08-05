#!/usr/bin/env python3
"""
快速启动综合数据抓取系统
提供简化的命令行界面
"""

import os
import sys
import subprocess
from datetime import datetime

def main():
    """主函数"""
    print("🚀 TuShare 综合数据抓取系统")
    print("=" * 50)
    
    # 检查必要文件
    required_files = [
        'comprehensive_data_fetcher.py',
        'fetch_sector_data.py', 
        'fetch_moneyflow_data.py',
        'config.py'
    ]
    
    missing_files = []
    for file in required_files:
        if not os.path.exists(file):
            missing_files.append(file)
    
    if missing_files:
        print("❌ 缺少必要文件:")
        for file in missing_files:
            print(f"  - {file}")
        print("\n请确保所有脚本文件都已创建")
        return 1
    
    # 检查日志目录
    if not os.path.exists('logs'):
        os.makedirs('logs')
        print("📁 创建日志目录")
    
    # 启动综合抓取系统
    try:
        print("🎯 启动综合数据抓取系统...")
        print("📝 使用交互模式，你可以选择要抓取的数据类型")
        print("-" * 50)
        
        result = subprocess.run(['python', 'comprehensive_data_fetcher.py'], 
                              timeout=None)
        return result.returncode
        
    except KeyboardInterrupt:
        print("\n👋 用户中断程序")
        return 0
    except Exception as e:
        print(f"❌ 启动失败: {e}")
        return 1

if __name__ == "__main__":
    exit(main())