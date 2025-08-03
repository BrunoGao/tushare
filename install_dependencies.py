#!/usr/bin/env python3
"""
ljwx-stock持续训练系统依赖安装脚本
"""

import subprocess
import sys
import os

def install_package(package):
    """安装Python包"""
    try:
        subprocess.check_call([sys.executable, '-m', 'pip', 'install', package])
        return True
    except subprocess.CalledProcessError:
        return False

def check_ollama():
    """检查Ollama是否安装"""
    try:
        result = subprocess.run(['ollama', '--version'], capture_output=True, text=True)
        return result.returncode == 0
    except FileNotFoundError:
        return False

def main():
    print("🚀 ljwx-stock持续训练系统 - 依赖安装")
    print("=" * 60)
    
    # Python依赖包
    required_packages = [
        'pandas',
        'numpy', 
        'requests',
        'tushare',
        'schedule',
        'sqlalchemy',
        'pymysql'
    ]
    
    print("📦 安装Python依赖包:")
    for package in required_packages:
        print(f"   安装 {package}...", end="")
        if install_package(package):
            print(" ✅")
        else:
            print(" ❌")
    
    # 检查Ollama
    print("\n🤖 检查Ollama:")
    if check_ollama():
        print("   Ollama已安装 ✅")
    else:
        print("   Ollama未安装 ❌")
        print("   请访问 https://ollama.ai 下载安装")
    
    # 创建必要目录
    print("\n📁 创建目录结构:")
    directories = [
        'data/llm_training',
        'data/models', 
        'data/tushare_dataset',
        'logs'
    ]
    
    for directory in directories:
        try:
            os.makedirs(directory, exist_ok=True)
            print(f"   {directory} ✅")
        except Exception as e:
            print(f"   {directory} ❌ ({e})")
    
    print("\n" + "=" * 60)
    print("✨ 安装完成！")
    print("\n📋 下一步操作:")
    print("1. 设置TuShare token (可选): export TUSHARE_TOKEN='your_token'")
    print("2. 启动Ollama服务: ollama serve")
    print("3. 创建基础模型: python create_ljwx_stock_model.py") 
    print("4. 运行演示: python demo_continuous_training.py")

if __name__ == "__main__":
    main()