#!/usr/bin/env python3
"""
修复系统依赖问题
"""
import subprocess
import sys
import os

def install_package(package):
    """安装Python包"""
    try:
        subprocess.check_call([sys.executable, "-m", "pip", "install", package])
        print(f"✅ 成功安装 {package}")
        return True
    except subprocess.CalledProcessError as e:
        print(f"❌ 安装 {package} 失败: {e}")
        return False

def main():
    print("🔧 开始修复系统依赖...")
    
    # 需要安装的包
    packages = [
        "psutil",
        "schedule", 
        "requests"
    ]
    
    success_count = 0
    for package in packages:
        if install_package(package):
            success_count += 1
    
    print(f"\n📊 安装结果: {success_count}/{len(packages)} 成功")
    
    # 检查talib问题
    print("\n🔍 检查talib状态...")
    try:
        import talib
        print("✅ talib 可以正常导入")
    except ImportError as e:
        print(f"❌ talib 导入失败: {e}")
        print("💡 建议:")
        print("  1. 重新安装talib: pip uninstall talib && pip install talib")
        print("  2. 或者使用conda: conda install -c conda-forge ta-lib")
        print("  3. 或者暂时禁用talib相关功能")
    
    print("\n✅ 依赖修复完成!")

if __name__ == "__main__":
    main()