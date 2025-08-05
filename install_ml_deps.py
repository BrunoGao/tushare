#!/usr/bin/env python3
"""
专业模型开发组件依赖安装脚本
安装MLflow、Optuna等机器学习组件
"""

import subprocess
import sys
import os
from pathlib import Path

def run_command(command, description):
    """运行命令并显示进度"""
    print(f"\n🔄 {description}...")
    print(f"执行命令: {command}")
    
    try:
        result = subprocess.run(command, shell=True, check=True, 
                              capture_output=True, text=True)
        print(f"✅ {description} 成功完成")
        if result.stdout:
            print(f"输出: {result.stdout}")
        return True
    except subprocess.CalledProcessError as e:
        print(f"❌ {description} 失败")
        print(f"错误: {e.stderr}")
        return False

def check_python_version():
    """检查Python版本"""
    version = sys.version_info
    print(f"🐍 当前Python版本: {version.major}.{version.minor}.{version.micro}")
    
    if version.major < 3 or (version.major == 3 and version.minor < 8):
        print("❌ 需要Python 3.8或更高版本")
        return False
    
    print("✅ Python版本满足要求")
    return True

def install_dependencies():
    """安装专业模型开发依赖"""
    
    print("=" * 60)
    print("🚀 LJWX Stock 专业模型开发组件安装器")
    print("=" * 60)
    
    # 检查Python版本
    if not check_python_version():
        return False
    
    # 检查requirements.txt文件
    req_file = Path("requirements.txt")
    if not req_file.exists():
        print("❌ 未找到requirements.txt文件")
        return False
    
    print(f"📋 找到requirements.txt文件: {req_file.absolute()}")
    
    # 安装依赖包
    dependencies = [
        ("pip install --upgrade pip", "升级pip"),
        ("pip install mlflow>=2.8.0", "安装MLflow实验管理"),
        ("pip install optuna>=3.4.0", "安装Optuna超参数优化"),
        ("pip install lightgbm>=4.1.0", "安装LightGBM"),
        ("pip install xgboost>=2.0.0", "安装XGBoost"),
        ("pip install catboost>=1.2.0", "安装CatBoost"),
        ("pip install scipy>=1.11.0", "安装SciPy科学计算"),
        ("pip install statsmodels>=0.14.0", "安装统计模型"),
    ]
    
    success_count = 0
    total_count = len(dependencies)
    
    for command, description in dependencies:
        if run_command(command, description):
            success_count += 1
        else:
            print(f"⚠️ {description} 安装失败，但继续其他组件安装...")
    
    print("\n" + "=" * 60)
    print("📊 安装结果总结")
    print("=" * 60)
    print(f"成功安装: {success_count}/{total_count} 个组件")
    
    if success_count == total_count:
        print("🎉 所有专业模型开发组件安装完成！")
        print("\n下一步:")
        print("1. 重启LJWX Stock服务器")
        print("2. 检查admin界面中的模型开发功能")
        print("3. 开始使用专业的量化金融模型训练流程")
        return True
    else:
        print("⚠️ 部分组件安装失败，请手动安装或检查错误信息")
        print("\n手动安装命令:")
        print("pip install -r requirements.txt")
        return False

def verify_installation():
    """验证安装是否成功"""
    print("\n🔍 验证安装...")
    
    modules_to_check = [
        ("mlflow", "MLflow实验管理"),
        ("optuna", "Optuna超参数优化"), 
        ("lightgbm", "LightGBM"),
        ("xgboost", "XGBoost"),
        ("catboost", "CatBoost"),
        ("scipy", "SciPy"),
        ("statsmodels", "统计模型")
    ]
    
    available_modules = []
    missing_modules = []
    
    for module_name, description in modules_to_check:
        try:
            __import__(module_name)
            print(f"✅ {description} - 可用")
            available_modules.append(module_name)
        except ImportError:
            print(f"❌ {description} - 不可用")
            missing_modules.append(module_name)
    
    print(f"\n📊 验证结果: {len(available_modules)}/{len(modules_to_check)} 个模块可用")
    
    if missing_modules:
        print(f"缺失模块: {', '.join(missing_modules)}")
        print("请运行以下命令手动安装:")
        for module in missing_modules:
            print(f"pip install {module}")
    
    return len(missing_modules) == 0

if __name__ == "__main__":
    print("开始安装专业模型开发组件...")
    
    # 安装依赖
    install_success = install_dependencies()
    
    # 验证安装
    if install_success:
        verify_success = verify_installation()
        
        if verify_success:
            print("\n🎯 专业模型开发环境配置完成！")
            print("现在可以使用完整的量化金融模型开发功能了。")
        else:
            print("\n⚠️ 部分组件验证失败，请检查安装")
    
    print("\n感谢使用LJWX Stock！")