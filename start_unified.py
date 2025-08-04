#!/usr/bin/env python3
"""
ljwx-stock 统一应用启动器
端口: 5005
"""

import os
import sys
import subprocess
from pathlib import Path

def load_env_file():
    """加载.env文件中的环境变量"""
    env_file = Path('.env')
    if env_file.exists():
        print("📝 加载.env环境变量...")
        with open(env_file, 'r', encoding='utf-8') as f:
            for line in f:
                line = line.strip()
                if line and not line.startswith('#') and '=' in line:
                    key, value = line.split('=', 1)
                    os.environ[key.strip()] = value.strip()
                    if key.strip() == 'TUSHARE_TOKEN':
                        print(f"   ✅ TuShare Token已设置: {value.strip()[:8]}...")
        return True
    return False

def check_dependencies():
    """检查依赖"""
    print("🔍 检查依赖...")
    
    required_packages = [
        'Flask', 'Flask-SocketIO', 'pandas', 'numpy', 'requests', 'tushare'
    ]
    
    missing_packages = []
    
    for package in required_packages:
        try:
            if package == 'Flask':
                import flask
                print(f"   ✅ {package.lower()}")
            elif package == 'Flask-SocketIO':
                import flask_socketio
                print(f"   ✅ flask-socketio")
            elif package == 'pandas':
                import pandas
                print(f"   ✅ {package.lower()}")
            elif package == 'numpy':
                import numpy
                print(f"   ✅ {package.lower()}")
            elif package == 'requests':
                import requests
                print(f"   ✅ {package.lower()}")
            elif package == 'tushare':
                import tushare
                print(f"   ✅ {package.lower()}")
        except ImportError:
            missing_packages.append(package)
            print(f"   ❌ {package.lower()}")
    
    if missing_packages:
        print(f"\n❌ 缺少依赖包: {', '.join(missing_packages)}")
        print("请运行以下命令安装:")
        print(f"pip install {' '.join(missing_packages)}")
        return False
    
    return True

def check_ollama():
    """检查Ollama"""
    print("\n🤖 检查Ollama...")
    
    try:
        # 检查Ollama是否安装
        result = subprocess.run(['ollama', '--version'], 
                              capture_output=True, text=True, timeout=10)
        if result.returncode == 0:
            print("   ✅ Ollama已安装")
        else:
            print("   ❌ Ollama未正确安装")
            return False
            
        # 检查ljwx-stock模型
        result = subprocess.run(['ollama', 'list'], 
                              capture_output=True, text=True, timeout=10)
        if 'ljwx-stock' in result.stdout:
            print("   ✅ ljwx-stock模型已安装")
        else:
            print("   ⚠️  ljwx-stock模型未安装")
            print("      可在Web界面中使用其他模型进行训练")
            
    except (subprocess.TimeoutExpired, FileNotFoundError):
        print("   ⚠️  Ollama未安装或不在PATH中")
        print("      模型相关功能将受限")
        return False
    
    return True

def setup_environment():
    """设置环境"""
    print("\n🔧 设置环境...")
    
    # 创建必要目录
    directories = [
        'training_data',
        'models',
        'strategies',
        'backtest_results',
        'recommendation_data',
        'static',
        'templates'
    ]
    
    for directory in directories:
        Path(directory).mkdir(exist_ok=True)
        print(f"   ✅ {directory}")
    
    # 检查TuShare Token状态
    tushare_token = os.getenv('TUSHARE_TOKEN')
    if tushare_token:
        print(f"\n✅ TuShare Pro Token已配置: {tushare_token[:8]}...{tushare_token[-4:]}")
        print("   享受Pro用户权限和更高的API调用频率")
    else:
        print("\n💡 提示: 可以设置TUSHARE_TOKEN环境变量以获得更好的数据访问权限")
        print("   或在Web界面中配置TuShare Token")

def main():
    """主函数"""
    print("🚀 ljwx-stock 统一应用启动器")
    print("=" * 50)
    
    # 加载环境变量
    load_env_file()
    
    # 检查依赖
    if not check_dependencies():
        sys.exit(1)
    
    # 检查Ollama
    check_ollama()
    
    # 设置环境
    setup_environment()
    
    print("\n🌐 启动统一Web应用...")
    print("📱 管理端访问地址: http://localhost:5005")
    print("📱 移动端API地址: http://localhost:5005/api/mobile/")
    print("🛑 按 Ctrl+C 停止服务")
    print("=" * 50)
    
    try:
        # 启动统一应用
        from unified_app import UnifiedStockApp
        
        app = UnifiedStockApp()
        app.run(port=5005, debug=False)
        
    except KeyboardInterrupt:
        print("\n👋 服务已停止")
    except ImportError as e:
        print(f"❌ 导入失败: {e}")
        print("请确保所有文件都在正确位置")
        sys.exit(1)
    except Exception as e:
        print(f"❌ 启动失败: {e}")
        sys.exit(1)

if __name__ == '__main__':
    main()