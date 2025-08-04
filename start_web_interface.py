#!/usr/bin/env python3
"""
ljwx-stock 训练管理界面启动脚本
"""

import os
import sys
import subprocess
import webbrowser
import time
from threading import Timer

def check_dependencies():
    """检查依赖"""
    print("🔍 检查Python依赖...")
    
    required_packages = [
        'flask', 'flask_socketio', 'pandas', 'numpy', 'requests', 'tushare'
    ]
    
    missing_packages = []
    
    for package in required_packages:
        try:
            __import__(package.replace('-', '_'))
            print(f"   ✅ {package}")
        except ImportError:
            missing_packages.append(package)
            print(f"   ❌ {package} (未安装)")
    
    if missing_packages:
        print(f"\n📦 需要安装缺失的依赖:")
        print(f"   pip install {' '.join(missing_packages)}")
        
        user_input = input("\n是否现在安装？(y/N): ")
        if user_input.lower() == 'y':
            try:
                subprocess.check_call([sys.executable, '-m', 'pip', 'install'] + missing_packages)
                print("✅ 依赖安装完成")
            except subprocess.CalledProcessError:
                print("❌ 依赖安装失败，请手动安装")
                return False
        else:
            print("❌ 请先安装缺失的依赖")
            return False
    
    return True

def check_ollama():
    """检查Ollama"""
    print("\n🤖 检查Ollama...")
    
    try:
        result = subprocess.run(['ollama', '--version'], capture_output=True, text=True)
        if result.returncode == 0:
            print("   ✅ Ollama已安装")
            
            # 检查ljwx-stock模型
            list_result = subprocess.run(['ollama', 'list'], capture_output=True, text=True)
            if 'ljwx-stock' in list_result.stdout:
                print("   ✅ ljwx-stock基础模型已安装")
            else:
                print("   ⚠️  ljwx-stock基础模型未安装")
                print("   💡 建议先运行: python create_ljwx_stock_model.py")
            
            return True
        else:
            print("   ❌ Ollama未正确安装")
            return False
    except FileNotFoundError:
        print("   ❌ Ollama未安装")
        print("   💡 请访问 https://ollama.ai 下载安装")
        return False

def open_browser(url):
    """延迟打开浏览器"""
    time.sleep(2)
    try:
        webbrowser.open(url)
        print(f"🌐 已在浏览器中打开: {url}")
    except:
        print(f"🌐 请手动打开浏览器访问: {url}")

def main():
    """主函数"""
    print("🚀 ljwx-stock 训练管理界面启动器")
    print("=" * 50)
    
    # 检查依赖
    if not check_dependencies():
        return
    
    # 检查Ollama
    if not check_ollama():
        print("\n⚠️  Ollama未正确配置，部分功能可能无法使用")
        user_input = input("是否继续启动界面？(y/N): ")
        if user_input.lower() != 'y':
            return
    
    # 设置环境变量
    tushare_token = os.getenv('TUSHARE_TOKEN')
    if not tushare_token:
        print("\n💡 提示: 可以设置TUSHARE_TOKEN环境变量以获得更好的数据访问权限")
        print("   或在Web界面中配置TuShare Token")
    else:
        print(f"\n✅ TuShare Token已配置: {tushare_token[:20]}...")
    
    # 启动Web界面
    print("\n🌐 启动Web界面...")
    print("📱 访问地址: http://localhost:5002")
    print("🛑 按 Ctrl+C 停止服务")
    print("=" * 50)
    
    # 延迟打开浏览器
    timer = Timer(3.0, open_browser, ['http://localhost:5002'])
    timer.start()
    
    try:
        # 切换到web_interface目录
        web_dir = os.path.join(os.path.dirname(__file__), 'web_interface')
        os.chdir(web_dir)
        
        # 启动Flask应用
        sys.path.insert(0, web_dir)
        from app import app, socketio
        socketio.run(app, debug=False, host='0.0.0.0', port=5002, allow_unsafe_werkzeug=True)
        
    except KeyboardInterrupt:
        print("\n\n🛑 服务已停止")
    except Exception as e:
        print(f"\n❌ 启动失败: {e}")
        print("\n💡 故障排除:")
        print("   1. 确保端口5002未被占用")
        print("   2. 检查防火墙设置")
        print("   3. 确保所有依赖已正确安装")

if __name__ == "__main__":
    main()