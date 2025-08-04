#!/usr/bin/env python3
"""
ljwx-stock Web界面演示脚本
启动Web界面并提供使用指南
"""

import os
import sys
import time
import subprocess
import webbrowser
from threading import Timer

def check_environment():
    """检查运行环境"""
    print("🔍 检查运行环境...")
    
    # 检查Python版本
    python_version = sys.version_info
    if python_version.major >= 3 and python_version.minor >= 8:
        print(f"   ✅ Python {python_version.major}.{python_version.minor}")
    else:
        print(f"   ❌ Python版本过低: {python_version.major}.{python_version.minor}")
        return False
    
    # 检查必要模块
    required_modules = ['flask', 'flask_socketio', 'pandas', 'requests']
    for module in required_modules:
        try:
            __import__(module.replace('-', '_'))
            print(f"   ✅ {module}")
        except ImportError:
            print(f"   ❌ {module} 未安装")
            return False
    
    # 检查Ollama
    try:
        result = subprocess.run(['ollama', 'list'], capture_output=True, text=True)
        if result.returncode == 0:
            print("   ✅ Ollama服务")
            
            # 检查ljwx-stock模型
            if 'ljwx-stock' in result.stdout:
                print("   ✅ ljwx-stock模型")
            else:
                print("   ⚠️  ljwx-stock模型未找到")
        else:
            print("   ❌ Ollama服务异常")
    except FileNotFoundError:
        print("   ❌ Ollama未安装")
    
    return True

def open_browser(url):
    """延迟打开浏览器"""
    time.sleep(3)
    try:
        webbrowser.open(url)
        print(f"\n🌐 已在浏览器中打开: {url}")
    except:
        print(f"\n🌐 请手动在浏览器中打开: {url}")

def show_usage_guide():
    """显示使用指南"""
    print("\n" + "="*60)
    print("🎯 ljwx-stock Web界面使用指南")
    print("="*60)
    print("""
📋 主要功能:
   1. 数据管理 - TuShare数据获取和训练数据生成
   2. 模型训练 - 可视化模型训练配置和监控
   3. 效果监控 - 模型性能测试和结果展示
   4. 实时日志 - WebSocket实时日志显示

🚀 快速开始:
   1. 访问 http://localhost:5002
   2. 点击"数据管理"标签页
   3. 配置TuShare Token (可选)
   4. 设置参数并生成训练数据
   5. 切换到"模型训练"进行训练
   6. 在"效果监控"中测试模型

💡 建议配置:
   • 股票数量: 100-200只 (快速测试)
   • 历史天数: 365天 (完整年度数据)
   • 训练样本: 500-1000个 (平衡质量和速度)
   • Temperature: 0.7 (推荐值)

⚠️  注意事项:
   • 首次训练可能需要10-30分钟
   • 建议在网络稳定时进行数据获取
   • 训练过程中请勿关闭界面
   • 可以在实时日志中查看详细进度

🔧 故障排除:
   • 如果端口冲突，修改app.py中的端口号
   • TuShare连接失败时检查网络和Token
   • Ollama错误时确保服务正常运行
""")
    print("="*60)

def main():
    """主函数"""
    print("🚀 ljwx-stock Web界面演示")
    print("=" * 40)
    
    # 检查环境
    if not check_environment():
        print("\n❌ 环境检查失败，请先解决依赖问题")
        return
    
    print("\n✅ 环境检查通过")
    
    # 设置TuShare Token
    token = '58cf834df2a4b9a5404f6416248cffa0da78d10e31496385251d7aef'
    os.environ['TUSHARE_TOKEN'] = token
    print(f"✅ TuShare Token已配置: {token[:20]}...")
    
    # 切换到web_interface目录
    web_dir = os.path.join(os.path.dirname(__file__), 'web_interface')
    if not os.path.exists(web_dir):
        print("❌ web_interface目录不存在")
        return
    
    os.chdir(web_dir)
    
    # 显示使用指南
    show_usage_guide()
    
    # 询问是否启动
    print("\n🤔 是否现在启动Web界面？")
    print("   1. 自动启动 (推荐)")
    print("   2. 手动启动")
    print("   3. 退出")
    
    try:
        choice = input("\n请选择 (1/2/3): ").strip()
    except (EOFError, KeyboardInterrupt):
        choice = "1"  # 默认自动启动
    
    if choice == "1":
        print("\n🌐 启动Web界面...")
        print("📱 访问地址: http://localhost:5002")
        print("🛑 按 Ctrl+C 停止服务")
        print("\n" + "="*40)
        
        # 延迟打开浏览器
        timer = Timer(3.0, open_browser, ['http://localhost:5002'])
        timer.start()
        
        try:
            # 启动Flask应用
            sys.path.insert(0, web_dir)
            from app import app, socketio
            socketio.run(app, debug=False, host='0.0.0.0', port=5002, allow_unsafe_werkzeug=True)
        except KeyboardInterrupt:
            print("\n\n🛑 Web界面已停止")
        except Exception as e:
            print(f"\n❌ 启动失败: {e}")
            
    elif choice == "2":
        print("\n📖 手动启动指令:")
        print(f"   cd {web_dir}")
        print("   python app.py")
        print("\n   然后访问: http://localhost:5002")
        
    else:
        print("\n👋 退出演示")

if __name__ == "__main__":
    main()