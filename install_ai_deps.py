#!/usr/bin/env python3
"""
AI系统依赖安装脚本
自动检查和安装缺失的Python包
"""
import subprocess
import sys
import os

def run_command(command):
    """运行系统命令"""
    try:
        result = subprocess.run(command, shell=True, capture_output=True, text=True)
        return result.returncode == 0, result.stdout, result.stderr
    except Exception as e:
        return False, "", str(e)

def check_and_install_package(package_name, install_name=None):
    """检查并安装包"""
    if install_name is None:
        install_name = package_name
    
    print(f"📦 检查 {package_name}...")
    
    # 检查包是否已安装
    try:
        if package_name == 'sklearn':
            import sklearn
        elif package_name == 'talib':
            import talib
        elif package_name == 'xgboost':
            import xgboost
        else:
            __import__(package_name)
        print(f"✅ {package_name} 已安装")
        return True
    except ImportError:
        print(f"❌ {package_name} 未安装，开始安装...")
        
        # 安装包
        success, stdout, stderr = run_command(f"{sys.executable} -m pip install {install_name}")
        
        if success:
            print(f"✅ {package_name} 安装成功")
            return True
        else:
            print(f"❌ {package_name} 安装失败: {stderr}")
            return False

def install_ai_dependencies():
    """安装AI系统依赖"""
    print("🚀 开始安装AI系统依赖包...")
    print("=" * 50)
    
    # 定义依赖包
    dependencies = [
        ('pandas', 'pandas>=1.5.0'),
        ('numpy', 'numpy>=1.21.0'),
        ('sklearn', 'scikit-learn>=1.1.0'),
        ('joblib', 'joblib>=1.2.0'),
        ('xgboost', 'xgboost>=1.6.0'),
        ('talib', 'talib-binary>=0.4.25'),  # 使用binary版本更容易安装
    ]
    
    success_count = 0
    total_count = len(dependencies)
    
    for package_name, install_name in dependencies:
        if check_and_install_package(package_name, install_name):
            success_count += 1
    
    print("\n" + "=" * 50)
    print(f"📊 安装结果: {success_count}/{total_count} 成功")
    
    if success_count == total_count:
        print("🎉 所有依赖包安装完成！")
        return True
    else:
        print("⚠️ 部分依赖包安装失败，系统可能无法完全正常工作")
        print("\n💡 手动安装建议:")
        
        failed_packages = []
        for package_name, install_name in dependencies:
            try:
                if package_name == 'sklearn':
                    import sklearn
                elif package_name == 'talib':
                    import talib
                elif package_name == 'xgboost':
                    import xgboost
                else:
                    __import__(package_name)
            except ImportError:
                failed_packages.append(install_name)
        
        if failed_packages:
            print(f"pip install {' '.join(failed_packages)}")
        
        return False

def verify_installation():
    """验证安装结果"""
    print("\n🔍 验证AI系统安装...")
    
    try:
        # 检查AI健康状态
        sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
        
        from ai.health_check import get_ai_system_health
        health = get_ai_system_health()
        
        print(f"整体状态: {health['overall_status']}")
        print(f"系统可用性: {health['overall_availability']:.1%}")
        
        print("\n📦 依赖包状态:")
        for dep, status in health['dependencies']['dependencies'].items():
            print(f"  {dep}: {'✅' if status else '❌'}")
        
        print("\n🧩 模块状态:")
        for mod, status in health['modules']['modules'].items():
            print(f"  {mod}: {'✅' if status else '❌'}")
        
        if health['overall_status'] == 'healthy':
            print("\n🎉 AI系统验证通过！")
            return True
        else:
            print(f"\n⚠️ AI系统状态: {health['overall_status']}")
            print("修复建议:")
            for rec in health['recommendations']:
                print(f"  - {rec}")
            return False
            
    except Exception as e:
        print(f"❌ 验证失败: {e}")
        return False

def main():
    """主函数"""
    print("🤖 AI股票分析系统依赖安装工具")
    print("=" * 60)
    
    # 检查Python版本
    if sys.version_info < (3, 8):
        print("❌ 需要Python 3.8或更高版本")
        return
    
    print(f"✅ Python版本: {sys.version}")
    
    # 安装依赖
    install_success = install_ai_dependencies()
    
    # 验证安装
    if install_success:
        verify_installation()
    
    print("\n📝 安装完成！")
    print("现在可以访问 http://192.168.1.83:5005/ai-analysis 使用AI分析功能")

if __name__ == "__main__":
    main()