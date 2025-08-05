#!/usr/bin/env python3
"""
修复模板加载问题
确保企业级界面正确显示
"""

import os
import sys
from pathlib import Path

def fix_template_loading():
    """修复模板加载问题"""
    print("🔧 修复模板加载问题...")
    
    # 1. 检查模板文件
    template_path = Path('templates/enterprise_dashboard.html')
    if not template_path.exists():
        print("❌ enterprise_dashboard.html 不存在")
        return False
    
    print("✅ enterprise_dashboard.html 存在")
    
    # 2. 检查文件内容
    content = template_path.read_text(encoding='utf-8')
    if "企业版 v2.0" in content:
        print("✅ 模板内容已更新")
    else:
        print("❌ 模板内容未更新")
        return False
    
    # 3. 检查unified_app.py路由
    unified_app_path = Path('unified_app.py')
    if unified_app_path.exists():
        app_content = unified_app_path.read_text(encoding='utf-8')
        if "render_template('enterprise_dashboard.html')" in app_content:
            print("✅ 路由配置正确")
        else:
            print("❌ 路由配置错误")
            return False
    
    # 4. 清理可能的缓存
    cache_dirs = [
        '__pycache__',
        'templates/__pycache__',
        'static/.cache'
    ]
    
    for cache_dir in cache_dirs:
        cache_path = Path(cache_dir)
        if cache_path.exists():
            import shutil
            shutil.rmtree(cache_path)
            print(f"🗑️ 清理缓存: {cache_dir}")
    
    return True

def create_force_refresh_script():
    """创建强制刷新脚本"""
    print("📄 创建强制刷新脚本...")
    
    refresh_script = '''#!/bin/bash

echo "🔄 强制刷新应用..."

# 停止现有进程
pkill -f "python.*wsgi_app" 2>/dev/null || true
pkill -f "python.*unified_app" 2>/dev/null || true

sleep 2

# 清理Python缓存
find . -name "*.pyc" -delete 2>/dev/null || true
find . -name "__pycache__" -type d -exec rm -rf {} + 2>/dev/null || true

echo "✅ 缓存已清理"

# 重新启动应用
echo "🚀 重新启动应用..."
./start_app.sh
'''
    
    with open('force_refresh.sh', 'w') as f:
        f.write(refresh_script)
    
    os.chmod('force_refresh.sh', 0o755)
    print("✅ 创建了 force_refresh.sh 脚本")

def main():
    """主函数"""
    print("🚀 LJWX-Stock 模板加载修复工具")
    print("=" * 50)
    
    if fix_template_loading():
        create_force_refresh_script()
        
        print("\n🎯 问题诊断和解决方案:")
        print("1. ✅ 模板文件检查通过")
        print("2. ✅ 路由配置检查通过")
        print("3. 🗑️ 缓存已清理")
        
        print("\n🔧 接下来请执行以下步骤:")
        print("1. 在浏览器中按 Ctrl+Shift+R (强制刷新页面)")
        print("2. 或者运行: ./force_refresh.sh")
        print("3. 访问: http://localhost:5005")
        print("4. 检查页面标题是否显示 '企业版 v2.0'")
        
        print("\n📋 如果问题仍然存在:")
        print("• 访问 http://localhost:5005/debug-template 查看调试信息")
        print("• 访问 http://localhost:5005/full 强制加载完整版")
        print("• 检查浏览器开发者工具的控制台错误")
        
    else:
        print("\n❌ 修复失败，请手动检查模板文件")

if __name__ == "__main__":
    main()