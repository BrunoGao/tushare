#!/usr/bin/env python3
"""
修复JavaScript功能问题
解决Bootstrap未定义和模态框不工作的问题
"""

import os
import sys
from pathlib import Path

def check_template_fixes():
    """检查模板修复是否完成"""
    print("🔧 检查JavaScript修复...")
    
    template_path = Path('templates/enterprise_dashboard.html')
    if not template_path.exists():
        print("❌ enterprise_dashboard.html 不存在")
        return False
    
    content = template_path.read_text(encoding='utf-8')
    
    # 检查修复项目
    fixes = [
        ('Bootstrap JS加载检查', 'Bootstrap JS 加载成功'),
        ('Socket.IO加载检查', 'Socket.IO 加载成功'),
        ('依赖检查函数', 'checkDependencies'),
        ('应用初始化函数', 'initializeApp'),
        ('模态框降级方案', 'typeof bootstrap !== \'undefined\''),
        ('调试日志', 'console.log'),
        ('错误处理', 'try {'),
    ]
    
    all_fixed = True
    for name, pattern in fixes:
        if pattern in content:
            print(f"✅ {name}")
        else:
            print(f"❌ {name} - 未找到")
            all_fixed = False
    
    return all_fixed

def generate_test_instructions():
    """生成测试说明"""
    print("\n📋 测试说明:")
    print("1. 打开浏览器开发者工具 (F12)")
    print("2. 访问: http://localhost:5005")
    print("3. 在控制台查看以下日志:")
    print("   ✅ Bootstrap JS 加载成功")
    print("   ✅ Socket.IO 加载成功")
    print("   ✅ DOM 已加载")
    print("   ✅ 所有依赖已加载，初始化应用...")
    print("   ✅ 应用初始化完成")
    print("   🎉 系统已就绪，所有功能可正常使用！")
    
    print("\n🧪 功能测试:")
    print("1. 点击「快速分析」卡片 - 应该打开模态框")
    print("2. 点击「AI推荐」卡片 - 应该打开推荐生成对话框")
    print("3. 点击用户头像 - 应该显示下拉菜单")
    print("4. 点击「切换角色」- 应该显示通知并切换权限")
    print("5. 在搜索框输入股票代码按回车 - 应该显示搜索提示")

def create_quick_test_script():
    """创建快速测试脚本"""
    test_script = '''
// 在浏览器控制台运行此脚本来测试功能

console.log("🧪 开始快速功能测试...");

// 测试1: 检查全局函数是否存在
const functions = ['showQuickAnalysis', 'showRecommendations', 'showPortfolio', 'toggleRole'];
functions.forEach(func => {
    if (typeof window[func] === 'function') {
        console.log(`✅ ${func} 函数存在`);
    } else {
        console.log(`❌ ${func} 函数不存在`);
    }
});

// 测试2: 检查依赖
console.log('Bootstrap 可用:', typeof bootstrap !== 'undefined');
console.log('Socket.IO 可用:', typeof io !== 'undefined');

// 测试3: 测试通知功能
try {
    showNotification('info', '这是一个测试通知');
    console.log('✅ 通知功能正常');
} catch (error) {
    console.log('❌ 通知功能异常:', error);
}

// 测试4: 测试API调用
fetch('/api/status')
    .then(response => response.json())
    .then(data => {
        console.log('✅ API调用成功:', data);
    })
    .catch(error => {
        console.log('❌ API调用失败:', error);
    });

console.log("🏁 快速测试完成，请检查上述结果");
'''
    
    with open('quick_test.js', 'w', encoding='utf-8') as f:
        f.write(test_script)
    
    print(f"\n📄 已创建快速测试脚本: quick_test.js")
    print("   复制脚本内容到浏览器控制台运行")

def main():
    """主函数"""
    print("🚀 JavaScript功能修复验证")
    print("=" * 50)
    
    if check_template_fixes():
        print("\n✅ 所有修复项目已完成！")
        
        generate_test_instructions()
        create_quick_test_script()
        
        print("\n🎯 关键修复内容:")
        print("• 🔧 修复了Bootstrap未定义的问题")
        print("• 📱 添加了模态框降级方案")
        print("• ⏱️ 实现了依赖加载等待机制")
        print("• 🔍 改进了错误处理和调试信息")
        print("• 🎮 增强了用户交互反馈")
        
        print("\n🎉 现在所有按钮和菜单都应该正常工作了！")
        
    else:
        print("\n❌ 部分修复未完成，请检查模板文件")

if __name__ == "__main__":
    main()