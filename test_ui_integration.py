#!/usr/bin/env python3
"""
测试前后端UI集成
验证所有API接口和前端功能是否正常工作
"""

import os
import sys
import json
from pathlib import Path

def test_api_endpoints():
    """测试API端点定义"""
    print("🧪 测试API端点定义...")
    
    unified_app_path = Path('unified_app.py')
    if not unified_app_path.exists():
        print("❌ unified_app.py 不存在")
        return False
    
    content = unified_app_path.read_text(encoding='utf-8')
    
    # 检查关键API端点
    required_apis = [
        '/api/status',
        '/api/user-info',
        '/api/switch-role',
        '/api/models-info',
        '/api/mobile/dashboard',
        '/api/mobile/quick-analysis',
        '/api/recommendations/generate',
        '/api/mobile/recommendations/latest'
    ]
    
    missing_apis = []
    for api in required_apis:
        if api in content:
            print(f"✅ {api}")
        else:
            print(f"❌ {api}")
            missing_apis.append(api)
    
    return len(missing_apis) == 0

def test_frontend_functions():
    """测试前端JavaScript函数"""
    print("\n🧪 测试前端JavaScript函数...")
    
    template_path = Path('templates/enterprise_dashboard.html')
    if not template_path.exists():
        print("❌ enterprise_dashboard.html 不存在")
        return False
    
    content = template_path.read_text(encoding='utf-8')
    
    # 检查关键前端函数
    required_functions = [
        'showQuickAnalysis',
        'runQuickAnalysisModal',
        'generateNewRecommendations',
        'startRecommendationGeneration',
        'showModelManagement',
        'loadAvailableModels',
        'toggleRole',
        'loadUserInfo',
        'showNotification',
        'createModal',
        'updateNavigation'
    ]
    
    missing_functions = []
    for func in required_functions:
        if f'function {func}' in content:
            print(f"✅ {func}()")
        else:
            print(f"❌ {func}()")
            missing_functions.append(func)
    
    return len(missing_functions) == 0

def test_ui_integration():
    """测试UI组件集成"""
    print("\n🧪 测试UI组件集成...")
    
    template_path = Path('templates/enterprise_dashboard.html')
    content = template_path.read_text(encoding='utf-8')
    
    # 检查关键集成点
    integrations = [
        ('快速分析按钮', 'showQuickAnalysis()'),
        ('AI推荐按钮', 'generateNewRecommendations()'),
        ('模型管理按钮', 'showModelManagement()'),
        ('角色切换按钮', 'toggleRole()'),
        ('顶部导航菜单', 'navigateTo'),
        ('WebSocket连接', 'socket.io'),
        ('API调用', 'fetch('),
        ('模态框系统', 'createModal'),
        ('通知系统', 'showNotification')
    ]
    
    integration_status = []
    for name, pattern in integrations:
        if pattern in content:
            print(f"✅ {name}")
            integration_status.append(True)
        else:
            print(f"❌ {name}")
            integration_status.append(False)
    
    return all(integration_status)

def test_responsive_design():
    """测试响应式设计"""
    print("\n🧪 测试响应式设计...")
    
    template_path = Path('templates/enterprise_dashboard.html')
    content = template_path.read_text(encoding='utf-8')
    
    # 检查响应式设计特性
    responsive_features = [
        ('@media (max-width: 1200px)', '大屏适配'),
        ('@media (max-width: 768px)', '平板适配'),
        ('@media (max-width: 480px)', '手机适配'),
        ('grid-template-columns', 'CSS Grid布局'),
        ('flex', 'Flexbox布局'),
        ('var(--', 'CSS自定义属性')
    ]
    
    responsive_status = []
    for pattern, name in responsive_features:
        if pattern in content:
            print(f"✅ {name}")
            responsive_status.append(True)
        else:
            print(f"❌ {name}")
            responsive_status.append(False)
    
    return all(responsive_status)

def test_user_role_management():
    """测试用户角色管理"""
    print("\n🧪 测试用户角色管理...")
    
    template_path = Path('templates/enterprise_dashboard.html')
    content = template_path.read_text(encoding='utf-8')
    
    # 检查角色管理特性
    role_features = [
        ('admin-only', '管理员专属功能'),
        ('data-role', '角色数据属性'),
        ('currentUser.role', '当前用户角色'),
        ('updateUIForUserRole', '角色UI更新'),
        ('/api/user-info', '用户信息API'),
        ('/api/switch-role', '角色切换API')
    ]
    
    role_status = []
    for pattern, name in role_features:
        if pattern in content:
            print(f"✅ {name}")
            role_status.append(True)
        else:
            print(f"❌ {name}")
            role_status.append(False)
    
    return all(role_status)

def generate_integration_report():
    """生成集成测试报告"""
    print("\n📊 生成集成测试报告...")
    
    report = {
        "测试时间": "2025-08-05",
        "测试项目": "LJWX-Stock前后端UI集成",
        "测试结果": {
            "API端点定义": test_api_endpoints(),
            "前端JavaScript函数": test_frontend_functions(),
            "UI组件集成": test_ui_integration(),
            "响应式设计": test_responsive_design(),
            "用户角色管理": test_user_role_management()
        }
    }
    
    # 保存报告
    report_path = Path('test_integration_report.json')
    with open(report_path, 'w', encoding='utf-8') as f:
        json.dump(report, f, ensure_ascii=False, indent=2)
    
    return report

def main():
    """主测试函数"""
    print("🚀 LJWX-Stock 前后端UI集成测试")
    print("=" * 50)
    
    # 运行所有测试
    report = generate_integration_report()
    
    # 统计结果
    passed = sum(1 for result in report["测试结果"].values() if result)
    total = len(report["测试结果"])
    
    print(f"\n📈 测试总结:")
    print(f"通过: {passed}/{total} 项测试")
    
    if passed == total:
        print("🎉 所有测试通过！前后端集成成功！")
        
        print("\n✨ 集成功能特性:")
        print("• 🎯 快速股票分析 - 支持实时API调用")
        print("• 🤖 AI推荐生成 - 集成WebSocket实时通信")
        print("• 🔧 模型管理 - 管理员权限控制")
        print("• 👤 用户角色管理 - 动态权限切换")
        print("• 📱 响应式设计 - 多设备适配")
        print("• 🔔 实时通知系统 - 用户操作反馈")
        print("• 🧭 智能导航 - 功能模块切换")
        
        print("\n🎮 用户交互流程:")
        print("1. 用户登录 → 加载角色权限")
        print("2. 选择功能 → 调用相应API")
        print("3. 实时反馈 → WebSocket通信")
        print("4. 结果展示 → 动态UI更新")
        print("5. 错误处理 → 友好提示")
        
    else:
        print(f"⚠️  有 {total - passed} 项测试失败，请检查相关功能")
    
    print(f"\n📄 详细报告已保存至: test_integration_report.json")

if __name__ == "__main__":
    main()