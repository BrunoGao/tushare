#!/usr/bin/env python3
"""
测试启动优化效果
"""
import time
import sys
import os

# 添加项目路径
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

def test_startup_performance():
    """测试启动性能"""
    print("🚀 测试ljwx-stock启动优化")
    print("=" * 50)
    
    start_time = time.time()
    
    try:
        # 模拟应用启动过程
        print("📦 导入模块...")
        from unified_app import UnifiedStockApp
        import_time = time.time()
        print(f"   导入耗时: {import_time - start_time:.2f}秒")
        
        print("🔧 创建应用实例...")
        app = UnifiedStockApp()
        init_time = time.time()
        print(f"   初始化耗时: {init_time - import_time:.2f}秒")
        
        print("✅ 启动完成")
        total_time = init_time - start_time
        print(f"   总启动耗时: {total_time:.2f}秒")
        
        # 分析性能
        if total_time < 2:
            print("🎉 启动速度优秀 (< 2秒)")
        elif total_time < 5:
            print("✅ 启动速度良好 (< 5秒)")
        elif total_time < 10:
            print("⚠️  启动速度一般 (< 10秒)")
        else:
            print("❌ 启动速度较慢 (> 10秒)")
        
        # 测试延迟加载
        print()
        print("🔄 测试延迟加载...")
        
        lazy_start = time.time()
        data_extractor = app.get_data_extractor()
        lazy_end = time.time()
        print(f"   TuShare数据提取器延迟加载: {lazy_end - lazy_start:.2f}秒")
        
        lazy_start = time.time()
        trainer = app.get_trainer()
        lazy_end = time.time()
        print(f"   训练器延迟加载: {lazy_end - lazy_start:.2f}秒")
        
        # 测试数据库连接
        print()
        print("💾 测试数据库连接...")
        db_start = time.time()
        
        # 测试获取训练任务
        tasks = app._get_all_training_tasks()
        db_end = time.time()
        print(f"   数据库查询耗时: {db_end - db_start:.2f}秒")
        print(f"   当前训练任务数量: {len(tasks)}")
        
        return True
        
    except Exception as e:
        print(f"❌ 启动测试失败: {e}")
        return False

def show_optimization_tips():
    """显示优化建议"""
    print()
    print("💡 启动优化建议:")
    print("   1. 使用延迟加载减少初始化时间")
    print("   2. 优先使用轻量级SQLite，按需升级MySQL")  
    print("   3. 减少不必要的网络请求")
    print("   4. 缓存重复计算结果")
    print("   5. 异步初始化非关键组件")
    print()
    print("🔧 环境优化:")
    print("   - 设置MYSQL_HOST环境变量启用MySQL")
    print("   - 设置TUSHARE_TOKEN环境变量启用数据服务")
    print("   - 使用SSD硬盘提升数据库性能")

if __name__ == '__main__':
    success = test_startup_performance()
    
    if success:
        show_optimization_tips()
        print("🎯 启动优化测试完成")
    else:
        print("❌ 测试失败，请检查配置")