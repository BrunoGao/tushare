#!/usr/bin/env python3
"""
港股支持测试脚本
"""

import os
import sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from database.tushare_data_manager import TuShareDataManager

def test_hk_support():
    """测试港股支持功能"""
    
    # 配置
    db_config = {
        'type': 'sqlite',
        'database': 'data/test_hk_stock.db'
    }
    
    # 使用测试token（实际使用时需要真实的token）
    token = "e43b6eab95ac0d2d9de22f6ca3b1b4ef3483650893794569337dc973"
    
    print("🔧 初始化TuShare数据管理器...")
    data_manager = TuShareDataManager(token, db_config)
    
    print("\n📊 测试1: 同步股票基础信息（包含港股）")
    try:
        result = data_manager.sync_stock_basic_info(include_hk=True)
        print(f"✅ 同步结果: 成功={result.success}, 处理={result.records_processed}, 成功={result.records_success}, 失败={result.records_failed}")
        if result.error_messages:
            print(f"⚠️ 错误信息: {result.error_messages}")
    except Exception as e:
        print(f"❌ 测试失败: {e}")
    
    print("\n📈 测试2: 获取活跃A股列表")
    try:
        a_stocks = data_manager._get_active_stocks(stock_type='A股')
        print(f"✅ A股数量: {len(a_stocks)}")
        if a_stocks:
            print(f"   前5只A股: {a_stocks[:5]}")
    except Exception as e:
        print(f"❌ 测试失败: {e}")
    
    print("\n🏢 测试3: 获取活跃港股列表")
    try:
        hk_stocks = data_manager._get_active_stocks(stock_type='港股')
        print(f"✅ 港股数量: {len(hk_stocks)}")
        if hk_stocks:
            print(f"   前5只港股: {hk_stocks[:5]}")
    except Exception as e:
        print(f"❌ 测试失败: {e}")
    
    print("\n📊 测试4: 获取同步状态")
    try:
        status = data_manager.get_sync_status()
        print("✅ 数据库统计:")
        for table, count in status.get('table_stats', {}).items():
            print(f"   {table}: {count} 条记录")
    except Exception as e:
        print(f"❌ 测试失败: {e}")
    
    print("\n🎯 港股支持测试完成!")

if __name__ == "__main__":
    test_hk_support()