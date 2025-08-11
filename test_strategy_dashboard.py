#!/usr/bin/env python3
"""
测试新增的投资策略功能
"""

import requests
import json

def test_strategy_apis():
    """测试策略API功能"""
    base_url = "http://localhost:5005"
    headers = {'X-User-ID': 'test_user'}
    
    print("🧪 测试投资策略API功能...")
    
    # 1. 测试获取策略模板
    print("\n1. 获取策略模板...")
    try:
        response = requests.get(f"{base_url}/api/strategies/templates")
        if response.status_code == 200:
            data = response.json()
            if data['success']:
                templates = data['data']['templates']
                print(f"✅ 成功获取 {len(templates)} 个策略模板")
                for template in templates[:3]:  # 显示前3个
                    print(f"   - {template['name']} ({template['strategy_type']})")
            else:
                print(f"❌ API返回错误: {data.get('error')}")
        else:
            print(f"❌ HTTP错误: {response.status_code}")
    except Exception as e:
        print(f"❌ 请求失败: {e}")
    
    # 2. 测试获取用户策略
    print("\n2. 获取用户策略...")
    try:
        response = requests.get(f"{base_url}/api/strategies", headers=headers)
        if response.status_code == 200:
            data = response.json()
            if data['success']:
                strategies = data['data']['strategies']
                print(f"✅ 成功获取 {len(strategies)} 个策略")
                print(f"   用户策略: {data['data']['user_strategies_count']}")
                print(f"   模板策略: {data['data']['template_strategies_count']}")
            else:
                print(f"❌ API返回错误: {data.get('error')}")
        else:
            print(f"❌ HTTP错误: {response.status_code}")
    except Exception as e:
        print(f"❌ 请求失败: {e}")
    
    # 3. 测试克隆策略
    print("\n3. 测试克隆策略...")
    try:
        clone_data = {
            "name": "测试策略 - 克隆版本",
            "description": "这是一个测试克隆的策略"
        }
        response = requests.post(
            f"{base_url}/api/strategies/triple_ma_strategy/clone",
            headers={**headers, 'Content-Type': 'application/json'},
            json=clone_data
        )
        if response.status_code == 201:
            data = response.json()
            if data['success']:
                print(f"✅ 策略克隆成功: {data['data']['strategy_id']}")
            else:
                print(f"❌ 克隆失败: {data.get('error')}")
        else:
            print(f"❌ HTTP错误: {response.status_code}")
    except Exception as e:
        print(f"❌ 克隆请求失败: {e}")
    
    # 4. 测试策略详情
    print("\n4. 获取策略详情...")
    try:
        response = requests.get(f"{base_url}/api/strategies/value_investment")
        if response.status_code == 200:
            data = response.json()
            if data['success']:
                strategy = data['data']
                print(f"✅ 成功获取策略详情: {strategy['name']}")
                print(f"   类型: {strategy['strategy_type']}")
                print(f"   买入规则: {len(strategy['buy_rules'])}")
                print(f"   卖出规则: {len(strategy['sell_rules'])}")
            else:
                print(f"❌ 获取详情失败: {data.get('error')}")
        else:
            print(f"❌ HTTP错误: {response.status_code}")
    except Exception as e:
        print(f"❌ 详情请求失败: {e}")
    
    # 5. 测试仪表板页面
    print("\n5. 测试仪表板页面...")
    try:
        response = requests.get(f"{base_url}/dashboard")
        if response.status_code == 200:
            print("✅ 仪表板页面访问成功")
        else:
            print(f"❌ 仪表板页面HTTP错误: {response.status_code}")
    except Exception as e:
        print(f"❌ 仪表板页面请求失败: {e}")
    
    print("\n🎉 策略功能测试完成!")

def display_strategy_types():
    """显示策略类型说明"""
    print("\n📚 主流投资策略说明:")
    print("=" * 60)
    
    strategies = [
        {
            "name": "三均线策略", 
            "type": "技术分析",
            "risk": "稳健型",
            "desc": "基于5、10、20日均线的多重确认策略"
        },
        {
            "name": "RSI-MACD组合策略", 
            "type": "技术分析",
            "risk": "稳健型", 
            "desc": "结合RSI超买超卖和MACD背离的组合策略"
        },
        {
            "name": "价值投资策略",
            "type": "基本面分析", 
            "risk": "保守型",
            "desc": "基于巴菲特价值投资理念的长期策略"
        },
        {
            "name": "动量策略",
            "type": "量化策略",
            "risk": "激进型",
            "desc": "追涨强势股票的动量投资策略"
        },
        {
            "name": "质量动量策略", 
            "type": "混合策略",
            "risk": "稳健型",
            "desc": "结合财务质量和价格动量的混合策略"
        },
        {
            "name": "配对交易策略",
            "type": "量化策略", 
            "risk": "投机型",
            "desc": "基于股票对相关性的统计套利策略"
        }
    ]
    
    for i, strategy in enumerate(strategies, 1):
        print(f"{i}. {strategy['name']}")
        print(f"   类型: {strategy['type']} | 风险: {strategy['risk']}")
        print(f"   描述: {strategy['desc']}")
        print()

if __name__ == "__main__":
    print("🚀 投资策略仪表板测试工具")
    print("=" * 50)
    
    display_strategy_types()
    
    # 等待用户确认
    input("\n按回车键开始API测试...")
    
    test_strategy_apis()
    
    print("\n" + "=" * 50)
    print("📖 使用说明:")
    print("1. 启动系统: python3 start_realtime_system.py")
    print("2. 访问仪表板: http://localhost:5005/dashboard")
    print("3. 浏览策略模板，克隆喜欢的策略")
    print("4. 根据风险偏好和市场环境选择合适策略")
    print("5. 自定义策略参数和风险管理设置")