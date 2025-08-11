#!/usr/bin/env python3
"""
测试个人用户回测API的脚本
"""

import requests
import json
import time

def test_backtest_api():
    """测试回测API端点"""
    
    print("🧪 开始测试个人用户回测API...")
    
    # 回测参数
    test_config = {
        'stock_code': '000001',  # 平安银行
        'start_date': '2024-01-01',
        'end_date': '2024-06-01', 
        'initial_capital': 100000,
        'commission': 0.0003,
        'risk_management': {
            'stop_loss': 0.05,      # 5% 止损
            'take_profit': 0.10,    # 10% 止盈
            'max_position_size': 0.20  # 20% 最大仓位
        }
    }
    
    try:
        print(f"📊 测试配置: {json.dumps(test_config, indent=2, ensure_ascii=False)}")
        
        # 发送请求
        url = 'http://localhost:5005/api/strategies/demo1/backtest'
        response = requests.post(
            url,
            json=test_config,
            headers={'Content-Type': 'application/json'},
            timeout=30
        )
        
        print(f"📡 请求状态: {response.status_code}")
        
        if response.status_code == 200:
            result = response.json()
            print(f"✅ API响应成功: {json.dumps(result, indent=2, ensure_ascii=False)}")
            
            if result.get('status') == 'started':
                print("🔄 回测任务已启动，等待完成...")
                # 实际应用中会通过WebSocket接收完成通知
                print("💡 提示：请查看服务器日志了解回测进度")
            
        else:
            print(f"❌ API响应失败: {response.status_code}")
            try:
                error_data = response.json()
                print(f"错误详情: {json.dumps(error_data, indent=2, ensure_ascii=False)}")
            except:
                print(f"原始响应: {response.text}")
                
    except requests.exceptions.ConnectionError:
        print("❌ 连接失败：请确保服务器在 http://localhost:5005 上运行")
    except requests.exceptions.Timeout:
        print("❌ 请求超时：回测可能需要更长时间")
    except Exception as e:
        print(f"❌ 测试失败: {e}")

def test_tushare_data_access():
    """测试TuShare数据访问"""
    print("\n🔍 测试TuShare数据访问...")
    
    try:
        from llm.tushare_data_extractor import TuShareDataExtractor
        
        # 使用默认token
        extractor = TuShareDataExtractor('e43b6eab95ac0d2d9de22f6ca3b1b4ef3483650893794569337dc973')
        
        # 测试获取数据
        print("📈 尝试获取平安银行(000001)历史数据...")
        data = extractor.get_stock_daily_data('000001.SZ', '20240101', '20240201')
        
        if not data.empty:
            print(f"✅ 数据获取成功！")
            print(f"📊 数据行数: {len(data)}")
            print(f"📋 列名: {list(data.columns)}")
            print(f"📅 日期范围: {data.index.min() if hasattr(data, 'index') else data['trade_date'].min()} 到 {data.index.max() if hasattr(data, 'index') else data['trade_date'].max()}")
            print(f"💰 价格范围: {data['close'].min():.2f} - {data['close'].max():.2f}")
        else:
            print("❌ 数据为空")
            
    except ImportError as e:
        print(f"❌ 导入错误: {e}")
    except Exception as e:
        print(f"❌ 数据访问失败: {e}")

if __name__ == "__main__":
    print("=" * 60)
    print("🚀 个人用户回测功能测试")
    print("=" * 60)
    
    # 先测试数据访问
    test_tushare_data_access()
    
    # 再测试API
    test_backtest_api()
    
    print("\n" + "=" * 60)
    print("🏁 测试完成")
    print("=" * 60)