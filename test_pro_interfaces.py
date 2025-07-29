#!/usr/bin/env python3
"""
快速测试TuShare Pro接口可用性
"""
import tushare as ts
import config
import pandas as pd
from datetime import datetime, timedelta

# 初始化
ts.set_token(config.TS_TOKEN)
pro = ts.pro_api()

def test_interface(name, func, **kwargs):
    """测试接口"""
    try:
        print(f"🔍 测试 {name} 接口...")
        df = func(**kwargs)
        
        if df.empty:
            print(f"⚠️  {name}: 返回空数据")
            return False
        else:
            print(f"✅ {name}: 成功获取 {len(df)} 条记录")
            print(f"   字段: {list(df.columns)}")
            return True
    except Exception as e:
        print(f"❌ {name}: 失败 - {e}")
        return False

print("🚀 开始测试TuShare Pro接口可用性")
print("=" * 50)

# 测试基础免费接口
print("\n📋 免费接口测试:")
test_interface("股票基本信息", pro.stock_basic)
test_interface("停复牌信息", pro.suspend_d, start_date='20240101', end_date='20241231')

# 测试Pro接口
print("\n💰 Pro接口测试:")

# 财务数据
period = "20240930"
test_interface("资产负债表", pro.balancesheet, period=period)
test_interface("利润表", pro.income, period=period)
test_interface("现金流量表", pro.cashflow, period=period)
test_interface("财务指标", pro.fina_indicator, period=period)

# 业绩数据
test_interface("业绩预告", pro.forecast, ann_date='20241201')
test_interface("业绩快报", pro.express, ann_date='20241201')

# 分红回购
test_interface("分红送配", pro.dividend, ann_date='20241201')
test_interface("股票回购", pro.repurchase, ann_date='20241201')

# 股东数据
test_interface("十大股东", pro.top10_holders, ts_code='000001.SZ', period='20240930')
test_interface("十大流通股东", pro.top10_floatholders, ts_code='000001.SZ', period='20240930')
test_interface("股东户数", pro.stk_holdernumber, ts_code='000001.SZ', start_date='20240101', end_date='20241231')

# 限售解禁
test_interface("限售解禁", pro.share_float, ts_code='000001.SZ')

# 资金流
today = datetime.now().strftime('%Y%m%d')
test_interface("个股资金流", pro.moneyflow, ts_code='000001.SZ', trade_date=today)
test_interface("沪深港通资金流", pro.moneyflow_hsgt, trade_date=today)

# 板块概念
test_interface("概念分类", pro.concept)
test_interface("概念成分股", pro.concept_detail, id='TS1')

# 指数成分
test_interface("指数成分股", pro.index_weight, index_code='000300.SH', trade_date=today)

# 融资融券
test_interface("融资融券明细", pro.margin_detail, trade_date=today)

# 龙虎榜
test_interface("龙虎榜", pro.top_list, trade_date=today)

# 涨跌停
test_interface("涨跌停价格", pro.stk_limit, ts_code='000001.SZ', trade_date=today)

print("\n" + "=" * 50)
print("✅ 接口测试完成") 