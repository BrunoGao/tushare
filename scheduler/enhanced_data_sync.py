#!/usr/bin/env python3
"""
增强数据同步系统 - 获取资金流、板块、概念等完整市场数据
包含：资金流、板块分类、概念股、龙虎榜、沪深港通等
"""

import sys
import os
import time
import logging
from datetime import datetime, timedelta
from concurrent.futures import ThreadPoolExecutor, as_completed
from threading import Lock
import tushare as ts
from tqdm import tqdm

# 添加项目路径
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import config
from database.db_manager import DatabaseManager

# 配置日志
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler('logs/enhanced_sync.log'),
        logging.StreamHandler()
    ]
)
logger = logging.getLogger(__name__)

class EnhancedDataSync:
    """增强数据同步器"""
    
    def __init__(self):
        self.db = DatabaseManager()
        self.ts_api = ts.pro_api(config.TS_TOKEN)
        self.stats_lock = Lock()
        
        # 确保日志目录存在
        os.makedirs('logs', exist_ok=True)
    
    def sync_all_enhanced_data(self):
        """同步所有增强数据"""
        logger.info("🚀 开始增强数据同步")
        
        tasks = [
            ("概念板块数据", self.sync_concept_data),
            ("行业板块数据", self.sync_industry_sectors), 
            ("资金流数据", self.sync_money_flow_data),
            ("龙虎榜数据", self.sync_dragon_tiger_data),
            ("沪深港通资金流", self.sync_hsgt_money_flow),
            ("融资融券数据", self.sync_margin_data)
        ]
        
        success_count = 0
        total_tasks = len(tasks)
        
        for task_name, task_func in tasks:
            logger.info(f"📊 开始同步: {task_name}")
            try:
                success = task_func()
                if success:
                    success_count += 1
                    logger.info(f"✅ {task_name} 同步完成")
                else:
                    logger.warning(f"⚠️ {task_name} 同步失败")
            except Exception as e:
                logger.error(f"❌ {task_name} 同步异常: {e}")
            
            # 任务间休息
            time.sleep(2)
        
        logger.info(f"🎉 增强数据同步完成: {success_count}/{total_tasks} 成功")
        return success_count == total_tasks
    
    def sync_concept_data(self):
        """同步概念板块数据"""
        try:
            # 1. 获取概念分类
            logger.info("📋 获取概念分类...")
            concept_df = self.ts_api.concept()
            
            if not concept_df.empty:
                success = self.db.insert_dataframe('t_concept', concept_df, replace=True)
                if success:
                    logger.info(f"✅ 概念分类: {len(concept_df)} 个概念")
                else:
                    return False
            
            # 2. 获取概念成分股
            logger.info("📋 获取概念成分股...")
            concept_codes = concept_df['code'].tolist() if not concept_df.empty else []
            
            all_concept_detail = []
            for concept_code in tqdm(concept_codes[:50], desc="概念成分股"):  # 限制前50个概念避免超时
                try:
                    detail_df = self.ts_api.concept_detail(id=concept_code)
                    if not detail_df.empty:
                        detail_df['concept_code'] = concept_code
                        all_concept_detail.append(detail_df)
                    time.sleep(0.2)  # API限制
                except Exception as e:
                    logger.error(f"概念{concept_code}详情获取失败: {e}")
                    continue
            
            if all_concept_detail:
                import pandas as pd
                combined_df = pd.concat(all_concept_detail, ignore_index=True)
                success = self.db.insert_dataframe('t_concept_detail', combined_df, replace=True)
                if success:
                    logger.info(f"✅ 概念成分股: {len(combined_df)} 条记录")
                    return True
            
            return True
            
        except Exception as e:
            logger.error(f"概念数据同步失败: {e}")
            return False
    
    def sync_industry_sectors(self):
        """同步行业板块数据"""
        try:
            # 获取行业分类数据
            logger.info("📋 获取行业板块...")
            
            # 获取申万行业分类
            industry_df = self.ts_api.index_classify(level='L1', src='SW2021')
            
            if not industry_df.empty:
                # 处理数据格式
                industry_df['sector_type'] = 'SW_L1'  # 申万一级行业
                industry_df['created_at'] = datetime.now()
                
                success = self.db.insert_dataframe('industry_sectors', industry_df, replace=True)
                if success:
                    logger.info(f"✅ 申万一级行业: {len(industry_df)} 个板块")
                    return True
            
            return False
            
        except Exception as e:
            logger.error(f"行业板块同步失败: {e}")
            return False
    
    def sync_money_flow_data(self):
        """同步资金流数据"""
        try:
            # 获取最近5个交易日的资金流数据
            end_date = datetime.now().strftime('%Y%m%d')
            start_date = (datetime.now() - timedelta(days=7)).strftime('%Y%m%d')
            
            logger.info(f"📋 获取资金流数据 {start_date} - {end_date}")
            
            # 获取个股资金流
            money_flow_df = self.ts_api.moneyflow(start_date=start_date, end_date=end_date)
            
            if not money_flow_df.empty:
                money_flow_df['created_at'] = datetime.now()
                success = self.db.insert_dataframe('t_money_flow', money_flow_df, replace=False)
                if success:
                    logger.info(f"✅ 个股资金流: {len(money_flow_df)} 条记录")
                    return True
            
            return False
            
        except Exception as e:
            logger.error(f"资金流数据同步失败: {e}")
            return False
    
    def sync_dragon_tiger_data(self):
        """同步龙虎榜数据"""
        try:
            # 获取最近5个交易日的龙虎榜，需要按日获取
            end_date = datetime.now()
            
            logger.info("📋 获取龙虎榜数据...")
            
            all_dragon_data = []
            for i in range(7):  # 获取最近7天
                trade_date = (end_date - timedelta(days=i)).strftime('%Y%m%d')
                try:
                    dragon_df = self.ts_api.top_list(trade_date=trade_date)
                    if not dragon_df.empty:
                        dragon_df['created_at'] = datetime.now()
                        all_dragon_data.append(dragon_df)
                    time.sleep(0.3)  # API限制
                except Exception as e:
                    if "没有数据" not in str(e):
                        logger.debug(f"龙虎榜 {trade_date}: {e}")
                    continue
            
            if all_dragon_data:
                import pandas as pd
                combined_df = pd.concat(all_dragon_data, ignore_index=True)
                success = self.db.insert_dataframe('t_dragon_tiger_list', combined_df, replace=False)
                if success:
                    logger.info(f"✅ 龙虎榜数据: {len(combined_df)} 条记录")
                    return True
            
            logger.info("📊 龙虎榜: 近期无数据")
            return True  # 无数据也算成功
            
        except Exception as e:
            logger.error(f"龙虎榜数据同步失败: {e}")
            return False
    
    def sync_hsgt_money_flow(self):
        """同步沪深港通资金流"""
        try:
            # 获取最近30天的沪深港通资金流
            end_date = datetime.now().strftime('%Y%m%d')
            start_date = (datetime.now() - timedelta(days=30)).strftime('%Y%m%d')
            
            logger.info(f"📋 获取沪深港通资金流 {start_date} - {end_date}")
            
            hsgt_df = self.ts_api.hsgt_top10(start_date=start_date, end_date=end_date, market_type='1')
            
            if not hsgt_df.empty:
                hsgt_df['created_at'] = datetime.now()
                success = self.db.insert_dataframe('t_money_flow_hsgt', hsgt_df, replace=False)
                if success:
                    logger.info(f"✅ 沪深港通资金流: {len(hsgt_df)} 条记录")
                    return True
            
            return False
            
        except Exception as e:
            logger.error(f"沪深港通资金流同步失败: {e}")
            return False
    
    def sync_margin_data(self):
        """同步融资融券数据"""
        try:
            # 获取最近30天的融资融券数据
            end_date = datetime.now().strftime('%Y%m%d')
            start_date = (datetime.now() - timedelta(days=30)).strftime('%Y%m%d')
            
            logger.info(f"📋 获取融资融券数据 {start_date} - {end_date}")
            
            # 获取融资融券明细
            margin_df = self.ts_api.margin_detail(start_date=start_date, end_date=end_date)
            
            if not margin_df.empty:
                margin_df['created_at'] = datetime.now()
                success = self.db.insert_dataframe('t_margin_detail', margin_df, replace=False)
                if success:
                    logger.info(f"✅ 融资融券数据: {len(margin_df)} 条记录")
                    return True
            
            return False
            
        except Exception as e:
            logger.error(f"融资融券数据同步失败: {e}")
            return False
    
    def get_sync_summary(self):
        """获取同步摘要"""
        try:
            tables_info = [
                ('概念板块', 't_concept'),
                ('概念成分股', 't_concept_detail'), 
                ('行业板块', 'industry_sectors'),
                ('资金流', 't_money_flow'),
                ('龙虎榜', 't_dragon_tiger_list'),
                ('沪深港通', 't_money_flow_hsgt'),
                ('融资融券', 't_margin_detail')
            ]
            
            logger.info("📊 数据同步摘要:")
            total_records = 0
            
            for name, table in tables_info:
                try:
                    count = self.db.fetch_data(f'SELECT COUNT(*) as count FROM {table}').iloc[0]['count']
                    logger.info(f"   {name}: {count:,} 条记录")
                    total_records += count
                except:
                    logger.info(f"   {name}: 无数据")
            
            logger.info(f"📈 增强数据总计: {total_records:,} 条记录")
            
        except Exception as e:
            logger.error(f"获取同步摘要失败: {e}")

def main():
    """主函数"""
    import argparse
    
    parser = argparse.ArgumentParser(description='增强数据同步系统')
    parser.add_argument('--type', choices=['all', 'concept', 'industry', 'money', 'dragon', 'hsgt', 'margin'], 
                        default='all', help='同步数据类型')
    
    args = parser.parse_args()
    
    syncer = EnhancedDataSync()
    
    try:
        if args.type == 'all':
            success = syncer.sync_all_enhanced_data()
        elif args.type == 'concept':
            success = syncer.sync_concept_data()
        elif args.type == 'industry':
            success = syncer.sync_industry_sectors()
        elif args.type == 'money':
            success = syncer.sync_money_flow_data()
        elif args.type == 'dragon':
            success = syncer.sync_dragon_tiger_data()
        elif args.type == 'hsgt':
            success = syncer.sync_hsgt_money_flow()
        elif args.type == 'margin':
            success = syncer.sync_margin_data()
        
        # 显示摘要
        syncer.get_sync_summary()
        
        if success:
            logger.info("🎉 增强数据同步成功完成！")
        else:
            logger.warning("⚠️ 增强数据同步未完全成功")
            
    except KeyboardInterrupt:
        logger.info("⏸️ 用户中断操作")
    except Exception as e:
        logger.error(f"❌ 系统异常: {e}")

if __name__ == "__main__":
    main()