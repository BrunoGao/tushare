#!/usr/bin/env python3
"""
全量股票数据同步系统
智能分批更新所有A股数据，支持断点续传和进度监控
"""

import sys
import os
import time
import json
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
        logging.FileHandler('logs/full_sync.log'),
        logging.StreamHandler()
    ]
)
logger = logging.getLogger(__name__)

class FullStockDataSync:
    """全量股票数据同步器"""
    
    def __init__(self):
        self.db = DatabaseManager()
        self.ts_api = ts.pro_api(config.TS_TOKEN)
        self.progress_file = 'logs/sync_progress.json'
        self.stats_lock = Lock()
        
        # 同步统计
        self.stats = {
            'total_stocks': 0,
            'completed_stocks': 0,
            'failed_stocks': 0,
            'start_time': None,
            'current_batch': 0,
            'total_batches': 0,
            'failed_list': []
        }
        
        # 确保日志目录存在
        os.makedirs('logs', exist_ok=True)
    
    def sync_all_stocks(self, days: int = 30, batch_size: int = 50, max_workers: int = 5):
        """同步所有股票数据"""
        logger.info("🚀 开始全量股票数据同步")
        self.stats['start_time'] = datetime.now()
        
        try:
            # 1. 更新股票基本信息
            self._update_stock_basic()
            
            # 2. 获取所有A股代码
            all_stocks = self._get_all_a_stocks()
            if not all_stocks:
                logger.error("❌ 无法获取股票列表")
                return False
            
            self.stats['total_stocks'] = len(all_stocks)
            self.stats['total_batches'] = (len(all_stocks) + batch_size - 1) // batch_size
            
            logger.info(f"📊 总共需要同步 {len(all_stocks)} 只A股")
            logger.info(f"📦 分 {self.stats['total_batches']} 批处理，每批 {batch_size} 只")
            
            # 3. 检查是否有未完成的进度
            resume_from = self._load_progress()
            if resume_from > 0:
                logger.info(f"🔄 从第 {resume_from + 1} 批继续同步")
                all_stocks = all_stocks[resume_from * batch_size:]
                self.stats['current_batch'] = resume_from
                self.stats['completed_stocks'] = resume_from * batch_size
            
            # 4. 分批并行同步
            self._batch_sync_stocks(all_stocks, days, batch_size, max_workers)
            
            # 5. 输出最终统计
            self._print_final_stats()
            
            # 6. 清理进度文件
            self._clear_progress()
            
            return True
            
        except KeyboardInterrupt:
            logger.info("⏸️ 用户中断同步，保存进度...")
            self._save_progress()
            return False
        except Exception as e:
            logger.error(f"❌ 同步异常: {e}")
            self._save_progress()
            return False
    
    def _update_stock_basic(self):
        """更新股票基本信息"""
        logger.info("📋 更新股票基本信息...")
        
        try:
            # 获取所有A股基本信息
            stock_basic = self.ts_api.stock_basic(exchange='', list_status='L')
            
            if not stock_basic.empty:
                # 过滤A股（排除港股、美股等）
                a_stocks = stock_basic[
                    (stock_basic['ts_code'].str.endswith('.SZ')) | 
                    (stock_basic['ts_code'].str.endswith('.SH'))
                ]
                
                # 使用REPLACE模式更新
                success = self.db.insert_dataframe('stock_basic', a_stocks, replace=True)
                if success:
                    logger.info(f"✅ 股票基本信息更新完成: {len(a_stocks)} 只A股")
                else:
                    logger.error("❌ 股票基本信息更新失败")
            else:
                logger.warning("⚠️ 未获取到股票基本信息")
                
        except Exception as e:
            logger.error(f"❌ 更新股票基本信息失败: {e}")
    
    def _get_all_a_stocks(self):
        """获取所有A股代码"""
        try:
            sql = """
            SELECT ts_code, name, industry, market
            FROM stock_basic 
            WHERE (ts_code LIKE '%%.SZ' OR ts_code LIKE '%%.SH')
              AND ts_code NOT LIKE '688%%'
            ORDER BY ts_code
            """
            df = self.db.fetch_data(sql)
            
            if not df.empty:
                stocks = df['ts_code'].tolist()
                logger.info(f"📈 获取到 {len(stocks)} 只A股代码")
                return stocks
            else:
                logger.error("❌ 数据库中无股票基本信息")
                return []
                
        except Exception as e:
            logger.error(f"❌ 获取股票列表失败: {e}")
            return []
    
    def _batch_sync_stocks(self, all_stocks, days, batch_size, max_workers):
        """分批同步股票数据"""
        
        # 计算日期范围
        end_date = datetime.now().strftime('%Y%m%d')
        start_date = (datetime.now() - timedelta(days=days)).strftime('%Y%m%d')
        
        # 分批处理
        for i in range(0, len(all_stocks), batch_size):
            batch = all_stocks[i:i + batch_size]
            batch_num = self.stats['current_batch'] + 1
            
            logger.info(f"📦 处理批次 {batch_num}/{self.stats['total_batches']} ({len(batch)} 只股票)")
            
            # 使用线程池并行处理当前批次
            with ThreadPoolExecutor(max_workers=max_workers) as executor:
                # 提交任务
                future_to_stock = {
                    executor.submit(self._sync_single_stock, ts_code, start_date, end_date): ts_code 
                    for ts_code in batch
                }
                
                # 使用tqdm显示批次内进度
                batch_progress = tqdm(
                    as_completed(future_to_stock), 
                    total=len(batch),
                    desc=f"Batch {batch_num}",
                    unit="stock"
                )
                
                batch_success = 0
                for future in batch_progress:
                    ts_code = future_to_stock[future]
                    try:
                        success = future.result(timeout=30)
                        if success:
                            batch_success += 1
                            with self.stats_lock:
                                self.stats['completed_stocks'] += 1
                        else:
                            with self.stats_lock:
                                self.stats['failed_stocks'] += 1
                                self.stats['failed_list'].append(ts_code)
                    except Exception as e:
                        logger.error(f"❌ {ts_code}: {e}")
                        with self.stats_lock:
                            self.stats['failed_stocks'] += 1
                            self.stats['failed_list'].append(ts_code)
                
                batch_progress.close()
            
            # 批次完成统计
            success_rate = (batch_success / len(batch)) * 100
            logger.info(f"✅ 批次 {batch_num} 完成: {batch_success}/{len(batch)} 成功 ({success_rate:.1f}%)")
            
            # 更新进度
            self.stats['current_batch'] = batch_num
            if batch_num % 5 == 0:  # 每5批保存一次进度
                self._save_progress()
            
            # 批次间休息，避免API限制
            if batch_num < self.stats['total_batches']:
                time.sleep(2)
    
    def _get_table_name_for_date(self, trade_date):
        """根据交易日期获取对应的分表名称"""
        if isinstance(trade_date, str):
            if len(trade_date) == 8:  # YYYYMMDD格式
                year_month = trade_date[:6]
            else:  # YYYY-MM-DD格式
                year_month = trade_date.replace('-', '')[:6]
        else:
            year_month = trade_date.strftime('%Y%m')
        
        return f'stock_daily_{year_month}'
    
    def _sync_single_stock(self, ts_code, start_date, end_date):
        """同步单只股票数据"""
        try:
            # 获取日线数据
            df = self.ts_api.daily(
                ts_code=ts_code, 
                start_date=start_date, 
                end_date=end_date
            )
            
            if not df.empty:
                # 添加计算字段
                df['change'] = df['close'] - df['pre_close']
                df['change_pct'] = df['pct_chg']
                
                # 按月份分组数据，分别插入对应表
                success_count = 0
                grouped = df.groupby(df['trade_date'].str[:6])  # 按年月分组
                
                for year_month, month_data in grouped:
                    table_name = f'stock_daily_{year_month}'
                    success = self.db.insert_dataframe(table_name, month_data, replace=False)
                    if success:
                        success_count += 1
                
                return success_count > 0
            else:
                logger.debug(f"📊 {ts_code}: 无数据")
                return True  # 无数据也算成功
                
        except Exception as e:
            logger.error(f"❌ {ts_code}: {e}")
            return False
    
    def _save_progress(self):
        """保存同步进度"""
        try:
            progress_data = {
                'current_batch': self.stats['current_batch'],
                'completed_stocks': self.stats['completed_stocks'],
                'failed_stocks': self.stats['failed_stocks'],
                'failed_list': self.stats['failed_list'],
                'last_save_time': datetime.now().isoformat()
            }
            
            with open(self.progress_file, 'w', encoding='utf-8') as f:
                json.dump(progress_data, f, ensure_ascii=False, indent=2)
                
            logger.info(f"💾 进度已保存: 完成 {self.stats['completed_stocks']} 只股票")
            
        except Exception as e:
            logger.error(f"❌ 保存进度失败: {e}")
    
    def _load_progress(self):
        """加载同步进度"""
        try:
            if os.path.exists(self.progress_file):
                with open(self.progress_file, 'r', encoding='utf-8') as f:
                    progress_data = json.load(f)
                
                self.stats['completed_stocks'] = progress_data.get('completed_stocks', 0)
                self.stats['failed_stocks'] = progress_data.get('failed_stocks', 0)
                self.stats['failed_list'] = progress_data.get('failed_list', [])
                
                return progress_data.get('current_batch', 0)
            else:
                return 0
                
        except Exception as e:
            logger.error(f"❌ 加载进度失败: {e}")
            return 0
    
    def _clear_progress(self):
        """清理进度文件"""
        try:
            if os.path.exists(self.progress_file):
                os.remove(self.progress_file)
        except Exception as e:
            logger.error(f"❌ 清理进度文件失败: {e}")
    
    def _print_final_stats(self):
        """打印最终统计信息"""
        end_time = datetime.now()
        duration = end_time - self.stats['start_time']
        
        logger.info("=" * 60)
        logger.info("🎉 全量股票数据同步完成！")
        logger.info(f"📊 总股票数: {self.stats['total_stocks']}")
        logger.info(f"✅ 成功同步: {self.stats['completed_stocks']}")
        logger.info(f"❌ 失败数量: {self.stats['failed_stocks']}")
        logger.info(f"⏱️ 总耗时: {duration}")
        logger.info(f"🚀 平均速度: {self.stats['completed_stocks'] / duration.total_seconds():.2f} 股票/秒")
        
        if self.stats['failed_list']:
            logger.warning(f"❌ 失败股票列表: {self.stats['failed_list'][:10]}...")
        
        # 数据库统计
        self._print_database_stats()
    
    def _print_database_stats(self):
        """打印数据库统计信息"""
        try:
            # 获取当前月的表名
            current_table = self._get_table_name_for_date(datetime.now().strftime('%Y%m%d'))
            
            # 当前月统计
            current_stats = self.db.fetch_data(f"""
                SELECT 
                    COUNT(DISTINCT ts_code) as total_stocks,
                    COUNT(*) as total_records,
                    MIN(trade_date) as earliest_date,
                    MAX(trade_date) as latest_date
                FROM {current_table}
            """)
            
            if not current_stats.empty:
                stats = current_stats.iloc[0]
                logger.info("📈 当前月数据库统计:")
                logger.info(f"   股票总数: {stats['total_stocks']}")
                logger.info(f"   记录总数: {stats['total_records']:,}")
                logger.info(f"   日期范围: {stats['earliest_date']} - {stats['latest_date']}")
                
            # 今日更新统计 - 检查最新交易日期
            today_stats = self.db.fetch_data(f"""
                SELECT COUNT(DISTINCT ts_code) as today_updates
                FROM {current_table}
                WHERE trade_date >= DATE_SUB(CURDATE(), INTERVAL 3 DAY)
            """)
            
            if not today_stats.empty:
                today_count = today_stats.iloc[0]['today_updates']
                logger.info(f"   今日更新: {today_count} 只股票")
                
        except Exception as e:
            logger.error(f"❌ 获取数据库统计失败: {e}")
    
    def retry_failed_stocks(self, days: int = 30):
        """重试失败的股票"""
        if not self.stats['failed_list']:
            logger.info("✅ 没有失败的股票需要重试")
            return
        
        logger.info(f"🔄 重试 {len(self.stats['failed_list'])} 只失败的股票")
        
        end_date = datetime.now().strftime('%Y%m%d')
        start_date = (datetime.now() - timedelta(days=days)).strftime('%Y%m%d')
        
        retry_success = 0
        for ts_code in self.stats['failed_list']:
            success = self._sync_single_stock(ts_code, start_date, end_date)
            if success:
                retry_success += 1
                logger.info(f"✅ 重试成功: {ts_code}")
            else:
                logger.error(f"❌ 重试失败: {ts_code}")
            
            time.sleep(0.2)  # 避免API限制
        
        logger.info(f"🔄 重试完成: {retry_success}/{len(self.stats['failed_list'])} 成功")

def main():
    """主函数"""
    import argparse
    
    parser = argparse.ArgumentParser(description='全量股票数据同步系统')
    parser.add_argument('--days', type=int, default=30, help='同步天数 (默认30天)')
    parser.add_argument('--batch-size', type=int, default=50, help='批处理大小 (默认50)')
    parser.add_argument('--workers', type=int, default=5, help='并发线程数 (默认5)')
    parser.add_argument('--retry', action='store_true', help='重试失败的股票')
    parser.add_argument('--resume', action='store_true', help='从上次中断处继续')
    
    args = parser.parse_args()
    
    syncer = FullStockDataSync()
    
    try:
        if args.retry:
            syncer.retry_failed_stocks(args.days)
        else:
            success = syncer.sync_all_stocks(
                days=args.days,
                batch_size=args.batch_size,
                max_workers=args.workers
            )
            
            if success:
                logger.info("🎉 全量同步成功完成！")
            else:
                logger.warning("⚠️ 同步未完成，可使用 --resume 参数继续")
                
    except KeyboardInterrupt:
        logger.info("⏸️ 用户中断操作")
    except Exception as e:
        logger.error(f"❌ 系统异常: {e}")

if __name__ == "__main__":
    main()