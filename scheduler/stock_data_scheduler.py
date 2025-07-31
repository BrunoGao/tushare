#!/usr/bin/env python3
"""
定时任务调度器 - 股票数据自动更新系统
支持多策略、多线程、智能重试的数据获取
"""

import schedule
import time
import threading
import logging
from datetime import datetime, timedelta
from concurrent.futures import ThreadPoolExecutor, as_completed
import tushare as ts
import pandas as pd
from typing import List, Dict, Optional
import sys
import os

# 添加项目路径
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import config
from database.db_manager import DatabaseManager
from utils.system_monitor import system_monitor

# 配置日志
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler('logs/scheduler.log'),
        logging.StreamHandler()
    ]
)
logger = logging.getLogger(__name__)

class StockDataScheduler:
    """股票数据定时调度器"""
    
    def __init__(self):
        self.db = DatabaseManager()
        self.ts_api = ts.pro_api(config.TS_TOKEN)
        self.is_running = False
        self.thread_pool = ThreadPoolExecutor(max_workers=config.TS_THREAD_COUNT)
        self.stats = {
            'total_updates': 0,
            'successful_updates': 0,
            'failed_updates': 0,
            'last_update_time': None,
            'current_batch': 0
        }
        
    def start_scheduler(self):
        """启动调度器"""
        logger.info("🚀 启动股票数据定时调度器")
        self.is_running = True
        
        # 配置定时任务
        self._setup_schedules()
        
        # 启动调度循环
        try:
            while self.is_running:
                schedule.run_pending()
                time.sleep(1)
        except KeyboardInterrupt:
            logger.info("收到停止信号，正在关闭调度器...")
            self.stop_scheduler()
    
    def stop_scheduler(self):
        """停止调度器"""
        self.is_running = False
        self.thread_pool.shutdown(wait=True)
        logger.info("📴 调度器已停止")
    
    def _setup_schedules(self):
        """设置调度任务"""
        
        # 1. 交易日实时数据更新 (工作日 9:30-15:00 每5分钟)
        if self._is_trading_day():
            schedule.every(5).minutes.do(self._update_realtime_data).tag('realtime')
            logger.info("⏰ 设置实时数据更新: 每5分钟")
        
        # 2. 每日历史数据更新 (工作日 16:00)
        schedule.every().monday.at("16:00").do(self._update_daily_data).tag('daily')
        schedule.every().tuesday.at("16:00").do(self._update_daily_data).tag('daily')
        schedule.every().wednesday.at("16:00").do(self._update_daily_data).tag('daily')
        schedule.every().thursday.at("16:00").do(self._update_daily_data).tag('daily')
        schedule.every().friday.at("16:00").do(self._update_daily_data).tag('daily')
        logger.info("⏰ 设置每日数据更新: 工作日16:00")
        
        # 3. 股票基本信息更新 (每周日 20:00)
        schedule.every().sunday.at("20:00").do(self._update_stock_basic).tag('basic')
        logger.info("⏰ 设置基本信息更新: 每周日20:00")
        
        # 4. 财务数据更新 (每月1日 21:00)
        schedule.every().day.at("21:00").do(self._check_monthly_update).tag('financial')
        logger.info("⏰ 设置财务数据更新: 每月1日21:00")
        
        # 5. 系统监控 (每小时)
        schedule.every().hour.do(self._system_health_check).tag('monitor')
        logger.info("⏰ 设置系统监控: 每小时")
    
    def _is_trading_day(self) -> bool:
        """判断是否为交易日"""
        today = datetime.now()
        # 简化判断：周一到周五
        return today.weekday() < 5
    
    def _is_trading_time(self) -> bool:
        """判断是否为交易时间"""
        now = datetime.now().time()
        morning_start = datetime.strptime("09:30", "%H:%M").time()
        morning_end = datetime.strptime("11:30", "%H:%M").time()
        afternoon_start = datetime.strptime("13:00", "%H:%M").time()
        afternoon_end = datetime.strptime("15:00", "%H:%M").time()
        
        return (morning_start <= now <= morning_end) or (afternoon_start <= now <= afternoon_end)
    
    def _update_realtime_data(self):
        """更新实时数据"""
        if not self._is_trading_time():
            logger.info("非交易时间，跳过实时数据更新")
            return
            
        logger.info("🔄 开始实时数据更新")
        start_time = time.time()
        
        try:
            # 获取活跃股票列表
            active_stocks = self._get_active_stocks()
            logger.info(f"获取到 {len(active_stocks)} 只活跃股票")
            
            # 并行更新
            successful_updates = self._parallel_update_stocks(active_stocks, 'realtime')
            
            # 更新统计
            self.stats['total_updates'] += len(active_stocks)
            self.stats['successful_updates'] += successful_updates
            self.stats['failed_updates'] += len(active_stocks) - successful_updates
            self.stats['last_update_time'] = datetime.now()
            
            elapsed_time = time.time() - start_time
            logger.info(f"✅ 实时数据更新完成: {successful_updates}/{len(active_stocks)} 成功, 耗时 {elapsed_time:.2f}s")
            
        except Exception as e:
            logger.error(f"❌ 实时数据更新失败: {e}")
    
    def _update_daily_data(self):
        """更新每日数据"""
        logger.info("🔄 开始每日数据更新")
        start_time = time.time()
        
        try:
            # 获取所有股票
            all_stocks = self._get_all_stocks()
            logger.info(f"获取到 {len(all_stocks)} 只股票")
            
            # 分批处理
            batch_size = config.TS_BATCH_SIZE
            total_batches = (len(all_stocks) + batch_size - 1) // batch_size
            successful_updates = 0
            
            for i in range(0, len(all_stocks), batch_size):
                batch = all_stocks[i:i + batch_size]
                self.stats['current_batch'] = i // batch_size + 1
                
                logger.info(f"处理批次 {self.stats['current_batch']}/{total_batches}")
                batch_success = self._parallel_update_stocks(batch, 'daily')
                successful_updates += batch_success
                
                # 批次间休息，避免API限制
                if self.stats['current_batch'] < total_batches:
                    time.sleep(1)
            
            # 更新统计
            self.stats['total_updates'] += len(all_stocks)
            self.stats['successful_updates'] += successful_updates
            self.stats['failed_updates'] += len(all_stocks) - successful_updates
            self.stats['last_update_time'] = datetime.now()
            
            elapsed_time = time.time() - start_time
            logger.info(f"✅ 每日数据更新完成: {successful_updates}/{len(all_stocks)} 成功, 耗时 {elapsed_time:.2f}s")
            
        except Exception as e:
            logger.error(f"❌ 每日数据更新失败: {e}")
    
    def _update_stock_basic(self):
        """更新股票基本信息"""
        logger.info("🔄 开始股票基本信息更新")
        
        try:
            # 获取股票基本信息
            stock_basic = self.ts_api.stock_basic(exchange='', list_status='L')
            
            if not stock_basic.empty:
                # 更新数据库
                self.db.insert_dataframe('stock_basic', stock_basic, replace=True)
                logger.info(f"✅ 股票基本信息更新完成: {len(stock_basic)} 条记录")
            else:
                logger.warning("⚠️ 未获取到股票基本信息")
                
        except Exception as e:
            logger.error(f"❌ 股票基本信息更新失败: {e}")
    
    def _check_monthly_update(self):
        """检查是否需要月度更新"""
        today = datetime.now()
        if today.day == 1:  # 每月1日
            self._update_financial_data()
    
    def _update_financial_data(self):
        """更新财务数据"""
        logger.info("🔄 开始财务数据更新")
        
        try:
            # 获取所有股票代码
            stocks = self._get_all_stocks()
            
            # 分批获取财务数据
            batch_size = 50  # 财务数据API限制较严格
            for i in range(0, len(stocks), batch_size):
                batch = stocks[i:i + batch_size]
                self._update_financial_batch(batch)
                time.sleep(2)  # 避免API限制
                
            logger.info("✅ 财务数据更新完成")
            
        except Exception as e:
            logger.error(f"❌ 财务数据更新失败: {e}")
    
    def _update_financial_batch(self, stock_codes: List[str]):
        """批量更新财务数据"""
        for ts_code in stock_codes:
            try:
                # 获取利润表
                income = self.ts_api.income(ts_code=ts_code, period='20231231')
                if not income.empty:
                    self.db.insert_dataframe('income_statement', income, replace=True)
                
                # 获取资产负债表
                balancesheet = self.ts_api.balancesheet(ts_code=ts_code, period='20231231')
                if not balancesheet.empty:
                    self.db.insert_dataframe('balance_sheet', balancesheet, replace=True)
                
                time.sleep(0.2)  # API限制
                
            except Exception as e:
                logger.error(f"更新 {ts_code} 财务数据失败: {e}")
    
    def _system_health_check(self):
        """系统健康检查"""
        logger.info("🔍 系统健康检查")
        
        try:
            # 检查数据库连接
            if not self.db.test_connection():
                logger.error("❌ 数据库连接异常")
                return
            
            # 检查API连接
            try:
                test_data = self.ts_api.stock_basic(ts_code='000001.SZ')
                if test_data.empty:
                    logger.warning("⚠️ TuShare API响应异常")
            except Exception as e:
                logger.error(f"❌ TuShare API连接失败: {e}")
            
            # 记录系统状态
            system_monitor.log_system_status({
                'scheduler_status': 'running' if self.is_running else 'stopped',
                'total_updates': self.stats['total_updates'],
                'success_rate': self.stats['successful_updates'] / max(self.stats['total_updates'], 1) * 100,
                'last_update': self.stats['last_update_time']
            })
            
            logger.info("✅ 系统健康检查完成")
            
        except Exception as e:
            logger.error(f"❌ 系统健康检查失败: {e}")
    
    def _get_active_stocks(self) -> List[str]:
        """获取活跃股票列表"""
        try:
            # 从数据库获取最近有交易的股票
            sql = """
            SELECT DISTINCT ts_code 
            FROM daily_data 
            WHERE trade_date >= DATE_SUB(CURDATE(), INTERVAL 5 DAY)
            AND vol > 0 
            ORDER BY trade_date DESC 
            LIMIT 200
            """
            df = self.db.fetch_data(sql)
            return df['ts_code'].tolist() if not df.empty else []
        except Exception as e:
            logger.error(f"获取活跃股票失败: {e}")
            return []
    
    def _get_all_stocks(self) -> List[str]:
        """获取所有股票代码"""
        try:
            sql = "SELECT ts_code FROM stock_basic WHERE list_status = 'L'"
            df = self.db.fetch_data(sql)
            return df['ts_code'].tolist() if not df.empty else []
        except Exception as e:
            logger.error(f"获取股票列表失败: {e}")
            return []
    
    def _parallel_update_stocks(self, stock_codes: List[str], update_type: str) -> int:
        """并行更新股票数据"""
        successful_updates = 0
        
        # 提交任务到线程池
        future_to_stock = {}
        for ts_code in stock_codes:
            if update_type == 'realtime':
                future = self.thread_pool.submit(self._update_single_stock_realtime, ts_code)
            else:
                future = self.thread_pool.submit(self._update_single_stock_daily, ts_code)
            future_to_stock[future] = ts_code
        
        # 收集结果
        for future in as_completed(future_to_stock):
            ts_code = future_to_stock[future]
            try:
                success = future.result(timeout=30)
                if success:
                    successful_updates += 1
            except Exception as e:
                logger.error(f"更新 {ts_code} 失败: {e}")
        
        return successful_updates
    
    def _update_single_stock_realtime(self, ts_code: str) -> bool:
        """更新单只股票实时数据"""
        try:
            # 获取实时行情（这里用日线数据模拟，实际应用可用实时接口）
            today = datetime.now().strftime('%Y%m%d')
            df = self.ts_api.daily(ts_code=ts_code, start_date=today, end_date=today)
            
            if not df.empty:
                self.db.insert_dataframe('daily_data', df, replace=True)
                return True
            return False
            
        except Exception as e:
            logger.error(f"更新 {ts_code} 实时数据失败: {e}")
            return False
    
    def _update_single_stock_daily(self, ts_code: str) -> bool:
        """更新单只股票日线数据"""
        try:
            # 获取最近30天数据
            start_date = (datetime.now() - timedelta(days=30)).strftime('%Y%m%d')
            end_date = datetime.now().strftime('%Y%m%d')
            
            df = self.ts_api.daily(ts_code=ts_code, start_date=start_date, end_date=end_date)
            
            if not df.empty:
                self.db.insert_dataframe('daily_data', df, replace=True)
                return True
            return False
            
        except Exception as e:
            logger.error(f"更新 {ts_code} 日线数据失败: {e}")
            return False
    
    def get_status(self) -> Dict:
        """获取调度器状态"""
        return {
            'is_running': self.is_running,
            'stats': self.stats.copy(),
            'scheduled_jobs': len(schedule.jobs),
            'thread_pool_active': self.thread_pool._threads if hasattr(self.thread_pool, '_threads') else 0
        }

def main():
    """主函数"""
    scheduler = StockDataScheduler()
    
    try:
        scheduler.start_scheduler()
    except KeyboardInterrupt:
        logger.info("收到中断信号，正在停止...")
        scheduler.stop_scheduler()

if __name__ == "__main__":
    main()