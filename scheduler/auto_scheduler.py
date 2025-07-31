#!/usr/bin/env python3
"""
自动定时更新系统 - 智能调度股票数据更新
支持多种更新策略和时间配置
"""

import sys
import os
import time
import schedule
import threading
import logging
from datetime import datetime, timedelta
from concurrent.futures import ThreadPoolExecutor
import json

# 添加项目路径
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import config
from database.db_manager import DatabaseManager
from scheduler.full_stock_sync import FullStockDataSync
from scheduler.enhanced_data_sync import EnhancedDataSync
from scheduler.technical_indicators_sync import TechnicalIndicatorSync

# 配置日志
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler('logs/auto_scheduler.log'),
        logging.StreamHandler()
    ]
)
logger = logging.getLogger(__name__)

class AutoStockScheduler:
    """自动股票数据调度器"""
    
    def __init__(self):
        self.db = DatabaseManager()
        self.full_syncer = FullStockDataSync()
        self.enhanced_syncer = EnhancedDataSync()
        self.technical_syncer = TechnicalIndicatorSync()
        self.is_running = False
        self.executor = ThreadPoolExecutor(max_workers=3)
        self.schedule_config_file = 'config/scheduler_config.json'
        
        # 默认调度配置
        self.schedule_config = {
            "daily_update": {
                "enabled": True,
                "times": ["09:35", "15:05", "21:00"],  # 开盘后、收盘后、晚上
                "weekdays": [0, 1, 2, 3, 4]  # 周一到周五
            },
            "enhanced_data_update": {
                "enabled": True,
                "times": ["10:00", "16:00"],  # 增强数据更新
                "weekdays": [0, 1, 2, 3, 4]
            },
            "technical_indicators_update": {
                "enabled": True,
                "times": ["10:30", "16:30"],  # 技术指标更新
                "weekdays": [0, 1, 2, 3, 4]
            },
            "weekly_full_sync": {
                "enabled": True,
                "day": 6,  # 周日
                "time": "20:00",
                "days_to_sync": 7
            },
            "monthly_enhanced_sync": {
                "enabled": True,
                "day_of_month": 1,
                "time": "03:00"
            },
            "market_hours": {
                "morning_start": "09:30",
                "morning_end": "11:30", 
                "afternoon_start": "13:00",
                "afternoon_end": "15:00"
            }
        }
        
        # 加载配置
        self._load_schedule_config()
        
        # 确保目录存在
        os.makedirs('logs', exist_ok=True)
        os.makedirs('config', exist_ok=True)
    
    def start_scheduler(self):
        """启动自动调度器"""
        logger.info("🚀 启动自动股票数据调度器")
        self.is_running = True
        
        # 设置调度任务
        self._setup_schedules()
        
        # 立即执行一次数据检查
        self._initial_data_check()
        
        # 启动调度循环
        try:
            while self.is_running:
                schedule.run_pending()
                time.sleep(60)  # 每分钟检查一次
        except KeyboardInterrupt:
            logger.info("收到停止信号")
            self.stop_scheduler()
    
    def stop_scheduler(self):
        """停止调度器"""
        self.is_running = False
        self.executor.shutdown(wait=True)
        logger.info("📴 自动调度器已停止")
    
    def _setup_schedules(self):
        """设置调度任务"""
        config = self.schedule_config
        
        # 1. 每日更新任务
        if config["daily_update"]["enabled"]:
            for update_time in config["daily_update"]["times"]:
                schedule.every().monday.at(update_time).do(self._run_daily_update)
                schedule.every().tuesday.at(update_time).do(self._run_daily_update)
                schedule.every().wednesday.at(update_time).do(self._run_daily_update)
                schedule.every().thursday.at(update_time).do(self._run_daily_update)
                schedule.every().friday.at(update_time).do(self._run_daily_update)
            
            logger.info(f"⏰ 每日更新: 工作日 {config['daily_update']['times']}")
        
        # 2. 增强数据更新任务
        if config["enhanced_data_update"]["enabled"]:
            for update_time in config["enhanced_data_update"]["times"]:
                schedule.every().monday.at(update_time).do(self._run_enhanced_update)
                schedule.every().tuesday.at(update_time).do(self._run_enhanced_update)
                schedule.every().wednesday.at(update_time).do(self._run_enhanced_update)
                schedule.every().thursday.at(update_time).do(self._run_enhanced_update)
                schedule.every().friday.at(update_time).do(self._run_enhanced_update)
            
            logger.info(f"⏰ 增强数据更新: 工作日 {config['enhanced_data_update']['times']}")
        
        # 3. 技术指标更新任务
        if config["technical_indicators_update"]["enabled"]:
            for update_time in config["technical_indicators_update"]["times"]:
                schedule.every().monday.at(update_time).do(self._run_technical_indicators_update)
                schedule.every().tuesday.at(update_time).do(self._run_technical_indicators_update)
                schedule.every().wednesday.at(update_time).do(self._run_technical_indicators_update)
                schedule.every().thursday.at(update_time).do(self._run_technical_indicators_update)
                schedule.every().friday.at(update_time).do(self._run_technical_indicators_update)
            
            logger.info(f"⏰ 技术指标更新: 工作日 {config['technical_indicators_update']['times']}")
        
        # 4. 周度全量同步
        if config["weekly_full_sync"]["enabled"]:
            if config["weekly_full_sync"]["day"] == 0:  # 周一
                schedule.every().monday.at(config["weekly_full_sync"]["time"]).do(
                    self._run_weekly_sync
                )
            elif config["weekly_full_sync"]["day"] == 6:  # 周日
                schedule.every().sunday.at(config["weekly_full_sync"]["time"]).do(
                    self._run_weekly_sync
                )
            # 可以扩展其他星期几
            
            logger.info(f"⏰ 周度同步: 每周{self._get_weekday_name(config['weekly_full_sync']['day'])} {config['weekly_full_sync']['time']}")
        
        # 5. 月度全量同步
        if config["monthly_full_sync"]["enabled"]:
            schedule.every().day.at(config["monthly_full_sync"]["time"]).do(
                self._check_monthly_sync
            )
            
            logger.info(f"⏰ 月度同步: 每月{config['monthly_full_sync']['day_of_month']}日 {config['monthly_full_sync']['time']}")
        
        # 6. 市场开盘检查
        schedule.every().monday.at("09:25").do(self._market_open_check)
        schedule.every().tuesday.at("09:25").do(self._market_open_check)
        schedule.every().wednesday.at("09:25").do(self._market_open_check)
        schedule.every().thursday.at("09:25").do(self._market_open_check)
        schedule.every().friday.at("09:25").do(self._market_open_check)
        
        logger.info("⏰ 市场开盘检查: 工作日 09:25")
        
        # 7. 数据健康检查 (每小时)
        schedule.every().hour.do(self._data_health_check)
        logger.info("⏰ 数据健康检查: 每小时")
    
    def _run_enhanced_update(self):
        """执行增强数据更新"""
        if not self._is_trading_day():
            logger.info("⏭️ 非交易日，跳过增强数据更新")
            return
        
        logger.info("🔄 开始增强数据更新")
        
        def enhanced_task():
            try:
                # 更新资金流数据
                success1 = self.enhanced_syncer.sync_money_flow_data()
                # 更新龙虎榜数据  
                success2 = self.enhanced_syncer.sync_dragon_tiger_data()
                
                if success1 or success2:
                    logger.info("✅ 增强数据更新完成")
                else:
                    logger.warning("⚠️ 增强数据更新无新数据")
            except Exception as e:
                logger.error(f"❌ 增强数据更新失败: {e}")
        
        # 在后台执行
        self.executor.submit(enhanced_task)
    
    def _run_technical_indicators_update(self):
        """执行技术指标更新"""
        if not self._is_trading_day():
            logger.info("⏭️ 非交易日，跳过技术指标更新")
            return
        
        logger.info("🔄 开始技术指标更新")
        
        def technical_task():
            try:
                # 更新技术指标数据
                success = self.technical_syncer.sync_all_technical_data()
                
                if success:
                    logger.info("✅ 技术指标更新完成")
                else:
                    logger.warning("⚠️ 技术指标更新失败")
            except Exception as e:
                logger.error(f"❌ 技术指标更新失败: {e}")
        
        # 在后台执行
        self.executor.submit(technical_task)
    
    def _run_daily_update(self):
        """执行每日更新"""
        if not self._is_trading_day():
            logger.info("⏭️ 非交易日，跳过每日更新")
            return
        
        logger.info("🔄 开始每日数据更新")
        
        def update_task():
            try:
                # 这里可以调用daily_updater如果需要
                # 目前使用enhanced_syncer的资金流更新作为每日更新
                success_count = self.enhanced_syncer.sync_money_flow_data()
                if success_count:
                    logger.info(f"✅ 每日更新完成")
                else:
                    logger.warning("⚠️ 每日更新无数据")
            except Exception as e:
                logger.error(f"❌ 每日更新失败: {e}")
        
        # 在后台执行
        self.executor.submit(update_task)
    
    def _run_weekly_sync(self):
        """执行周度全量同步"""
        logger.info("🔄 开始周度全量同步")
        
        def weekly_task():
            try:
                days = self.schedule_config["weekly_full_sync"]["days_to_sync"]
                success = self.full_syncer.sync_all_stocks(
                    days=days, 
                    batch_size=30,  # 周末可以用较小批次
                    max_workers=3
                )
                if success:
                    logger.info("✅ 周度全量同步完成")
                else:
                    logger.warning("⚠️ 周度全量同步未完成")
            except Exception as e:
                logger.error(f"❌ 周度全量同步失败: {e}")
        
        # 在后台执行
        self.executor.submit(weekly_task)
    
    def _check_monthly_sync(self):
        """检查是否需要月度同步"""
        today = datetime.now()
        target_day = self.schedule_config["monthly_full_sync"]["day_of_month"]
        
        if today.day == target_day:
            self._run_monthly_sync()
    
    def _run_monthly_sync(self):
        """执行月度全量同步"""
        logger.info("🔄 开始月度全量同步")
        
        def monthly_task():
            try:
                days = self.schedule_config["monthly_full_sync"]["days_to_sync"]
                success = self.full_syncer.sync_all_stocks(
                    days=days,
                    batch_size=100,  # 月度同步可以用大批次
                    max_workers=8
                )
                if success:
                    logger.info("✅ 月度全量同步完成")
                    # 同步完成后清理旧数据
                    self._cleanup_old_data(days=90)
                else:
                    logger.warning("⚠️ 月度全量同步未完成")
            except Exception as e:
                logger.error(f"❌ 月度全量同步失败: {e}")
        
        # 在后台执行
        self.executor.submit(monthly_task)
    
    def _market_open_check(self):
        """市场开盘前检查"""
        logger.info("🔍 市场开盘前数据检查")
        
        try:
            # 检查昨日数据完整性
            yesterday = (datetime.now() - timedelta(days=1)).strftime('%Y-%m-%d')
            
            missing_data = self.db.fetch_data(f"""
                SELECT COUNT(*) as missing_count
                FROM stock_basic sb
                LEFT JOIN daily_data dd ON sb.ts_code = dd.ts_code 
                    AND dd.trade_date = '{yesterday}'
                WHERE dd.ts_code IS NULL
                LIMIT 100
            """)
            
            if not missing_data.empty:
                missing_count = missing_data.iloc[0]['missing_count']
                if missing_count > 100:  # 如果缺失较多
                    logger.warning(f"⚠️ 昨日数据缺失 {missing_count} 只股票，将执行补充同步")
                    # 触发补充同步
                    self.executor.submit(self._补充昨日数据)
                
        except Exception as e:
            logger.error(f"❌ 开盘前检查失败: {e}")
    
    def _data_health_check(self):
        """数据健康检查"""
        try:
            # 获取当前月表名
            current_table = f'stock_daily_{datetime.now().strftime("%Y%m")}'
            
            # 检查最新数据时间
            latest_data = self.db.fetch_data(f"""
                SELECT MAX(trade_date) as latest_date,
                       COUNT(DISTINCT ts_code) as stock_count
                FROM {current_table}
            """)
            
            if not latest_data.empty:
                latest_date = latest_data.iloc[0]['latest_date']
                stock_count = latest_data.iloc[0]['stock_count']
                
                # 检查数据是否过时
                if latest_date:
                    if isinstance(latest_date, str):
                        latest_date_obj = datetime.strptime(latest_date, '%Y%m%d').date()
                    else:
                        latest_date_obj = latest_date
                    
                    days_old = (datetime.now().date() - latest_date_obj).days
                    if days_old > 3:  # 数据超过3天
                        logger.warning(f"⚠️ 数据已过时 {days_old} 天，最新日期: {latest_date}")
                
                # 检查股票数量是否正常
                if stock_count < 3000:  # A股总数应该在4000+
                    logger.warning(f"⚠️ 股票数据不完整，仅有 {stock_count} 只")
                
        except Exception as e:
            logger.error(f"❌ 数据健康检查失败: {e}")
    
    def _initial_data_check(self):
        """初始数据检查"""
        logger.info("🔍 执行初始数据检查")
        
        try:
            # 检查是否有基础数据
            stock_count = self.db.fetch_data("SELECT COUNT(*) as count FROM stock_basic").iloc[0]['count']
            
            # 检查当前月的数据表
            current_table = f'stock_daily_{datetime.now().strftime("%Y%m")}'
            try:
                data_count = self.db.fetch_data(f"SELECT COUNT(*) as count FROM {current_table}").iloc[0]['count']
            except:
                data_count = 0
                logger.info(f"📅 当前月表 {current_table} 不存在，将创建")
            
            logger.info(f"📊 当前数据状态: {stock_count} 只股票, {data_count:,} 条记录")
            
            # 如果数据很少，触发初始同步
            if data_count < 10000:  # 少于1万条记录
                logger.info("🚀 数据较少，启动初始全量同步")
                self.executor.submit(lambda: self.full_syncer.sync_all_stocks(days=7, batch_size=30, max_workers=3))
            
        except Exception as e:
            logger.error(f"❌ 初始数据检查失败: {e}")
    
    def _cleanup_old_data(self, days: int = 90):
        """清理旧数据"""
        try:
            cutoff_date = (datetime.now() - timedelta(days=days)).strftime('%Y-%m-%d')
            
            result = self.db.execute_sql(f"""
                DELETE FROM daily_data 
                WHERE trade_date < '{cutoff_date}'
            """)
            
            if result:
                logger.info(f"🧹 清理 {days} 天前的旧数据完成")
            
        except Exception as e:
            logger.error(f"❌ 清理旧数据失败: {e}")
    
    def _补充昨日数据(self):
        """补充昨日缺失数据"""
        logger.info("🔄 补充昨日缺失数据")
        
        try:
            yesterday = (datetime.now() - timedelta(days=1)).strftime('%Y%m%d')
            today = datetime.now().strftime('%Y%m%d')
            
            # 获取有缺失的股票
            missing_stocks = self.db.fetch_data(f"""
                SELECT sb.ts_code
                FROM stock_basic sb
                LEFT JOIN daily_data dd ON sb.ts_code = dd.ts_code 
                    AND dd.trade_date = '{yesterday}'
                WHERE dd.ts_code IS NULL
                LIMIT 500
            """)
            
            if not missing_stocks.empty:
                stock_list = missing_stocks['ts_code'].tolist()
                
                # 使用full_syncer进行补充同步
                success_count = 0
                syncer = FullStockDataSync()
                for ts_code in stock_list[:10]:  # 限制处理数量避免超时
                    success = syncer._sync_single_stock(ts_code, yesterday, today)
                    if success:
                        success_count += 1
                
                logger.info(f"✅ 补充昨日数据完成: {success_count}/{len(stock_list)}")
            
        except Exception as e:
            logger.error(f"❌ 补充昨日数据失败: {e}")
    
    def _is_trading_day(self):
        """判断是否为交易日"""
        today = datetime.now()
        # 简化判断：周一到周五
        return today.weekday() < 5
    
    def _get_weekday_name(self, day):
        """获取星期名称"""
        names = ["周一", "周二", "周三", "周四", "周五", "周六", "周日"]
        return names[day] if 0 <= day < 7 else f"第{day}天"
    
    def _load_schedule_config(self):
        """加载调度配置"""
        try:
            if os.path.exists(self.schedule_config_file):
                with open(self.schedule_config_file, 'r', encoding='utf-8') as f:
                    loaded_config = json.load(f)
                    self.schedule_config.update(loaded_config)
                logger.info("📋 调度配置已加载")
            else:
                self._save_schedule_config()
        except Exception as e:
            logger.error(f"❌ 加载调度配置失败: {e}")
    
    def _save_schedule_config(self):
        """保存调度配置"""
        try:
            with open(self.schedule_config_file, 'w', encoding='utf-8') as f:
                json.dump(self.schedule_config, f, ensure_ascii=False, indent=2)
            logger.info("💾 调度配置已保存")
        except Exception as e:
            logger.error(f"❌ 保存调度配置失败: {e}")
    
    def get_schedule_status(self):
        """获取调度状态"""
        return {
            'is_running': self.is_running,
            'scheduled_jobs': len(schedule.jobs),
            'next_run': schedule.next_run(),
            'config': self.schedule_config
        }

def main():
    """主函数"""
    import argparse
    
    parser = argparse.ArgumentParser(description='自动股票数据调度系统')
    parser.add_argument('--config', help='配置文件路径')
    parser.add_argument('--test', action='store_true', help='测试模式')
    
    args = parser.parse_args()
    
    scheduler = AutoStockScheduler()
    
    if args.test:
        # 测试模式：立即执行各种检查
        logger.info("🧪 测试模式")
        scheduler._initial_data_check()
        scheduler._data_health_check()
        scheduler._market_open_check()
        return
    
    try:
        scheduler.start_scheduler()
    except KeyboardInterrupt:
        logger.info("收到中断信号，正在停止...")
        scheduler.stop_scheduler()

if __name__ == "__main__":
    main()