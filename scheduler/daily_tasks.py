import logging
from datetime import datetime, timedelta
from apscheduler.schedulers.blocking import BlockingScheduler
from apscheduler.triggers.cron import CronTrigger
import sys, os
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import config
from scripts.fetch_stock_daily import StockDailyFetcher
from analysis.recommender import recommender
from utils.db_helper import db
from sqlalchemy import text

logging.basicConfig(level=getattr(logging, config.LOG_LEVEL), format=config.LOG_FORMAT)
logger = logging.getLogger(__name__)

class DailyTaskScheduler:
    def __init__(self):
        self.scheduler = BlockingScheduler()
        self.fetcher = StockDailyFetcher()
        
    def daily_data_update(self):  # 每日数据更新任务
        logger.info("开始执行每日数据更新任务")
        start_time = datetime.now()
        
        try:
            # 更新昨日股票数据
            success_count = self.fetcher.daily_update()
            
            duration = int((datetime.now() - start_time).total_seconds())
            if success_count > 0:
                db.log_operation('scheduler', 'daily_update', 'SUCCESS', f'更新{success_count}只股票数据', duration)
                logger.info(f"每日数据更新完成，成功{success_count}只股票，耗时{duration}秒")
            else:
                db.log_operation('scheduler', 'daily_update', 'FAILED', '未更新任何数据', duration)
                logger.error("每日数据更新失败，未获取到任何数据")
                
        except Exception as e:
            duration = int((datetime.now() - start_time).total_seconds())
            error_msg = f"每日数据更新异常: {e}"
            logger.error(error_msg)
            db.log_operation('scheduler', 'daily_update', 'ERROR', error_msg, duration)
            
    def daily_recommend_update(self):  # 每日推荐更新任务
        logger.info("开始执行每日推荐更新任务")
        start_time = datetime.now()
        
        try:
            # 清理旧推荐(7天前)
            with db.engine.connect() as conn:
                old_date = (datetime.now() - timedelta(days=7)).date()
                conn.execute(text("UPDATE recommend_result SET is_valid=0 WHERE recommend_date < :old_date"), {'old_date': old_date})
                conn.commit()
                
            # 生成新推荐
            strategies = ['ma_crossover', 'momentum']
            total_recommendations = 0
            
            for strategy in strategies:
                recommendations = recommender.generate_recommendations(strategy, config.MAX_RECOMMEND)
                total_recommendations += len(recommendations)
                logger.info(f"完成{strategy}策略推荐，生成{len(recommendations)}只")
                
            duration = int((datetime.now() - start_time).total_seconds())
            db.log_operation('scheduler', 'daily_recommend', 'SUCCESS', f'生成{total_recommendations}只推荐股票', duration)
            logger.info(f"每日推荐更新完成，共生成{total_recommendations}只推荐，耗时{duration}秒")
            
        except Exception as e:
            duration = int((datetime.now() - start_time).total_seconds())
            error_msg = f"每日推荐更新异常: {e}"
            logger.error(error_msg)
            db.log_operation('scheduler', 'daily_recommend', 'ERROR', error_msg, duration)
            
    def weekly_cleanup(self):  # 每周清理任务
        logger.info("开始执行每周清理任务")
        
        try:
            with db.engine.connect() as conn:
                # 清理30天前的日志
                old_log_date = (datetime.now() - timedelta(days=30)).date()
                result = conn.execute(text("DELETE FROM system_log WHERE DATE(created_at) < :old_date"), {'old_date': old_log_date})
                conn.commit()
                logger.info(f"清理旧日志{result.rowcount}条")
                
                # 清理无效推荐
                result = conn.execute(text("DELETE FROM recommend_result WHERE is_valid=0 AND recommend_date < :old_date"), {'old_date': old_log_date})
                conn.commit()
                logger.info(f"清理无效推荐{result.rowcount}条")
                
            db.log_operation('scheduler', 'weekly_cleanup', 'SUCCESS', f'清理完成')
            
        except Exception as e:
            error_msg = f"每周清理任务异常: {e}"
            logger.error(error_msg)
            db.log_operation('scheduler', 'weekly_cleanup', 'ERROR', error_msg)
            
    def start_scheduler(self):  # 启动调度器
        logger.info("启动定时任务调度器")
        
        # 每个交易日17:00执行数据更新(周一到周五)
        self.scheduler.add_job(
            self.daily_data_update,
            CronTrigger(hour=17, minute=0, day_of_week='mon-fri'),
            id='daily_data_update',
            name='每日数据更新',
            max_instances=1
        )
        
        # 每个交易日18:00执行推荐更新
        self.scheduler.add_job(
            self.daily_recommend_update,
            CronTrigger(hour=18, minute=0, day_of_week='mon-fri'),
            id='daily_recommend_update',
            name='每日推荐更新',
            max_instances=1
        )
        
        # 每周日23:00执行清理任务
        self.scheduler.add_job(
            self.weekly_cleanup,
            CronTrigger(hour=23, minute=0, day_of_week='sun'),
            id='weekly_cleanup',
            name='每周清理任务',
            max_instances=1
        )
        
        # 立即执行一次推荐更新(用于测试)
        # self.scheduler.add_job(self.daily_recommend_update, 'date', run_date=datetime.now() + timedelta(seconds=10))
        
        try:
            logger.info("定时任务已启动，等待执行...")
            self.scheduler.start()
        except KeyboardInterrupt:
            logger.info("收到停止信号，关闭定时任务")
            self.scheduler.shutdown()
        except Exception as e:
            logger.error(f"定时任务异常: {e}")
            self.scheduler.shutdown()

# 手动执行任务的工具函数
def run_manual_task(task_name: str):
    scheduler = DailyTaskScheduler()
    if task_name == 'update':
        scheduler.daily_data_update()
    elif task_name == 'recommend':
        scheduler.daily_recommend_update()
    elif task_name == 'cleanup':
        scheduler.weekly_cleanup()
    else:
        print("可用任务: update, recommend, cleanup")

if __name__ == "__main__":
    if len(sys.argv) > 1:
        task_name = sys.argv[1]
        run_manual_task(task_name)
    else:
        scheduler = DailyTaskScheduler()
        scheduler.start_scheduler() 