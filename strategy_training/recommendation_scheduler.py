#!/usr/bin/env python3
"""
异步推荐调度系统
Asynchronous Recommendation Scheduler for Real-time Stock Recommendations
"""

import os
import sys
import json
import logging
import asyncio
import threading
from datetime import datetime, timedelta, time
from typing import Dict, List, Optional, Any
from concurrent.futures import ThreadPoolExecutor
import schedule
import time as time_module

# 添加项目路径
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from recommendation_backtest.model_recommender import ModelRecommender
from recommendation_backtest.recommendation_tracker import RecommendationTracker
from strategy.market_mainstream_strategies import get_all_strategy_ids, MAINSTREAM_STRATEGIES

class RecommendationScheduler:
    """异步推荐调度器"""
    
    def __init__(self, tushare_token: str = None):
        self.logger = logging.getLogger(__name__)
        self.tushare_token = tushare_token
        
        # 核心组件
        self.model_recommender = None
        self.recommendation_tracker = None
        self.websocket_broadcaster = None
        
        # 调度器状态
        self.is_running = False
        self.scheduler_thread = None
        self.executor = ThreadPoolExecutor(max_workers=4)
        
        # 配置
        self.config = {
            "general_recommendations": {
                "enabled": True,
                "schedule": "08:00",  # 每日早上8点
                "num_stocks": 20,
                "strategies": ["technical_analysis", "momentum_trading", "value_investment"]
            },
            "personalized_recommendations": {
                "enabled": True,
                "interval_hours": 2,  # 每2小时
                "num_stocks_per_user": 10
            },
            "realtime_recommendations": {
                "enabled": True,
                "market_hours": {"start": "09:30", "end": "15:00"},
                "interval_minutes": 15,  # 开盘期间每15分钟
                "weekdays_only": True
            }
        }
        
        # 缓存
        self.recommendation_cache = {}
        self.user_strategies_cache = {}
        
        # WebSocket连接管理
        self.websocket_connections = set()
        
    def initialize(self):
        """初始化调度器组件"""
        try:
            self.logger.info("初始化推荐调度器组件...")
            
            # 初始化推荐生成器
            self.model_recommender = ModelRecommender(self.tushare_token)
            self.logger.info("✅ 模型推荐器已初始化")
            
            # 初始化推荐跟踪器
            self.recommendation_tracker = RecommendationTracker()
            self.logger.info("✅ 推荐跟踪器已初始化")
            
            # 初始化WebSocket广播器
            self._setup_websocket_broadcaster()
            
            self.logger.info("推荐调度器初始化完成")
            
        except Exception as e:
            self.logger.error(f"推荐调度器初始化失败: {e}")
            raise
    
    def _setup_websocket_broadcaster(self):
        """设置WebSocket广播器"""
        # 这里可以集成现有的WebSocket系统
        # 暂时使用简单的连接管理
        self.websocket_broadcaster = WebSocketBroadcaster()
        self.logger.info("✅ WebSocket广播器已初始化")
    
    def start_scheduler(self):
        """启动调度器"""
        if self.is_running:
            self.logger.warning("调度器已在运行")
            return
        
        try:
            self.logger.info("启动推荐调度器...")
            
            # 初始化组件
            if not self.model_recommender:
                self.initialize()
            
            # 设置定时任务
            self._setup_scheduled_tasks()
            
            # 启动调度器线程
            self.is_running = True
            self.scheduler_thread = threading.Thread(target=self._run_scheduler, daemon=True)
            self.scheduler_thread.start()
            
            self.logger.info("✅ 推荐调度器已启动")
            
        except Exception as e:
            self.logger.error(f"启动推荐调度器失败: {e}")
            raise
    
    def stop_scheduler(self):
        """停止调度器"""
        self.logger.info("停止推荐调度器...")
        
        self.is_running = False
        
        if self.scheduler_thread:
            self.scheduler_thread.join(timeout=5)
        
        self.executor.shutdown(wait=True)
        
        self.logger.info("✅ 推荐调度器已停止")
    
    def _setup_scheduled_tasks(self):
        """设置定时任务"""
        self.logger.info("设置定时任务...")
        
        # 通用推荐 - 每日早上8点
        if self.config["general_recommendations"]["enabled"]:
            schedule_time = self.config["general_recommendations"]["schedule"]
            schedule.every().day.at(schedule_time).do(self._schedule_general_recommendations)
            self.logger.info(f"✅ 通用推荐任务已设置: 每日 {schedule_time}")
        
        # 个性化推荐 - 每2小时
        if self.config["personalized_recommendations"]["enabled"]:
            interval = self.config["personalized_recommendations"]["interval_hours"]
            schedule.every(interval).hours.do(self._schedule_personalized_recommendations)
            self.logger.info(f"✅ 个性化推荐任务已设置: 每 {interval} 小时")
        
        # 实时推荐 - 交易时间每15分钟
        if self.config["realtime_recommendations"]["enabled"]:
            interval = self.config["realtime_recommendations"]["interval_minutes"]
            schedule.every(interval).minutes.do(self._schedule_realtime_recommendations)
            self.logger.info(f"✅ 实时推荐任务已设置: 每 {interval} 分钟")
    
    def _run_scheduler(self):
        """运行调度器主循环"""
        self.logger.info("调度器主循环开始运行...")
        
        while self.is_running:
            try:
                schedule.run_pending()
                time_module.sleep(30)  # 每30秒检查一次
                
            except Exception as e:
                self.logger.error(f"调度器主循环异常: {e}")
                time_module.sleep(60)
    
    def _schedule_general_recommendations(self):
        """调度通用推荐生成"""
        if not self.is_running:
            return
        
        self.logger.info("开始生成通用推荐...")
        future = self.executor.submit(self._generate_general_recommendations)
        future.add_done_callback(self._on_general_recommendations_complete)
    
    def _schedule_personalized_recommendations(self):
        """调度个性化推荐生成"""
        if not self.is_running:
            return
        
        self.logger.info("开始生成个性化推荐...")
        future = self.executor.submit(self._generate_personalized_recommendations)
        future.add_done_callback(self._on_personalized_recommendations_complete)
    
    def _schedule_realtime_recommendations(self):
        """调度实时推荐生成"""
        if not self.is_running:
            return
        
        # 检查是否在交易时间
        if not self._is_market_hours():
            return
        
        self.logger.info("开始生成实时推荐...")
        future = self.executor.submit(self._generate_realtime_recommendations)
        future.add_done_callback(self._on_realtime_recommendations_complete)
    
    def _generate_general_recommendations(self) -> Dict[str, Any]:
        """生成通用推荐"""
        try:
            config = self.config["general_recommendations"]
            recommendations = []
            
            # 使用主流策略生成推荐
            strategy_ids = config["strategies"]
            num_stocks = config["num_stocks"]
            
            for strategy_id in strategy_ids:
                if strategy_id in MAINSTREAM_STRATEGIES:
                    strategy_recs = self.model_recommender.generate_recommendations(
                        strategy_id=strategy_id,
                        num_recommendations=num_stocks // len(strategy_ids),
                        recommendation_type="general"
                    )
                    recommendations.extend(strategy_recs)
            
            # 去重和排序
            unique_recommendations = self._deduplicate_recommendations(recommendations)
            top_recommendations = unique_recommendations[:num_stocks]
            
            # 缓存推荐
            self.recommendation_cache["general"] = {
                "recommendations": top_recommendations,
                "generated_at": datetime.now().isoformat(),
                "type": "general"
            }
            
            # 保存到数据库
            for rec in top_recommendations:
                self.recommendation_tracker.add_recommendation(rec)
            
            self.logger.info(f"生成了 {len(top_recommendations)} 个通用推荐")
            
            return {
                "success": True,
                "count": len(top_recommendations),
                "recommendations": top_recommendations
            }
            
        except Exception as e:
            self.logger.error(f"生成通用推荐失败: {e}")
            return {"success": False, "error": str(e)}
    
    def _generate_personalized_recommendations(self) -> Dict[str, Any]:
        """生成个性化推荐"""
        try:
            # 获取活跃用户列表
            active_users = self._get_active_users()
            results = {}
            
            for user_id in active_users:
                try:
                    # 获取用户策略偏好
                    user_strategies = self._get_user_strategies(user_id)
                    
                    if not user_strategies:
                        continue
                    
                    user_recommendations = []
                    
                    # 为每个用户策略生成推荐
                    for strategy in user_strategies:
                        strategy_recs = self.model_recommender.generate_recommendations(
                            strategy_id=strategy["id"],
                            num_recommendations=5,
                            recommendation_type="personalized",
                            user_id=user_id
                        )
                        user_recommendations.extend(strategy_recs)
                    
                    # 个性化排序和过滤
                    personalized_recs = self._personalize_recommendations(
                        user_recommendations, user_id
                    )
                    
                    # 缓存用户推荐
                    cache_key = f"user_{user_id}"
                    self.recommendation_cache[cache_key] = {
                        "recommendations": personalized_recs,
                        "generated_at": datetime.now().isoformat(),
                        "type": "personalized",
                        "user_id": user_id
                    }
                    
                    results[user_id] = {
                        "success": True,
                        "count": len(personalized_recs)
                    }
                    
                    self.logger.info(f"为用户 {user_id} 生成了 {len(personalized_recs)} 个个性化推荐")
                    
                except Exception as e:
                    self.logger.error(f"为用户 {user_id} 生成个性化推荐失败: {e}")
                    results[user_id] = {"success": False, "error": str(e)}
            
            return {
                "success": True,
                "user_results": results,
                "total_users": len(active_users)
            }
            
        except Exception as e:
            self.logger.error(f"生成个性化推荐失败: {e}")
            return {"success": False, "error": str(e)}
    
    def _generate_realtime_recommendations(self) -> Dict[str, Any]:
        """生成实时推荐"""
        try:
            # 基于市场实时数据生成推荐
            realtime_strategies = ["momentum_trading", "breakout_strategy", "mean_reversion"]
            recommendations = []
            
            for strategy_id in realtime_strategies:
                if strategy_id in MAINSTREAM_STRATEGIES:
                    strategy_recs = self.model_recommender.generate_recommendations(
                        strategy_id=strategy_id,
                        num_recommendations=5,
                        recommendation_type="realtime"
                    )
                    recommendations.extend(strategy_recs)
            
            # 实时推荐优先级排序
            prioritized_recs = self._prioritize_realtime_recommendations(recommendations)
            
            # 缓存实时推荐
            self.recommendation_cache["realtime"] = {
                "recommendations": prioritized_recs,
                "generated_at": datetime.now().isoformat(),
                "type": "realtime"
            }
            
            self.logger.info(f"生成了 {len(prioritized_recs)} 个实时推荐")
            
            return {
                "success": True,
                "count": len(prioritized_recs),
                "recommendations": prioritized_recs
            }
            
        except Exception as e:
            self.logger.error(f"生成实时推荐失败: {e}")
            return {"success": False, "error": str(e)}
    
    def _on_general_recommendations_complete(self, future):
        """通用推荐生成完成回调"""
        try:
            result = future.result()
            if result["success"]:
                # 广播更新
                self._broadcast_recommendations("general", result["recommendations"])
                self.logger.info("通用推荐生成并广播完成")
            else:
                self.logger.error(f"通用推荐生成失败: {result.get('error', '未知错误')}")
        except Exception as e:
            self.logger.error(f"通用推荐完成回调异常: {e}")
    
    def _on_personalized_recommendations_complete(self, future):
        """个性化推荐生成完成回调"""
        try:
            result = future.result()
            if result["success"]:
                # 为每个用户广播个性化推荐
                for user_id in result["user_results"]:
                    if result["user_results"][user_id]["success"]:
                        cache_key = f"user_{user_id}"
                        if cache_key in self.recommendation_cache:
                            user_recs = self.recommendation_cache[cache_key]["recommendations"]
                            self._broadcast_user_recommendations(user_id, user_recs)
                
                self.logger.info(f"个性化推荐生成并广播完成，涉及 {result['total_users']} 个用户")
            else:
                self.logger.error(f"个性化推荐生成失败: {result.get('error', '未知错误')}")
        except Exception as e:
            self.logger.error(f"个性化推荐完成回调异常: {e}")
    
    def _on_realtime_recommendations_complete(self, future):
        """实时推荐生成完成回调"""
        try:
            result = future.result()
            if result["success"]:
                # 广播实时更新
                self._broadcast_recommendations("realtime", result["recommendations"])
                self.logger.info("实时推荐生成并广播完成")
            else:
                self.logger.error(f"实时推荐生成失败: {result.get('error', '未知错误')}")
        except Exception as e:
            self.logger.error(f"实时推荐完成回调异常: {e}")
    
    def _is_market_hours(self) -> bool:
        """检查是否在交易时间"""
        config = self.config["realtime_recommendations"]
        
        if config["weekdays_only"] and datetime.now().weekday() >= 5:
            return False
        
        current_time = datetime.now().time()
        start_time = time.fromisoformat(config["market_hours"]["start"])
        end_time = time.fromisoformat(config["market_hours"]["end"])
        
        return start_time <= current_time <= end_time
    
    def _get_active_users(self) -> List[str]:
        """获取活跃用户列表"""
        # 这里应该从数据库获取活跃用户
        # 暂时返回模拟数据
        return ["user_001", "user_002", "user_003"]
    
    def _get_user_strategies(self, user_id: str) -> List[Dict[str, Any]]:
        """获取用户策略偏好"""
        # 检查缓存
        if user_id in self.user_strategies_cache:
            cache_entry = self.user_strategies_cache[user_id]
            if cache_entry["expires_at"] > datetime.now():
                return cache_entry["strategies"]
        
        # 这里应该从数据库获取用户策略偏好
        # 暂时返回默认策略
        default_strategies = [
            {"id": "technical_analysis", "weight": 0.4},
            {"id": "momentum_trading", "weight": 0.3},
            {"id": "value_investment", "weight": 0.3}
        ]
        
        # 缓存用户策略
        self.user_strategies_cache[user_id] = {
            "strategies": default_strategies,
            "expires_at": datetime.now() + timedelta(hours=1)
        }
        
        return default_strategies
    
    def _deduplicate_recommendations(self, recommendations: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """去重推荐"""
        seen_stocks = set()
        unique_recs = []
        
        for rec in recommendations:
            stock_code = rec.get("stock_code")
            if stock_code and stock_code not in seen_stocks:
                seen_stocks.add(stock_code)
                unique_recs.append(rec)
        
        return unique_recs
    
    def _personalize_recommendations(self, recommendations: List[Dict[str, Any]], 
                                   user_id: str) -> List[Dict[str, Any]]:
        """个性化推荐排序"""
        # 这里可以基于用户画像、历史行为等进行个性化排序
        # 暂时按置信度排序
        return sorted(recommendations, key=lambda x: x.get("confidence", 0), reverse=True)[:10]
    
    def _prioritize_realtime_recommendations(self, recommendations: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """实时推荐优先级排序"""
        # 基于紧急度、市场波动等因素排序
        return sorted(recommendations, 
                     key=lambda x: (x.get("urgency", 0), x.get("confidence", 0)), 
                     reverse=True)[:15]
    
    def _broadcast_recommendations(self, category: str, recommendations: List[Dict[str, Any]]):
        """广播推荐更新"""
        if self.websocket_broadcaster:
            message = {
                "type": "recommendation_update",
                "category": category,
                "data": recommendations,
                "timestamp": datetime.now().isoformat(),
                "count": len(recommendations)
            }
            self.websocket_broadcaster.broadcast(message)
    
    def _broadcast_user_recommendations(self, user_id: str, recommendations: List[Dict[str, Any]]):
        """广播用户个性化推荐"""
        if self.websocket_broadcaster:
            message = {
                "type": "personalized_recommendation_update",
                "user_id": user_id,
                "data": recommendations,
                "timestamp": datetime.now().isoformat(),
                "count": len(recommendations)
            }
            self.websocket_broadcaster.send_to_user(user_id, message)
    
    def get_cached_recommendations(self, category: str = "general", 
                                 user_id: str = None) -> Optional[Dict[str, Any]]:
        """获取缓存的推荐"""
        if user_id:
            cache_key = f"user_{user_id}"
            return self.recommendation_cache.get(cache_key)
        else:
            return self.recommendation_cache.get(category)
    
    def force_generate_recommendations(self, category: str = "general") -> Dict[str, Any]:
        """强制生成推荐"""
        if category == "general":
            return self._generate_general_recommendations()
        elif category == "personalized":
            return self._generate_personalized_recommendations()
        elif category == "realtime":
            return self._generate_realtime_recommendations()
        else:
            return {"success": False, "error": "未知推荐类型"}

class WebSocketBroadcaster:
    """WebSocket广播器"""
    
    def __init__(self):
        self.connections = set()
        self.user_connections = {}
    
    def add_connection(self, websocket, user_id: str = None):
        """添加连接"""
        self.connections.add(websocket)
        if user_id:
            if user_id not in self.user_connections:
                self.user_connections[user_id] = set()
            self.user_connections[user_id].add(websocket)
    
    def remove_connection(self, websocket, user_id: str = None):
        """移除连接"""
        self.connections.discard(websocket)
        if user_id and user_id in self.user_connections:
            self.user_connections[user_id].discard(websocket)
    
    def broadcast(self, message: Dict[str, Any]):
        """广播消息"""
        message_str = json.dumps(message, ensure_ascii=False)
        for connection in self.connections:
            try:
                # 这里应该调用实际的WebSocket发送方法
                # connection.send(message_str)
                pass
            except Exception as e:
                # 连接已断开，移除
                self.connections.discard(connection)
    
    def send_to_user(self, user_id: str, message: Dict[str, Any]):
        """发送消息给特定用户"""
        if user_id in self.user_connections:
            message_str = json.dumps(message, ensure_ascii=False)
            for connection in self.user_connections[user_id]:
                try:
                    # connection.send(message_str)
                    pass
                except Exception as e:
                    self.user_connections[user_id].discard(connection)

def main():
    """主函数"""
    import argparse
    
    # 设置日志
    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
        handlers=[
            logging.FileHandler('logs/recommendation_scheduler.log'),
            logging.StreamHandler()
        ]
    )
    
    parser = argparse.ArgumentParser(description="推荐调度器")
    parser.add_argument("--action", type=str, choices=["start", "test"], 
                       default="start", help="执行动作")
    parser.add_argument("--category", type=str, 
                       choices=["general", "personalized", "realtime"],
                       help="测试推荐类型")
    
    args = parser.parse_args()
    
    # 从环境变量获取TuShare token
    tushare_token = os.getenv('TUSHARE_TOKEN')
    if not tushare_token:
        print("警告: 未设置TUSHARE_TOKEN环境变量")
    
    scheduler = RecommendationScheduler(tushare_token)
    
    if args.action == "start":
        try:
            scheduler.start_scheduler()
            print("推荐调度器已启动，按 Ctrl+C 停止")
            
            # 保持运行
            while True:
                time_module.sleep(1)
                
        except KeyboardInterrupt:
            print("\n正在停止推荐调度器...")
            scheduler.stop_scheduler()
            print("推荐调度器已停止")
        
    elif args.action == "test":
        scheduler.initialize()
        
        if args.category:
            result = scheduler.force_generate_recommendations(args.category)
            print(f"{args.category} 推荐生成结果: {result}")
        else:
            # 测试所有类型
            for category in ["general", "personalized", "realtime"]:
                result = scheduler.force_generate_recommendations(category)
                print(f"{category} 推荐生成结果: {result}")

if __name__ == "__main__":
    main()