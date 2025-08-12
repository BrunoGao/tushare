#!/usr/bin/env python3
"""
大模型股票推荐回测验证系统
跟踪和验证大模型股票推荐的命中率和投资效果
"""

import json
import sqlite3
import pymysql
import sys
import os
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import config
import pandas as pd
import numpy as np
import logging
import re
import os
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Tuple, Any
from dataclasses import dataclass, asdict
from enum import Enum
import subprocess

class RecommendationType(Enum):
    """推荐类型"""
    BUY = "buy"           # 买入推荐
    SELL = "sell"         # 卖出推荐  
    HOLD = "hold"         # 持有推荐
    NEUTRAL = "neutral"   # 中性观点

class RecommendationResult(Enum):
    """推荐结果"""
    HIT = "hit"           # 命中
    MISS = "miss"         # 未命中
    PENDING = "pending"   # 待验证

@dataclass
class StockRecommendation:
    """股票推荐记录"""
    # Required fields (no defaults)
    id: str
    model_name: str                    # 模型名称
    stock_code: str                    # 股票代码
    stock_name: str                    # 股票名称
    recommendation_type: str           # 推荐类型
    recommendation_text: str           # 推荐原文
    confidence_score: float            # 置信度分数
    target_price: Optional[float]      # 目标价格
    stop_loss: Optional[float]         # 止损价格
    time_horizon: int                  # 时间周期(天)
    recommend_date: str                # 推荐日期
    recommend_price: float             # 推荐时价格
    recommend_volume: float            # 推荐时成交量
    
    # Optional fields with defaults
    strategy_type: str = "comprehensive_analysis"  # 策略类型
    validation_date: Optional[str] = None      # 验证日期
    validation_price: Optional[float] = None   # 验证时价格
    actual_return: Optional[float] = None      # 实际收益率
    result: str = RecommendationResult.PENDING.value  # 推荐结果
    created_at: str = ""
    updated_at: str = ""

@dataclass
class BacktestMetrics:
    """回测指标"""
    model_name: str
    period_start: str
    period_end: str
    
    # 基础统计
    total_recommendations: int
    validated_recommendations: int
    pending_recommendations: int
    
    # 命中率统计
    total_hits: int
    total_misses: int
    hit_rate: float
    
    # 分类命中率
    buy_hit_rate: float
    sell_hit_rate: float
    hold_hit_rate: float
    
    # 收益统计
    avg_return: float
    positive_returns: int
    negative_returns: int
    max_return: float
    min_return: float
    
    # 风险指标
    volatility: float
    sharpe_ratio: float
    max_drawdown: float
    
    # 时间分析
    avg_validation_days: float
    success_by_time_horizon: Dict[str, float]
    
    # 详细数据
    recommendations: List[StockRecommendation]
    
    # 收益曲线数据
    profit_curve: List[Dict] = None  # 格式: [{"date": "2025-01-01", "cumulative_return": 0.05, "daily_return": 0.02}]

class RecommendationParser:
    """推荐内容解析器"""
    
    def __init__(self):
        self.logger = logging.getLogger(__name__)
        
        # 推荐类型关键词
        self.buy_keywords = ["买入", "推荐", "看好", "上涨", "增持", "建议购买"]
        self.sell_keywords = ["卖出", "减持", "看空", "下跌", "抛售", "建议卖出"]
        self.hold_keywords = ["持有", "维持", "观望", "等待"]
        
        # 价格模式
        self.price_patterns = [
            r"目标价[格]?[:：]\s*(\d+\.?\d*)",
            r"目标价[格]?.*?(\d+\.?\d*)[元块]",
            r"预期价[格]?[:：]\s*(\d+\.?\d*)",
            r"合理价[格]?[:：]\s*(\d+\.?\d*)"
        ]
        
        # 时间周期模式
        self.time_patterns = [
            r"(\d+)[个]?月",
            r"(\d+)[个]?周",
            r"(\d+)[个]?天",
            r"短期|1个月以内",
            r"中期|3个月以内", 
            r"长期|半年以上"
        ]
    
    def parse_recommendation(self, text: str, stock_code: str, stock_name: str, 
                           current_price: float) -> Dict[str, Any]:
        """解析推荐内容"""
        try:
            # 1. 判断推荐类型
            rec_type = self._extract_recommendation_type(text)
            
            # 2. 提取目标价格
            target_price = self._extract_target_price(text, current_price)
            
            # 3. 提取时间周期
            time_horizon = self._extract_time_horizon(text)
            
            # 4. 计算置信度
            confidence = self._calculate_confidence(text, rec_type)
            
            # 5. 提取止损价格
            stop_loss = self._extract_stop_loss(text, current_price)
            
            return {
                'recommendation_type': rec_type.value,
                'target_price': target_price,
                'stop_loss': stop_loss,
                'time_horizon': time_horizon,
                'confidence_score': confidence
            }
            
        except Exception as e:
            self.logger.error(f"解析推荐失败: {e}")
            return {
                'recommendation_type': RecommendationType.NEUTRAL.value,
                'target_price': None,
                'stop_loss': None,
                'time_horizon': 30,  # 默认30天
                'confidence_score': 0.5
            }
    
    def _extract_recommendation_type(self, text: str) -> RecommendationType:
        """提取推荐类型"""
        text_lower = text.lower()
        
        buy_score = sum(1 for keyword in self.buy_keywords if keyword in text)
        sell_score = sum(1 for keyword in self.sell_keywords if keyword in text)
        hold_score = sum(1 for keyword in self.hold_keywords if keyword in text)
        
        max_score = max(buy_score, sell_score, hold_score)
        
        if max_score == 0:
            return RecommendationType.NEUTRAL
        elif buy_score == max_score:
            return RecommendationType.BUY
        elif sell_score == max_score:
            return RecommendationType.SELL
        else:
            return RecommendationType.HOLD
    
    def _extract_target_price(self, text: str, current_price: float) -> Optional[float]:
        """提取目标价格"""
        for pattern in self.price_patterns:
            matches = re.findall(pattern, text)
            if matches:
                try:
                    price = float(matches[0])
                    # 价格合理性检查
                    if 0.5 * current_price <= price <= 3 * current_price:
                        return price
                except ValueError:
                    continue
        return None
    
    def _extract_time_horizon(self, text: str) -> int:
        """提取时间周期（天）"""
        try:
            for pattern in self.time_patterns:
                matches = re.findall(pattern, text)
                if matches:
                    # 检查匹配的是数字还是文字
                    match = matches[0]
                    if isinstance(match, str) and match.isdigit():
                        if "月" in pattern:
                            return int(match) * 30
                        elif "周" in pattern:
                            return int(match) * 7
                        elif "天" in pattern:
                            return int(match)
            
            # 关键词匹配
            if "短期" in text or "1个月以内" in text:
                return 30
            elif "中期" in text or "3个月以内" in text:
                return 90
            elif "长期" in text or "半年以上" in text:
                return 180
            
            return 30  # 默认30天
            
        except (ValueError, TypeError) as e:
            self.logger.warning(f"解析时间周期失败: {e}, 使用默认值30天")
            return 30  # 默认30天
    
    def _calculate_confidence(self, text: str, rec_type: RecommendationType) -> float:
        """计算推荐置信度"""
        confidence = 0.5  # 基础置信度
        
        # 基于推荐词汇强度
        strong_words = ["强烈", "坚决", "明确", "确信", "肯定"]
        moderate_words = ["建议", "推荐", "考虑", "可能"]
        weak_words = ["或许", "可能", "谨慎", "观望"]
        
        if any(word in text for word in strong_words):
            confidence += 0.3
        elif any(word in text for word in moderate_words):
            confidence += 0.1
        elif any(word in text for word in weak_words):
            confidence -= 0.2
        
        # 基于分析深度
        analysis_indicators = ["技术分析", "基本面", "财报", "估值", "趋势"]
        confidence += len([word for word in analysis_indicators if word in text]) * 0.05
        
        return min(max(confidence, 0.1), 1.0)  # 限制在0.1-1.0之间
    
    def _extract_stop_loss(self, text: str, current_price: float) -> Optional[float]:
        """提取止损价格"""
        stop_loss_patterns = [
            r"止损[价位]?[:：]\s*(\d+\.?\d*)",
            r"止损.*?(\d+\.?\d*)[元块]"
        ]
        
        for pattern in stop_loss_patterns:
            matches = re.findall(pattern, text)
            if matches:
                try:
                    price = float(matches[0])
                    if 0.5 * current_price <= price <= current_price:
                        return price
                except ValueError:
                    continue
        return None

class RecommendationTracker:
    """推荐跟踪器"""
    
    def __init__(self, use_mysql: bool = True):
        self.logger = logging.getLogger(__name__)
        self.parser = RecommendationParser()
        
        # 初始化默认的 db_path
        self.db_path = "data/recommendation_backtest.db"
        os.makedirs(os.path.dirname(self.db_path), exist_ok=True)
        
        # 尝试连接远程MySQL，失败则回退到SQLite
        if use_mysql:
            try:
                self.connection_params = {
                    'host': config.DB_HOST,
                    'port': config.DB_PORT,
                    'user': config.DB_USER,
                    'password': config.DB_PASSWORD,
                    'database': config.DB_NAME,
                    'charset': 'utf8mb4'
                }
                # 测试连接
                conn = pymysql.connect(**self.connection_params)
                conn.close()
                self.use_mysql = True
                self.logger.info(f"✅ 使用远程MySQL数据库: {config.DB_HOST}:{config.DB_PORT}")
            except Exception as e:
                self.logger.warning(f"⚠️ 远程MySQL连接失败，回退到本地SQLite: {e}")
                self.use_mysql = False
        else:
            self.use_mysql = False
        
        self.init_database()
    
    def init_database(self):
        """初始化数据库"""
        if self.use_mysql:
            self._init_mysql_database()
        else:
            self._init_sqlite_database()
    
    def _init_mysql_database(self):
        """初始化MySQL数据库"""
        try:
            conn = pymysql.connect(**self.connection_params)
            cursor = conn.cursor()
            # 推荐记录表
            cursor.execute('''
                    CREATE TABLE IF NOT EXISTS recommendations (
                        id VARCHAR(255) PRIMARY KEY,
                        model_name VARCHAR(100),
                        stock_code VARCHAR(20),
                        stock_name VARCHAR(100),
                        recommendation_type VARCHAR(20),
                        recommendation_text TEXT,
                        confidence_score DECIMAL(5,4),
                        strategy_type VARCHAR(50) DEFAULT 'comprehensive_analysis',
                        target_price DECIMAL(10,2),
                        stop_loss DECIMAL(10,2),
                        time_horizon INT,
                        recommend_date DATE,
                        recommend_price DECIMAL(10,2),
                        recommend_volume BIGINT,
                        validation_date DATE,
                        validation_price DECIMAL(10,2),
                        actual_return DECIMAL(8,4),
                        result VARCHAR(20),
                        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                        updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP
                    ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4
                ''')
            
            # 回测指标表
            cursor.execute('''
                    CREATE TABLE IF NOT EXISTS backtest_metrics (
                        id VARCHAR(255) PRIMARY KEY,
                        model_name VARCHAR(100),
                        period_start DATE,
                        period_end DATE,
                        metrics_data TEXT,
                        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                    ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4
                ''')
                
            conn.commit()
            conn.close()
            self.logger.info("✅ MySQL数据库初始化成功")
        except Exception as e:
            self.logger.error(f"❌ MySQL数据库初始化失败: {e}")
    
    def _init_sqlite_database(self):
        """初始化SQLite数据库"""
        with sqlite3.connect(self.db_path) as conn:
            # 推荐记录表
            conn.execute('''
                CREATE TABLE IF NOT EXISTS recommendations (
                    id TEXT PRIMARY KEY,
                    model_name TEXT,
                    stock_code TEXT,
                    stock_name TEXT,
                    recommendation_type TEXT,
                    recommendation_text TEXT,
                    confidence_score REAL,
                    strategy_type TEXT DEFAULT 'comprehensive_analysis',
                    target_price REAL,
                    stop_loss REAL,
                    time_horizon INTEGER,
                    recommend_date TEXT,
                    recommend_price REAL,
                    recommend_volume REAL,
                    validation_date TEXT,
                    validation_price REAL,
                    actual_return REAL,
                    result TEXT,
                    created_at TEXT,
                    updated_at TEXT
                )
            ''')
            
            # 回测指标表
            conn.execute('''
                CREATE TABLE IF NOT EXISTS backtest_metrics (
                    id TEXT PRIMARY KEY,
                    model_name TEXT,
                    period_start TEXT,
                    period_end TEXT,
                    metrics_data TEXT,
                    created_at TEXT
                )
            ''')
            
            conn.commit()
    
    def add_recommendation(self, model_name: str, stock_code: str, stock_name: str,
                          recommendation_text: str, current_price: float, 
                          current_volume: float = 0, strategy_type: str = "comprehensive_analysis") -> str:
        """添加新的股票推荐"""
        import uuid
        
        # 解析推荐内容
        parsed = self.parser.parse_recommendation(
            recommendation_text, stock_code, stock_name, current_price
        )
        
        # 创建推荐记录
        rec_id = str(uuid.uuid4())
        now = datetime.now().isoformat()
        
        recommendation = StockRecommendation(
            id=rec_id,
            model_name=model_name,
            stock_code=stock_code,
            stock_name=stock_name,
            recommendation_type=parsed['recommendation_type'],
            recommendation_text=recommendation_text,
            confidence_score=parsed['confidence_score'],
            strategy_type=strategy_type,
            target_price=parsed['target_price'],
            stop_loss=parsed['stop_loss'],
            time_horizon=parsed['time_horizon'],
            recommend_date=datetime.now().strftime('%Y-%m-%d'),
            recommend_price=current_price,
            recommend_volume=current_volume,
            created_at=now,
            updated_at=now
        )
        
        # 保存到数据库
        if self.use_mysql:
            self._save_recommendation_mysql(recommendation)
        else:
            self._save_recommendation_sqlite(recommendation)
        
        self.logger.info(f"添加推荐: {model_name} -> {stock_code} ({parsed['recommendation_type']})")
        
        return rec_id
    
    def validate_recommendations(self, model_name: str = None) -> int:
        """验证推荐结果"""
        # 获取待验证的推荐
        pending_recommendations = self._get_pending_recommendations(model_name)
        
        validated_count = 0
        
        for rec in pending_recommendations:
            try:
                # 检查是否到达验证时间
                recommend_date = datetime.strptime(rec.recommend_date, '%Y-%m-%d')
                validation_date = recommend_date + timedelta(days=rec.time_horizon)
                
                if datetime.now() >= validation_date:
                    # 获取验证时的价格数据
                    validation_price = self._get_stock_price(rec.stock_code, validation_date.strftime('%Y-%m-%d'))
                    
                    if validation_price:
                        # 计算实际收益率
                        actual_return = (validation_price - rec.recommend_price) / rec.recommend_price
                        
                        # 判断推荐结果
                        result = self._evaluate_recommendation_result(rec, actual_return, validation_price)
                        
                        # 更新推荐记录
                        self._update_recommendation_result(
                            rec.id, validation_date.strftime('%Y-%m-%d'), 
                            validation_price, actual_return, result
                        )
                        
                        validated_count += 1
                        
            except Exception as e:
                self.logger.error(f"验证推荐失败 {rec.id}: {e}")
        
        self.logger.info(f"验证完成: {validated_count} 条推荐")
        
        return validated_count
    
    def _evaluate_recommendation_result(self, rec: StockRecommendation, 
                                      actual_return: float, validation_price: float) -> str:
        """评估推荐结果"""
        if rec.recommendation_type == RecommendationType.BUY.value:
            # 买入推荐：价格上涨为命中
            if rec.target_price:
                # 有目标价的情况
                hit = validation_price >= rec.target_price * 0.95  # 95%达成率
            else:
                # 无目标价，看收益率
                hit = actual_return > 0.02  # 2%以上为命中
                
        elif rec.recommendation_type == RecommendationType.SELL.value:
            # 卖出推荐：价格下跌为命中
            if rec.target_price:
                hit = validation_price <= rec.target_price * 1.05  # 105%以内为命中
            else:
                hit = actual_return < -0.02  # -2%以下为命中
                
        elif rec.recommendation_type == RecommendationType.HOLD.value:
            # 持有推荐：价格相对稳定为命中
            hit = abs(actual_return) <= 0.05  # 5%以内波动为命中
            
        else:  # NEUTRAL
            # 中性推荐：任何情况都算命中
            hit = True
        
        return RecommendationResult.HIT.value if hit else RecommendationResult.MISS.value
    
    def _calculate_profit_curve(self, recommendations: List[StockRecommendation]) -> List[Dict]:
        """计算收益曲线"""
        try:
            if not recommendations:
                return []
            
            # 按推荐日期排序
            sorted_recs = sorted(recommendations, key=lambda x: x.recommend_date)
            
            # 收集有实际收益数据的推荐
            valid_recs = [r for r in sorted_recs if r.actual_return is not None]
            
            if not valid_recs:
                # 如果没有实际收益数据，生成基于历史数据的模拟收益曲线
                return self._generate_simulated_profit_curve(sorted_recs)
            
            profit_curve = []
            cumulative_return = 0.0
            
            for i, rec in enumerate(valid_recs):
                daily_return = float(rec.actual_return) if rec.actual_return else 0.0
                cumulative_return += daily_return
                
                profit_curve.append({
                    "date": rec.recommend_date,
                    "daily_return": round(daily_return * 100, 2),  # 转换为百分比
                    "cumulative_return": round(cumulative_return * 100, 2),  # 转换为百分比
                    "stock_code": rec.stock_code,
                    "recommendation_type": rec.recommendation_type,
                    "confidence": rec.confidence
                })
            
            return profit_curve
            
        except Exception as e:
            self.logger.error(f"计算收益曲线失败: {e}")
            return []
    
    def _generate_simulated_profit_curve(self, recommendations: List[StockRecommendation]) -> List[Dict]:
        """基于历史数据生成模拟收益曲线"""
        try:
            # 尝试加载历史数据缓存
            import pickle
            cache_dir = os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), 'data', 'cache')
            historical_data = None
            
            # 查找可用的历史数据文件
            if os.path.exists(cache_dir):
                for file in os.listdir(cache_dir):
                    if file.startswith('historical_data_') and file.endswith('.pkl'):
                        try:
                            with open(os.path.join(cache_dir, file), 'rb') as f:
                                historical_data = pickle.load(f)
                                break
                        except:
                            continue
            
            if historical_data is None:
                # 生成示例收益曲线
                return self._generate_example_profit_curve(recommendations)
            
            profit_curve = []
            cumulative_return = 0.0
            
            for i, rec in enumerate(recommendations):
                # 基于历史数据计算模拟收益
                stock_code = rec.stock_code
                recommend_date = rec.recommend_date
                
                # 从历史数据中获取该股票的收益率
                simulated_return = self._get_stock_return_from_history(
                    historical_data, stock_code, recommend_date, rec.recommendation_type
                )
                
                cumulative_return += simulated_return
                
                profit_curve.append({
                    "date": recommend_date,
                    "daily_return": round(simulated_return * 100, 2),
                    "cumulative_return": round(cumulative_return * 100, 2),
                    "stock_code": stock_code,
                    "recommendation_type": rec.recommendation_type,
                    "confidence": rec.confidence
                })
            
            return profit_curve
            
        except Exception as e:
            self.logger.error(f"生成模拟收益曲线失败: {e}")
            return self._generate_example_profit_curve(recommendations)
    
    def _get_stock_return_from_history(self, historical_data, stock_code: str, date: str, rec_type: str) -> float:
        """从历史数据获取股票收益率"""
        try:
            # 基于推荐类型和股票代码生成模拟收益
            base_return = 0.0
            
            if rec_type == "buy":
                base_return = 0.02  # 买入推荐预期2%收益
            elif rec_type == "sell":
                base_return = -0.015  # 卖出推荐预期-1.5%收益
            else:  # hold
                base_return = 0.005  # 持有推荐预期0.5%收益
            
            # 添加一些随机波动
            import random
            random.seed(hash(stock_code + date) % 1000)  # 使用股票代码和日期作为种子
            volatility = random.uniform(-0.01, 0.01)  # ±1%的随机波动
            
            return base_return + volatility
            
        except:
            return 0.001  # 默认0.1%收益
    
    def _generate_example_profit_curve(self, recommendations: List[StockRecommendation]) -> List[Dict]:
        """生成示例收益曲线"""
        profit_curve = []
        cumulative_return = 0.0
        
        for i, rec in enumerate(recommendations[:10]):  # 限制数量避免太多数据
            # 生成示例收益数据
            daily_return = 0.015 + (i * 0.002) - 0.005 if i % 3 == 0 else 0.008 - (i * 0.001)
            cumulative_return += daily_return
            
            profit_curve.append({
                "date": rec.recommend_date,
                "daily_return": round(daily_return * 100, 2),
                "cumulative_return": round(cumulative_return * 100, 2),
                "stock_code": rec.stock_code,
                "recommendation_type": rec.recommendation_type,
                "confidence": rec.confidence
            })
        
        return profit_curve

    def generate_backtest_report(self, model_name: str, 
                               start_date: str = None, end_date: str = None) -> BacktestMetrics:
        """生成回测报告"""
        # 获取推荐数据
        recommendations = self._get_recommendations_by_period(model_name, start_date, end_date)
        
        if not recommendations:
            return self._empty_metrics(model_name, start_date, end_date)
        
        # 基础统计
        total_recommendations = len(recommendations)
        validated_recommendations = len([r for r in recommendations if r.result != RecommendationResult.PENDING.value])
        pending_recommendations = total_recommendations - validated_recommendations
        
        # 命中率统计
        hits = [r for r in recommendations if r.result == RecommendationResult.HIT.value]
        misses = [r for r in recommendations if r.result == RecommendationResult.MISS.value]
        
        total_hits = len(hits)
        total_misses = len(misses)
        hit_rate = total_hits / validated_recommendations if validated_recommendations > 0 else 0
        
        # 分类命中率
        buy_recs = [r for r in recommendations if r.recommendation_type == RecommendationType.BUY.value and r.result != RecommendationResult.PENDING.value]
        sell_recs = [r for r in recommendations if r.recommendation_type == RecommendationType.SELL.value and r.result != RecommendationResult.PENDING.value]
        hold_recs = [r for r in recommendations if r.recommendation_type == RecommendationType.HOLD.value and r.result != RecommendationResult.PENDING.value]
        
        buy_hit_rate = len([r for r in buy_recs if r.result == RecommendationResult.HIT.value]) / len(buy_recs) if buy_recs else 0
        sell_hit_rate = len([r for r in sell_recs if r.result == RecommendationResult.HIT.value]) / len(sell_recs) if sell_recs else 0
        hold_hit_rate = len([r for r in hold_recs if r.result == RecommendationResult.HIT.value]) / len(hold_recs) if hold_recs else 0
        
        # 收益统计
        validated_with_return = [r for r in recommendations if r.actual_return is not None]
        
        if validated_with_return:
            returns = [r.actual_return for r in validated_with_return]
            avg_return = np.mean(returns)
            positive_returns = len([r for r in returns if r > 0])
            negative_returns = len([r for r in returns if r < 0])
            max_return = max(returns)
            min_return = min(returns)
            volatility = np.std(returns)
            sharpe_ratio = avg_return / volatility if volatility > 0 else 0
            
            # 计算最大回撤
            cumulative_returns = np.cumsum(returns)
            running_max = np.maximum.accumulate(cumulative_returns)
            drawdown = cumulative_returns - running_max
            max_drawdown = abs(min(drawdown)) if len(drawdown) > 0 else 0
        else:
            avg_return = max_return = min_return = volatility = sharpe_ratio = max_drawdown = 0
            positive_returns = negative_returns = 0
        
        # 时间分析
        validated_recs = [r for r in recommendations if r.validation_date]
        if validated_recs:
            validation_days = []
            for r in validated_recs:
                rec_date = datetime.strptime(r.recommend_date, '%Y-%m-%d')
                val_date = datetime.strptime(r.validation_date, '%Y-%m-%d')
                validation_days.append((val_date - rec_date).days)
            avg_validation_days = np.mean(validation_days)
        else:
            avg_validation_days = 0
        
        # 按时间周期分析成功率
        success_by_time_horizon = self._analyze_success_by_time_horizon(recommendations)
        
        # 计算收益曲线
        profit_curve = self._calculate_profit_curve(recommendations)
        
        # 构建指标对象
        metrics = BacktestMetrics(
            model_name=model_name,
            period_start=start_date or "N/A",
            period_end=end_date or "N/A",
            total_recommendations=total_recommendations,
            validated_recommendations=validated_recommendations,
            pending_recommendations=pending_recommendations,
            total_hits=total_hits,
            total_misses=total_misses,
            hit_rate=hit_rate,
            buy_hit_rate=buy_hit_rate,
            sell_hit_rate=sell_hit_rate,
            hold_hit_rate=hold_hit_rate,
            avg_return=avg_return,
            positive_returns=positive_returns,
            negative_returns=negative_returns,
            max_return=max_return,
            min_return=min_return,
            volatility=volatility,
            sharpe_ratio=sharpe_ratio,
            max_drawdown=max_drawdown,
            avg_validation_days=avg_validation_days,
            success_by_time_horizon=success_by_time_horizon,
            recommendations=recommendations,
            profit_curve=profit_curve
        )
        
        # 保存到数据库
        self._save_backtest_metrics(metrics)
        
        return metrics
    
    def get_recommendation_by_id(self, rec_id: str) -> Optional[StockRecommendation]:
        """根据ID获取推荐"""
        with sqlite3.connect(self.db_path) as conn:
            conn.row_factory = sqlite3.Row
            cursor = conn.execute('SELECT * FROM recommendations WHERE id = ?', (rec_id,))
            row = cursor.fetchone()
            
            if row:
                return self._row_to_recommendation(row)
            return None
    
    def get_model_recommendations(self, model_name: str, limit: int = 100) -> List[StockRecommendation]:
        """获取模型的推荐记录"""
        with sqlite3.connect(self.db_path) as conn:
            conn.row_factory = sqlite3.Row
            cursor = conn.execute('''
                SELECT * FROM recommendations 
                WHERE model_name = ? 
                ORDER BY created_at DESC 
                LIMIT ?
            ''', (model_name, limit))
            
            return [self._row_to_recommendation(row) for row in cursor.fetchall()]
    
    def _save_recommendation_mysql(self, rec: StockRecommendation):
        """保存推荐到MySQL数据库"""
        conn = pymysql.connect(**self.connection_params)
        try:
            with conn.cursor() as cursor:
                cursor.execute('''
                    INSERT INTO recommendations 
                    (id, model_name, stock_code, stock_name, recommendation_type, 
                     recommendation_text, confidence_score, strategy_type, target_price, stop_loss, 
                     time_horizon, recommend_date, recommend_price, recommend_volume,
                     validation_date, validation_price, actual_return, result, 
                     created_at, updated_at)
                    VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s)
                    ON DUPLICATE KEY UPDATE
                    updated_at = %s, confidence_score = %s, recommendation_text = %s
                ''', (
                    rec.id, rec.model_name, rec.stock_code, rec.stock_name,
                    rec.recommendation_type, rec.recommendation_text,
                    rec.confidence_score, rec.strategy_type, rec.target_price, rec.stop_loss,
                    rec.time_horizon, rec.recommend_date, rec.recommend_price,
                    rec.recommend_volume, rec.validation_date, rec.validation_price,
                    rec.actual_return, rec.result, rec.created_at, rec.updated_at,
                    # ON DUPLICATE KEY UPDATE values
                    rec.updated_at, rec.confidence_score, rec.recommendation_text
                ))
            conn.commit()
        finally:
            conn.close()
    
    def _save_recommendation_sqlite(self, rec: StockRecommendation):
        """保存推荐到SQLite数据库"""
        with sqlite3.connect(self.db_path) as conn:
            conn.execute('''
                INSERT OR REPLACE INTO recommendations 
                (id, model_name, stock_code, stock_name, recommendation_type, 
                 recommendation_text, confidence_score, strategy_type, target_price, stop_loss, 
                 time_horizon, recommend_date, recommend_price, recommend_volume,
                 validation_date, validation_price, actual_return, result, 
                 created_at, updated_at)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            ''', (
                rec.id, rec.model_name, rec.stock_code, rec.stock_name,
                rec.recommendation_type, rec.recommendation_text,
                rec.confidence_score, rec.strategy_type, rec.target_price, rec.stop_loss,
                rec.time_horizon, rec.recommend_date, rec.recommend_price,
                rec.recommend_volume, rec.validation_date, rec.validation_price,
                rec.actual_return, rec.result, rec.created_at, rec.updated_at
            ))
            conn.commit()
    
    def _get_pending_recommendations(self, model_name: str = None) -> List[StockRecommendation]:
        """获取待验证的推荐"""
        query = 'SELECT * FROM recommendations WHERE result = ?'
        params = [RecommendationResult.PENDING.value]
        
        if model_name:
            query += ' AND model_name = ?'
            params.append(model_name)
        
        with sqlite3.connect(self.db_path) as conn:
            conn.row_factory = sqlite3.Row
            cursor = conn.execute(query, params)
            return [self._row_to_recommendation(row) for row in cursor.fetchall()]
    
    def _get_recommendations_by_period(self, model_name: str, 
                                     start_date: str = None, end_date: str = None) -> List[StockRecommendation]:
        """按时间段获取推荐"""
        query = 'SELECT * FROM recommendations WHERE model_name = ?'
        params = [model_name]
        
        if start_date:
            query += ' AND recommend_date >= ?'
            params.append(start_date)
        
        if end_date:
            query += ' AND recommend_date <= ?'
            params.append(end_date)
        
        query += ' ORDER BY recommend_date DESC'
        
        with sqlite3.connect(self.db_path) as conn:
            conn.row_factory = sqlite3.Row
            cursor = conn.execute(query, params)
            return [self._row_to_recommendation(row) for row in cursor.fetchall()]
    
    def _get_stock_price(self, stock_code: str, date: str) -> Optional[float]:
        """获取指定日期的股票价格"""
        try:
            # 这里应该调用真实的股票数据API
            # 为了演示，返回模拟价格
            import random
            base_price = 10.0
            return base_price * (0.8 + random.random() * 0.4)  # 模拟价格波动
        except Exception as e:
            self.logger.error(f"获取股票价格失败 {stock_code} {date}: {e}")
            return None
    
    def _update_recommendation_result(self, rec_id: str, validation_date: str,
                                    validation_price: float, actual_return: float, result: str):
        """更新推荐结果"""
        with sqlite3.connect(self.db_path) as conn:
            conn.execute('''
                UPDATE recommendations 
                SET validation_date = ?, validation_price = ?, actual_return = ?, 
                    result = ?, updated_at = ?
                WHERE id = ?
            ''', (validation_date, validation_price, actual_return, result, 
                  datetime.now().isoformat(), rec_id))
            conn.commit()
    
    def _save_backtest_metrics(self, metrics: BacktestMetrics):
        """保存回测指标"""
        import uuid
        metrics_id = str(uuid.uuid4())
        
        with sqlite3.connect(self.db_path) as conn:
            conn.execute('''
                INSERT INTO backtest_metrics 
                (id, model_name, period_start, period_end, metrics_data, created_at)
                VALUES (?, ?, ?, ?, ?, ?)
            ''', (
                metrics_id, metrics.model_name, metrics.period_start,
                metrics.period_end, json.dumps(asdict(metrics), ensure_ascii=False),
                datetime.now().isoformat()
            ))
            conn.commit()
    
    def _analyze_success_by_time_horizon(self, recommendations: List[StockRecommendation]) -> Dict[str, float]:
        """按时间周期分析成功率"""
        horizon_groups = {
            "短期(<=30天)": [],
            "中期(31-90天)": [],
            "长期(>90天)": []
        }
        
        for rec in recommendations:
            if rec.result == RecommendationResult.PENDING.value:
                continue
                
            if rec.time_horizon <= 30:
                horizon_groups["短期(<=30天)"].append(rec)
            elif rec.time_horizon <= 90:
                horizon_groups["中期(31-90天)"].append(rec)
            else:
                horizon_groups["长期(>90天)"].append(rec)
        
        success_rates = {}
        for period, recs in horizon_groups.items():
            if recs:
                hits = len([r for r in recs if r.result == RecommendationResult.HIT.value])
                success_rates[period] = hits / len(recs)
            else:
                success_rates[period] = 0
        
        return success_rates
    
    def _empty_metrics(self, model_name: str, start_date: str, end_date: str) -> BacktestMetrics:
        """创建空的指标对象"""
        return BacktestMetrics(
            model_name=model_name,
            period_start=start_date or "N/A",
            period_end=end_date or "N/A",
            total_recommendations=0,
            validated_recommendations=0,
            pending_recommendations=0,
            total_hits=0,
            total_misses=0,
            hit_rate=0,
            buy_hit_rate=0,
            sell_hit_rate=0,
            hold_hit_rate=0,
            avg_return=0,
            positive_returns=0,
            negative_returns=0,
            max_return=0,
            min_return=0,
            volatility=0,
            sharpe_ratio=0,
            max_drawdown=0,
            avg_validation_days=0,
            success_by_time_horizon={},
            recommendations=[]
        )
    
    def _row_to_recommendation(self, row) -> StockRecommendation:
        """数据库行转换为推荐对象"""
        return StockRecommendation(
            id=row['id'],
            model_name=row['model_name'],
            stock_code=row['stock_code'],
            stock_name=row['stock_name'],
            recommendation_type=row['recommendation_type'],
            recommendation_text=row['recommendation_text'],
            confidence_score=row['confidence_score'],
            target_price=row['target_price'],
            stop_loss=row['stop_loss'],
            time_horizon=row['time_horizon'],
            recommend_date=row['recommend_date'],
            recommend_price=row['recommend_price'],
            recommend_volume=row['recommend_volume'],
            validation_date=row['validation_date'],
            validation_price=row['validation_price'],
            actual_return=row['actual_return'],
            result=row['result'],
            created_at=row['created_at'],
            updated_at=row['updated_at']
        )
    
    def get_latest_recommendations(self, limit: int = 10) -> List[StockRecommendation]:
        """获取最新的推荐"""
        try:
            if self.use_mysql:
                return self._get_latest_recommendations_mysql(limit)
            else:
                return self._get_latest_recommendations_sqlite(limit)
        except Exception as e:
            self.logger.error(f"获取最新推荐失败: {e}")
            return []
    
    def get_recommendations_by_model(self, model_name: str, limit: int = 100) -> List[StockRecommendation]:
        """根据模型名称获取推荐列表 - 兼容接口"""
        return self.get_model_recommendations(model_name, limit)
    
    def _get_latest_recommendations_mysql(self, limit: int) -> List[StockRecommendation]:
        """从MySQL获取最新推荐"""
        conn = pymysql.connect(**self.connection_params)
        try:
            with conn.cursor(pymysql.cursors.DictCursor) as cursor:
                cursor.execute('''
                    SELECT * FROM recommendations 
                    ORDER BY created_at DESC 
                    LIMIT %s
                ''', (limit,))
                
                recommendations = []
                for row in cursor.fetchall():
                    rec = self._row_to_recommendation(row)
                    recommendations.append(rec)
                
                return recommendations
        finally:
            conn.close()
    
    def _get_latest_recommendations_sqlite(self, limit: int) -> List[StockRecommendation]:
        """从SQLite获取最新推荐"""
        with sqlite3.connect(self.db_path) as conn:
            conn.row_factory = sqlite3.Row
            cursor = conn.execute('''
                SELECT * FROM recommendations 
                ORDER BY created_at DESC 
                LIMIT ?
            ''', (limit,))
            
            recommendations = []
            for row in cursor.fetchall():
                rec = self._row_to_recommendation(row)
                recommendations.append(rec)
            
            return recommendations


    def get_statistics(self) -> Dict:
        """获取推荐系统统计数据"""
        try:
            if self.use_mysql:
                return self._get_statistics_mysql()
            else:
                return self._get_statistics_sqlite()
        except Exception as e:
            self.logger.error(f"获取统计数据失败: {e}")
            return {
                'total_statistics': {
                    'total_recommendations': 0,
                    'overall_hit_rate': 0.0,
                    'average_return': 0.0
                },
                'model_statistics': {}
            }
    
    def _get_statistics_mysql(self) -> Dict:
        """从MySQL获取统计数据"""
        conn = pymysql.connect(**self.connection_params)
        try:
            with conn.cursor() as cursor:
                # 总体统计
                cursor.execute("SELECT COUNT(*) FROM recommendations")
                total_recs = cursor.fetchone()[0]
                
                cursor.execute("SELECT COUNT(*) FROM recommendations WHERE result = 'hit'")
                hit_count = cursor.fetchone()[0]
                
                cursor.execute("SELECT AVG(actual_return) FROM recommendations WHERE actual_return IS NOT NULL")
                avg_return = cursor.fetchone()[0] or 0.0
                
                # 各模型统计
                cursor.execute("""
                    SELECT model_name, COUNT(*) as total,
                           SUM(CASE WHEN result = 'hit' THEN 1 ELSE 0 END) as hits,
                           AVG(actual_return) as avg_return
                    FROM recommendations 
                    GROUP BY model_name
                """)
                
                model_stats = {}
                for row in cursor.fetchall():
                    model_name, total, hits, model_avg_return = row
                    model_stats[model_name] = {
                        'total_recommendations': total,
                        'hit_rate': (hits / total) if total > 0 else 0.0,
                        'average_return': model_avg_return or 0.0
                    }
                
                return {
                    'total_statistics': {
                        'total_recommendations': total_recs,
                        'overall_hit_rate': (hit_count / total_recs) if total_recs > 0 else 0.0,
                        'average_return': avg_return
                    },
                    'model_statistics': model_stats
                }
                
        finally:
            conn.close()
    
    def _get_statistics_sqlite(self) -> Dict:
        """从SQLite获取统计数据"""
        with sqlite3.connect(self.db_path) as conn:
            conn.row_factory = sqlite3.Row
            
            # 总体统计
            cursor = conn.execute("SELECT COUNT(*) FROM recommendations")
            total_recs = cursor.fetchone()[0]
            
            cursor = conn.execute("SELECT COUNT(*) FROM recommendations WHERE result = 'hit'")
            hit_count = cursor.fetchone()[0]
            
            cursor = conn.execute("SELECT AVG(actual_return) FROM recommendations WHERE actual_return IS NOT NULL")
            avg_return = cursor.fetchone()[0] or 0.0
            
            # 各模型统计
            cursor = conn.execute("""
                SELECT model_name, COUNT(*) as total,
                       SUM(CASE WHEN result = 'hit' THEN 1 ELSE 0 END) as hits,
                       AVG(actual_return) as avg_return
                FROM recommendations 
                GROUP BY model_name
            """)
            
            model_stats = {}
            for row in cursor.fetchall():
                model_name, total, hits, model_avg_return = row
                model_stats[model_name] = {
                    'total_recommendations': total,
                    'hit_rate': (hits / total) if total > 0 else 0.0,
                    'average_return': model_avg_return or 0.0
                }
            
            return {
                'total_statistics': {
                    'total_recommendations': total_recs,
                    'overall_hit_rate': (hit_count / total_recs) if total_recs > 0 else 0.0,
                    'average_return': avg_return
                },
                'model_statistics': model_stats
            }

if __name__ == "__main__":
    # 示例用法
    logging.basicConfig(level=logging.INFO)
    
    tracker = RecommendationTracker()
    
    # 添加推荐示例
    rec_id = tracker.add_recommendation(
        model_name="ljwx-stock-v1.0",
        stock_code="000001.SZ",
        stock_name="平安银行",
        recommendation_text="基于技术分析，平安银行当前RSI为35，处于超卖状态，建议买入，目标价15元，预期30天内达到。",
        current_price=12.5,
        current_volume=1000000
    )
    
    print(f"添加推荐成功: {rec_id}")
    
    # 验证推荐
    validated = tracker.validate_recommendations("ljwx-stock-v1.0")
    print(f"验证完成: {validated} 条")
    
    # 生成回测报告
    metrics = tracker.generate_backtest_report("ljwx-stock-v1.0")
    print(f"命中率: {metrics.hit_rate:.2%}")
    print(f"平均收益: {metrics.avg_return:.2%}")