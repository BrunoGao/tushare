import pandas as pd
import numpy as np
import logging
from datetime import datetime, timedelta, date
from typing import List, Dict, Any
from sklearn.preprocessing import StandardScaler
from sklearn.ensemble import RandomForestRegressor
import sys, os
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import config
from utils.db_helper import db

logging.basicConfig(level=getattr(logging, config.LOG_LEVEL), format=config.LOG_FORMAT)
logger = logging.getLogger(__name__)

class StockRecommender:
    def __init__(self):
        self.scaler = StandardScaler()
        self.model = RandomForestRegressor(n_estimators=100, random_state=42)
        
    def calculate_technical_indicators(self, df: pd.DataFrame) -> pd.DataFrame:  # 计算技术指标
        df = df.sort_values('trade_date').copy()
        
        # 移动平均线
        df['ma5'] = df['close'].rolling(window=config.MA_SHORT).mean()
        df['ma20'] = df['close'].rolling(window=config.MA_LONG).mean()
        df['ma60'] = df['close'].rolling(window=60).mean()
        
        # 价格动量
        df['price_change'] = df['close'].pct_change()
        df['vol_change'] = df['vol'].pct_change()
        
        # RSI指标
        delta = df['close'].diff()
        gain = (delta.where(delta > 0, 0)).rolling(window=14).mean()
        loss = (-delta.where(delta < 0, 0)).rolling(window=14).mean()
        rs = gain / loss
        df['rsi'] = 100 - (100 / (1 + rs))
        
        # MACD
        exp1 = df['close'].ewm(span=12).mean()
        exp2 = df['close'].ewm(span=26).mean()
        df['macd'] = exp1 - exp2
        df['macd_signal'] = df['macd'].ewm(span=9).mean()
        
        # 波动率
        df['volatility'] = df['price_change'].rolling(window=20).std()
        
        return df
        
    def ma_crossover_strategy(self, ts_code: str, name: str = None) -> Dict[str, Any]:  # 均线交叉策略
        try:
            end_date = datetime.now().strftime('%Y%m%d')
            start_date = (datetime.now() - timedelta(days=90)).strftime('%Y%m%d')
            df = db.query_stock_data(ts_code, start_date, end_date)
            
            if len(df) < config.MA_LONG:
                return None
                
            df = self.calculate_technical_indicators(df)
            latest = df.iloc[-1]
            
            score = 0.0
            reasons = []
            
            # 均线策略评分
            if latest['ma5'] > latest['ma20']:
                score += 0.3
                reasons.append("短期均线上穿长期均线")
            if latest['close'] > latest['ma5']:
                score += 0.2
                reasons.append("价格突破短期均线")
            if latest['vol'] > df['vol'].rolling(20).mean().iloc[-1]:
                score += 0.2
                reasons.append("成交量放大")
            if 30 <= latest['rsi'] <= 70:
                score += 0.1
                reasons.append("RSI处于合理区间")
            if latest['macd'] > latest['macd_signal']:
                score += 0.2
                reasons.append("MACD金叉")
                
            return {
                'ts_code': ts_code,
                'name': name or ts_code,
                'score': round(score, 4),
                'strategy': 'ma_crossover',
                'reason': '; '.join(reasons) if reasons else '无明显信号',
                'recommend_date': datetime.now().date()
            }
            
        except Exception as e:
            logger.error(f"分析股票{ts_code}失败: {e}")
            return None
            
    def momentum_strategy(self, ts_code: str, name: str = None) -> Dict[str, Any]:  # 动量策略
        try:
            end_date = datetime.now().strftime('%Y%m%d')
            start_date = (datetime.now() - timedelta(days=60)).strftime('%Y%m%d')
            df = db.query_stock_data(ts_code, start_date, end_date)
            
            if len(df) < 30:
                return None
                
            df = self.calculate_technical_indicators(df)
            
            # 计算动量分数
            recent_return = (df['close'].iloc[-1] / df['close'].iloc[-20] - 1) * 100  # 20日收益率
            vol_trend = df['vol'].rolling(5).mean().iloc[-1] / df['vol'].rolling(20).mean().iloc[-1]  # 成交量趋势
            
            score = 0.0
            reasons = []
            
            if recent_return > 5:
                score += 0.4
                reasons.append(f"近20日上涨{recent_return:.1f}%")
            elif recent_return > 0:
                score += 0.2
                reasons.append(f"近期上涨{recent_return:.1f}%")
                
            if vol_trend > 1.2:
                score += 0.3
                reasons.append("成交量显著放大")
            elif vol_trend > 1.0:
                score += 0.1
                reasons.append("成交量温和放大")
                
            # 相对强度
            if df['rsi'].iloc[-1] < 80:
                score += 0.2
                reasons.append("未超买")
                
            return {
                'ts_code': ts_code,
                'name': name or ts_code,
                'score': round(min(score, 1.0), 4),
                'strategy': 'momentum',
                'reason': '; '.join(reasons) if reasons else '动量不足',
                'recommend_date': datetime.now().date()
            }
            
        except Exception as e:
            logger.error(f"动量分析股票{ts_code}失败: {e}")
            return None
            
    def generate_recommendations(self, strategy: str = 'ma_crossover', limit: int = None) -> List[Dict]:  # 生成推荐列表
        start_time = datetime.now()
        recommendations = []
        
        try:
            # 获取股票列表
            with db.engine.connect() as conn:
                sql = "SELECT ts_code, name FROM stock_basic WHERE list_status='L' ORDER BY ts_code"
                result = conn.execute(db.text(sql))
                stocks = [(row[0], row[1]) for row in result]
                
            logger.info(f"开始生成推荐，策略:{strategy}，股票数量:{len(stocks)}")
            
            # 分析每只股票
            for i, (ts_code, name) in enumerate(stocks):
                try:
                    if strategy == 'ma_crossover':
                        recommendation = self.ma_crossover_strategy(ts_code, name)
                    elif strategy == 'momentum':
                        recommendation = self.momentum_strategy(ts_code, name)
                    else:
                        continue
                        
                    if recommendation and recommendation['score'] >= config.SCORE_THRESHOLD:
                        recommendations.append(recommendation)
                        
                    if (i + 1) % 100 == 0:
                        logger.info(f"已分析{i + 1}/{len(stocks)}只股票，当前推荐{len(recommendations)}只")
                        
                except Exception as e:
                    logger.error(f"分析股票{ts_code}异常: {e}")
                    continue
                    
            # 排序并限制数量
            recommendations.sort(key=lambda x: x['score'], reverse=True)
            if limit:
                recommendations = recommendations[:limit]
                
            # 添加排名
            for i, rec in enumerate(recommendations, 1):
                rec['rank_no'] = i
                
            logger.info(f"推荐生成完成，共{len(recommendations)}只股票")
            
            # 保存到数据库
            if recommendations:
                self.save_recommendations(recommendations)
                
            duration = int((datetime.now() - start_time).total_seconds())
            db.log_operation('recommend', f'generate_{strategy}', 'SUCCESS', f'生成{len(recommendations)}只推荐股票', duration)
            
        except Exception as e:
            duration = int((datetime.now() - start_time).total_seconds())
            error_msg = f"推荐生成失败: {e}"
            logger.error(error_msg)
            db.log_operation('recommend', f'generate_{strategy}', 'FAILED', error_msg, duration)
            
        return recommendations
        
    def save_recommendations(self, recommendations: List[Dict]):  # 保存推荐结果
        try:
            df = pd.DataFrame(recommendations)
            df.to_sql('recommend_result', con=db.engine, if_exists='append', index=False, chunksize=config.CHUNK_SIZE)
            logger.info(f"推荐结果已保存，共{len(recommendations)}条")
        except Exception as e:
            logger.error(f"保存推荐结果失败: {e}")
            
    def get_recommendations(self, date_filter: str = None, strategy: str = None, limit: int = config.MAX_RECOMMEND) -> List[Dict]:  # 获取推荐结果
        try:
            sql = "SELECT * FROM recommend_result WHERE is_valid=1"
            params = {}
            
            if date_filter:
                sql += " AND recommend_date = :date_filter"
                params['date_filter'] = date_filter
            else:
                sql += " AND recommend_date = (SELECT MAX(recommend_date) FROM recommend_result)"
                
            if strategy:
                sql += " AND strategy = :strategy"
                params['strategy'] = strategy
                
            sql += " ORDER BY score DESC"
            if limit:
                sql += f" LIMIT {limit}"
                
            with db.engine.connect() as conn:
                result = conn.execute(db.text(sql), params)
                return [dict(row._mapping) for row in result]
                
        except Exception as e:
            logger.error(f"获取推荐结果失败: {e}")
            return []

# 创建全局推荐器实例
recommender = StockRecommender()

if __name__ == "__main__":
    strategy = sys.argv[1] if len(sys.argv) > 1 else 'ma_crossover'
    limit = int(sys.argv[2]) if len(sys.argv) > 2 else config.MAX_RECOMMEND
    
    recommendations = recommender.generate_recommendations(strategy, limit)
    print(f"生成推荐完成，共{len(recommendations)}只股票")
    
    for rec in recommendations[:10]:  # 显示前10只
        print(f"{rec['rank_no']}. {rec['name']}({rec['ts_code']}) - 分数:{rec['score']:.3f} - {rec['reason']}") 