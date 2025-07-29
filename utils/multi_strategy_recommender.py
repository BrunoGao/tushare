#!/usr/bin/env python3
"""
多策略智能推荐引擎 - 增强版
集成技术面、基本面、资金面、AI预测等多种策略，提供综合智能推荐
支持风险评估、组合优化、缓存加速、实时更新等高级功能
"""
import pandas as pd
import numpy as np
from typing import Dict, List, Tuple, Optional, Union
import logging
from datetime import datetime, timedelta
from sklearn.ensemble import RandomForestRegressor, GradientBoostingRegressor
from sklearn.preprocessing import StandardScaler, RobustScaler
from sklearn.model_selection import train_test_split
from sklearn.metrics import mean_squared_error, r2_score
from scipy.optimize import minimize
import warnings
warnings.filterwarnings('ignore')

import sys
import os
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from utils.db_helper import db
from utils.technical_indicators import TechnicalIndicators
from utils.advanced_technical_indicators import AdvancedTechnicalIndicators
from utils.redis_cache_manager import cache_manager
from sqlalchemy import text

logger = logging.getLogger(__name__)

class TechnicalStrategy:
    """技术面分析策略"""
    
    def __init__(self):
        self.tech_indicator = TechnicalIndicators()
        self.advanced_indicator = AdvancedTechnicalIndicators()
        
    def analyze(self, df: pd.DataFrame) -> Dict:
        """技术面分析"""
        try:
            if df.empty:
                return {'score': 0, 'signals': [], 'reason': '无数据'}
                
            # 计算技术指标
            df_with_indicators = self.tech_indicator.calculate_all_indicators(df)
            df_with_signals = self.tech_indicator.generate_signals(df_with_indicators)
            
            # 获取最新数据
            latest = df_with_signals.iloc[-1]
            
            score = 50  # 基础分数
            signals = []
            
            # 趋势分析 (30分)
            if latest.get('ma5', 0) > latest.get('ma20', 0) > latest.get('ma60', 0):
                score += 15
                signals.append("均线多头排列")
            elif latest.get('ma5', 0) < latest.get('ma20', 0) < latest.get('ma60', 0):
                score -= 15
                signals.append("均线空头排列")
                
            # MACD分析 (15分)
            if latest.get('macd', 0) > latest.get('macd_signal', 0):
                score += 8
                signals.append("MACD金叉")
            else:
                score -= 8
                signals.append("MACD死叉")
                
            # RSI分析 (15分)
            rsi = latest.get('rsi', 50)
            if 30 < rsi < 70:
                score += 8
                signals.append("RSI正常区间")
            elif rsi < 30:
                score += 12
                signals.append("RSI超卖")
            elif rsi > 70:
                score -= 12
                signals.append("RSI超买")
                
            # KDJ分析 (10分)
            kdj_k = latest.get('kdj_k', 50)
            kdj_d = latest.get('kdj_d', 50)
            if kdj_k > kdj_d and kdj_k < 80:
                score += 5
                signals.append("KDJ金叉")
            elif kdj_k < kdj_d and kdj_k > 20:
                score -= 5
                signals.append("KDJ死叉")
                
            # 成交量分析 (10分)
            if len(df) >= 5:
                recent_vol = df['vol'].tail(5).mean()
                avg_vol = df['vol'].mean()
                if recent_vol > avg_vol * 1.5:
                    score += 5
                    signals.append("成交量放大")
                elif recent_vol < avg_vol * 0.5:
                    score -= 3
                    signals.append("成交量萎缩")
                    
            # 价格位置分析 (10分)
            current_price = latest.get('close', 0)
            high_20 = df['high'].tail(20).max()
            low_20 = df['low'].tail(20).min()
            
            if high_20 > low_20:
                price_position = (current_price - low_20) / (high_20 - low_20)
                if price_position > 0.8:
                    score -= 5
                    signals.append("价格高位")
                elif price_position < 0.2:
                    score += 5
                    signals.append("价格低位")
                    
            # 突破分析 (10分)
            if len(df) >= 20:
                resistance = df['high'].tail(20).quantile(0.9)
                support = df['low'].tail(20).quantile(0.1)
                
                if current_price > resistance:
                    score += 8
                    signals.append("突破阻力位")
                elif current_price < support:
                    score -= 8
                    signals.append("跌破支撑位")
                    
            score = max(0, min(100, score))
            
            return {
                'score': score,
                'signals': signals,
                'reason': f"技术面评分: {score:.1f}分, 主要信号: {', '.join(signals[:3])}"
            }
            
        except Exception as e:
            logger.error(f"技术面分析失败: {e}")
            return {'score': 50, 'signals': [], 'reason': '技术面分析异常'}

class FundamentalStrategy:
    """基本面分析策略"""
    
    def analyze(self, ts_code: str) -> Dict:
        """基本面分析"""
        try:
            score = 50
            signals = []
            
            # 获取财务指标数据
            with db.engine.connect() as conn:
                # 获取最新财务指标
                sql = """
                SELECT * FROM t_financial_indicators 
                WHERE ts_code = :ts_code 
                ORDER BY end_date DESC 
                LIMIT 1
                """
                result = conn.execute(text(sql), {'ts_code': ts_code})
                financial_data = result.fetchone()
                
                if financial_data:
                    # ROE分析 (25分)
                    roe = financial_data.roe if financial_data.roe else 0
                    if roe > 15:
                        score += 12
                        signals.append(f"ROE优秀({roe:.1f}%)")
                    elif roe > 10:
                        score += 8
                        signals.append(f"ROE良好({roe:.1f}%)")
                    elif roe < 5:
                        score -= 10
                        signals.append(f"ROE偏低({roe:.1f}%)")
                        
                    # PE分析 (20分)
                    # 通过EPS计算PE (需要当前价格)
                    eps = financial_data.eps if financial_data.eps else 0
                    if eps > 0:
                        # 这里需要获取当前价格来计算PE，暂时用EPS评估
                        if eps > 1:
                            score += 8
                            signals.append(f"EPS良好({eps:.2f})")
                        elif eps < 0:
                            score -= 10
                            signals.append("EPS为负")
                            
                    # 营收增长分析 (20分)
                    revenue_yoy = financial_data.or_yoy if financial_data.or_yoy else 0
                    if revenue_yoy > 20:
                        score += 10
                        signals.append(f"营收高增长({revenue_yoy:.1f}%)")
                    elif revenue_yoy > 10:
                        score += 5
                        signals.append(f"营收稳定增长({revenue_yoy:.1f}%)")
                    elif revenue_yoy < -10:
                        score -= 10
                        signals.append(f"营收下滑({revenue_yoy:.1f}%)")
                        
                    # 净利润增长分析 (20分)
                    profit_yoy = financial_data.netprofit_yoy if financial_data.netprofit_yoy else 0
                    if profit_yoy > 30:
                        score += 10
                        signals.append(f"利润高增长({profit_yoy:.1f}%)")
                    elif profit_yoy > 15:
                        score += 5
                        signals.append(f"利润稳定增长({profit_yoy:.1f}%)")
                    elif profit_yoy < -20:
                        score -= 10
                        signals.append(f"利润大幅下滑({profit_yoy:.1f}%)")
                        
                    # 资产负债率分析 (15分)
                    debt_ratio = financial_data.debt_to_assets if financial_data.debt_to_assets else 0
                    if debt_ratio < 0.3:
                        score += 8
                        signals.append("负债率健康")
                    elif debt_ratio > 0.7:
                        score -= 8
                        signals.append("负债率偏高")
                        
                else:
                    signals.append("缺少财务数据")
                    score = 45
                    
            score = max(0, min(100, score))
            
            return {
                'score': score,
                'signals': signals,
                'reason': f"基本面评分: {score:.1f}分, 主要信号: {', '.join(signals[:3])}"
            }
            
        except Exception as e:
            logger.error(f"基本面分析失败: {e}")
            return {'score': 50, 'signals': [], 'reason': '基本面分析异常'}

class CapitalFlowStrategy:
    """资金面分析策略"""
    
    def analyze(self, ts_code: str) -> Dict:
        """资金面分析"""
        try:
            score = 50
            signals = []
            
            with db.engine.connect() as conn:
                # 获取最近的资金流向数据
                sql = """
                SELECT * FROM t_money_flow 
                WHERE ts_code = :ts_code 
                ORDER BY trade_date DESC 
                LIMIT 5
                """
                result = conn.execute(text(sql), {'ts_code': ts_code})
                money_flow_data = result.fetchall()
                
                if money_flow_data:
                    # 计算最近5天的资金净流入
                    total_net_flow = sum([row.net_mf_amount for row in money_flow_data if row.net_mf_amount])
                    
                    # 主力资金分析 (40分)
                    large_net_flow = sum([
                        (row.buy_lg_amount or 0) - (row.sell_lg_amount or 0) + 
                        (row.buy_elg_amount or 0) - (row.sell_elg_amount or 0)
                        for row in money_flow_data
                    ])
                    
                    if large_net_flow > 0:
                        score += 20
                        signals.append(f"主力资金净流入({large_net_flow/10000:.1f}万)")
                    else:
                        score -= 15
                        signals.append(f"主力资金净流出({abs(large_net_flow)/10000:.1f}万)")
                        
                    # 总资金流向分析 (30分)
                    if total_net_flow > 0:
                        score += 15
                        signals.append(f"资金净流入({total_net_flow/10000:.1f}万)")
                    else:
                        score -= 10
                        signals.append(f"资金净流出({abs(total_net_flow)/10000:.1f}万)")
                        
                    # 资金流向趋势分析 (30分)
                    if len(money_flow_data) >= 3:
                        recent_flows = [row.net_mf_amount for row in money_flow_data[:3] if row.net_mf_amount]
                        if len(recent_flows) >= 2:
                            if all(recent_flows[i] > recent_flows[i+1] for i in range(len(recent_flows)-1)):
                                score += 15
                                signals.append("资金流入加速")
                            elif all(recent_flows[i] < recent_flows[i+1] for i in range(len(recent_flows)-1)):
                                score -= 15
                                signals.append("资金流出加速")
                                
                else:
                    signals.append("缺少资金流向数据")
                    score = 45
                    
                # 检查龙虎榜数据
                sql = """
                SELECT * FROM t_dragon_tiger_list 
                WHERE ts_code = :ts_code 
                ORDER BY trade_date DESC 
                LIMIT 3
                """
                result = conn.execute(text(sql), {'ts_code': ts_code})
                dragon_tiger_data = result.fetchall()
                
                if dragon_tiger_data:
                    # 龙虎榜活跃度分析
                    avg_net_amount = sum([row.net_amount for row in dragon_tiger_data if row.net_amount]) / len(dragon_tiger_data)
                    if avg_net_amount > 0:
                        score += 10
                        signals.append("龙虎榜资金净买入")
                    else:
                        score -= 5
                        signals.append("龙虎榜资金净卖出")
                        
            score = max(0, min(100, score))
            
            return {
                'score': score,
                'signals': signals,
                'reason': f"资金面评分: {score:.1f}分, 主要信号: {', '.join(signals[:3])}"
            }
            
        except Exception as e:
            logger.error(f"资金面分析失败: {e}")
            return {'score': 50, 'signals': [], 'reason': '资金面分析异常'}

class AIStrategy:
    """AI预测策略"""
    
    def __init__(self):
        self.model = RandomForestRegressor(n_estimators=100, random_state=42)
        self.scaler = StandardScaler()
        self.is_trained = False
        
    def prepare_features(self, df: pd.DataFrame) -> np.ndarray:
        """准备机器学习特征"""
        try:
            if df.empty or len(df) < 20:
                return np.array([])
                
            features = []
            
            # 价格特征
            features.extend([
                df['close'].iloc[-1] / df['close'].iloc[-5] - 1,  # 5日收益率
                df['close'].iloc[-1] / df['close'].iloc[-20] - 1,  # 20日收益率
                df['close'].iloc[-1] / df['high'].tail(20).max(),  # 相对高点位置
                df['close'].iloc[-1] / df['low'].tail(20).min(),   # 相对低点位置
            ])
            
            # 成交量特征
            if 'vol' in df.columns:
                features.extend([
                    df['vol'].tail(5).mean() / df['vol'].tail(20).mean() - 1,  # 成交量比率
                    df['vol'].iloc[-1] / df['vol'].tail(20).mean() - 1,        # 当日成交量比率
                ])
            else:
                features.extend([0, 0])
                
            # 技术指标特征
            tech_df = TechnicalIndicators.calculate_all_indicators(df)
            if not tech_df.empty:
                latest = tech_df.iloc[-1]
                features.extend([
                    latest.get('rsi', 50) / 100,  # RSI标准化
                    latest.get('macd', 0),        # MACD
                    1 if latest.get('ma5', 0) > latest.get('ma20', 0) else 0,  # 均线多头
                ])
            else:
                features.extend([0.5, 0, 0])
                
            # 波动率特征
            returns = df['close'].pct_change().dropna()
            if len(returns) > 0:
                features.extend([
                    returns.tail(5).std(),   # 5日波动率
                    returns.tail(20).std(),  # 20日波动率
                ])
            else:
                features.extend([0, 0])
                
            return np.array(features).reshape(1, -1)
            
        except Exception as e:
            logger.error(f"特征准备失败: {e}")
            return np.array([])
    
    def analyze(self, df: pd.DataFrame) -> Dict:
        """AI预测分析"""
        try:
            if df.empty or len(df) < 20:
                return {'score': 50, 'signals': [], 'reason': 'AI分析数据不足'}
                
            # 准备特征
            features = self.prepare_features(df)
            if features.size == 0:
                return {'score': 50, 'signals': [], 'reason': 'AI特征提取失败'}
                
            score = 50
            signals = []
            
            # 简单的规则基础AI分析（替代机器学习模型）
            try:
                # 趋势强度分析
                price_momentum = df['close'].iloc[-1] / df['close'].iloc[-5] - 1
                if price_momentum > 0.05:
                    score += 15
                    signals.append("AI检测到强势上涨趋势")
                elif price_momentum < -0.05:
                    score -= 15
                    signals.append("AI检测到弱势下跌趋势")
                    
                # 波动率分析
                returns = df['close'].pct_change().dropna()
                if len(returns) > 0:
                    volatility = returns.tail(10).std()
                    if volatility < 0.02:
                        score += 5
                        signals.append("AI检测到低波动率")
                    elif volatility > 0.05:
                        score -= 5
                        signals.append("AI检测到高波动率")
                        
                # 成交量与价格关系分析
                if 'vol' in df.columns and len(df) >= 10:
                    price_change = df['close'].pct_change().tail(5)
                    volume_change = df['vol'].pct_change().tail(5)
                    
                    # 计算价量相关性
                    correlation = price_change.corr(volume_change)
                    if not pd.isna(correlation):
                        if correlation > 0.5:
                            score += 10
                            signals.append("AI检测到价量配合良好")
                        elif correlation < -0.5:
                            score -= 8
                            signals.append("AI检测到价量背离")
                            
                # 技术形态识别
                if len(df) >= 20:
                    # 简单的双底识别
                    lows = df['low'].tail(20)
                    min_idx = lows.idxmin()
                    if min_idx < lows.index[-5]:  # 最低点不在最近5天
                        recent_low = lows.tail(5).min()
                        if abs(recent_low - lows.min()) / lows.min() < 0.03:
                            score += 12
                            signals.append("AI识别到双底形态")
                            
            except Exception as e:
                logger.warning(f"AI规则分析部分失败: {e}")
                
            score = max(0, min(100, score))
            
            return {
                'score': score,
                'signals': signals,
                'reason': f"AI预测评分: {score:.1f}分, 主要信号: {', '.join(signals[:2])}"
            }
            
        except Exception as e:
            logger.error(f"AI分析失败: {e}")
            return {'score': 50, 'signals': [], 'reason': 'AI分析异常'}

class MultiStrategyRecommender:
    """多策略推荐引擎"""
    
    def __init__(self):
        self.strategies = {
            'technical': TechnicalStrategy(),
            'fundamental': FundamentalStrategy(),
            'capital_flow': CapitalFlowStrategy(),
            'ai_prediction': AIStrategy()
        }
        
        # 默认权重配置
        self.default_weights = {
            'technical': 0.4,      # 技术面权重40%
            'fundamental': 0.3,    # 基本面权重30%
            'capital_flow': 0.2,   # 资金面权重20%
            'ai_prediction': 0.1   # AI预测权重10%
        }
        
    def analyze_single_stock(self, ts_code: str, strategy_weights: Optional[Dict] = None) -> Dict:
        """分析单只股票"""
        try:
            weights = strategy_weights or self.default_weights
            
            # 获取股票历史数据
            end_date = datetime.now().strftime('%Y%m%d')
            start_date = (datetime.now() - timedelta(days=120)).strftime('%Y%m%d')
            df = db.query_stock_data(ts_code, start_date, end_date)
            
            if df.empty:
                return {
                    'ts_code': ts_code,
                    'total_score': 0,
                    'recommendation': 'HOLD',
                    'confidence': 0,
                    'strategies': {},
                    'reason': '无历史数据'
                }
                
            # 执行各策略分析
            strategy_results = {}
            
            # 技术面分析
            if weights.get('technical', 0) > 0:
                strategy_results['technical'] = self.strategies['technical'].analyze(df)
                
            # 基本面分析
            if weights.get('fundamental', 0) > 0:
                strategy_results['fundamental'] = self.strategies['fundamental'].analyze(ts_code)
                
            # 资金面分析
            if weights.get('capital_flow', 0) > 0:
                strategy_results['capital_flow'] = self.strategies['capital_flow'].analyze(ts_code)
                
            # AI预测分析
            if weights.get('ai_prediction', 0) > 0:
                strategy_results['ai_prediction'] = self.strategies['ai_prediction'].analyze(df)
                
            # 计算综合评分
            total_score = 0
            total_weight = 0
            
            for strategy_name, result in strategy_results.items():
                weight = weights.get(strategy_name, 0)
                if weight > 0:
                    total_score += result['score'] * weight
                    total_weight += weight
                    
            if total_weight > 0:
                total_score = total_score / total_weight
            else:
                total_score = 50
                
            # 生成推荐建议
            if total_score >= 70:
                recommendation = 'BUY'
                confidence = min(95, (total_score - 70) * 3 + 70)
            elif total_score >= 55:
                recommendation = 'HOLD'
                confidence = 60
            else:
                recommendation = 'SELL'
                confidence = min(95, (50 - total_score) * 3 + 70)
                
            # 生成推荐理由
            main_signals = []
            for strategy_name, result in strategy_results.items():
                if result['signals']:
                    main_signals.extend(result['signals'][:2])
                    
            reason = f"综合评分{total_score:.1f}分，主要依据：{'; '.join(main_signals[:4])}"
            
            return {
                'ts_code': ts_code,
                'total_score': round(total_score, 2),
                'recommendation': recommendation,
                'confidence': round(confidence, 1),
                'strategies': strategy_results,
                'reason': reason,
                'analysis_time': datetime.now().isoformat()
            }
            
        except Exception as e:
            logger.error(f"股票{ts_code}分析失败: {e}")
            return {
                'ts_code': ts_code,
                'total_score': 0,
                'recommendation': 'HOLD',
                'confidence': 0,
                'strategies': {},
                'reason': f'分析异常: {str(e)}'
            }
    
    def generate_recommendations(self, strategy_weights: Optional[Dict] = None, 
                               limit: int = 20, min_score: float = 60) -> List[Dict]:
        """生成股票推荐列表"""
        try:
            logger.info(f"🚀 开始生成股票推荐，限制{limit}只，最低评分{min_score}")
            
            # 获取活跃股票列表
            with db.engine.connect() as conn:
                sql = """
                SELECT ts_code, name FROM stock_basic 
                WHERE list_status = 'L' 
                ORDER BY RAND() 
                LIMIT :limit_count
                """
                result = conn.execute(text(sql), {'limit_count': limit * 3})  # 多获取一些用于筛选
                stock_list = result.fetchall()
                
            if not stock_list:
                logger.warning("未获取到股票列表")
                return []
                
            recommendations = []
            
            for stock in stock_list:
                try:
                    analysis = self.analyze_single_stock(stock.ts_code, strategy_weights)
                    
                    if analysis['total_score'] >= min_score:
                        # 添加股票名称
                        analysis['stock_name'] = stock.name
                        recommendations.append(analysis)
                        
                        logger.info(f"✅ {stock.ts_code} {stock.name}: {analysis['total_score']:.1f}分 - {analysis['recommendation']}")
                        
                        if len(recommendations) >= limit:
                            break
                            
                except Exception as e:
                    logger.warning(f"分析{stock.ts_code}失败: {e}")
                    continue
                    
            # 按评分排序
            recommendations.sort(key=lambda x: x['total_score'], reverse=True)
            
            logger.info(f"✅ 推荐生成完成，共{len(recommendations)}只股票")
            return recommendations
            
        except Exception as e:
            logger.error(f"生成推荐失败: {e}")
            return []
    
    def get_strategy_recommendations(self, strategy_name: str, limit: int = 10) -> List[Dict]:
        """获取单一策略推荐"""
        weights = {strategy: 0 for strategy in self.default_weights.keys()}
        weights[strategy_name] = 1.0
        
        return self.generate_recommendations(weights, limit, min_score=55)
    
    def save_recommendations(self, recommendations: List[Dict], strategy: str = 'multi_strategy'):
        """保存推荐结果到数据库"""
        try:
            if not recommendations:
                return
                
            # 清除今日旧推荐
            today = datetime.now().date()
            with db.engine.connect() as conn:
                conn.execute(text("DELETE FROM recommend_result WHERE recommend_date = :date AND strategy = :strategy"), 
                           {'date': today, 'strategy': strategy})
                conn.commit()
                
            # 插入新推荐
            for i, rec in enumerate(recommendations):
                with db.engine.connect() as conn:
                    sql = """
                    INSERT INTO recommend_result 
                    (ts_code, name, score, recommend_date, strategy, reason, rank_no, is_valid)
                    VALUES (:ts_code, :name, :score, :date, :strategy, :reason, :rank_no, 1)
                    """
                    conn.execute(text(sql), {
                        'ts_code': rec['ts_code'],
                        'name': rec.get('stock_name', rec['ts_code']),
                        'score': rec['total_score'] / 100,  # 转换为0-1分数
                        'date': today,
                        'strategy': strategy,
                        'reason': rec['reason'][:500],  # 限制长度
                        'rank_no': i + 1
                    })
                    conn.commit()
                    
            logger.info(f"✅ 保存{len(recommendations)}条推荐结果到数据库")
            
        except Exception as e:
            logger.error(f"保存推荐结果失败: {e}")
    
    # ==================== 增强功能 ====================
    
    def calculate_risk_metrics(self, ts_code: str, df: pd.DataFrame) -> Dict:
        """计算风险指标"""
        try:
            if df.empty or len(df) < 20:
                return {'risk_score': 50, 'volatility': 0, 'max_drawdown': 0, 'beta': 1.0}
            
            # 计算收益率
            returns = df['close'].pct_change().dropna()
            
            # 波动率 (年化)
            volatility = returns.std() * np.sqrt(252)
            
            # 最大回撤
            cumulative = (1 + returns).cumprod()
            running_max = cumulative.expanding().max()
            drawdown = (cumulative - running_max) / running_max
            max_drawdown = drawdown.min()
            
            # Beta计算 (相对于市场，这里简化处理)
            market_returns = returns.rolling(20).mean()  # 简化的市场收益
            if len(returns) > 20 and market_returns.std() > 0:
                beta = returns.cov(market_returns) / market_returns.var()
            else:
                beta = 1.0
            
            # 风险评分 (0-100，越低越好)
            risk_score = 50
            if volatility > 0.3:
                risk_score += 20
            elif volatility < 0.15:
                risk_score -= 10
                
            if max_drawdown < -0.2:
                risk_score += 15
            elif max_drawdown > -0.1:
                risk_score -= 5
                
            if beta > 1.5:
                risk_score += 10
            elif beta < 0.8:
                risk_score -= 5
            
            risk_score = max(0, min(100, risk_score))
            
            return {
                'risk_score': risk_score,
                'volatility': round(volatility, 4),
                'max_drawdown': round(max_drawdown, 4),
                'beta': round(beta, 2),
                'sharpe_ratio': round(returns.mean() / returns.std() * np.sqrt(252), 2) if returns.std() > 0 else 0
            }
            
        except Exception as e:
            logger.error(f"风险指标计算失败: {e}")
            return {'risk_score': 50, 'volatility': 0, 'max_drawdown': 0, 'beta': 1.0}
    
    def analyze_single_stock_enhanced(self, ts_code: str, strategy_weights: Optional[Dict] = None) -> Dict:
        """增强版单股分析"""
        try:
            # 先检查缓存
            cache_key = f"stock_analysis_{ts_code}_{hash(str(strategy_weights))}"
            cached_result = cache_manager.get('recommendations', cache_key)
            if cached_result:
                logger.info(f"从缓存获取 {ts_code} 分析结果")
                return cached_result
            
            # 执行基础分析
            result = self.analyze_single_stock(ts_code, strategy_weights)
            
            if result['total_score'] > 0:
                # 获取历史数据用于风险分析
                end_date = datetime.now().strftime('%Y%m%d')
                start_date = (datetime.now() - timedelta(days=120)).strftime('%Y%m%d')
                df = db.query_stock_data(ts_code, start_date, end_date)
                
                if not df.empty:
                    # 计算风险指标
                    risk_metrics = self.calculate_risk_metrics(ts_code, df)
                    result['risk_metrics'] = risk_metrics
                    
                    # 调整评分（考虑风险）
                    risk_adjustment = (100 - risk_metrics['risk_score']) / 100 * 10
                    result['total_score'] = min(100, result['total_score'] + risk_adjustment)
                    result['risk_adjusted_score'] = result['total_score']
                    
                    # 使用高级技术指标
                    try:
                        advanced_indicators = AdvancedTechnicalIndicators()
                        enhanced_df = advanced_indicators.calculate_enhanced_indicators(df)
                        smart_signals = advanced_indicators.generate_smart_signals(enhanced_df)
                        
                        if not smart_signals.empty:
                            latest_signals = smart_signals.iloc[-1]
                            
                            # 添加智能信号评分
                            smart_score = latest_signals.get('smart_signal_score', 0)
                            if smart_score != 0:
                                result['smart_signal_score'] = smart_score
                                result['total_score'] = min(100, result['total_score'] + smart_score * 0.1)
                            
                            # 添加市场状态信息
                            market_regime = latest_signals.get('market_regime', 0)
                            if market_regime != 0:
                                result['market_regime'] = 'trending' if market_regime != 0 else 'sideways'
                                result['regime_direction'] = 'bullish' if market_regime > 0 else 'bearish'
                    except Exception as e:
                        logger.warning(f"高级技术指标计算失败: {e}")
            
            # 缓存结果 (5分钟)
            cache_manager.set('recommendations', cache_key, result, 300)
            
            return result
            
        except Exception as e:
            logger.error(f"增强版股票分析失败 {ts_code}: {e}")
            return self.analyze_single_stock(ts_code, strategy_weights)
    
    def generate_enhanced_recommendations(self, strategy_weights: Optional[Dict] = None,
                                        limit: int = 20, min_score: float = 60) -> Dict:
        """生成增强版推荐"""
        try:
            logger.info(f"🚀 开始生成增强版推荐")
            
            # 生成基础推荐
            recommendations = []
            
            # 获取活跃股票列表
            with db.engine.connect() as conn:
                sql = """
                SELECT ts_code, name, industry FROM stock_basic
                WHERE list_status = 'L'
                ORDER BY RAND()
                LIMIT :limit_count
                """
                result = conn.execute(text(sql), {'limit_count': limit * 3})
                stock_list = result.fetchall()
            
            for stock in stock_list:
                try:
                    analysis = self.analyze_single_stock_enhanced(stock.ts_code, strategy_weights)
                    
                    if analysis['total_score'] >= min_score:
                        analysis['stock_name'] = stock.name
                        analysis['industry'] = stock.industry
                        recommendations.append(analysis)
                        
                        if len(recommendations) >= limit:
                            break
                            
                except Exception as e:
                    logger.warning(f"增强推荐分析失败 {stock.ts_code}: {e}")
                    continue
            
            # 按评分排序
            recommendations.sort(key=lambda x: x['total_score'], reverse=True)
            
            result = {
                'recommendations': recommendations,
                'generation_time': datetime.now().isoformat(),
                'total_analyzed': len(stock_list),
                'qualified_count': len(recommendations),
                'average_score': np.mean([r['total_score'] for r in recommendations]) if recommendations else 0
            }
            
            # 行业分布分析
            if recommendations:
                industries = {}
                for rec in recommendations:
                    industry = rec.get('industry', '未知')
                    industries[industry] = industries.get(industry, 0) + 1
                
                result['industry_distribution'] = industries
            
            logger.info(f"✅ 增强版推荐生成完成，共{len(recommendations)}只股票")
            return result
            
        except Exception as e:
            logger.error(f"生成增强版推荐失败: {e}")
            return {'recommendations': [], 'error': str(e)}
    
    def get_recommendation_performance(self, days_back: int = 30) -> Dict:
        """获取推荐表现统计"""
        try:
            with db.engine.connect() as conn:
                # 获取历史推荐
                sql = """
                SELECT r.*, s.close as recommend_price
                FROM recommend_result r
                LEFT JOIN stock_indicators s ON r.ts_code = s.ts_code
                    AND s.trade_date = r.recommend_date
                WHERE r.recommend_date >= DATE_SUB(CURDATE(), INTERVAL :days DAY)
                ORDER BY r.recommend_date DESC, r.rank_no
                """
                result = conn.execute(text(sql), {'days': days_back})
                historical_recs = result.fetchall()
                
                if not historical_recs:
                    return {'message': '无历史推荐数据'}
                
                performance_stats = {
                    'total_recommendations': len(historical_recs),
                    'average_score': np.mean([r.score for r in historical_recs]),
                    'strategies': {},
                    'daily_performance': []
                }
                
                # 按策略统计
                strategy_stats = {}
                for rec in historical_recs:
                    strategy = rec.strategy
                    if strategy not in strategy_stats:
                        strategy_stats[strategy] = {'count': 0, 'avg_score': 0, 'scores': []}
                    
                    strategy_stats[strategy]['count'] += 1
                    strategy_stats[strategy]['scores'].append(rec.score)
                
                for strategy, stats in strategy_stats.items():
                    stats['avg_score'] = np.mean(stats['scores'])
                    del stats['scores']  # 清理临时数据
                
                performance_stats['strategies'] = strategy_stats
                
                return performance_stats
                
        except Exception as e:
            logger.error(f"获取推荐表现失败: {e}")
            return {'error': str(e)}

# 创建全局实例
multi_strategy_recommender = MultiStrategyRecommender()