"""
基于Ollama的智能股票推荐分析模块
结合技术分析、基本面分析和AI大模型的综合推荐系统
"""
import asyncio
import json
import logging
import pandas as pd
import numpy as np
from typing import Dict, List, Optional, Any, Tuple
from datetime import datetime, timedelta
from dataclasses import dataclass, asdict
from concurrent.futures import ThreadPoolExecutor
import sys, os

# 添加项目路径
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import config
from database.db_manager import DatabaseManager
from llm.ollama_analyzer import OllamaStockAnalyzer, StockData, StockAnalysisResponse
from analysis.recommender import StockRecommender

# 日志配置
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

@dataclass
class MarketContext:
    """市场环境上下文"""
    market_trend: str  # 牛市/熊市/震荡
    volatility_level: str  # 高/中/低
    sector_rotation: Dict[str, float]  # 板块轮动情况
    risk_sentiment: str  # 风险偏好: 积极/谨慎/保守
    macro_factors: Dict[str, Any]  # 宏观经济因素

@dataclass
class IntelligentRecommendation:
    """智能推荐结果"""
    ts_code: str
    name: str
    recommendation: str  # buy/sell/hold
    confidence: float  # 0-1
    score: float  # 综合评分
    technical_score: float  # 技术分析得分
    ai_score: float  # AI分析得分
    target_price: Optional[float]
    stop_loss: Optional[float]
    reasoning: str
    risk_level: str
    key_factors: List[str]
    technical_signals: Dict[str, Any]
    market_position: str  # 市场地位: 龙头/成长/价值/小盘
    sector: str  # 所属板块
    recommendation_date: datetime
    holding_period: str  # 建议持有周期

class IntelligentStockRecommender:
    """智能股票推荐器"""
    
    def __init__(self):
        self.db_manager = DatabaseManager()
        self.ollama_analyzer = OllamaStockAnalyzer()
        self.traditional_recommender = StockRecommender()
        self.market_context = None
        
    async def analyze_market_context(self) -> MarketContext:
        """分析市场环境"""
        try:
            # 获取市场指数数据
            market_data = self.db_manager.fetch_data("""
                SELECT 
                    AVG(pct_chg) as avg_change,
                    STD(pct_chg) as volatility,
                    COUNT(*) as sample_size
                FROM stock_daily_202507 
                WHERE trade_date >= DATE_SUB(CURDATE(), INTERVAL 30 DAY)
            """)
            
            if market_data.empty:
                # 默认市场环境
                return MarketContext(
                    market_trend="震荡",
                    volatility_level="中",
                    sector_rotation={},
                    risk_sentiment="谨慎",
                    macro_factors={}
                )
            
            avg_change = market_data.iloc[0]['avg_change'] or 0
            volatility = market_data.iloc[0]['volatility'] or 2.0
            
            # 判断市场趋势
            if avg_change > 1.0:
                market_trend = "牛市"
                risk_sentiment = "积极"
            elif avg_change < -1.0:
                market_trend = "熊市"
                risk_sentiment = "保守"
            else:
                market_trend = "震荡"
                risk_sentiment = "谨慎"
            
            # 判断波动率水平
            if volatility > 3.0:
                volatility_level = "高"
            elif volatility < 1.5:
                volatility_level = "低"
            else:
                volatility_level = "中"
            
            # 获取板块表现
            sector_rotation = await self._analyze_sector_rotation()
            
            return MarketContext(
                market_trend=market_trend,
                volatility_level=volatility_level,
                sector_rotation=sector_rotation,
                risk_sentiment=risk_sentiment,
                macro_factors={"avg_market_change": avg_change, "market_volatility": volatility}
            )
            
        except Exception as e:
            logger.error(f"分析市场环境失败: {e}")
            return MarketContext(
                market_trend="震荡",
                volatility_level="中",
                sector_rotation={},
                risk_sentiment="谨慎",
                macro_factors={}
            )
    
    async def _analyze_sector_rotation(self) -> Dict[str, float]:
        """分析板块轮动"""
        try:
            # 获取主要板块的表现
            sector_data = self.db_manager.fetch_data("""
                SELECT 
                    b.industry,
                    AVG(d.pct_chg) as avg_change,
                    COUNT(*) as stock_count
                FROM stock_basic b
                JOIN stock_daily_202507 d ON b.ts_code = d.ts_code
                WHERE d.trade_date >= DATE_SUB(CURDATE(), INTERVAL 7 DAY)
                    AND b.industry IS NOT NULL
                GROUP BY b.industry
                HAVING stock_count >= 5
                ORDER BY avg_change DESC
                LIMIT 20
            """)
            
            if sector_data.empty:
                return {}
            
            return dict(zip(sector_data['industry'], sector_data['avg_change']))
            
        except Exception as e:
            logger.error(f"分析板块轮动失败: {e}")
            return {}
    
    async def get_candidate_stocks(self, limit: int = 100) -> List[Tuple[str, str, str]]:
        """获取候选股票列表"""
        try:
            # 基于技术指标筛选候选股票
            candidates = self.db_manager.fetch_data(f"""
                SELECT DISTINCT
                    b.ts_code, 
                    b.name, 
                    b.industry,
                    d.close,
                    d.pct_chg,
                    d.vol,
                    t.ma5,
                    t.ma20,
                    t.rsi6,
                    t.macd_dif
                FROM stock_basic b
                JOIN stock_daily_202507 d ON b.ts_code = d.ts_code
                LEFT JOIN technical_indicators t ON b.ts_code = t.ts_code
                    AND t.trade_date = d.trade_date
                WHERE d.trade_date = (SELECT MAX(trade_date) FROM stock_daily_202507)
                    AND d.vol > 10000  -- 有一定成交量
                    AND d.close > 3    -- 价格不能太低
                    AND t.ma5 IS NOT NULL
                    AND t.ma20 IS NOT NULL
                ORDER BY d.vol DESC
                LIMIT {limit}
            """)
            
            if candidates.empty:
                logger.warning("未找到合适的候选股票")
                return []
            
            return [(row['ts_code'], row['name'], row['industry'] or '其他') 
                   for _, row in candidates.iterrows()]
            
        except Exception as e:
            logger.error(f"获取候选股票失败: {e}")
            return []
    
    def prepare_stock_data(self, ts_code: str) -> Optional[StockData]:
        """准备股票数据用于分析"""
        try:
            # 获取最新的股票数据
            stock_data = self.db_manager.fetch_data(f"""
                SELECT 
                    d.close, d.pct_chg, d.vol, d.amount,
                    t.ma5, t.ma20, t.rsi6, t.macd_dif,
                    b.name
                FROM stock_daily_202507 d
                JOIN stock_basic b ON d.ts_code = b.ts_code
                LEFT JOIN technical_indicators t ON d.ts_code = t.ts_code
                WHERE d.ts_code = '{ts_code}'
                    AND d.trade_date = (SELECT MAX(trade_date) FROM stock_daily_202507)
            """)
            
            if stock_data.empty:
                return None
            
            row = stock_data.iloc[0]
            
            return StockData(
                ts_code=ts_code,
                name=row['name'],
                close=float(row['close'] or 0),
                pct_chg=float(row['pct_chg'] or 0),
                volume=float(row['vol'] or 0),
                turnover=float(row['amount'] or 0),
                pe=None,  # 可以后续添加
                pb=None,  # 可以后续添加
                ma5=float(row['ma5'] or 0),
                ma20=float(row['ma20'] or 0),
                rsi=float(row['rsi6'] or 0),
                macd=float(row['macd_dif'] or 0)
            )
            
        except Exception as e:
            logger.error(f"准备股票数据失败 {ts_code}: {e}")
            return None
    
    async def analyze_single_stock_intelligent(self, ts_code: str, name: str, sector: str) -> Optional[IntelligentRecommendation]:
        """智能分析单只股票"""
        try:
            # 1. 准备股票数据
            stock_data = self.prepare_stock_data(ts_code)
            if not stock_data:
                return None
            
            # 2. 传统技术分析
            traditional_analysis = self.traditional_recommender.ma_crossover_strategy(ts_code, name)
            technical_score = traditional_analysis['score'] if traditional_analysis else 0.0
            
            # 3. AI分析 (Ollama) - 暂时跳过以提高响应速度
            # ai_analysis = await self.ollama_analyzer.analyze_single_stock(stock_data)
            # ai_score = ai_analysis.confidence if ai_analysis else 0.5
            ai_analysis = None
            ai_score = self._calculate_rule_based_score(stock_data)  # 使用规则基础评分
            
            # 4. 机器学习预测 (如果可用)
            ml_score = 0.5  # 默认值
            ml_prediction = None
            try:
                from ai.ml_trainer import ml_trainer
                if ml_trainer.models:  # 如果有训练好的模型
                    ml_prediction = ml_trainer.predict_single_stock(ts_code)
                    # 将ML预测转换为分数 (-1,0,1) -> (0, 0.5, 1)
                    ml_score = (ml_prediction['prediction'] + 1) / 2.0
            except Exception as e:
                logger.warning(f"ML预测失败 {ts_code}: {e}")
            
            # 5. 综合评分计算
            # 权重: 技术分析30% + AI分析40% + 机器学习30%
            combined_score = (technical_score * 0.3) + (ai_score * 0.4) + (ml_score * 0.3)
            
            # 6. 确定推荐动作
            if combined_score >= 0.75:
                recommendation = "buy"
            elif combined_score <= 0.3:
                recommendation = "sell"
            else:
                recommendation = "hold"
            
            # 7. 风险评估
            risk_level = self._assess_risk_level_simple(stock_data)
            
            # 8. 市场地位判断
            market_position = self._determine_market_position(stock_data, sector)
            
            # 9. 简化推理
            reasoning = self._generate_simple_reasoning(
                traditional_analysis, stock_data, combined_score, recommendation
            )
            
            # 10. 关键因素提取
            key_factors = self._extract_simple_key_factors(traditional_analysis, stock_data, ml_prediction)
            
            return IntelligentRecommendation(
                ts_code=ts_code,
                name=name,
                recommendation=recommendation,
                confidence=combined_score,
                score=combined_score,
                technical_score=technical_score,
                ai_score=ai_score,
                target_price=stock_data.close * (1.1 if recommendation == "buy" else 0.95),
                stop_loss=stock_data.close * (0.95 if recommendation == "buy" else 1.05),
                reasoning=reasoning,
                risk_level=risk_level,
                key_factors=key_factors,
                technical_signals=self._extract_technical_signals(stock_data),
                market_position=market_position,
                sector=sector,
                recommendation_date=datetime.now(),
                holding_period=self._suggest_holding_period(recommendation, combined_score)
            )
            
        except Exception as e:
            logger.error(f"智能分析股票失败 {ts_code}: {e}")
            return None
    
    def _assess_risk_level(self, stock_data: StockData, ai_analysis: Optional[StockAnalysisResponse]) -> str:
        """评估风险等级"""
        risk_factors = 0
        
        # 技术风险因素
        if abs(stock_data.pct_chg) > 5:  # 日涨跌幅过大
            risk_factors += 1
        if stock_data.rsi > 80 or stock_data.rsi < 20:  # RSI极值
            risk_factors += 1
        if stock_data.volume < 50000:  # 成交量过低
            risk_factors += 1
        
        # AI评估的风险
        if ai_analysis and ai_analysis.risk_level:
            if ai_analysis.risk_level == "high":
                risk_factors += 2
            elif ai_analysis.risk_level == "low":
                risk_factors -= 1
        
        if risk_factors >= 3:
            return "high"
        elif risk_factors <= 0:
            return "low"
        else:
            return "medium"
    
    def _determine_market_position(self, stock_data: StockData, sector: str) -> str:
        """判断市场地位"""
        # 简化的市场地位判断
        if stock_data.turnover > 1000000000:  # 成交额超过10亿
            return "龙头"
        elif stock_data.pct_chg > 3:  # 涨幅较大
            return "成长"
        elif stock_data.close < 10:  # 低价股
            return "小盘"
        else:
            return "价值"
    
    def _generate_comprehensive_reasoning(self, traditional_analysis: Optional[Dict], 
                                        ai_analysis: Optional[StockAnalysisResponse],
                                        stock_data: StockData, 
                                        market_context: Optional[MarketContext],
                                        ml_prediction: Optional[Dict] = None) -> str:
        """生成综合分析推理"""
        reasoning_parts = []
        
        # 技术分析部分
        if traditional_analysis:
            reasoning_parts.append(f"技术分析: {traditional_analysis.get('reason', '无明显信号')}")
        
        # AI分析部分
        if ai_analysis:
            reasoning_parts.append(f"AI分析: {ai_analysis.reasoning[:200]}..." if ai_analysis.reasoning else "AI分析: 综合评估")
        
        # 机器学习预测部分
        if ml_prediction:
            ml_text = ml_prediction.get('prediction_text', '未知')
            ml_conf = ml_prediction.get('confidence', 0) * 100
            reasoning_parts.append(f"ML预测: {ml_text} (置信度: {ml_conf:.1f}%)")
        
        # 市场环境
        if market_context:
            reasoning_parts.append(f"市场环境: {market_context.market_trend}市场，波动性{market_context.volatility_level}")
        
        # 基本面信息
        reasoning_parts.append(f"基本面: 当前价格{stock_data.close:.2f}元，涨跌幅{stock_data.pct_chg:.2f}%")
        
        return " | ".join(reasoning_parts)
    
    def _extract_key_factors(self, traditional_analysis: Optional[Dict], 
                           ai_analysis: Optional[StockAnalysisResponse],
                           ml_prediction: Optional[Dict] = None) -> List[str]:
        """提取关键因素"""
        factors = []
        
        if traditional_analysis and traditional_analysis.get('score', 0) > 0.6:
            factors.append("技术形态良好")
        
        if ai_analysis:
            factors.extend(ai_analysis.key_factors[:3])  # 取前3个关键因素
        
        if ml_prediction and ml_prediction.get('confidence', 0) > 0.7:
            factors.append(f"ML高置信度{ml_prediction.get('prediction_text', '')}")
        
        return factors[:5]  # 最多返回5个关键因素
    
    def _suggest_holding_period(self, recommendation: str, confidence: float) -> str:
        """建议持有周期"""
        if recommendation == "buy":
            if confidence > 0.8:
                return "中长期(3-6个月)"
            else:
                return "短期(1-3个月)"
        elif recommendation == "sell":
            return "立即"
        else:
            return "继续观察"
    
    async def generate_intelligent_recommendations(self, max_stocks: int = 50, 
                                                 max_concurrent: int = 10) -> List[IntelligentRecommendation]:
        """生成智能推荐"""
        logger.info("🤖 开始生成智能股票推荐...")
        
        # 1. 分析市场环境
        self.market_context = await self.analyze_market_context()
        logger.info(f"📊 市场环境: {self.market_context.market_trend}，波动性: {self.market_context.volatility_level}")
        
        # 2. 获取候选股票
        candidates = await self.get_candidate_stocks(max_stocks * 2)  # 获取2倍数量用于筛选
        logger.info(f"📋 获取候选股票: {len(candidates)}只")
        
        if not candidates:
            return []
        
        # 3. 并发分析股票
        semaphore = asyncio.Semaphore(max_concurrent)
        
        async def analyze_with_semaphore(candidate):
            async with semaphore:
                ts_code, name, sector = candidate
                return await self.analyze_single_stock_intelligent(ts_code, name, sector)
        
        # 批量分析
        tasks = [analyze_with_semaphore(candidate) for candidate in candidates]
        results = await asyncio.gather(*tasks, return_exceptions=True)
        
        # 4. 筛选有效结果
        recommendations = []
        for result in results:
            if isinstance(result, IntelligentRecommendation):
                recommendations.append(result)
            elif isinstance(result, Exception):
                logger.error(f"分析异常: {result}")
        
        # 5. 排序和筛选
        # 按综合得分排序
        recommendations.sort(key=lambda x: x.score, reverse=True)
        
        # 筛选出买入推荐
        buy_recommendations = [r for r in recommendations if r.recommendation == "buy"]
        hold_recommendations = [r for r in recommendations if r.recommendation == "hold"]
        
        # 返回最终推荐列表
        final_recommendations = buy_recommendations[:max_stocks//2] + hold_recommendations[:max_stocks//2]
        final_recommendations.sort(key=lambda x: x.score, reverse=True)
        
        logger.info(f"✅ 智能推荐生成完成: 买入{len(buy_recommendations)}只，持有{len(hold_recommendations)}只")
        
        return final_recommendations[:max_stocks]
    
    async def save_intelligent_recommendations(self, recommendations: List[IntelligentRecommendation]) -> bool:
        """保存智能推荐结果"""
        try:
            if not recommendations:
                return True
            
            # 转换为DataFrame
            data = []
            for rec in recommendations:
                data.append({
                    'ts_code': rec.ts_code,
                    'name': rec.name,
                    'recommendation': rec.recommendation,
                    'confidence': rec.confidence,
                    'score': rec.score,
                    'technical_score': rec.technical_score,
                    'ai_score': rec.ai_score,
                    'target_price': rec.target_price,
                    'stop_loss': rec.stop_loss,
                    'reasoning': rec.reasoning,
                    'risk_level': rec.risk_level,
                    'key_factors': json.dumps(rec.key_factors, ensure_ascii=False),
                    'technical_signals': json.dumps(rec.technical_signals, ensure_ascii=False),
                    'market_position': rec.market_position,
                    'sector': rec.sector,
                    'holding_period': rec.holding_period,
                    'recommendation_date': rec.recommendation_date.date(),
                    'created_at': datetime.now()
                })
            
            df = pd.DataFrame(data)
            
            # 保存到数据库 (创建新表或使用现有表)
            df.to_sql('intelligent_recommendations', 
                     con=self.db_manager.engine, 
                     if_exists='append', 
                     index=False, 
                     chunksize=100)
            
            logger.info(f"💾 已保存{len(recommendations)}条智能推荐")
            return True
            
        except Exception as e:
            logger.error(f"保存智能推荐失败: {e}")
            return False
    
    async def get_latest_intelligent_recommendations(self, limit: int = 20) -> List[Dict[str, Any]]:
        """获取最新的智能推荐"""
        try:
            recommendations = self.db_manager.fetch_data(f"""
                SELECT *
                FROM intelligent_recommendations
                WHERE recommendation_date = (
                    SELECT MAX(recommendation_date) 
                    FROM intelligent_recommendations
                )
                ORDER BY score DESC
                LIMIT {limit}
            """)
            
            if recommendations.empty:
                return []
            
            # 转换为字典列表
            results = []
            for _, row in recommendations.iterrows():
                rec_dict = row.to_dict()
                # 解析JSON字段
                if rec_dict.get('key_factors'):
                    try:
                        rec_dict['key_factors'] = json.loads(rec_dict['key_factors'])
                    except:
                        rec_dict['key_factors'] = []
                
                if rec_dict.get('technical_signals'):
                    try:
                        rec_dict['technical_signals'] = json.loads(rec_dict['technical_signals'])
                    except:
                        rec_dict['technical_signals'] = {}
                
                results.append(rec_dict)
            
            return results
            
        except Exception as e:
            logger.error(f"获取智能推荐失败: {e}")
            return []

    def _calculate_rule_based_score(self, stock_data: StockData) -> float:
        """基于规则的评分计算"""
        score = 0.5  # 基础分
        
        # 均线趋势分析 (30%)
        if stock_data.ma5 and stock_data.ma20:
            if stock_data.close > stock_data.ma5 > stock_data.ma20:
                score += 0.15  # 强势上升趋势
            elif stock_data.close > stock_data.ma5:
                score += 0.1   # 温和上升
            elif stock_data.close < stock_data.ma5 < stock_data.ma20:
                score -= 0.15  # 下降趋势
            elif stock_data.close < stock_data.ma5:
                score -= 0.1   # 弱势
        
        # RSI分析 (20%)
        if stock_data.rsi:
            if 30 <= stock_data.rsi <= 70:
                score += 0.1   # 正常范围
            elif stock_data.rsi < 30:
                score += 0.05  # 超卖，可能反弹
            elif stock_data.rsi > 70:
                score -= 0.05  # 超买，可能回调
        
        # MACD分析 (20%)
        if stock_data.macd:
            if stock_data.macd > 0:
                score += 0.1
            else:
                score -= 0.05
        
        # 成交量分析 (15%)
        if stock_data.volume > 0:
            # 简化的成交量评分
            score += 0.05
        
        # 价格变化分析 (15%)
        if stock_data.pct_chg:
            if 0 < stock_data.pct_chg <= 3:
                score += 0.05  # 温和上涨
            elif stock_data.pct_chg > 5:
                score += 0.02  # 大涨但需谨慎
            elif -3 <= stock_data.pct_chg < 0:
                score -= 0.02  # 小跌
            elif stock_data.pct_chg < -5:
                score -= 0.05  # 大跌
        
        return max(0.0, min(1.0, score))
    
    def _assess_risk_level_simple(self, stock_data: StockData) -> str:
        """简化的风险评估"""
        risk_score = 0
        
        # 基于RSI的风险评估
        if stock_data.rsi:
            if stock_data.rsi > 80:
                risk_score += 2
            elif stock_data.rsi > 70:
                risk_score += 1
            elif stock_data.rsi < 20:
                risk_score += 2
            elif stock_data.rsi < 30:
                risk_score += 1
        
        # 基于价格变化的风险评估
        if abs(stock_data.pct_chg) > 5:
            risk_score += 1
        elif abs(stock_data.pct_chg) > 9:
            risk_score += 2
        
        if risk_score >= 3:
            return "高"
        elif risk_score >= 1:
            return "中"
        else:
            return "低"
    
    def _generate_simple_reasoning(self, traditional_analysis, stock_data: StockData, score: float, recommendation: str) -> str:
        """生成简化推理"""
        reasons = []
        
        # 技术分析推理
        if traditional_analysis and traditional_analysis.get('score', 0) > 0.6:
            reasons.append(f"技术指标显示{traditional_analysis.get('signal', '中性')}信号")
        
        # 均线推理
        if stock_data.ma5 and stock_data.ma20:
            if stock_data.close > stock_data.ma5 > stock_data.ma20:
                reasons.append("价格位于均线之上，趋势向好")
            elif stock_data.close < stock_data.ma5 < stock_data.ma20:
                reasons.append("价格跌破均线，趋势偏弱")
        
        # RSI推理
        if stock_data.rsi:
            if stock_data.rsi < 30:
                reasons.append("RSI显示超卖，可能存在反弹机会")
            elif stock_data.rsi > 70:
                reasons.append("RSI显示超买，需谨慎追高")
        
        # 综合评分推理
        if score > 0.7:
            reasons.append("综合评分较高，投资价值较好")
        elif score < 0.4:
            reasons.append("综合评分偏低，需谨慎投资")
        
        return f"{recommendation}建议：" + "；".join(reasons) if reasons else f"{recommendation}建议基于综合技术分析"
    
    def _extract_simple_key_factors(self, traditional_analysis, stock_data: StockData, ml_prediction) -> List[str]:
        """提取简化关键因素"""
        factors = []
        
        if traditional_analysis and traditional_analysis.get('score', 0) > 0.6:
            factors.append("技术指标积极")
        
        if stock_data.ma5 and stock_data.ma20 and stock_data.close > stock_data.ma5:
            factors.append("均线支撑")
        
        if stock_data.rsi and 30 <= stock_data.rsi <= 70:
            factors.append("RSI正常")
        elif stock_data.rsi and stock_data.rsi < 30:
            factors.append("超卖反弹")
        
        if ml_prediction:
            factors.append(f"AI预测{ml_prediction.get('prediction_text', '中性')}")
        
        if not factors:
            factors.append("基础技术分析")
        
        return factors
    
    def _extract_technical_signals(self, stock_data: StockData) -> Dict[str, Any]:
        """提取技术信号"""
        signals = {}
        
        # 趋势信号
        if stock_data.ma5 and stock_data.ma20:
            if stock_data.ma5 > stock_data.ma20:
                signals['trend'] = '上升'
            elif stock_data.ma5 < stock_data.ma20:
                signals['trend'] = '下降'
            else:
                signals['trend'] = '横盘'
        
        # 均线信号
        if stock_data.close and stock_data.ma5:
            if stock_data.close > stock_data.ma5:
                signals['ma_signal'] = '多头'
            else:
                signals['ma_signal'] = '空头'
        
        # RSI信号
        if stock_data.rsi:
            if stock_data.rsi > 70:
                signals['rsi_signal'] = '超买'
            elif stock_data.rsi < 30:
                signals['rsi_signal'] = '超卖'
            else:
                signals['rsi_signal'] = '正常'
        
        # 成交量信号（简化）
        if stock_data.volume:
            signals['volume_signal'] = '正常'
        
        return signals

# 全局实例
intelligent_recommender = IntelligentStockRecommender()

async def main():
    """主函数 - 用于测试"""
    try:
        # 生成智能推荐
        recommendations = await intelligent_recommender.generate_intelligent_recommendations(max_stocks=20)
        
        print(f"\n🎯 智能推荐结果 ({len(recommendations)}只股票):")
        print("=" * 80)
        
        for i, rec in enumerate(recommendations, 1):
            print(f"{i:2d}. {rec.name}({rec.ts_code})")
            print(f"    推荐: {rec.recommendation} | 置信度: {rec.confidence:.2f} | 风险: {rec.risk_level}")
            print(f"    板块: {rec.sector} | 地位: {rec.market_position}")
            print(f"    推理: {rec.reasoning[:100]}...")
            print(f"    关键因素: {', '.join(rec.key_factors)}")
            print("-" * 80)
        
        # 保存推荐结果
        success = await intelligent_recommender.save_intelligent_recommendations(recommendations)
        if success:
            print("✅ 推荐结果已保存到数据库")
        
    except Exception as e:
        logger.error(f"主程序运行失败: {e}")

if __name__ == "__main__":
    asyncio.run(main())