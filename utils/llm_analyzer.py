import openai
import json
import logging
from typing import Dict, List, Optional, Any
from datetime import datetime, timedelta
import pandas as pd
import config
from utils.technical_indicators import TechnicalIndicators

logger = logging.getLogger(__name__)

class LLMAnalyzer:
    """LLM智能分析器 - 自然语言股票分析"""
    
    def __init__(self):
        self.client = openai.OpenAI(api_key=config.OPENAI_API_KEY, base_url=config.OPENAI_BASE_URL)
        self.model = config.LLM_MODEL
        
    def analyze_stock_technical(self, stock_data: pd.DataFrame, stock_info: Dict) -> Dict:  # 技术分析
        """技术面分析"""
        try:
            # 计算技术指标
            tech_data = TechnicalIndicators.calculate_all_indicators(stock_data)
            signals = TechnicalIndicators.generate_signals(tech_data)
            
            # 获取最新数据
            latest = tech_data.iloc[-1] if not tech_data.empty else {}
            
            # 构建分析提示词
            prompt = f"""
作为专业股票分析师，请分析以下股票技术面数据：

股票信息：
- 股票代码：{stock_info.get('ts_code', 'N/A')}
- 股票名称：{stock_info.get('name', 'N/A')}
- 行业：{stock_info.get('industry', 'N/A')}

最新技术指标数据：
- 收盘价：{latest.get('close', 0):.2f}
- 5日均线：{latest.get('ma5', 0):.2f}
- 10日均线：{latest.get('ma10', 0):.2f}
- 20日均线：{latest.get('ma20', 0):.2f}
- RSI：{latest.get('rsi', 0):.2f}
- MACD：{latest.get('macd', 0):.4f}
- MACD信号线：{latest.get('macd_signal', 0):.4f}
- 布林带上轨：{latest.get('boll_upper', 0):.2f}
- 布林带下轨：{latest.get('boll_lower', 0):.2f}
- KDJ-K：{latest.get('kdj_k', 0):.2f}
- KDJ-D：{latest.get('kdj_d', 0):.2f}

请从以下方面进行分析：
1. 趋势分析（上升/下降/震荡）
2. 技术指标信号强度
3. 支撑阻力位分析
4. 买卖时机建议
5. 风险评估

请用专业但易懂的中文回答，控制在300字以内。
"""
            
            response = self.client.chat.completions.create(
                model=self.model,
                messages=[{"role": "user", "content": prompt}],
                max_tokens=500,
                temperature=0.3
            )
            
            analysis = response.choices[0].message.content
            
            return {
                'analysis': analysis,
                'technical_data': latest.to_dict() if hasattr(latest, 'to_dict') else latest,
                'trend_score': self._calculate_trend_score(latest),
                'signal_strength': self._calculate_signal_strength(signals.iloc[-1] if not signals.empty else {}),
                'risk_level': self._assess_risk_level(latest)
            }
            
        except Exception as e:
            logger.error(f"❌ 技术分析失败: {e}")
            return {'error': str(e)}
    
    def analyze_market_sentiment(self, query: str, market_data: List[Dict]) -> str:  # 市场情绪分析
        """市场情绪分析"""
        try:
            prompt = f"""
作为资深股票分析师，请根据用户问题和市场数据进行分析：

用户问题：{query}

市场概况：
- 涨停股票数：{len([s for s in market_data if s.get('pct_chg', 0) >= 9.8])}
- 跌停股票数：{len([s for s in market_data if s.get('pct_chg', 0) <= -9.8])}
- 平均涨跌幅：{sum([s.get('pct_chg', 0) for s in market_data]) / len(market_data):.2f}%
- 活跃股票数：{len(market_data)}

请从以下角度分析：
1. 市场整体情绪
2. 热点板块分析
3. 资金流向判断
4. 后市展望
5. 操作建议

用自然、专业的中文回答，控制在400字以内。
"""
            
            response = self.client.chat.completions.create(
                model=self.model,
                messages=[{"role": "user", "content": prompt}],
                max_tokens=600,
                temperature=0.4
            )
            
            return response.choices[0].message.content
            
        except Exception as e:
            logger.error(f"❌ 市场分析失败: {e}")
            return f"分析过程中出现错误：{str(e)}"
    
    def recommend_stocks(self, user_preference: Dict, candidate_stocks: List[Dict]) -> List[Dict]:  # 智能推荐
        """智能股票推荐"""
        try:
            # 构建推荐提示词
            stocks_info = "\n".join([
                f"- {s['ts_code']} {s['name']} 涨跌幅:{s.get('pct_chg', 0):.2f}% 成交额:{s.get('amount', 0):.0f}万"
                for s in candidate_stocks[:20]  # 限制数量
            ])
            
            prompt = f"""
作为AI投资顾问，根据用户偏好推荐股票：

用户偏好：
- 风险偏好：{user_preference.get('risk_level', '中等')}
- 投资期限：{user_preference.get('time_horizon', '中长期')}
- 关注行业：{user_preference.get('preferred_industries', '不限')}
- 投资金额：{user_preference.get('amount', '10万')}

候选股票池：
{stocks_info}

请推荐3-5只最适合的股票，包含：
1. 推荐理由
2. 目标价位
3. 风险提示
4. 建议仓位

用专业的中文回答，每只股票分析控制在100字以内。
"""
            
            response = self.client.chat.completions.create(
                model=self.model,
                messages=[{"role": "user", "content": prompt}],
                max_tokens=800,
                temperature=0.3
            )
            
            recommendation = response.choices[0].message.content
            
            # 解析推荐结果
            recommended_stocks = []
            for stock in candidate_stocks[:5]:
                score = self._calculate_recommendation_score(stock)
                recommended_stocks.append({
                    **stock,
                    'recommendation_score': score,
                    'recommendation_reason': recommendation
                })
            
            return sorted(recommended_stocks, key=lambda x: x['recommendation_score'], reverse=True)
            
        except Exception as e:
            logger.error(f"❌ 股票推荐失败: {e}")
            return []
    
    def natural_language_query(self, query: str, context_data: Dict) -> str:  # 自然语言查询
        """自然语言查询处理"""
        try:
            prompt = f"""
你是专业的股票分析AI助手，请根据用户问题和提供的数据给出准确、专业的回答。

用户问题：{query}

相关数据：{json.dumps(context_data, ensure_ascii=False, indent=2)}

回答要求：
1. 基于提供的真实数据
2. 使用专业但易懂的语言
3. 提供具体的数据支撑
4. 给出明确的结论或建议
5. 控制在300字以内

请用中文回答。
"""
            
            response = self.client.chat.completions.create(
                model=self.model,
                messages=[{"role": "user", "content": prompt}],
                max_tokens=500,
                temperature=0.2
            )
            
            return response.choices[0].message.content
            
        except Exception as e:
            logger.error(f"❌ 自然语言查询失败: {e}")
            return f"查询处理失败：{str(e)}"
    
    def _calculate_trend_score(self, data: Dict) -> float:  # 计算趋势评分
        """计算趋势评分 (0-100)"""
        try:
            score = 50  # 基础分数
            
            # MA趋势
            if data.get('ma5', 0) > data.get('ma10', 0) > data.get('ma20', 0):
                score += 20
            elif data.get('ma5', 0) < data.get('ma10', 0) < data.get('ma20', 0):
                score -= 20
            
            # MACD
            if data.get('macd', 0) > data.get('macd_signal', 0):
                score += 15
            else:
                score -= 15
            
            # RSI
            rsi = data.get('rsi', 50)
            if 30 < rsi < 70:
                score += 10
            elif rsi > 80 or rsi < 20:
                score -= 15
            
            return max(0, min(100, score))
            
        except Exception:
            return 50
    
    def _calculate_signal_strength(self, signals: Dict) -> str:  # 计算信号强度
        """计算信号强度"""
        try:
            buy_signals = sum([
                signals.get('ma_golden_cross', 0),
                signals.get('macd_golden', 0),
                signals.get('rsi_oversold', 0),
                signals.get('boll_breakthrough_down', 0)
            ])
            
            sell_signals = sum([
                signals.get('ma_death_cross', 0),
                signals.get('macd_death', 0),
                signals.get('rsi_overbought', 0),
                signals.get('boll_breakthrough_up', 0)
            ])
            
            if buy_signals >= 2:
                return "强买入"
            elif buy_signals >= 1:
                return "买入"
            elif sell_signals >= 2:
                return "强卖出"
            elif sell_signals >= 1:
                return "卖出"
            else:
                return "持有"
                
        except Exception:
            return "无信号"
    
    def _assess_risk_level(self, data: Dict) -> str:  # 评估风险等级
        """评估风险等级"""
        try:
            risk_score = 0
            
            # 波动率风险
            rsi = data.get('rsi', 50)
            if rsi > 80 or rsi < 20:
                risk_score += 2
            
            # 价格位置风险
            close = data.get('close', 0)
            boll_upper = data.get('boll_upper', 0)
            boll_lower = data.get('boll_lower', 0)
            
            if close > boll_upper:
                risk_score += 2
            elif close < boll_lower:
                risk_score += 1
            
            # MACD背离风险
            if abs(data.get('macd', 0)) > abs(data.get('macd_signal', 0)) * 2:
                risk_score += 1
            
            if risk_score >= 4:
                return "高风险"
            elif risk_score >= 2:
                return "中风险"
            else:
                return "低风险"
                
        except Exception:
            return "未知"
    
    def _calculate_recommendation_score(self, stock: Dict) -> float:  # 计算推荐评分
        """计算推荐评分"""
        try:
            score = 50
            
            # 涨跌幅
            pct_chg = stock.get('pct_chg', 0)
            if -3 < pct_chg < 3:
                score += 10
            elif pct_chg > 5:
                score -= 10
            
            # 成交量
            amount = stock.get('amount', 0)
            if amount > 100000:  # 成交额大于10亿
                score += 15
            elif amount < 10000:  # 成交额小于1亿
                score -= 10
            
            # 换手率
            turnover_rate = stock.get('turnover_rate', 0)
            if 2 < turnover_rate < 8:
                score += 10
            elif turnover_rate > 15:
                score -= 15
            
            return max(0, min(100, score))
            
        except Exception:
            return 50 