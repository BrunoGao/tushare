import requests
import json
import logging
from typing import Dict, List, Any, Optional
from datetime import datetime, timedelta
import sys, os
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import config
from utils.db_helper import db
from analysis.technical_indicators import tech_indicator
from sqlalchemy import text
import re

logger = logging.getLogger(__name__)

class StockAnalyzer:
    """专业股票分析师 - LLM增强版"""
    
    def __init__(self):
        self.api_url = config.LLM_API_URL
        self.model = config.LLM_MODEL
        self.timeout = config.LLM_TIMEOUT
        
        # 技术分析专业术语映射
        self.tech_terms = {
            '金叉': 'golden_cross', '死叉': 'death_cross',
            '超买': 'overbought', '超卖': 'oversold',
            '突破': 'breakout', '回调': 'pullback',
            '支撑': 'support', '阻力': 'resistance',
            '量价背离': 'price_volume_divergence',
            '多头排列': 'bullish_alignment',
            '空头排列': 'bearish_alignment'
        }
        
        # 市场情绪词汇
        self.sentiment_words = {
            '看涨': 'bullish', '看跌': 'bearish', '中性': 'neutral',
            '强势': 'strong', '弱势': 'weak', '震荡': 'sideways',
            '突破': 'breakout', '下跌': 'decline', '上涨': 'rise'
        }
        
    def parse_user_query(self, query: str) -> Dict[str, Any]:
        """解析用户自然语言查询"""
        parsed = {
            'intent': 'unknown',
            'stock_codes': [],
            'time_range': 'recent',
            'analysis_type': 'comprehensive',
            'indicators': [],
            'sentiment': 'neutral'
        }
        
        # 1. 提取股票代码
        stock_pattern = r'(\d{6}[.\s]*[SZ|SH]?|\d{6})'
        matches = re.findall(stock_pattern, query.upper())
        for match in matches:
            if len(match) == 6:
                # 根据开头数字判断交易所
                if match.startswith(('0', '3')):
                    parsed['stock_codes'].append(f"{match}.SZ")
                elif match.startswith('6'):
                    parsed['stock_codes'].append(f"{match}.SH")
            else:
                parsed['stock_codes'].append(match.replace(' ', ''))
                
        # 2. 判断查询意图
        if any(word in query for word in ['推荐', '买入', '选股']):
            parsed['intent'] = 'recommendation'
        elif any(word in query for word in ['分析', '技术分析', '走势']):
            parsed['intent'] = 'analysis'
        elif any(word in query for word in ['预测', '预判', '未来']):
            parsed['intent'] = 'prediction'
        elif any(word in query for word in ['风险', '止损']):
            parsed['intent'] = 'risk_assessment'
        elif any(word in query for word in ['对比', '比较']):
            parsed['intent'] = 'comparison'
            
        # 3. 提取时间范围
        if any(word in query for word in ['今日', '今天']):
            parsed['time_range'] = 'today'
        elif any(word in query for word in ['本周', '这周']):
            parsed['time_range'] = 'week'
        elif any(word in query for word in ['本月', '这月']):
            parsed['time_range'] = 'month'
        elif any(word in query for word in ['近期', '最近']):
            parsed['time_range'] = 'recent'
            
        # 4. 提取技术指标
        for term, indicator in self.tech_terms.items():
            if term in query:
                parsed['indicators'].append(indicator)
                
        # 5. 分析情感倾向
        for word, sentiment in self.sentiment_words.items():
            if word in query:
                parsed['sentiment'] = sentiment
                break
                
        return parsed
        
    def get_comprehensive_stock_data(self, ts_code: str, days: int = 60) -> Dict[str, Any]:
        """获取股票综合数据"""
        try:
            # 1. 基本信息
            with db.engine.connect() as conn:
                sql = "SELECT * FROM stock_basic WHERE ts_code = :ts_code"
                result = conn.execute(text(sql), {'ts_code': ts_code})
                basic_info = result.fetchone()
                
            if not basic_info:
                return {'error': f'未找到股票 {ts_code}'}
                
            # 2. 历史价格数据
            end_date = datetime.now().strftime('%Y%m%d')
            start_date = (datetime.now() - timedelta(days=days)).strftime('%Y%m%d')
            price_df = db.query_stock_data(ts_code, start_date, end_date)
            
            if price_df.empty:
                return {'error': f'{ts_code} 无历史数据'}
                
            # 3. 计算技术指标
            df_with_indicators = tech_indicator.calculate_all_indicators(price_df)
            signals = tech_indicator.generate_signals(df_with_indicators)
            
            # 4. 最新数据
            latest = df_with_indicators.iloc[-1]
            latest_signals = signals.iloc[-1]
            
            # 5. 近期推荐记录
            with db.engine.connect() as conn:
                sql = """SELECT * FROM recommend_result 
                        WHERE ts_code = :ts_code AND is_valid=1 
                        ORDER BY recommend_date DESC LIMIT 5"""
                result = conn.execute(text(sql), {'ts_code': ts_code})
                recommendations = [dict(row._mapping) for row in result]
                
            return {
                'basic_info': dict(basic_info._mapping),
                'latest_price': {
                    'close': float(latest['close']),
                    'change': float(latest.get('change', 0)),
                    'change_pct': float(latest.get('change_pct', 0)),
                    'volume': int(latest['vol']),
                    'amount': float(latest.get('amount', 0))
                },
                'technical_indicators': {
                    'ma5': float(latest.get('ma5', 0)),
                    'ma20': float(latest.get('ma20', 0)),
                    'ma60': float(latest.get('ma60', 0)),
                    'rsi14': float(latest.get('rsi14', 50)),
                    'macd': float(latest.get('macd', 0)),
                    'macd_signal': float(latest.get('macd_signal', 0)),
                    'k': float(latest.get('k', 50)),
                    'd': float(latest.get('d', 50)),
                    'strength_score': float(latest.get('strength_score', 50)),
                    'bb_position': float(latest.get('bb_position', 0.5))
                },
                'trading_signals': {
                    'buy_signal': int(latest_signals.get('buy_signal', 0)),
                    'sell_signal': int(latest_signals.get('sell_signal', 0)),
                    'ma_golden_cross': int(latest_signals.get('ma_golden_cross', 0)),
                    'macd_golden_cross': int(latest_signals.get('macd_golden_cross', 0)),
                    'rsi_oversold': int(latest_signals.get('rsi_oversold', 0)),
                    'rsi_overbought': int(latest_signals.get('rsi_overbought', 0))
                },
                'support_resistance': {
                    'support': float(latest.get('support', 0)),
                    'resistance': float(latest.get('resistance', 0)),
                    'sr_ratio': float(latest.get('sr_ratio', 0.5))
                },
                'recommendations': recommendations,
                'data_points': len(df_with_indicators)
            }
            
        except Exception as e:
            logger.error(f"获取{ts_code}综合数据失败: {e}")
            return {'error': str(e)}
            
    def _call_llm(self, prompt: str, context: str = None, temperature: float = 0.7) -> Dict[str, Any]:
        """调用LLM生成分析"""
        try:
            full_prompt = f"""你是一位资深股票分析师，拥有20年A股市场经验。请基于以下信息进行专业分析：

{context if context else ''}

用户问题：{prompt}

请以专业、客观的角度回答，包含以下要素：
1. 技术面分析（基于提供的技术指标）
2. 趋势判断和关键价位
3. 交易建议和风险提示
4. 简明易懂的结论

回答要求：
- 专业术语需要简单解释
- 结论明确，建议具体
- 包含风险提示
- 语言简洁有力"""

            payload = {
                "model": self.model,
                "prompt": full_prompt,
                "stream": False,
                "options": {
                    "temperature": temperature,
                    "max_tokens": 1500,
                    "top_p": 0.9
                }
            }
            
            response = requests.post(
                f"{self.api_url}/api/generate",
                json=payload,
                timeout=self.timeout
            )
            response.raise_for_status()
            
            result = response.json()
            return {
                "success": True,
                "analysis": result.get("response", ""),
                "model": self.model,
                "timestamp": datetime.now().isoformat()
            }
            
        except Exception as e:
            logger.error(f"LLM调用失败: {e}")
            return {"success": False, "error": str(e)}
            
    def analyze_single_stock(self, ts_code: str, user_query: str = "") -> Dict[str, Any]:
        """单只股票深度分析"""
        try:
            # 获取综合数据
            stock_data = self.get_comprehensive_stock_data(ts_code)
            if 'error' in stock_data:
                return stock_data
                
            # 构建分析上下文
            basic = stock_data['basic_info']
            price = stock_data['latest_price']
            tech = stock_data['technical_indicators']
            signals = stock_data['trading_signals']
            sr = stock_data['support_resistance']
            
            context = f"""
股票基本信息：
- 代码：{basic['ts_code']}
- 名称：{basic['name']}
- 行业：{basic.get('industry', '未知')}
- 交易所：{basic.get('exchange', '未知')}

最新价格数据：
- 收盘价：{price['close']:.2f}元
- 涨跌幅：{price['change_pct']:.2f}%
- 成交量：{price['volume']:,}手
- 成交额：{price['amount']:.2f}万元

技术指标分析：
- 均线系统：MA5={tech['ma5']:.2f}, MA20={tech['ma20']:.2f}, MA60={tech['ma60']:.2f}
- RSI(14)：{tech['rsi14']:.2f} {'(超买)' if tech['rsi14'] > 70 else '(超卖)' if tech['rsi14'] < 30 else '(正常)'}
- MACD：{tech['macd']:.4f}, 信号线：{tech['macd_signal']:.4f}
- KDJ：K={tech['k']:.2f}, D={tech['d']:.2f}
- 综合强弱度：{tech['strength_score']:.2f}/100
- 布林带位置：{tech['bb_position']:.2f} {'(接近上轨)' if tech['bb_position'] > 0.8 else '(接近下轨)' if tech['bb_position'] < 0.2 else '(中性位置)'}

交易信号：
- 买入信号：{'是' if signals['buy_signal'] else '否'}
- 卖出信号：{'是' if signals['sell_signal'] else '否'}
- 均线金叉：{'是' if signals['ma_golden_cross'] else '否'}
- MACD金叉：{'是' if signals['macd_golden_cross'] else '否'}
- RSI超卖：{'是' if signals['rsi_oversold'] else '否'}
- RSI超买：{'是' if signals['rsi_overbought'] else '否'}

支撑阻力位：
- 支撑位：{sr['support']:.2f}元
- 阻力位：{sr['resistance']:.2f}元
- 位置比例：{sr['sr_ratio']:.2f} {'(接近阻力位)' if sr['sr_ratio'] > 0.8 else '(接近支撑位)' if sr['sr_ratio'] < 0.2 else '(中性位置)'}

历史推荐记录：
"""
            
            for i, rec in enumerate(stock_data['recommendations'][:3]):
                context += f"- {rec['recommend_date']}: {rec['strategy']}策略, 分数{rec['score']:.3f}, {rec['reason']}\n"
                
            # 生成分析
            analysis_prompt = user_query or f"请对{basic['name']}({ts_code})进行全面的技术分析"
            result = self._call_llm(analysis_prompt, context)
            
            if result['success']:
                return {
                    'success': True,
                    'stock_code': ts_code,
                    'stock_name': basic['name'],
                    'analysis': result['analysis'],
                    'data_summary': {
                        'price': price,
                        'strength_score': tech['strength_score'],
                        'signals': signals,
                        'support_resistance': sr
                    },
                    'timestamp': result['timestamp']
                }
            else:
                return result
                
        except Exception as e:
            logger.error(f"分析{ts_code}失败: {e}")
            return {'success': False, 'error': str(e)}
            
    def market_overview_analysis(self, industry: str = None) -> Dict[str, Any]:
        """市场概览分析"""
        try:
            # 获取市场统计数据
            with db.engine.connect() as conn:
                if industry:
                    sql = """
                    SELECT COUNT(*) as total_stocks,
                           AVG(score) as avg_score,
                           COUNT(CASE WHEN score > 0.7 THEN 1 END) as strong_stocks,
                           COUNT(CASE WHEN score < 0.3 THEN 1 END) as weak_stocks
                    FROM recommend_result r
                    JOIN stock_basic b ON r.ts_code = b.ts_code
                    WHERE r.recommend_date >= DATE_SUB(CURDATE(), INTERVAL 7 DAY)
                    AND b.industry = :industry AND r.is_valid=1
                    """
                    result = conn.execute(text(sql), {'industry': industry})
                else:
                    sql = """
                    SELECT COUNT(*) as total_stocks,
                           AVG(score) as avg_score,
                           COUNT(CASE WHEN score > 0.7 THEN 1 END) as strong_stocks,
                           COUNT(CASE WHEN score < 0.3 THEN 1 END) as weak_stocks
                    FROM recommend_result
                    WHERE recommend_date >= DATE_SUB(CURDATE(), INTERVAL 7 DAY)
                    AND is_valid=1
                    """
                    result = conn.execute(text(sql))
                    
                market_stats = result.fetchone()
                
                # 获取热门股票
                sql = """
                SELECT r.ts_code, b.name, r.score, r.strategy, r.reason
                FROM recommend_result r
                JOIN stock_basic b ON r.ts_code = b.ts_code
                WHERE r.recommend_date >= DATE_SUB(CURDATE(), INTERVAL 7 DAY)
                AND r.is_valid=1
                ORDER BY r.score DESC
                LIMIT 10
                """
                result = conn.execute(text(sql))
                hot_stocks = [dict(row._mapping) for row in result]
                
            # 构建分析上下文
            context = f"""
市场整体情况：
- 分析股票总数：{market_stats[0] if market_stats[0] else 0}只
- 平均推荐分数：{market_stats[1]:.3f if market_stats[1] else 0}
- 强势股票数量：{market_stats[2] if market_stats[2] else 0}只
- 弱势股票数量：{market_stats[3] if market_stats[3] else 0}只
{"- 聚焦行业：" + industry if industry else "- 全市场分析"}

近期热门推荐股票TOP10：
"""
            
            for i, stock in enumerate(hot_stocks[:10], 1):
                context += f"{i}. {stock['name']}({stock['ts_code']}) - 分数{stock['score']:.3f} - {stock['strategy']}策略\n"
                
            analysis_prompt = f"请分析当前{'行业' if industry else '市场'}整体状况，并给出投资建议"
            result = self._call_llm(analysis_prompt, context)
            
            if result['success']:
                return {
                    'success': True,
                    'market_stats': dict(market_stats._mapping) if market_stats else {},
                    'hot_stocks': hot_stocks,
                    'analysis': result['analysis'],
                    'industry': industry,
                    'timestamp': result['timestamp']
                }
            else:
                return result
                
        except Exception as e:
            logger.error(f"市场分析失败: {e}")
            return {'success': False, 'error': str(e)}
            
    def intelligent_qa(self, user_query: str) -> Dict[str, Any]:
        """智能问答主入口"""
        try:
            # 解析用户查询
            parsed_query = self.parse_user_query(user_query)
            logger.info(f"解析查询: {parsed_query}")
            
            # 根据意图路由到不同分析
            if parsed_query['stock_codes']:
                # 单只或多只股票分析
                if len(parsed_query['stock_codes']) == 1:
                    return self.analyze_single_stock(parsed_query['stock_codes'][0], user_query)
                else:
                    # 多只股票对比分析
                    return self.compare_stocks(parsed_query['stock_codes'], user_query)
            elif parsed_query['intent'] == 'recommendation':
                # 推荐分析
                return self.get_recommendations_with_analysis(user_query)
            else:
                # 市场概览分析
                return self.market_overview_analysis()
                
        except Exception as e:
            logger.error(f"智能问答失败: {e}")
            return {'success': False, 'error': str(e)}
            
    def compare_stocks(self, stock_codes: List[str], user_query: str) -> Dict[str, Any]:
        """多只股票对比分析"""
        try:
            comparison_data = []
            for code in stock_codes[:5]:  # 最多对比5只
                data = self.get_comprehensive_stock_data(code)
                if 'error' not in data:
                    comparison_data.append(data)
                    
            if not comparison_data:
                return {'success': False, 'error': '未找到有效股票数据'}
                
            # 构建对比上下文
            context = "股票对比分析：\n\n"
            for data in comparison_data:
                basic = data['basic_info']
                price = data['latest_price']
                tech = data['technical_indicators']
                
                context += f"""
{basic['name']}({basic['ts_code']})：
- 行业：{basic.get('industry', '未知')}
- 收盘价：{price['close']:.2f}元，涨跌幅：{price['change_pct']:.2f}%
- 强弱度：{tech['strength_score']:.2f}/100
- RSI：{tech['rsi14']:.2f}
- 均线排列：MA5={tech['ma5']:.2f}, MA20={tech['ma20']:.2f}, MA60={tech['ma60']:.2f}
"""
                
            analysis_prompt = user_query or "请对以上股票进行对比分析，指出各自优劣势"
            result = self._call_llm(analysis_prompt, context)
            
            if result['success']:
                return {
                    'success': True,
                    'comparison_stocks': [data['basic_info']['ts_code'] for data in comparison_data],
                    'analysis': result['analysis'],
                    'stocks_data': comparison_data,
                    'timestamp': result['timestamp']
                }
            else:
                return result
                
        except Exception as e:
            logger.error(f"股票对比失败: {e}")
            return {'success': False, 'error': str(e)}
            
    def get_recommendations_with_analysis(self, user_query: str) -> Dict[str, Any]:
        """获取推荐股票并进行分析"""
        try:
            # 获取最新推荐
            with db.engine.connect() as conn:
                sql = """
                SELECT r.*, b.name, b.industry
                FROM recommend_result r
                JOIN stock_basic b ON r.ts_code = b.ts_code
                WHERE r.recommend_date = (SELECT MAX(recommend_date) FROM recommend_result)
                AND r.is_valid=1
                ORDER BY r.score DESC
                LIMIT 10
                """
                result = conn.execute(text(sql))
                recommendations = [dict(row._mapping) for row in result]
                
            if not recommendations:
                return {'success': False, 'error': '暂无推荐数据'}
                
            # 构建推荐分析上下文
            context = f"最新股票推荐TOP10（{recommendations[0]['recommend_date']}）：\n\n"
            
            for i, rec in enumerate(recommendations, 1):
                context += f"{i}. {rec['name']}({rec['ts_code']}) - {rec['industry']}\n"
                context += f"   推荐分数：{rec['score']:.3f}，策略：{rec['strategy']}\n"
                context += f"   推荐理由：{rec['reason']}\n\n"
                
            analysis_prompt = user_query or "请分析这些推荐股票的投资价值和风险"
            result = self._call_llm(analysis_prompt, context)
            
            if result['success']:
                return {
                    'success': True,
                    'recommendations': recommendations,
                    'analysis': result['analysis'],
                    'recommendation_date': recommendations[0]['recommend_date'],
                    'timestamp': result['timestamp']
                }
            else:
                return result
                
        except Exception as e:
            logger.error(f"推荐分析失败: {e}")
            return {'success': False, 'error': str(e)}

# 全局股票分析师实例
stock_analyzer = StockAnalyzer()

if __name__ == "__main__":
    # 测试智能分析
    test_queries = [
        "分析一下000001的走势",
        "推荐几只好股票",
        "银行股怎么样",
        "000001和000002哪个更好"
    ]
    
    for query in test_queries:
        print(f"\n用户问题：{query}")
        result = stock_analyzer.intelligent_qa(query)
        if result['success']:
            print(f"分析结果：{result.get('analysis', '无分析内容')}")
        else:
            print(f"分析失败：{result.get('error', '未知错误')}") 