#!/usr/bin/env python3
"""
大模型股票推荐生成器
调用ljwx-stock模型生成股票投资推荐并进行跟踪
"""

import json
import subprocess
import logging
import pandas as pd
import numpy as np
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Tuple
import sys
import os

# 添加项目路径
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from llm.tushare_data_extractor import TuShareDataExtractor
from .recommendation_tracker import RecommendationTracker, StockRecommendation

class ModelRecommender:
    """模型推荐生成器"""
    
    def __init__(self, model_name: str = "ljwx-stock", tushare_token: str = None):
        self.model_name = model_name
        self.logger = logging.getLogger(__name__)
        
        # 初始化数据提取器和推荐跟踪器
        self.data_extractor = TuShareDataExtractor(tushare_token)
        self.tracker = RecommendationTracker()
        
        # 推荐模板
        self.recommendation_templates = {
            'technical_analysis': """
请基于以下技术分析数据，对股票{stock_name}({stock_code})提供投资建议：

技术指标数据：
- 当前价格: {current_price}元
- 5日均线: {ma5}元  
- 20日均线: {ma20}元
- RSI(14日): {rsi}
- MACD: {macd}
- 成交量: {volume}
- 涨跌幅: {pct_change}%

请提供具体的投资建议，包括：
1. 买入/卖出/持有建议
2. 目标价格（如果有）
3. 风险控制建议
4. 预期时间周期

请给出专业、明确的建议。
            """,
            
            'comprehensive_analysis': """
请对股票{stock_name}({stock_code})进行综合分析并提供投资建议：

市场数据：
- 当前价格: {current_price}元
- 价格变动: {price_change}元 ({pct_change}%)
- 成交量: {volume}
- 市值: {market_cap}

技术指标：
- 5日均线: {ma5}元
- 20日均线: {ma20}元  
- 60日均线: {ma60}元
- RSI: {rsi}
- MACD: {macd}

请从技术分析、趋势判断、风险评估等角度，给出投资建议：
1. 明确的操作建议（买入/卖出/持有）
2. 理由分析
3. 目标价位
4. 止损建议
5. 持有期建议

要求建议具体、专业、可操作。
            """,
            
            'risk_assessment': """
请对股票{stock_name}({stock_code})进行风险评估并提供投资建议：

风险相关数据：
- 当前价格: {current_price}元
- 近期波动率: {volatility}%
- 最大回撤: {max_drawdown}%
- 成交量变化: {volume_change}%
- 行业表现: {industry_performance}

请从风险控制角度分析：
1. 当前投资风险等级
2. 适合的仓位配置
3. 具体操作建议
4. 风险控制措施
5. 适合的投资期限

请给出风险意识较强的专业建议。
            """
        }
    
    def generate_stock_recommendations(self, stock_codes: List[str], 
                                     analysis_type: str = 'comprehensive_analysis',
                                     max_stocks: int = 10) -> List[str]:
        """批量生成股票推荐"""
        recommendations = []
        
        # 限制处理数量
        stock_codes = stock_codes[:max_stocks]
        
        self.logger.info(f"开始生成 {len(stock_codes)} 只股票的推荐")
        
        for i, stock_code in enumerate(stock_codes):
            try:
                self.logger.info(f"处理股票 {i+1}/{len(stock_codes)}: {stock_code}")
                
                # 获取股票数据
                stock_data = self._get_stock_analysis_data(stock_code)
                if not stock_data:
                    self.logger.warning(f"无法获取股票数据: {stock_code}")
                    continue
                
                # 生成推荐
                rec_id = self._generate_single_recommendation(stock_code, stock_data, analysis_type)
                
                if rec_id:
                    recommendations.append(rec_id)
                    self.logger.info(f"生成推荐成功: {stock_code} -> {rec_id}")
                else:
                    self.logger.warning(f"生成推荐失败: {stock_code}")
                    
            except Exception as e:
                self.logger.error(f"处理股票失败 {stock_code}: {e}")
                continue
        
        self.logger.info(f"批量推荐完成: 成功生成 {len(recommendations)} 个推荐")
        
        return recommendations
    
    def generate_recommendations(self, strategy_id: str = None, 
                               num_recommendations: int = 10,
                               recommendation_type: str = "general",
                               user_id: str = None) -> List[Dict]:
        """为策略训练系统生成推荐 - 兼容接口"""
        try:
            self.logger.info(f"生成推荐: strategy_id={strategy_id}, type={recommendation_type}, num={num_recommendations}")
            
            # 获取活跃股票列表
            active_stocks = self._get_active_stocks(num_recommendations * 2)
            
            if not active_stocks:
                self.logger.warning("无法获取活跃股票列表")
                return []
            
            # 生成推荐
            recommendation_ids = self.generate_stock_recommendations(
                active_stocks[:num_recommendations], 
                'comprehensive_analysis'
            )
            
            # 转换为兼容格式
            recommendations = []
            for rec_id in recommendation_ids:
                try:
                    # 从数据库获取推荐详情
                    rec = self.tracker.get_recommendation(rec_id)
                    if rec:
                        recommendations.append({
                            "id": rec_id,
                            "stock_code": rec.stock_code,
                            "stock_name": getattr(rec, 'stock_name', rec.stock_code),
                            "recommendation": rec.recommendation_type,
                            "confidence": rec.confidence_score,
                            "target_price": getattr(rec, 'target_price', 0),
                            "current_price": getattr(rec, 'current_price', 0),
                            "analysis": rec.analysis_result[:200] if rec.analysis_result else "",
                            "strategy_id": strategy_id,
                            "recommendation_type": recommendation_type,
                            "user_id": user_id,
                            "urgency": 0.5,  # 默认紧急度
                            "generated_at": datetime.now().isoformat()
                        })
                except Exception as e:
                    self.logger.error(f"转换推荐格式失败 {rec_id}: {e}")
                    continue
            
            self.logger.info(f"成功生成 {len(recommendations)} 个推荐")
            return recommendations
            
        except Exception as e:
            self.logger.error(f"生成推荐失败: {e}")
            return []
    
    def generate_daily_recommendations(self, num_stocks: int = 5) -> List[str]:
        """生成智能每日推荐"""
        try:
            self.logger.info(f"🎯 开始生成 {num_stocks} 个智能推荐")
            
            # 获取候选股票池 (更多候选用于排名)
            candidate_stocks = self._get_active_stocks(num_stocks * 4)  
            
            if not candidate_stocks:
                self.logger.warning("无法获取候选股票列表")
                return []
            
            # 智能筛选和排名
            ranked_candidates = self._rank_recommendation_candidates(candidate_stocks)
            
            if not ranked_candidates:
                self.logger.warning("无合适的推荐候选")
                return []
            
            # 策略多样化生成推荐
            recommendations = self._generate_diversified_recommendations(
                ranked_candidates[:num_stocks * 2], num_stocks
            )
            
            self.logger.info(f"✅ 成功生成 {len(recommendations)} 个多样化推荐")
            return recommendations
            
        except Exception as e:
            self.logger.error(f"生成每日推荐失败: {e}")
            return []
    
    def _rank_recommendation_candidates(self, stock_codes: List[str]) -> List[Dict]:
        """智能排名候选股票"""
        ranked_candidates = []
        
        for stock_code in stock_codes:
            try:
                # 获取股票分析数据
                stock_data = self._get_stock_analysis_data(stock_code)
                if not stock_data:
                    continue
                
                # 计算推荐得分
                score = self._calculate_recommendation_score(stock_data)
                
                ranked_candidates.append({
                    'stock_code': stock_code,
                    'stock_name': stock_data.get('stock_name', stock_code),
                    'score': score,
                    'data': stock_data
                })
                
            except Exception as e:
                self.logger.error(f"评估股票失败 {stock_code}: {e}")
                continue
        
        # 按得分排序
        ranked_candidates.sort(key=lambda x: x['score'], reverse=True)
        
        self.logger.info(f"📊 候选股票排名完成: {len(ranked_candidates)} 只股票")
        if ranked_candidates:
            top5_info = [(c['stock_name'], f"{c['score']:.2f}") for c in ranked_candidates[:5]]
            self.logger.info(f"🏆 前5名: {top5_info}")
        
        return ranked_candidates
    
    def _calculate_recommendation_score(self, stock_data: Dict) -> float:
        """计算推荐得分 (0-100分)"""
        score = 50.0  # 基础分数
        
        try:
            # 技术指标得分 (30分)
            rsi = stock_data.get('rsi', 50)
            if 30 <= rsi <= 70:  # RSI在合理区间
                score += 10
            elif rsi < 30:  # 超卖，有反弹机会
                score += 15
            elif rsi > 70:  # 超买，减分
                score -= 5
            
            # MACD得分
            macd = stock_data.get('macd', 0)
            if macd > 0:
                score += 8
            elif macd > -0.1:
                score += 3
            
            # 均线得分 (20分)
            current_price = stock_data.get('current_price', 0)
            ma5 = stock_data.get('ma5', 0)
            ma20 = stock_data.get('ma20', 0)
            
            if current_price > ma5 > ma20:  # 多头排列
                score += 20
            elif current_price > ma5:  # 短期趋势向上
                score += 10
            elif current_price < ma5 < ma20:  # 空头排列
                score -= 10
            
            # 成交量得分 (15分)
            volume_ratio = stock_data.get('volume_ratio', 1)
            if 1.2 <= volume_ratio <= 3:  # 成交量适中放大
                score += 15
            elif volume_ratio > 3:  # 异常放量
                score += 5
            elif volume_ratio < 0.8:  # 成交量萎缩
                score -= 5
            
            # 波动率得分 (15分) 
            volatility = stock_data.get('volatility', 0)
            if 15 <= volatility <= 35:  # 适中波动
                score += 15
            elif volatility > 50:  # 过度波动
                score -= 10
            
            # 价格合理性得分 (10分)
            if 5 <= current_price <= 100:  # 价格在合理区间
                score += 10
            elif current_price > 100:
                score += 5
            
            # 最大回撤得分 (10分)
            max_drawdown = stock_data.get('max_drawdown', 0)
            if max_drawdown < 10:  # 回撤较小
                score += 10
            elif max_drawdown < 20:
                score += 5
            elif max_drawdown > 30:  # 回撤过大
                score -= 5
            
        except Exception as e:
            self.logger.error(f"计算得分失败: {e}")
        
        return max(0, min(100, score))  # 限制在0-100分之间
    
    def _generate_diversified_recommendations(self, candidates: List[Dict], num_stocks: int) -> List[str]:
        """生成策略多样化的推荐"""
        recommendations = []
        
        # 策略类型分配
        strategy_types = [
            'comprehensive_analysis',  # 综合分析
            'technical_analysis',      # 技术分析  
            'risk_assessment'          # 风险评估
        ]
        
        try:
            # 为每个候选股票分配不同策略
            for i, candidate in enumerate(candidates):
                if len(recommendations) >= num_stocks:
                    break
                
                # 循环使用不同策略类型
                strategy_type = strategy_types[i % len(strategy_types)]
                
                # 生成推荐
                rec_id = self._generate_single_recommendation(
                    candidate['stock_code'], 
                    candidate['data'], 
                    strategy_type
                )
                
                if rec_id:
                    recommendations.append(rec_id)
                    self.logger.info(f"✅ 生成推荐: {candidate['stock_name']} ({strategy_type}, 得分{candidate['score']:.1f})")
            
            return recommendations
            
        except Exception as e:
            self.logger.error(f"生成多样化推荐失败: {e}")
            return recommendations
    
    def validate_pending_recommendations(self) -> Dict[str, int]:
        """验证待处理的推荐"""
        try:
            # 验证所有模型的推荐
            total_validated = self.tracker.validate_recommendations()
            
            # 验证当前模型的推荐
            current_model_validated = self.tracker.validate_recommendations(self.model_name)
            
            return {
                'total_validated': total_validated,
                'current_model_validated': current_model_validated
            }
            
        except Exception as e:
            self.logger.error(f"验证推荐失败: {e}")
            return {'total_validated': 0, 'current_model_validated': 0}
    
    def get_model_performance_summary(self, days: int = 30) -> Dict:
        """获取模型性能摘要"""
        try:
            # 计算日期范围
            end_date = datetime.now().strftime('%Y-%m-%d')
            start_date = (datetime.now() - timedelta(days=days)).strftime('%Y-%m-%d')
            
            # 生成回测报告
            metrics = self.tracker.generate_backtest_report(
                self.model_name, start_date, end_date
            )
            
            return {
                'model_name': metrics.model_name,
                'period': f"{start_date} 至 {end_date}",
                'total_recommendations': metrics.total_recommendations,
                'validated_recommendations': metrics.validated_recommendations,
                'hit_rate': f"{metrics.hit_rate:.2%}",
                'avg_return': f"{metrics.avg_return:.2%}",
                'sharpe_ratio': f"{metrics.sharpe_ratio:.2f}",
                'max_drawdown': f"{metrics.max_drawdown:.2%}",
                'buy_hit_rate': f"{metrics.buy_hit_rate:.2%}",
                'sell_hit_rate': f"{metrics.sell_hit_rate:.2%}",
                'hold_hit_rate': f"{metrics.hold_hit_rate:.2%}"
            }
            
        except Exception as e:
            self.logger.error(f"获取性能摘要失败: {e}")
            return {}
    
    def _generate_single_recommendation(self, stock_code: str, stock_data: Dict, 
                                      analysis_type: str) -> Optional[str]:
        """生成单个股票推荐"""
        try:
            # 构建推荐请求
            template = self.recommendation_templates.get(analysis_type, 
                                                        self.recommendation_templates['comprehensive_analysis'])
            
            prompt = template.format(**stock_data)
            
            # 调用模型生成推荐
            recommendation_text = self._call_model(prompt)
            
            if not recommendation_text:
                return None
            
            # 添加到跟踪系统
            rec_id = self.tracker.add_recommendation(
                model_name=self.model_name,
                stock_code=stock_code,
                stock_name=stock_data['stock_name'],
                recommendation_text=recommendation_text,
                current_price=stock_data['current_price'],
                current_volume=stock_data.get('volume', 0),
                strategy_type=analysis_type
            )
            
            return rec_id
            
        except Exception as e:
            self.logger.error(f"生成推荐失败 {stock_code}: {e}")
            return None
    
    def _call_model(self, prompt: str, timeout: int = 60) -> Optional[str]:
        """调用Ollama模型生成回答"""
        try:
            result = subprocess.run(
                ['ollama', 'run', self.model_name],
                input=prompt,
                capture_output=True,
                text=True,
                timeout=timeout,
                encoding='utf-8'
            )
            
            if result.returncode == 0:
                return result.stdout.strip()
            else:
                self.logger.error(f"模型调用失败: {result.stderr}")
                return None
                
        except subprocess.TimeoutExpired:
            self.logger.error("模型调用超时")
            return None
        except Exception as e:
            self.logger.error(f"模型调用异常: {e}")
            return None
    
    def _get_stock_analysis_data(self, stock_code: str) -> Optional[Dict]:
        """获取股票分析所需数据"""
        try:
            # 获取基础数据
            end_date = datetime.now().strftime('%Y%m%d')
            start_date = (datetime.now() - timedelta(days=90)).strftime('%Y%m%d')
            
            # 获取日线数据
            daily_data = self.data_extractor.get_stock_daily_data(stock_code, start_date, end_date)
            
            if daily_data.empty:
                return None
            
            # 获取最新数据
            latest = daily_data.iloc[-1]
            
            # 计算技术指标
            daily_data['ma5'] = daily_data['close'].rolling(window=5).mean()
            daily_data['ma20'] = daily_data['close'].rolling(window=20).mean()
            daily_data['ma60'] = daily_data['close'].rolling(window=60).mean()
            
            # 计算RSI
            delta = daily_data['close'].diff()
            gain = (delta.where(delta > 0, 0)).rolling(window=14).mean()
            loss = (-delta.where(delta < 0, 0)).rolling(window=14).mean()
            rs = gain / loss
            daily_data['rsi'] = 100 - (100 / (1 + rs))
            
            # 计算MACD
            ema12 = daily_data['close'].ewm(span=12).mean()
            ema26 = daily_data['close'].ewm(span=26).mean()
            daily_data['macd'] = ema12 - ema26
            
            # 获取股票基本信息
            stock_info = self._get_stock_info(stock_code)
            stock_name = stock_info.get('name', stock_code) if stock_info else stock_code
            
            # 计算波动率
            returns = daily_data['close'].pct_change().dropna()
            volatility = returns.std() * np.sqrt(252) * 100  # 年化波动率
            
            # 计算最大回撤
            cumulative = (1 + returns).cumprod()
            running_max = cumulative.expanding().max()
            drawdown = (cumulative - running_max) / running_max
            max_drawdown = abs(drawdown.min() * 100)
            
            # 构建分析数据
            analysis_data = {
                'stock_code': stock_code,
                'stock_name': stock_name,
                'current_price': float(latest['close']),
                'price_change': float(latest['close'] - daily_data.iloc[-2]['close']),
                'pct_change': float(latest['pct_chg']),
                'volume': float(latest['vol']),
                'ma5': float(daily_data.iloc[-1]['ma5']) if not pd.isna(daily_data.iloc[-1]['ma5']) else latest['close'],
                'ma20': float(daily_data.iloc[-1]['ma20']) if not pd.isna(daily_data.iloc[-1]['ma20']) else latest['close'],
                'ma60': float(daily_data.iloc[-1]['ma60']) if not pd.isna(daily_data.iloc[-1]['ma60']) else latest['close'],
                'rsi': float(daily_data.iloc[-1]['rsi']) if not pd.isna(daily_data.iloc[-1]['rsi']) else 50,
                'macd': float(daily_data.iloc[-1]['macd']) if not pd.isna(daily_data.iloc[-1]['macd']) else 0,
                'volatility': float(volatility),
                'max_drawdown': float(max_drawdown),
                'volume_change': float((latest['vol'] / daily_data.iloc[-2]['vol'] - 1) * 100) if daily_data.iloc[-2]['vol'] > 0 else 0,
                'market_cap': float(latest.get('total_mv', 0) * 10000) if 'total_mv' in latest else 0,  # 转换为元
                'industry_performance': 0  # 暂时设为0，可以后续添加行业数据
            }
            
            return analysis_data
            
        except Exception as e:
            self.logger.error(f"获取股票分析数据失败 {stock_code}: {e}")
            return None
    
    def _get_stock_info(self, stock_code: str) -> Optional[Dict]:
        """获取股票基本信息"""
        try:
            stocks_df = self.data_extractor.get_stock_list()
            if not stocks_df.empty:
                stock_info = stocks_df[stocks_df['ts_code'] == stock_code]
                if not stock_info.empty:
                    return {
                        'name': stock_info.iloc[0]['name'],
                        'industry': stock_info.iloc[0].get('industry', ''),
                        'market': stock_info.iloc[0].get('market', '')
                    }
            return None
        except Exception as e:
            self.logger.error(f"获取股票基本信息失败 {stock_code}: {e}")
            return None
    
    def _get_active_stocks(self, limit: int = 20) -> List[str]:
        """获取多样化活跃股票列表"""
        try:
            # 获取更多股票用于筛选
            all_stocks_df = self.data_extractor.get_stock_list(limit=1000)
            
            if all_stocks_df.empty:
                return []
            
            # 过滤掉ST股票和退市股票
            filtered_stocks = all_stocks_df[
                (~all_stocks_df['name'].str.contains('ST', na=False)) &
                (~all_stocks_df['name'].str.contains('退', na=False)) &
                (~all_stocks_df['name'].str.contains('*', na=False))
            ]
            
            # 多样化选择策略
            diversified_stocks = self._select_diversified_stocks(filtered_stocks, limit)
            
            return diversified_stocks
            
        except Exception as e:
            self.logger.error(f"获取活跃股票失败: {e}")
            # 回退到混合股票池
            return self._get_fallback_stock_pool(limit)
    
    def _select_diversified_stocks(self, stocks_df: pd.DataFrame, limit: int) -> List[str]:
        """选择多样化的股票组合"""
        selected_stocks = []
        
        try:
            # 1. 按市场分类选择 (40% 深市, 40% 沪市, 20% 创业板/科创板)
            sz_stocks = stocks_df[stocks_df['ts_code'].str.contains('.SZ')]['ts_code'].tolist()
            sh_stocks = stocks_df[stocks_df['ts_code'].str.contains('.SH')]['ts_code'].tolist()
            
            # 深市股票 (000, 002开头)
            sz_main = [s for s in sz_stocks if s.startswith('000') or s.startswith('002')]
            # 创业板 (300开头)
            cy_stocks = [s for s in sz_stocks if s.startswith('300')]
            # 科创板 (688开头)
            kc_stocks = [s for s in sh_stocks if s.startswith('688')]
            
            # 按比例分配
            sz_count = int(limit * 0.3)  # 30% 深市主板
            sh_count = int(limit * 0.4)  # 40% 沪市
            cy_count = int(limit * 0.2)  # 20% 创业板
            kc_count = limit - sz_count - sh_count - cy_count  # 剩余科创板
            
            # 随机选择避免总是相同股票
            import random
            random.seed()  # 使用当前时间作为随机种子
            
            selected_stocks.extend(random.sample(sz_main[:100], min(sz_count, len(sz_main))))
            selected_stocks.extend(random.sample(sh_stocks[:100], min(sh_count, len(sh_stocks))))
            selected_stocks.extend(random.sample(cy_stocks[:50], min(cy_count, len(cy_stocks))))
            selected_stocks.extend(random.sample(kc_stocks[:30], min(kc_count, len(kc_stocks))))
            
            # 如果不够，从所有股票中补充
            if len(selected_stocks) < limit:
                remaining = limit - len(selected_stocks)
                all_available = [s for s in stocks_df['ts_code'].tolist() if s not in selected_stocks]
                selected_stocks.extend(random.sample(all_available[:200], min(remaining, len(all_available))))
            
            self.logger.info(f"✅ 选择多样化股票: 深市{sz_count}只, 沪市{sh_count}只, 创业板{cy_count}只, 科创板{kc_count}只")
            return selected_stocks[:limit]
            
        except Exception as e:
            self.logger.error(f"多样化选择失败: {e}")
            # 简单随机选择作为回退
            import random
            all_codes = stocks_df['ts_code'].tolist()
            random.shuffle(all_codes)
            return all_codes[:limit]
    
    def _get_fallback_stock_pool(self, limit: int) -> List[str]:
        """回退股票池 - 包含各个板块的代表性股票"""
        fallback_pool = [
            # 沪市主板
            '600000.SH', '600036.SH', '600519.SH', '600887.SH', '601318.SH',
            '601398.SH', '601857.SH', '601988.SH', '600276.SH', '600690.SH',
            # 深市主板  
            '000001.SZ', '000002.SZ', '000858.SZ', '000725.SZ', '000776.SZ',
            '002415.SZ', '002714.SZ', '002304.SZ', '002475.SZ', '002352.SZ',
            # 创业板
            '300015.SZ', '300059.SZ', '300124.SZ', '300408.SZ', '300750.SZ',
            # 科创板
            '688111.SH', '688036.SH', '688599.SH', '688981.SH', '688187.SH'
        ]
        
        import random
        random.shuffle(fallback_pool)
        return fallback_pool[:limit]
    
    def _filter_recommendation_candidates(self, stock_codes: List[str]) -> List[str]:
        """筛选适合推荐的股票候选"""
        candidates = []
        
        for stock_code in stock_codes:
            try:
                # 获取近期数据进行筛选
                end_date = datetime.now().strftime('%Y%m%d')
                start_date = (datetime.now() - timedelta(days=30)).strftime('%Y%m%d')
                
                daily_data = self.data_extractor.get_stock_daily_data(stock_code, start_date, end_date)
                
                if daily_data.empty or len(daily_data) < 10:
                    continue
                
                # 筛选条件
                latest = daily_data.iloc[-1]
                
                # 1. 价格合理（5-200元）
                if not (5 <= latest['close'] <= 200):
                    continue
                
                # 2. 有一定成交量
                if latest['vol'] < 1000:  # 成交量过小
                    continue
                
                # 3. 近期有一定波动（排除僵尸股）
                price_std = daily_data['close'].tail(10).std()
                if price_std < 0.1:  # 波动太小
                    continue
                
                candidates.append(stock_code)
                
                if len(candidates) >= len(stock_codes):
                    break
                    
            except Exception as e:
                self.logger.error(f"筛选股票失败 {stock_code}: {e}")
                continue
        
        return candidates

def main():
    """主函数 - 演示用法"""
    logging.basicConfig(level=logging.INFO)
    
    # 创建推荐生成器
    recommender = ModelRecommender(
        model_name="ljwx-stock", 
        tushare_token="58cf834df2a4b9a5404f6416248cffa0da78d10e31496385251d7aef"
    )
    
    print("=== ljwx-stock 推荐系统演示 ===")
    
    # 1. 生成每日推荐
    print("\n1. 生成每日推荐...")
    daily_recommendations = recommender.generate_daily_recommendations(num_stocks=3)
    print(f"生成推荐数量: {len(daily_recommendations)}")
    
    for i, rec_id in enumerate(daily_recommendations[:3]):
        rec = recommender.tracker.get_recommendation_by_id(rec_id)
        if rec:
            print(f"\n推荐 {i+1}:")
            print(f"股票: {rec.stock_name}({rec.stock_code})")
            print(f"类型: {rec.recommendation_type}")
            print(f"置信度: {rec.confidence_score:.2f}")
            print(f"推荐内容: {rec.recommendation_text[:100]}...")
    
    # 2. 验证推荐
    print("\n2. 验证待处理推荐...")
    validation_result = recommender.validate_pending_recommendations()
    print(f"验证结果: {validation_result}")
    
    # 3. 获取性能摘要
    print("\n3. 模型性能摘要...")
    performance = recommender.get_model_performance_summary(days=30)
    if performance:
        print(f"模型: {performance['model_name']}")
        print(f"期间: {performance['period']}")
        print(f"总推荐数: {performance['total_recommendations']}")
        print(f"已验证数: {performance['validated_recommendations']}")
        print(f"命中率: {performance['hit_rate']}")
        print(f"平均收益: {performance['avg_return']}")
    
    print("\n=== 演示完成 ===")

if __name__ == "__main__":
    main()