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
from recommendation_tracker import RecommendationTracker, StockRecommendation

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
    
    def generate_daily_recommendations(self, num_stocks: int = 5) -> List[str]:
        """生成每日推荐"""
        try:
            # 获取活跃股票列表
            active_stocks = self._get_active_stocks(num_stocks * 2)  # 获取更多候选
            
            if not active_stocks:
                self.logger.warning("无法获取活跃股票列表")
                return []
            
            # 筛选适合推荐的股票
            candidate_stocks = self._filter_recommendation_candidates(active_stocks)
            
            # 生成推荐
            recommendations = self.generate_stock_recommendations(
                candidate_stocks[:num_stocks], 
                'comprehensive_analysis'
            )
            
            return recommendations
            
        except Exception as e:
            self.logger.error(f"生成每日推荐失败: {e}")
            return []
    
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
                current_volume=stock_data.get('volume', 0)
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
        """获取活跃股票列表"""
        try:
            # 获取股票列表
            stocks_df = self.data_extractor.get_stock_list(limit=limit * 2)
            
            if stocks_df.empty:
                return []
            
            # 过滤掉ST股票和退市股票
            active_stocks = stocks_df[
                (~stocks_df['name'].str.contains('ST', na=False)) &
                (~stocks_df['name'].str.contains('退', na=False))
            ]['ts_code'].tolist()
            
            return active_stocks[:limit]
            
        except Exception as e:
            self.logger.error(f"获取活跃股票失败: {e}")
            return []
    
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