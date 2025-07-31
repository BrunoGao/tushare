"""
Ollama集成模块 - 基于灵境万象70B模型的股票分析
"""
import asyncio
import json
import logging
from typing import Dict, List, Optional, Any
from datetime import datetime, timedelta
import pandas as pd
import numpy as np
from dataclasses import dataclass
import ollama
import httpx
from pydantic import BaseModel, Field

# 日志配置
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

@dataclass
class StockData:
    """股票数据结构"""
    ts_code: str
    name: str
    close: float
    pct_chg: float
    volume: float
    turnover: float
    pe: Optional[float] = None
    pb: Optional[float] = None
    ma5: Optional[float] = None
    ma20: Optional[float] = None
    rsi: Optional[float] = None
    macd: Optional[float] = None

class StockAnalysisRequest(BaseModel):
    """股票分析请求"""
    stock_data: Dict[str, Any]
    analysis_type: str = Field(default="comprehensive", description="分析类型")
    time_horizon: str = Field(default="short", description="时间周期: short/medium/long")
    risk_tolerance: str = Field(default="medium", description="风险偏好: low/medium/high")

class StockAnalysisResponse(BaseModel):
    """股票分析响应"""
    ts_code: str
    recommendation: str = Field(description="推荐动作: buy/sell/hold")
    confidence: float = Field(description="置信度 0-1")
    target_price: Optional[float] = Field(description="目标价格")
    stop_loss: Optional[float] = Field(description="止损价格")
    reasoning: str = Field(description="分析理由")
    risk_level: str = Field(description="风险等级")
    key_factors: List[str] = Field(description="关键因素")
    technical_signals: Dict[str, Any] = Field(description="技术信号")

class OllamaStockAnalyzer:
    """Ollama股票分析器"""
    
    def __init__(self, model_name: str = "lingjingwanxiang:70b", host: str = "http://localhost:11434"):
        self.model_name = model_name
        self.host = host
        self.client = ollama.Client(host=host)
        self.logger = logging.getLogger(__name__)
        
    async def check_model_availability(self) -> bool:
        """检查模型是否可用"""
        try:
            models = self.client.list()
            # 处理不同的返回格式
            if 'models' in models:
                available_models = [model.get('name', model.get('model', '')) for model in models['models']]
            else:
                available_models = [model.get('name', model.get('model', '')) for model in models]
            
            is_available = self.model_name in available_models
            
            if not is_available:
                self.logger.warning(f"模型 {self.model_name} 不可用，可用模型: {available_models}")
                # 尝试拉取模型
                try:
                    self.logger.info(f"尝试拉取模型 {self.model_name}...")
                    self.client.pull(self.model_name)
                    return True
                except Exception as pull_error:
                    self.logger.error(f"模型拉取失败: {pull_error}")
                    return False
            
            return True
        except Exception as e:
            self.logger.error(f"检查模型可用性失败: {e}")
            return False
    
    def _build_stock_prompt(self, stock_data: StockData, analysis_type: str = "comprehensive") -> str:
        """构建股票分析提示词"""
        
        prompt = f"""
你是一位专业的股票分析师，请基于以下数据对股票进行深度分析：

【股票基本信息】
股票代码: {stock_data.ts_code}
股票名称: {stock_data.name}
当前价格: {stock_data.close:.2f}元
涨跌幅: {stock_data.pct_chg:.2f}%
成交量: {stock_data.volume/10000:.0f}万手
成交额: {stock_data.turnover/100000000:.2f}亿元

【技术指标】
5日均线: {stock_data.ma5:.2f}元 (当前价格{'上' if stock_data.close > stock_data.ma5 else '下'}方)
20日均线: {stock_data.ma20:.2f}元 (当前价格{'上' if stock_data.close > stock_data.ma20 else '下'}方)
RSI指标: {stock_data.rsi:.1f} ({'超买' if stock_data.rsi > 70 else '超卖' if stock_data.rsi < 30 else '正常'})
MACD: {stock_data.macd:.4f}

【估值指标】
市盈率PE: {stock_data.pe if stock_data.pe else '无数据'}
市净率PB: {stock_data.pb if stock_data.pb else '无数据'}

请从以下几个维度进行分析，并给出明确的投资建议：

1. **技术面分析**
   - 价格趋势判断（上升/下降/横盘）
   - 关键技术指标信号解读
   - 支撑位和阻力位预测

2. **量价关系分析**
   - 成交量与价格的配合关系
   - 资金流入流出判断

3. **风险评估**
   - 技术风险点识别
   - 市场系统性风险

4. **投资建议**
   - 明确的操作建议：买入/卖出/持有
   - 建议仓位比例
   - 目标价位和止损位
   - 预期持有周期

请用JSON格式返回分析结果：
{{
    "recommendation": "买入/卖出/持有",
    "confidence": 0.85,
    "target_price": 目标价格,
    "stop_loss": 止损价格,
    "reasoning": "详细分析理由",
    "risk_level": "低/中/高",
    "key_factors": ["关键因素1", "关键因素2"],
    "technical_signals": {{
        "trend": "上升/下降/横盘",
        "ma_signal": "多头/空头/交叉",
        "rsi_signal": "超买/超卖/正常",
        "volume_signal": "放量/缩量/正常"
    }}
}}
"""
        return prompt
    
    async def analyze_single_stock(self, stock_data: StockData, analysis_type: str = "comprehensive") -> StockAnalysisResponse:
        """分析单只股票"""
        try:
            # 检查模型可用性
            if not await self.check_model_availability():
                raise Exception(f"模型 {self.model_name} 不可用")
            
            # 构建提示词
            prompt = self._build_stock_prompt(stock_data, analysis_type)
            
            # 调用Ollama模型
            response = self.client.chat(
                model=self.model_name,
                messages=[
                    {
                        'role': 'user',
                        'content': prompt
                    }
                ],
                options={
                    'temperature': 0.3,  # 降低随机性，提高一致性
                    'top_p': 0.9,
                    'top_k': 40
                }
            )
            
            # 解析响应
            content = response['message']['content']
            
            # 尝试提取JSON部分
            try:
                # 查找JSON开始和结束位置
                json_start = content.find('{')
                json_end = content.rfind('}') + 1
                
                if json_start != -1 and json_end > json_start:
                    json_str = content[json_start:json_end]
                    analysis_data = json.loads(json_str)
                    
                    return StockAnalysisResponse(
                        ts_code=stock_data.ts_code,
                        **analysis_data
                    )
                else:
                    # 如果没有找到JSON，使用文本解析
                    return self._parse_text_response(stock_data.ts_code, content)
            
            except json.JSONDecodeError:
                # JSON解析失败，使用文本解析
                return self._parse_text_response(stock_data.ts_code, content)
                
        except Exception as e:
            self.logger.error(f"股票分析失败 {stock_data.ts_code}: {e}")
            # 返回默认分析结果
            return StockAnalysisResponse(
                ts_code=stock_data.ts_code,
                recommendation="hold",
                confidence=0.5,
                target_price=None,
                stop_loss=None,
                reasoning=f"分析失败: {str(e)}",
                risk_level="medium",
                key_factors=["数据不足"],
                technical_signals={}
            )
    
    def _parse_text_response(self, ts_code: str, content: str) -> StockAnalysisResponse:
        """解析文本响应"""
        # 简单的文本解析逻辑
        recommendation = "hold"
        confidence = 0.6
        
        # 关键词匹配
        if any(word in content.lower() for word in ['买入', 'buy', '建议购买', '积极']):
            recommendation = "buy"
            confidence = 0.7
        elif any(word in content.lower() for word in ['卖出', 'sell', '建议卖出', '减仓']):
            recommendation = "sell"
            confidence = 0.7
        
        # 风险等级判断
        risk_level = "medium"
        if any(word in content.lower() for word in ['高风险', '谨慎', '风险较大']):
            risk_level = "high"
        elif any(word in content.lower() for word in ['低风险', '稳健', '安全']):
            risk_level = "low"
        
        return StockAnalysisResponse(
            ts_code=ts_code,
            recommendation=recommendation,
            confidence=confidence,
            target_price=None,
            stop_loss=None,
            reasoning=content[:500],  # 截取前500字符
            risk_level=risk_level,
            key_factors=["基于文本解析"],
            technical_signals={}
        )
    
    async def batch_analyze_stocks(self, stocks_data: List[StockData], max_concurrent: int = 5) -> List[StockAnalysisResponse]:
        """批量分析股票"""
        semaphore = asyncio.Semaphore(max_concurrent)
        
        async def analyze_with_semaphore(stock_data):
            async with semaphore:
                return await self.analyze_single_stock(stock_data)
        
        tasks = [analyze_with_semaphore(stock) for stock in stocks_data]
        results = await asyncio.gather(*tasks, return_exceptions=True)
        
        # 过滤异常结果
        valid_results = []
        for result in results:
            if isinstance(result, StockAnalysisResponse):
                valid_results.append(result)
            else:
                self.logger.error(f"批量分析中出现异常: {result}")
        
        return valid_results
    
    def generate_portfolio_recommendations(self, analysis_results: List[StockAnalysisResponse], 
                                         max_positions: int = 10) -> Dict[str, Any]:
        """生成投资组合推荐"""
        
        # 按置信度和推荐强度排序
        buy_recommendations = [r for r in analysis_results if r.recommendation == "buy"]
        buy_recommendations.sort(key=lambda x: x.confidence, reverse=True)
        
        hold_recommendations = [r for r in analysis_results if r.recommendation == "hold"]
        sell_recommendations = [r for r in analysis_results if r.recommendation == "sell"]
        
        # 风险分散
        high_confidence_buys = [r for r in buy_recommendations if r.confidence > 0.7]
        
        # 构建推荐组合
        portfolio = {
            "recommended_buys": high_confidence_buys[:max_positions],
            "holds": hold_recommendations,
            "sells": sell_recommendations,
            "portfolio_summary": {
                "total_analyzed": len(analysis_results),
                "buy_signals": len(buy_recommendations),
                "hold_signals": len(hold_recommendations),
                "sell_signals": len(sell_recommendations),
                "high_confidence_buys": len(high_confidence_buys),
                "avg_confidence": sum(r.confidence for r in analysis_results) / len(analysis_results) if analysis_results else 0
            }
        }
        
        return portfolio
    
    async def health_check(self) -> Dict[str, Any]:
        """健康检查"""
        try:
            # 检查Ollama服务是否可用
            async with httpx.AsyncClient() as client:
                response = await client.get(f"{self.host}/api/tags", timeout=5.0)
                if response.status_code == 200:
                    models = response.json()
                    model_available = any(model['name'] == self.model_name for model in models.get('models', []))
                    
                    return {
                        "status": "healthy" if model_available else "warning",
                        "ollama_available": True,
                        "model_available": model_available,
                        "model_name": self.model_name,
                        "host": self.host,
                        "available_models": [model['name'] for model in models.get('models', [])]
                    }
                else:
                    return {
                        "status": "error",
                        "ollama_available": False,
                        "error": f"HTTP {response.status_code}"
                    }
        except Exception as e:
            return {
                "status": "error",
                "ollama_available": False,
                "error": str(e)
            }

# 全局实例
ollama_analyzer = OllamaStockAnalyzer()

if __name__ == "__main__":
    # 测试代码
    async def test_analyzer():
        # 模拟股票数据
        test_stock = StockData(
            ts_code="000001.SZ",
            name="平安银行",
            close=12.50,
            pct_chg=2.35,
            volume=125000000,
            turnover=1567000000,
            pe=5.8,
            pb=0.65,
            ma5=12.20,
            ma20=11.80,
            rsi=65.2,
            macd=0.058
        )
        
        analyzer = OllamaStockAnalyzer()
        
        # 健康检查
        health = await analyzer.health_check()
        print("Health Check:", health)
        
        if health["status"] == "healthy":
            # 分析股票
            result = await analyzer.analyze_single_stock(test_stock)
            print("Analysis Result:", result.model_dump())
    
    # 运行测试
    asyncio.run(test_analyzer())