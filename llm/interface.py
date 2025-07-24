import requests
import json
import logging
from typing import Dict, List, Any, Optional
from datetime import datetime
import sys, os
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import config
from utils.db_helper import db
from sqlalchemy import text

logging.basicConfig(level=getattr(logging, config.LOG_LEVEL), format=config.LOG_FORMAT)
logger = logging.getLogger(__name__)

class LLMInterface:
    def __init__(self):
        self.api_url = config.LLM_API_URL
        self.model = config.LLM_MODEL
        self.timeout = config.LLM_TIMEOUT
        
    def _call_ollama(self, prompt: str, context: str = None) -> Dict[str, Any]:  # 调用Ollama本地LLM
        try:
            payload = {
                "model": self.model,
                "prompt": f"{context}\n\n问题: {prompt}" if context else prompt,
                "stream": False,
                "options": {
                    "temperature": 0.7,
                    "max_tokens": 1000
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
                "answer": result.get("response", ""),
                "model": self.model,
                "context_used": bool(context)
            }
            
        except requests.exceptions.RequestException as e:
            logger.error(f"Ollama API调用失败: {e}")
            return {"success": False, "error": f"LLM服务不可用: {e}"}
        except Exception as e:
            logger.error(f"LLM调用异常: {e}")
            return {"success": False, "error": str(e)}
            
    def ask_about_stock(self, question: str, ts_code: str = None) -> Dict[str, Any]:  # 股票相关问答
        try:
            context = ""
            
            # 如果指定股票代码，获取相关数据作为上下文
            if ts_code:
                # 获取股票基本信息
                with db.engine.connect() as conn:
                    sql = "SELECT ts_code, name, industry, exchange FROM stock_basic WHERE ts_code = :ts_code"
                    result = conn.execute(text(sql), {'ts_code': ts_code})
                    stock_info = result.fetchone()
                    
                if stock_info:
                    # 获取最新推荐信息
                    recommendations = []
                    with db.engine.connect() as conn:
                        sql = "SELECT strategy, score, reason FROM recommend_result WHERE ts_code = :ts_code AND is_valid=1 ORDER BY recommend_date DESC LIMIT 3"
                        result = conn.execute(text(sql), {'ts_code': ts_code})
                        recommendations = [dict(row._mapping) for row in result]
                    
                    # 构建上下文
                    context = f"""
股票信息:
- 代码: {stock_info[0]}
- 名称: {stock_info[1]}
- 行业: {stock_info[2]}
- 交易所: {stock_info[3]}

最新推荐信息:
"""
                    for rec in recommendations:
                        context += f"- {rec['strategy']}策略: 分数{rec['score']:.3f}, 理由: {rec['reason']}\n"
                        
                else:
                    context = f"未找到股票代码 {ts_code} 的信息"
                    
            # 调用LLM
            return self._call_ollama(question, context)
            
        except Exception as e:
            logger.error(f"股票问答失败: {e}")
            return {"success": False, "error": str(e)}
            
    def analyze_market_trend(self, industry: str = None, days: int = 7) -> Dict[str, Any]:  # 市场趋势分析
        try:
            # 获取推荐统计数据
            with db.engine.connect() as conn:
                if industry:
                    sql = """
                    SELECT r.strategy, COUNT(*) as count, AVG(r.score) as avg_score, b.industry
                    FROM recommend_result r 
                    JOIN stock_basic b ON r.ts_code = b.ts_code 
                    WHERE r.recommend_date >= DATE_SUB(CURDATE(), INTERVAL :days DAY) 
                    AND b.industry = :industry AND r.is_valid=1
                    GROUP BY r.strategy, b.industry
                    """
                    result = conn.execute(text(sql), {'days': days, 'industry': industry})
                else:
                    sql = """
                    SELECT r.strategy, COUNT(*) as count, AVG(r.score) as avg_score
                    FROM recommend_result r 
                    WHERE r.recommend_date >= DATE_SUB(CURDATE(), INTERVAL :days DAY) 
                    AND r.is_valid=1
                    GROUP BY r.strategy
                    """
                    result = conn.execute(text(sql), {'days': days})
                    
                stats = [dict(row._mapping) for row in result]
                
            # 构建分析上下文
            context = f"近{days}天市场推荐统计数据:\n"
            for stat in stats:
                context += f"- {stat['strategy']}策略: {stat['count']}只推荐股票, 平均分数{stat.get('avg_score', 0):.3f}\n"
                
            if industry:
                context += f"\n特定行业: {industry}"
                
            question = f"请根据以上数据分析{'行业' if industry else '整体'}市场趋势和投资建议"
            
            return self._call_ollama(question, context)
            
        except Exception as e:
            logger.error(f"市场趋势分析失败: {e}")
            return {"success": False, "error": str(e)}
            
    def generate_investment_advice(self, risk_level: str = "medium", amount: float = 10000) -> Dict[str, Any]:  # 生成投资建议
        try:
            # 获取最新推荐股票
            with db.engine.connect() as conn:
                sql = """
                SELECT r.ts_code, r.name, r.score, r.strategy, r.reason, b.industry 
                FROM recommend_result r 
                JOIN stock_basic b ON r.ts_code = b.ts_code 
                WHERE r.recommend_date = (SELECT MAX(recommend_date) FROM recommend_result) 
                AND r.is_valid=1 
                ORDER BY r.score DESC 
                LIMIT 20
                """
                result = conn.execute(text(sql))
                recommendations = [dict(row._mapping) for row in result]
                
            # 构建投资建议上下文
            context = f"""
投资者信息:
- 风险偏好: {risk_level}
- 投资金额: {amount}元

当前推荐股票列表:
"""
            for i, rec in enumerate(recommendations[:10], 1):
                context += f"{i}. {rec['name']}({rec['ts_code']}) - 行业:{rec['industry']} - 分数:{rec['score']:.3f} - 策略:{rec['strategy']}\n"
                
            question = f"请根据投资者的风险偏好和资金规模，从推荐股票中制定投资组合配置方案"
            
            return self._call_ollama(question, context)
            
        except Exception as e:
            logger.error(f"投资建议生成失败: {e}")
            return {"success": False, "error": str(e)}
            
    def explain_recommendation(self, ts_code: str, strategy: str) -> Dict[str, Any]:  # 解释推荐理由
        try:
            # 获取股票详细信息和推荐理由
            with db.engine.connect() as conn:
                sql = """
                SELECT r.*, b.name, b.industry, b.exchange 
                FROM recommend_result r 
                JOIN stock_basic b ON r.ts_code = b.ts_code 
                WHERE r.ts_code = :ts_code AND r.strategy = :strategy 
                AND r.is_valid=1 
                ORDER BY r.recommend_date DESC 
                LIMIT 1
                """
                result = conn.execute(text(sql), {'ts_code': ts_code, 'strategy': strategy})
                rec_data = result.fetchone()
                
            if not rec_data:
                return {"success": False, "error": "未找到推荐记录"}
                
            rec_dict = dict(rec_data._mapping)
            
            # 构建解释上下文
            context = f"""
股票: {rec_dict['name']}({rec_dict['ts_code']})
行业: {rec_dict['industry']}
推荐策略: {rec_dict['strategy']}
推荐分数: {rec_dict['score']:.3f}
系统理由: {rec_dict['reason']}
推荐日期: {rec_dict['recommend_date']}
"""
            
            question = "请详细解释这个股票推荐的技术分析逻辑和投资价值，并提供风险提示"
            
            return self._call_ollama(question, context)
            
        except Exception as e:
            logger.error(f"推荐解释失败: {e}")
            return {"success": False, "error": str(e)}
            
    def health_check(self) -> bool:  # LLM服务健康检查
        try:
            response = requests.get(f"{self.api_url}/api/tags", timeout=5)
            return response.status_code == 200
        except:
            return False

# 创建全局LLM接口实例
llm = LLMInterface()

# 命令行测试功能
if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("用法: python llm/interface.py [ask|trend|advice|explain] [参数...]")
        sys.exit(1)
        
    command = sys.argv[1]
    
    if command == "ask" and len(sys.argv) >= 3:
        question = sys.argv[2]
        ts_code = sys.argv[3] if len(sys.argv) > 3 else None
        result = llm.ask_about_stock(question, ts_code)
        
    elif command == "trend":
        industry = sys.argv[2] if len(sys.argv) > 2 else None
        result = llm.analyze_market_trend(industry)
        
    elif command == "advice":
        risk_level = sys.argv[2] if len(sys.argv) > 2 else "medium"
        amount = float(sys.argv[3]) if len(sys.argv) > 3 else 10000
        result = llm.generate_investment_advice(risk_level, amount)
        
    elif command == "explain" and len(sys.argv) >= 4:
        ts_code, strategy = sys.argv[2], sys.argv[3]
        result = llm.explain_recommendation(ts_code, strategy)
        
    else:
        print("无效命令或参数不足")
        sys.exit(1)
        
    print(json.dumps(result, ensure_ascii=False, indent=2)) 