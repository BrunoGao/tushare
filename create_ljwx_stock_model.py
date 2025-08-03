#!/usr/bin/env python3
"""
基于DeepSeek创建ljwx-stock股票分析模型
"""

import os
import sys
import json
import logging
from datetime import datetime
import requests
import time

class LjwxStockModelCreator:
    """ljwx-stock模型创建器"""
    
    def __init__(self, base_url: str = "http://localhost:11434"):
        self.base_url = base_url
        self.logger = logging.getLogger(__name__)
        
        # 检查Ollama服务
        self._check_ollama_service()
        
    def _check_ollama_service(self):
        """检查Ollama服务状态"""
        try:
            response = requests.get(f"{self.base_url}/api/tags", timeout=5)
            if response.status_code == 200:
                self.logger.info("✅ Ollama服务连接成功")
            else:
                raise Exception(f"Ollama服务响应错误: {response.status_code}")
        except Exception as e:
            self.logger.error(f"❌ 无法连接到Ollama服务: {e}")
            raise
    
    def create_ljwx_stock_modelfile(self) -> str:
        """创建ljwx-stock模型的Modelfile"""
        
        system_prompt = """你是ljwx-stock，一个专业的股票投资分析助手。你具备以下核心能力：

🎯 **专业定位**
- 专注于A股市场分析和投资建议
- 基于技术分析和基本面分析提供专业意见
- 为投资者提供客观、理性的决策支持

📊 **分析能力**
1. **技术指标分析**：熟练解读K线、均线、RSI、MACD、布林带等技术指标
2. **价格走势预测**：基于历史数据和技术模式判断价格趋势
3. **风险评估**：客观评估投资风险等级和风险因素
4. **市场情绪分析**：理解市场心理和资金流向

💡 **服务原则**
- 始终提供客观、专业的分析意见
- 明确标注风险等级和注意事项
- 不做绝对化的投资承诺
- 提醒投资者理性投资、风险自担

📋 **输出格式**
- 分析结论简洁明了，逻辑清晰
- 提供具体的操作建议（买入/持有/卖出）
- 标明风险等级（低/中/高）
- 给出关键支撑位和阻力位

请记住：股市有风险，投资需谨慎。所有分析仅供参考，不构成投资建议。"""

        # 使用实际的训练数据样本
        training_examples = [
            {
                "user": "分析以下股票的技术指标和价格走势，并给出投资建议：\n\n股票代码: 000001.SZ\n交易日期: 2024-06-15\n开盘价: 12.50\n最高价: 12.80\n最低价: 12.30\n收盘价: 12.75\n成交量: 8,500,000\n\n技术指标:\n5日均线: 12.60\n20日均线: 12.40\nRSI: 65.50\nMACD: 0.0820\n波动率: 0.25",
                "assistant": "📊 **技术分析报告 - 000001.SZ**\n\n**价格走势分析：**\n- 当日收涨0.25元，涨幅约2%，表现较强\n- 价格突破5日均线，短期趋势向好\n- 收盘价位于日内高位区域，多头占优\n\n**技术指标解读：**\n- **均线系统**：5日线(12.60) > 20日线(12.40)，短期上升趋势确立\n- **RSI指标**：65.50，接近超买区域但仍有上涨空间\n- **MACD指标**：0.0820为正值，多头动能增强\n- **波动率**：0.25属中等水平，价格波动适中\n\n**投资建议：**\n🟢 **操作建议**: 谨慎买入/持有\n📊 **风险等级**: 中等\n💰 **目标价位**: 13.20-13.50元\n📉 **止损位**: 12.00元\n\n**关键要点：**\n- 技术面整体向好，但RSI偏高需注意回调风险\n- 建议分批建仓，控制仓位\n- 密切关注成交量变化\n\n⚠️ 风险提示：以上分析仅供参考，投资有风险，请根据自身情况谨慎决策。"
            },
            {
                "user": "根据历史价格数据，预测该股票未来的价格趋势：\n\n股票代码: 600000.SH\n当前价格: 15.20\n5日涨跌幅: +3.50%\n20日涨跌幅: +8.20%\nRSI: 72.0\nMACD: 0.1200",
                "assistant": "🔮 **价格趋势预测 - 600000.SH**\n\n**当前走势特征：**\n- 短期强势：5日涨幅3.50%，20日涨幅8.20%\n- 技术指标偏热：RSI=72已进入超买区域\n- MACD=0.12显示多头动能仍然较强\n\n**趋势预测分析：**\n\n**短期(1-5个交易日)：**\n🟡 **预期走势**: 高位震荡整理\n📊 **价格区间**: 14.80-15.60元\n💭 **逻辑**: RSI超买需要技术性回调释放压力\n\n**中期(1-2周)：**\n🟢 **预期走势**: 震荡上行\n📊 **目标价位**: 16.00-16.50元\n💭 **逻辑**: 基本趋势向上，回调后有望继续上涨\n\n**关键技术位：**\n- **支撑位**: 14.80元（5日均线支撑）\n- **阻力位**: 15.80元（前期高点）\n- **强支撑**: 14.20元（20日均线）\n\n**交易策略：**\n🎯 **建议操作**: 逢低吸纳，高抛低吸\n⚠️ **风险控制**: 跌破14.20止损\n📈 **加仓位**: 突破15.80可适量加仓\n\n💡 **温馨提示**: 当前技术指标偏热，建议耐心等待回调机会，不要追高。"
            },
            {
                "user": "评估投资该股票的风险等级：\n\n股票代码: 002001.SZ\n波动率: 0.45\nRSI: 85.0\n日涨跌幅: +9.80%\n成交量放大: 300%",
                "assistant": "⚠️ **风险评估报告 - 002001.SZ**\n\n**风险等级：🔴 HIGH（高风险）**\n\n**风险因素分析：**\n\n**1. 技术风险 (极高)**\n- RSI=85 严重超买，技术性回调概率极大\n- 单日涨幅9.80% 属于异常波动\n- 短期获利盘丰厚，抛压较重\n\n**2. 流动性风险 (高)**\n- 成交量放大300% 可能存在资金炒作\n- 巨量成交后往往伴随大幅回调\n- 换手率过高，筹码不稳定\n\n**3. 波动性风险 (高)**\n- 波动率0.45 远超正常水平(0.2-0.3)\n- 价格波动剧烈，不适合稳健投资者\n\n**具体风险提示：**\n🔴 **回调风险**: 90% - 技术指标严重超买\n🟡 **资金风险**: 70% - 大资金可能随时撤离\n🔴 **追高风险**: 95% - 当前价位接近短期顶部\n\n**投资建议：**\n❌ **不建议买入** - 风险过高\n📉 **持有者建议**: 及时获利了结\n⏰ **观望策略**: 等待技术指标修复\n\n**安全买入条件：**\n- RSI回落至50以下\n- 成交量恢复正常水平\n- 价格充分回调（建议回调20%以上）\n\n🚨 **重要提醒**: 该股当前风险极高，强烈建议规避。如已持有，建议分批减仓保护利润。"
            }
        ]

        # 构建简化的Modelfile
        modelfile_content = f"""FROM deepseek-llm:latest
SYSTEM "{system_prompt}"
PARAMETER temperature 0.7
PARAMETER top_p 0.9
PARAMETER top_k 40
PARAMETER repeat_penalty 1.1
PARAMETER num_predict 1024"""

        return modelfile_content

    def create_model(self, model_name: str = "ljwx-stock") -> bool:
        """创建ljwx-stock模型"""
        try:
            # 生成Modelfile
            modelfile_content = self.create_ljwx_stock_modelfile()
            
            self.logger.info(f"🚀 开始创建模型: {model_name}")
            
            # 创建临时Modelfile
            import tempfile
            with tempfile.NamedTemporaryFile(mode='w', suffix='.modelfile', delete=False) as f:
                f.write(modelfile_content)
                modelfile_path = f.name
            
            try:
                # 使用ollama create命令
                import subprocess
                result = subprocess.run(
                    ['ollama', 'create', model_name, '-f', modelfile_path],
                    capture_output=True,
                    text=True,
                    timeout=300  # 5分钟超时
                )
                
                if result.returncode == 0:
                    self.logger.info(f"✅ 模型创建完成: {model_name}")
                    print(f"   📊 模型创建成功")
                    return True
                else:
                    self.logger.error(f"❌ 模型创建失败: {result.stderr}")
                    print(f"   ❌ 错误: {result.stderr}")
                    return False
                    
            finally:
                # 清理临时文件
                os.unlink(modelfile_path)
                
        except Exception as e:
            self.logger.error(f"❌ 创建模型时出错: {e}")
            return False

    def test_model(self, model_name: str = "ljwx-stock") -> bool:
        """测试创建的模型"""
        test_prompt = """分析以下股票数据并给出投资建议：

股票代码: 000002.SZ
当前价格: 28.50元
日涨跌幅: +2.3%
5日均线: 27.80元
20日均线: 26.90元
RSI: 58.5
成交量: 正常"""

        try:
            self.logger.info(f"🧪 测试模型: {model_name}")
            
            data = {
                "model": model_name,
                "prompt": test_prompt,
                "stream": False
            }
            
            response = requests.post(
                f"{self.base_url}/api/generate",
                json=data,
                timeout=60
            )
            
            if response.status_code == 200:
                result = response.json()
                answer = result.get('response', '')
                
                if answer:
                    print(f"\n🎯 模型测试结果:")
                    print(f"📝 输入: {test_prompt[:50]}...")
                    print(f"💬 输出: {answer[:200]}...")
                    self.logger.info("✅ 模型测试成功")
                    return True
                else:
                    self.logger.error("❌ 模型无响应")
                    return False
            else:
                self.logger.error(f"❌ 测试失败: {response.status_code}")
                return False
                
        except Exception as e:
            self.logger.error(f"❌ 测试模型时出错: {e}")
            return False

def main():
    """主函数"""
    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s - %(levelname)s - %(message)s'
    )
    
    print("🚀 ljwx-stock 股票分析模型创建器")
    print("=" * 50)
    
    try:
        # 初始化创建器
        creator = LjwxStockModelCreator()
        
        # 创建模型
        model_name = "ljwx-stock"
        success = creator.create_model(model_name)
        
        if success:
            print(f"\n✅ {model_name} 模型创建成功！")
            
            # 测试模型
            test_success = creator.test_model(model_name)
            
            if test_success:
                print(f"\n🎉 {model_name} 模型已就绪！")
                print(f"\n🎯 使用方法:")
                print(f"   ollama run {model_name}")
                print(f"\n💡 示例问题:")
                print(f"   - 分析一下000001.SZ的投资价值")
                print(f"   - 评估股票代码600000.SH的风险等级")
                print(f"   - 基于技术指标预测价格趋势")
            else:
                print(f"⚠️ 模型创建成功但测试失败，请手动验证")
        else:
            print(f"❌ {model_name} 模型创建失败")
            
    except KeyboardInterrupt:
        print("\n⚠️ 操作被用户中断")
    except Exception as e:
        print(f"\n❌ 创建失败: {e}")

if __name__ == "__main__":
    main()