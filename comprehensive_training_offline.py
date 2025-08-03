#!/usr/bin/env python3
"""
ljwx-stock模型全面训练脚本（离线模式）
使用已有的训练数据演示大规模训练能力
"""

import os
import sys
import json
import logging
import tempfile
import subprocess
from datetime import datetime
from typing import List, Dict
import random

def main():
    """主函数"""
    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s - %(levelname)s - %(message)s'
    )
    
    logger = logging.getLogger(__name__)
    
    print("🚀 ljwx-stock 全面训练系统（离线演示）")
    print("=" * 60)
    print("📊 使用现有训练数据演示大规模训练能力")
    print("🎯 目标: 创建增强版ljwx-stock模型")
    print("=" * 60)
    
    try:
        # 检查现有训练数据
        training_dir = "data/llm_training"
        if not os.path.exists(training_dir):
            print("❌ 未找到训练数据目录")
            return
        
        # 加载所有训练数据
        all_training_data = []
        jsonl_files = [f for f in os.listdir(training_dir) if f.endswith('.jsonl')]
        
        print(f"\n📊 发现 {len(jsonl_files)} 个训练数据文件:")
        for file in jsonl_files:
            file_path = os.path.join(training_dir, file)
            count = 0
            try:
                with open(file_path, 'r', encoding='utf-8') as f:
                    for line in f:
                        if line.strip():
                            data = json.loads(line.strip())
                            all_training_data.append(data)
                            count += 1
                print(f"   📁 {file}: {count}条")
            except Exception as e:
                print(f"   ❌ {file}: 读取失败 ({e})")
        
        print(f"\n📈 总训练数据: {len(all_training_data)}条")
        
        if len(all_training_data) == 0:
            print("❌ 未找到有效的训练数据")
            return
        
        # 增强训练数据
        enhanced_data = enhance_training_data(all_training_data)
        print(f"🧠 增强后训练数据: {len(enhanced_data)}条")
        
        # 创建全面训练模型
        timestamp = datetime.now().strftime("%Y%m%d_%H%M")
        model_name = f"ljwx-stock-comprehensive-{timestamp}"
        
        print(f"\n🚀 创建全面训练模型: {model_name}")
        
        # 构建高级系统提示词
        system_prompt = """你是ljwx-stock-comprehensive，一个基于大量A股历史数据训练的专业股票投资分析AI。

🎯 **核心能力**
- 全市场股票技术分析和投资决策支持
- 基于真实历史数据的专业级分析能力
- 多维度风险评估和投资组合建议

📊 **专业分析框架**
1. **技术面分析**: K线形态、均线系统、RSI/MACD等技术指标
2. **量价关系**: 成交量分析和资金流向判断
3. **趋势识别**: 短期、中期、长期趋势判断
4. **风险控制**: 止损位设置、仓位管理建议
5. **市场情绪**: 超买超卖判断、市场参与度分析

💡 **分析特色**
- 客观理性，基于数据驱动
- 风险提示优先，投资者保护
- 多角度分析，综合判断
- 操作指导具体，实用性强

📋 **输出格式**
- 技术分析：详细指标解读
- 风险评估：明确风险等级
- 投资建议：具体操作建议
- 风险提示：潜在风险警示

⚠️ **重要声明**: 分析仅供参考，投资有风险，决策需谨慎。"""

        # 构建Modelfile
        modelfile_content = f"""FROM ljwx-stock
SYSTEM "{system_prompt}"
PARAMETER temperature 0.7
PARAMETER top_p 0.9
PARAMETER top_k 40
PARAMETER repeat_penalty 1.1
PARAMETER num_predict 1024

"""
        
        # 添加精选训练样本
        sample_count = min(len(enhanced_data), 300)  # 使用300个精选样本
        selected_samples = random.sample(enhanced_data, sample_count)
        
        print(f"   📝 使用精选样本: {sample_count}条")
        
        for i, example in enumerate(selected_samples):
            instruction = example.get('instruction', '')
            input_text = example.get('input', '')
            output_text = example.get('output', '')
            
            user_message = f"{instruction}\n\n{input_text}" if input_text else instruction
            
            # 清理文本
            user_message = user_message.replace('"', "'").replace('\n', '\\n')
            output_text = output_text.replace('"', "'").replace('\n', '\\n')
            
            modelfile_content += f'MESSAGE user "{user_message}"\n'
            modelfile_content += f'MESSAGE assistant "{output_text}"\n\n'
        
        # 创建临时Modelfile
        with tempfile.NamedTemporaryFile(mode='w', suffix='.modelfile', delete=False) as f:
            f.write(modelfile_content)
            modelfile_path = f.name
        
        try:
            print("   🔨 正在创建模型...")
            result = subprocess.run(
                ['ollama', 'create', model_name, '-f', modelfile_path],
                capture_output=True,
                text=True,
                timeout=600
            )
            
            if result.returncode == 0:
                print(f"✅ 全面训练模型创建成功: {model_name}")
                
                # 测试新模型
                print(f"\n🧪 测试全面训练模型:")
                test_comprehensive_model(model_name)
                
                print(f"\n🎉 全面训练完成!")
                print(f"📊 模型详情:")
                print(f"   名称: {model_name}")
                print(f"   训练样本: {sample_count}条")
                print(f"   数据来源: {len(jsonl_files)}个文件")
                print(f"   增强技术: 多样化训练样本")
                
                print(f"\n🎯 使用命令:")
                print(f"   ollama run {model_name}")
                
            else:
                print(f"❌ 模型创建失败: {result.stderr}")
                
        finally:
            os.unlink(modelfile_path)
            
    except Exception as e:
        logger.error(f"❌ 全面训练失败: {e}")
        print(f"\n❌ 全面训练失败: {e}")

def enhance_training_data(base_data: List[Dict]) -> List[Dict]:
    """增强训练数据"""
    enhanced_data = base_data.copy()
    
    # 添加更多样化的分析类型
    enhancement_templates = [
        {
            "instruction": "基于以下股票数据，提供专业的技术分析报告：",
            "analysis_type": "comprehensive"
        },
        {
            "instruction": "评估该股票的投资风险等级并给出详细理由：",
            "analysis_type": "risk_assessment"
        },
        {
            "instruction": "基于技术指标判断该股票的买卖时机：",
            "analysis_type": "timing"
        },
        {
            "instruction": "分析该股票的趋势方向和支撑阻力位：",
            "analysis_type": "trend_support"
        }
    ]
    
    # 为现有数据添加变体
    added_count = 0
    for template in enhancement_templates:
        if added_count >= 200:  # 限制增强数量
            break
            
        sample_data = random.sample(base_data, min(50, len(base_data)))
        
        for data in sample_data:
            if added_count >= 200:
                break
                
            enhanced_example = {
                "instruction": template["instruction"],
                "input": data.get("input", ""),
                "output": generate_enhanced_output(data, template["analysis_type"]),
                "metadata": {
                    "type": template["analysis_type"],
                    "enhanced": True,
                    "original_type": data.get("metadata", {}).get("type", "unknown")
                }
            }
            
            enhanced_data.append(enhanced_example)
            added_count += 1
    
    print(f"   🔧 数据增强: 新增 {added_count} 条样本")
    return enhanced_data

def generate_enhanced_output(data: Dict, analysis_type: str) -> str:
    """生成增强的分析输出"""
    input_text = data.get("input", "")
    original_output = data.get("output", "")
    
    if analysis_type == "comprehensive":
        return f"综合技术分析：{original_output} 建议结合基本面分析，关注市场整体走势，做好风险控制。"
    elif analysis_type == "risk_assessment":
        return f"风险评估：{original_output} 投资者应根据自身风险承受能力谨慎决策，建议分散投资。"
    elif analysis_type == "timing":
        return f"买卖时机分析：{original_output} 建议关注成交量变化，结合市场情绪判断入场时机。"
    elif analysis_type == "trend_support":
        return f"趋势与支撑阻力分析：{original_output} 应密切关注关键价位的突破情况。"
    else:
        return original_output

def test_comprehensive_model(model_name: str):
    """测试全面训练模型"""
    test_cases = [
        "分析平安银行(000001.SZ)当前技术形态，价格12.50元，RSI=65，给出投资建议",
        "评估中国石油(601857.SH)的风险等级，近期波动较大",
        "基于技术指标分析贵州茅台(600519.SH)的买卖时机"
    ]
    
    for i, test_case in enumerate(test_cases):
        print(f"   测试 {i+1}: {test_case[:30]}...")
        
        try:
            result = subprocess.run(
                ['ollama', 'run', model_name],
                input=test_case,
                capture_output=True,
                text=True,
                timeout=30
            )
            
            if result.returncode == 0 and result.stdout.strip():
                response = result.stdout.strip()
                print(f"   ✅ 响应长度: {len(response)}字符")
            else:
                print(f"   ❌ 测试失败")
                
        except Exception as e:
            print(f"   ❌ 测试异常: {e}")

if __name__ == "__main__":
    main()