#!/usr/bin/env python3
"""
ljwx-stock持续训练系统演示脚本
展示完整的持续训练功能
"""

import os
import sys
import subprocess
import json
from datetime import datetime

def check_ollama_service():
    """检查Ollama服务状态"""
    try:
        result = subprocess.run(['ollama', 'list'], capture_output=True, text=True)
        return result.returncode == 0
    except:
        return False

def get_ljwx_models():
    """获取ljwx-stock相关模型"""
    try:
        result = subprocess.run(['ollama', 'list'], capture_output=True, text=True)
        if result.returncode == 0:
            lines = result.stdout.strip().split('\n')
            models = []
            for line in lines[1:]:
                if 'ljwx' in line:
                    model_name = line.split()[0]
                    models.append(model_name)
            return models
    except:
        pass
    return []

def show_training_data_summary():
    """显示训练数据摘要"""
    training_dir = "data/llm_training"
    if not os.path.exists(training_dir):
        return
    
    files = [f for f in os.listdir(training_dir) if f.endswith('.jsonl')]
    
    print("\n📊 训练数据文件:")
    for file in sorted(files):
        file_path = os.path.join(training_dir, file)
        line_count = 0
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                line_count = sum(1 for line in f if line.strip())
        except:
            line_count = 0
        
        print(f"   📁 {file}: {line_count}条数据")

def show_model_performance():
    """显示模型性能"""
    models_dir = "data/models"
    if not os.path.exists(models_dir):
        return
    
    results_files = [f for f in os.listdir(models_dir) if f.endswith('.json') and 'results' in f]
    
    if not results_files:
        return
    
    print("\n📈 模型性能历史:")
    for file in sorted(results_files)[-3:]:  # 显示最近3次
        file_path = os.path.join(models_dir, file)
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                data = json.load(f)
            
            if 'performance' in data and 'new_model' in data:
                perf = data['performance']['performance']
                print(f"   🤖 {data['new_model']}")
                print(f"      成功率: {perf['success_rate']:.2%}")
                print(f"      时间: {data.get('start_time', '')[:19]}")
        except:
            continue

def demo_basic_model():
    """演示基础模型"""
    print("\n🎯 演示基础ljwx-stock模型:")
    print("-" * 40)
    
    test_question = "分析股票000001.SZ，当前价格12.50元，RSI=65，MACD=0.08，给出投资建议"
    print(f"问题: {test_question}")
    print("\n回答:")
    
    try:
        result = subprocess.run(
            ['ollama', 'run', 'ljwx-stock'],
            input=test_question,
            capture_output=True,
            text=True,
            timeout=20
        )
        
        if result.returncode == 0:
            response = result.stdout.strip()
            # 限制显示长度
            if len(response) > 300:
                response = response[:300] + "..."
            print(response)
        else:
            print("❌ 模型响应失败")
    except subprocess.TimeoutExpired:
        print("⏰ 响应超时")
    except Exception as e:
        print(f"❌ 错误: {e}")

def demo_trained_model():
    """演示训练后的模型"""
    models = get_ljwx_models()
    trained_models = [m for m in models if 'trained' in m or 'test' in m]
    
    if not trained_models:
        print("\n❌ 未找到训练后的模型")
        return
    
    latest_model = sorted(trained_models)[-1]
    
    print(f"\n🚀 演示训练后模型: {latest_model}")
    print("-" * 40)
    
    test_question = "评估股票002001.SZ的风险等级，波动率0.45，RSI=85，日涨跌幅+9.8%"
    print(f"问题: {test_question}")
    print("\n回答:")
    
    try:
        result = subprocess.run(
            ['ollama', 'run', latest_model],
            input=test_question,
            capture_output=True,
            text=True,
            timeout=20
        )
        
        if result.returncode == 0:
            response = result.stdout.strip()
            if len(response) > 300:
                response = response[:300] + "..."
            print(response)
        else:
            print("❌ 模型响应失败")
    except subprocess.TimeoutExpired:
        print("⏰ 响应超时")
    except Exception as e:
        print(f"❌ 错误: {e}")

def show_system_architecture():
    """显示系统架构"""
    print("""
🏗️  系统架构图:

┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   TuShare API   │    │  训练数据生成器   │    │   Ollama引擎    │
│                 │────│                 │────│                 │
│ • 股票数据      │    │ • 数据处理      │    │ • 模型训练      │
│ • 技术指标      │    │ • 样本生成      │    │ • 模型部署      │
└─────────────────┘    └─────────────────┘    └─────────────────┘
         │                       │                       │
         └───────────────────────┼───────────────────────┘
                                 │
                    ┌─────────────────┐
                    │   调度管理器     │
                    │                 │
                    │ • 定时训练      │
                    │ • 性能监控      │
                    │ • 模型管理      │
                    │ • 报告生成      │
                    └─────────────────┘

🔄 持续训练流程:
1. 数据提取 → 2. 训练数据生成 → 3. 模型训练 → 4. 性能测试 → 5. 模型部署
""")

def show_usage_examples():
    """显示使用示例"""
    print("""
📋 使用示例:

1. 立即执行一次持续训练:
   python continuous_training.py

2. 启动自动调度器:
   python scheduler.py

3. 立即执行训练任务:
   python scheduler.py run

4. 查看训练报告:
   python scheduler.py report

5. 测试模拟训练:
   python test_continuous_training.py

6. 清理旧模型:
   python scheduler.py cleanup

🎯 模型使用:
   ollama run ljwx-stock                    # 基础模型
   ollama run ljwx-stock-trained-XXXXXX     # 训练后模型
""")

def main():
    """主函数"""
    print("🚀 ljwx-stock持续训练系统演示")
    print("=" * 60)
    
    # 检查环境
    print("🔍 环境检查:")
    
    if not check_ollama_service():
        print("❌ Ollama服务未运行，请先启动: ollama serve")
        return
    else:
        print("✅ Ollama服务正常")
    
    # 检查模型
    models = get_ljwx_models()
    print(f"🤖 可用模型: {len(models)}个")
    for model in models:
        print(f"   • {model}")
    
    if not models:
        print("❌ 未找到ljwx-stock模型，请先创建基础模型")
        return
    
    # 显示训练数据
    show_training_data_summary()
    
    # 显示性能历史
    show_model_performance()
    
    # 显示系统架构
    show_system_architecture()
    
    # 演示模型功能
    if 'ljwx-stock:latest' in models or 'ljwx-stock' in [m.split(':')[0] for m in models]:
        demo_basic_model()
    
    # 演示训练后模型
    demo_trained_model()
    
    # 显示使用示例
    show_usage_examples()
    
    print("\n" + "=" * 60)
    print("✨ 持续训练系统演示完成！")
    print("💡 建议：定期运行持续训练以保持模型最新状态")

if __name__ == "__main__":
    main()