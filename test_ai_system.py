#!/usr/bin/env python3
"""
AI股票分析系统测试脚本
测试机器学习模型训练、预测和智能推荐功能
"""
import asyncio
import sys
import os
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from ai.ml_trainer import ml_trainer
from llm.intelligent_recommender import intelligent_recommender

async def test_ai_system():
    """测试AI系统"""
    print("🚀 开始测试AI股票分析系统")
    print("=" * 80)
    
    # 1. 测试机器学习模型训练
    print("\n📊 步骤1: 训练机器学习模型")
    try:
        # 准备训练数据
        training_data, X, y = ml_trainer.prepare_training_data(days_back=180, stock_limit=100)
        print(f"✅ 训练数据准备完成: {X.shape[0]}条记录, {X.shape[1]}个特征")
        
        # 训练模型
        performance = ml_trainer.train_all_models(X, y)
        print(f"✅ 训练完成，共{len(performance)}个模型")
        
        # 显示最佳模型
        if performance:
            best_model = max(performance.keys(), key=lambda x: performance[x]['accuracy'])
            best_acc = performance[best_model]['accuracy']
            print(f"🏆 最佳模型: {best_model}, 准确率: {best_acc:.4f}")
        
        # 保存模型
        success = ml_trainer.save_models()
        print(f"💾 模型保存: {'成功' if success else '失败'}")
        
    except Exception as e:
        print(f"❌ 机器学习训练失败: {e}")
    
    # 2. 测试单股票预测
    print("\n🎯 步骤2: 测试股票AI预测")
    test_stocks = ["000001.SZ", "000002.SZ", "600000.SH"]
    
    for stock_code in test_stocks:
        try:
            prediction = ml_trainer.predict_single_stock(stock_code)
            print(f"📈 {stock_code}: {prediction['prediction_text']} (置信度: {prediction['confidence']:.3f})")
        except Exception as e:
            print(f"❌ 预测{stock_code}失败: {e}")
    
    # 3. 测试智能推荐系统（集成AI）
    print("\n🤖 步骤3: 测试智能推荐系统")
    try:
        recommendations = await intelligent_recommender.generate_intelligent_recommendations(max_stocks=10)
        
        print(f"✅ 智能推荐生成完成: {len(recommendations)}只股票")
        print("\n🎯 推荐结果:")
        print("-" * 80)
        
        for i, rec in enumerate(recommendations[:5], 1):  # 显示前5只
            print(f"{i:2d}. {rec.name}({rec.ts_code})")
            print(f"    推荐: {rec.recommendation} | 综合得分: {rec.score:.3f}")
            print(f"    技术分析: {rec.technical_score:.3f} | AI分析: {rec.ai_score:.3f}")
            print(f"    风险等级: {rec.risk_level} | 市场地位: {rec.market_position}")
            print(f"    推理: {rec.reasoning[:100]}...")
            print("-" * 80)
        
        # 保存推荐结果
        save_success = await intelligent_recommender.save_intelligent_recommendations(recommendations)
        print(f"💾 推荐结果保存: {'成功' if save_success else '失败'}")
        
    except Exception as e:
        print(f"❌ 智能推荐失败: {e}")
    
    # 4. 测试特征重要性分析
    print("\n📈 步骤4: 特征重要性分析")
    try:
        for model_name in list(ml_trainer.models.keys())[:2]:  # 测试前2个模型
            importance = ml_trainer.get_feature_importance(model_name)
            if importance:
                print(f"\n{model_name} 最重要特征:")
                for i, feature in enumerate(importance[:5], 1):
                    print(f"  {i}. {feature['feature']}: {feature['importance']:.4f}")
    except Exception as e:
        print(f"❌ 特征重要性分析失败: {e}")
    
    print("\n🎉 AI系统测试完成!")

def test_model_performance():
    """测试模型性能"""
    print("\n📊 模型性能统计:")
    print("=" * 60)
    
    for model_name, perf in ml_trainer.model_performance.items():
        print(f"{model_name:20} | 准确率: {perf['accuracy']:.4f} | F1: {perf['f1_score']:.4f}")
    
    print(f"\n特征数量: {len(ml_trainer.feature_names)}")
    print(f"已训练模型: {len(ml_trainer.models)}")

if __name__ == "__main__":
    # 运行异步测试
    asyncio.run(test_ai_system())
    
    # 显示性能统计
    if ml_trainer.model_performance:
        test_model_performance()