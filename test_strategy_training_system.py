#!/usr/bin/env python3
"""
策略训练系统测试脚本
Test Script for Strategy-Based Training System
"""

import os
import sys
import json
import logging
import requests
import time
from datetime import datetime
from typing import Dict, List, Any

# 添加项目路径
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

# 设置日志
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)

class StrategyTrainingSystemTester:
    """策略训练系统测试器"""
    
    def __init__(self, base_url: str = "http://localhost:5005"):
        self.base_url = base_url
        self.api_base = f"{base_url}/api/strategy-training"
        
    def test_health_check(self) -> bool:
        """测试健康检查"""
        logger.info("🩺 测试健康检查...")
        
        try:
            response = requests.get(f"{self.api_base}/health", timeout=10)
            
            if response.status_code == 200:
                data = response.json()
                if data.get("success"):
                    logger.info("✅ 健康检查通过")
                    logger.info(f"   组件状态: {data.get('components', {})}")
                    return True
                else:
                    logger.error(f"❌ 健康检查失败: {data}")
                    return False
            else:
                logger.error(f"❌ 健康检查API返回错误状态码: {response.status_code}")
                return False
                
        except Exception as e:
            logger.error(f"❌ 健康检查异常: {e}")
            return False
    
    def test_get_strategies(self) -> bool:
        """测试获取策略列表"""
        logger.info("📋 测试获取策略列表...")
        
        try:
            response = requests.get(f"{self.api_base}/strategies", timeout=10)
            
            if response.status_code == 200:
                data = response.json()
                if data.get("success"):
                    strategies = data.get("strategies", [])
                    logger.info(f"✅ 获取策略列表成功，共 {len(strategies)} 个策略")
                    
                    # 显示前5个策略
                    for i, strategy in enumerate(strategies[:5]):
                        logger.info(f"   {i+1}. {strategy.get('name')} ({strategy.get('id')})")
                    
                    return len(strategies) > 0
                else:
                    logger.error(f"❌ 获取策略列表失败: {data}")
                    return False
            else:
                logger.error(f"❌ 获取策略列表API返回错误状态码: {response.status_code}")
                return False
                
        except Exception as e:
            logger.error(f"❌ 获取策略列表异常: {e}")
            return False
    
    def test_generate_single_dataset(self, strategy_id: str = "rsi_mean_reversion") -> bool:
        """测试生成单个策略数据集"""
        logger.info(f"📊 测试生成单个策略数据集: {strategy_id}")
        
        try:
            payload = {
                "strategy_id": strategy_id,
                "num_stocks": 5,  # 少量股票用于测试
                "time_range": 90  # 90天历史数据
            }
            
            response = requests.post(
                f"{self.api_base}/dataset/generate",
                json=payload,
                timeout=120  # 2分钟超时
            )
            
            if response.status_code == 200:
                data = response.json()
                if data.get("success"):
                    samples_generated = data.get("samples_generated", 0)
                    logger.info(f"✅ 单个策略数据集生成成功")
                    logger.info(f"   策略ID: {data.get('strategy_id')}")
                    logger.info(f"   生成样本数: {samples_generated}")
                    logger.info(f"   参数: {data.get('parameters', {})}")
                    return samples_generated > 0
                else:
                    logger.error(f"❌ 单个策略数据集生成失败: {data}")
                    return False
            else:
                logger.error(f"❌ 数据集生成API返回错误状态码: {response.status_code}")
                return False
                
        except Exception as e:
            logger.error(f"❌ 单个策略数据集生成异常: {e}")
            return False
    
    def test_generate_recommendations(self) -> bool:
        """测试生成推荐"""
        logger.info("🎯 测试生成推荐...")
        
        try:
            payload = {
                "category": "general",
                "force_generate": True
            }
            
            response = requests.post(
                f"{self.api_base}/recommendations/generate",
                json=payload,
                timeout=60
            )
            
            if response.status_code == 200:
                data = response.json()
                if data.get("success"):
                    recommendations = data.get("recommendations", [])
                    logger.info(f"✅ 推荐生成成功，共 {len(recommendations)} 个推荐")
                    
                    # 显示前3个推荐
                    for i, rec in enumerate(recommendations[:3]):
                        logger.info(f"   {i+1}. {rec.get('stock_code', 'N/A')} - {rec.get('recommendation', 'N/A')}")
                    
                    return True
                else:
                    logger.error(f"❌ 推荐生成失败: {data}")
                    return False
            else:
                logger.error(f"❌ 推荐生成API返回错误状态码: {response.status_code}")
                return False
                
        except Exception as e:
            logger.error(f"❌ 推荐生成异常: {e}")
            return False
    
    def test_scheduler_operations(self) -> bool:
        """测试调度器操作"""
        logger.info("⏰ 测试调度器操作...")
        
        success_count = 0
        
        # 测试获取调度器状态
        try:
            response = requests.get(f"{self.api_base}/scheduler/status", timeout=10)
            if response.status_code == 200:
                data = response.json()
                if data.get("success"):
                    logger.info("✅ 调度器状态查询成功")
                    logger.info(f"   运行状态: {data.get('status', {}).get('is_running', False)}")
                    success_count += 1
                else:
                    logger.error(f"❌ 调度器状态查询失败: {data}")
            else:
                logger.error(f"❌ 调度器状态查询返回错误状态码: {response.status_code}")
        except Exception as e:
            logger.error(f"❌ 调度器状态查询异常: {e}")
        
        # 测试启动调度器
        try:
            response = requests.post(f"{self.api_base}/scheduler/start", timeout=10)
            if response.status_code == 200:
                data = response.json()
                if data.get("success"):
                    logger.info("✅ 调度器启动成功")
                    success_count += 1
                else:
                    logger.error(f"❌ 调度器启动失败: {data}")
            else:
                logger.error(f"❌ 调度器启动返回错误状态码: {response.status_code}")
        except Exception as e:
            logger.error(f"❌ 调度器启动异常: {e}")
        
        return success_count >= 1
    
    def test_system_status(self) -> bool:
        """测试系统状态"""
        logger.info("🖥️ 测试系统状态...")
        
        try:
            response = requests.get(f"{self.api_base}/status", timeout=10)
            
            if response.status_code == 200:
                data = response.json()
                if data.get("success"):
                    system_status = data.get("system_status", {})
                    logger.info("✅ 系统状态查询成功")
                    logger.info(f"   组件状态: {system_status.get('components', {})}")
                    logger.info(f"   Ollama状态: {system_status.get('ollama', 'unknown')}")
                    logger.info(f"   TuShare Token: {system_status.get('tushare_token', 'unknown')}")
                    return True
                else:
                    logger.error(f"❌ 系统状态查询失败: {data}")
                    return False
            else:
                logger.error(f"❌ 系统状态查询返回错误状态码: {response.status_code}")
                return False
                
        except Exception as e:
            logger.error(f"❌ 系统状态查询异常: {e}")
            return False
    
    def test_list_trained_models(self) -> bool:
        """测试列出已训练模型"""
        logger.info("🤖 测试列出已训练模型...")
        
        try:
            response = requests.get(f"{self.api_base}/models/list", timeout=10)
            
            if response.status_code == 200:
                data = response.json()
                if data.get("success"):
                    models = data.get("models", [])
                    logger.info(f"✅ 模型列表查询成功，共 {len(models)} 个模型")
                    
                    # 显示前3个模型
                    for i, model in enumerate(models[:3]):
                        logger.info(f"   {i+1}. {model.get('model_name', 'N/A')} - {model.get('created_at', 'N/A')}")
                    
                    return True
                else:
                    logger.error(f"❌ 模型列表查询失败: {data}")
                    return False
            else:
                logger.error(f"❌ 模型列表查询返回错误状态码: {response.status_code}")
                return False
                
        except Exception as e:
            logger.error(f"❌ 模型列表查询异常: {e}")
            return False
    
    def run_comprehensive_test(self) -> Dict[str, bool]:
        """运行综合测试"""
        logger.info("🚀 开始运行策略训练系统综合测试")
        logger.info("=" * 60)
        
        test_results = {}
        
        # 基础功能测试
        logger.info("1️⃣ 基础功能测试")
        test_results["health_check"] = self.test_health_check()
        test_results["get_strategies"] = self.test_get_strategies()
        test_results["system_status"] = self.test_system_status()
        
        # 数据生成测试
        logger.info("\n2️⃣ 数据生成测试")
        test_results["generate_dataset"] = self.test_generate_single_dataset()
        
        # 推荐生成测试
        logger.info("\n3️⃣ 推荐生成测试")
        test_results["generate_recommendations"] = self.test_generate_recommendations()
        
        # 调度器测试
        logger.info("\n4️⃣ 调度器测试")
        test_results["scheduler_operations"] = self.test_scheduler_operations()
        
        # 模型管理测试
        logger.info("\n5️⃣ 模型管理测试")
        test_results["list_models"] = self.test_list_trained_models()
        
        # 结果汇总
        logger.info("\n📊 测试结果汇总")
        logger.info("=" * 60)
        
        passed_tests = sum(1 for result in test_results.values() if result)
        total_tests = len(test_results)
        
        for test_name, result in test_results.items():
            status = "✅ PASS" if result else "❌ FAIL"
            logger.info(f"{test_name}: {status}")
        
        logger.info("=" * 60)
        logger.info(f"总体结果: {passed_tests}/{total_tests} 通过 ({passed_tests/total_tests*100:.1f}%)")
        
        if passed_tests == total_tests:
            logger.info("🎉 所有测试通过！策略训练系统运行正常")
        elif passed_tests >= total_tests * 0.8:
            logger.info("⚠️ 大部分测试通过，系统基本正常，部分功能可能需要调整")
        else:
            logger.error("🚨 多项测试失败，系统可能存在问题，请检查配置和服务状态")
        
        return test_results

def main():
    """主函数"""
    import argparse
    
    parser = argparse.ArgumentParser(description="策略训练系统测试")
    parser.add_argument("--host", type=str, default="localhost", help="服务器主机")
    parser.add_argument("--port", type=int, default=5005, help="服务器端口")
    parser.add_argument("--test", type=str, choices=[
        "health", "strategies", "dataset", "recommendations", 
        "scheduler", "status", "models", "all"
    ], default="all", help="指定测试类型")
    
    args = parser.parse_args()
    
    base_url = f"http://{args.host}:{args.port}"
    tester = StrategyTrainingSystemTester(base_url)
    
    logger.info(f"🔗 连接到服务器: {base_url}")
    
    # 首先检查服务器是否可达
    try:
        response = requests.get(base_url, timeout=5)
        logger.info("✅ 服务器连接成功")
    except Exception as e:
        logger.error(f"❌ 无法连接到服务器 {base_url}: {e}")
        logger.error("请确保服务器正在运行: python unified_app.py")
        return
    
    # 执行指定测试
    if args.test == "all":
        tester.run_comprehensive_test()
    elif args.test == "health":
        tester.test_health_check()
    elif args.test == "strategies":
        tester.test_get_strategies()
    elif args.test == "dataset":
        tester.test_generate_single_dataset()
    elif args.test == "recommendations":
        tester.test_generate_recommendations()
    elif args.test == "scheduler":
        tester.test_scheduler_operations()
    elif args.test == "status":
        tester.test_system_status()
    elif args.test == "models":
        tester.test_list_trained_models()

if __name__ == "__main__":
    main()