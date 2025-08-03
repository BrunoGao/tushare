#!/usr/bin/env python3
"""
M2 Ultra性能测试和基准测试脚本
测试优化后的AI训练系统在M2 Ultra上的性能表现
"""

import time
import numpy as np
import pandas as pd
from datetime import datetime
import logging
import sys
import os

# 添加项目路径
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

from utils.m2_ultra_config import M2UltraConfig, get_optimal_config
from utils.performance_monitor import M2UltraPerformanceMonitor
from ai.unified_trainer import unified_trainer

# 配置日志
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    handlers=[
        logging.StreamHandler(),
        logging.FileHandler('logs/m2_ultra_benchmark.log')
    ]
)
logger = logging.getLogger(__name__)

def cpu_intensive_task(n):
    """CPU密集型任务 - 移到模块级别以支持multiprocessing pickling"""
    return sum(i*i for i in range(n))

class M2UltraBenchmark:
    """M2 Ultra基准测试"""
    
    def __init__(self):
        self.config = M2UltraConfig()
        self.monitor = M2UltraPerformanceMonitor(log_interval=0.5)
        self.results = {}
        
        # 确保日志目录存在
        os.makedirs('logs', exist_ok=True)
        
        logger.info("🚀 M2 Ultra基准测试初始化完成")
        
    def run_all_benchmarks(self):
        """运行所有基准测试"""
        logger.info("开始运行M2 Ultra基准测试套件")
        
        # 打印系统信息
        self.config.print_system_info()
        
        # 启动性能监控
        self.monitor.start_monitoring()
        
        try:
            # 测试1: 配置加载性能
            self.test_config_loading()
            
            # 测试2: 数据处理性能
            self.test_data_processing()
            
            # 测试3: 机器学习模型训练性能
            self.test_ml_training()
            
            # 测试4: 内存利用率测试
            self.test_memory_utilization()
            
            # 测试5: 并行处理性能
            self.test_parallel_processing()
            
            # 生成综合报告
            self.generate_performance_report()
            
        finally:
            self.monitor.stop_monitoring()
    
    def test_config_loading(self):
        """测试配置加载性能"""
        logger.info("📋 测试1: 配置加载性能")
        
        start_time = time.time()
        
        # 测试各种配置加载
        configs = {}
        task_types = ['data_processing', 'traditional_ml', 'deep_learning', 'model_optimization']
        
        for task_type in task_types:
            config_start = time.time()
            configs[task_type] = get_optimal_config(task_type)
            config_time = time.time() - config_start
            logger.info(f"  {task_type}配置加载: {config_time*1000:.2f}ms")
        
        # 测试专用配置
        xgb_config = self.config.get_xgboost_config()
        lgb_config = self.config.get_lightgbm_config()
        pytorch_config = self.config.get_pytorch_config()
        
        total_time = time.time() - start_time
        
        self.results['config_loading'] = {
            'total_time_ms': total_time * 1000,
            'configs_loaded': len(configs) + 3,
            'avg_time_per_config_ms': (total_time * 1000) / (len(configs) + 3),
            'is_m2_ultra': self.config.is_m2_ultra
        }
        
        logger.info(f"✅ 配置加载测试完成: {total_time*1000:.2f}ms")
    
    def test_data_processing(self):
        """测试数据处理性能"""
        logger.info("📊 测试2: 数据处理性能")
        
        # 生成测试数据
        data_sizes = [1000, 5000, 10000, 50000]
        processing_results = {}
        
        for size in data_sizes:
            logger.info(f"  测试数据大小: {size}")
            
            # 生成模拟股票数据
            start_time = time.time()
            
            test_data = pd.DataFrame({
                'close': np.random.randn(size) * 100 + 1000,
                'volume': np.random.randint(1000, 100000, size),
                'high': np.random.randn(size) * 100 + 1020,
                'low': np.random.randn(size) * 100 + 980,
                'open': np.random.randn(size) * 100 + 1000,
            })
            
            # 数据处理操作
            processed_data = self._process_test_data(test_data)
            
            processing_time = time.time() - start_time
            throughput = size / processing_time
            
            processing_results[size] = {
                'processing_time_ms': processing_time * 1000,
                'throughput_rows_per_sec': throughput,
                'memory_usage_mb': test_data.memory_usage(deep=True).sum() / (1024**2)
            }
            
            logger.info(f"    处理时间: {processing_time*1000:.2f}ms, 吞吐量: {throughput:.0f} rows/sec")
        
        self.results['data_processing'] = processing_results
        logger.info("✅ 数据处理性能测试完成")
    
    def _process_test_data(self, data: pd.DataFrame) -> pd.DataFrame:
        """处理测试数据"""
        # 模拟真实的数据处理操作
        data['returns'] = data['close'].pct_change()
        data['ma5'] = data['close'].rolling(5).mean()
        data['ma20'] = data['close'].rolling(20).mean()
        data['volatility'] = data['returns'].rolling(20).std()
        data['rsi'] = self._calculate_rsi(data['close'])
        
        return data.fillna(0)
    
    def _calculate_rsi(self, prices: pd.Series, period: int = 14) -> pd.Series:
        """计算RSI指标"""
        delta = prices.diff()
        gain = delta.where(delta > 0, 0)
        loss = -delta.where(delta < 0, 0)
        
        avg_gain = gain.rolling(period).mean()
        avg_loss = loss.rolling(period).mean()
        
        rs = avg_gain / avg_loss
        rsi = 100 - (100 / (1 + rs))
        
        return rsi
    
    def test_ml_training(self):
        """测试机器学习训练性能"""
        logger.info("🤖 测试3: 机器学习训练性能")
        
        # 生成测试数据集
        sample_sizes = [1000, 5000, 10000]
        feature_counts = [50, 100, 200]
        
        ml_results = {}
        
        for n_samples in sample_sizes:
            for n_features in feature_counts:
                test_key = f"{n_samples}x{n_features}"
                logger.info(f"  测试: {test_key} (samples x features)")
                
                # 生成模拟训练数据
                X = np.random.randn(n_samples, n_features)
                y = np.random.randint(0, 3, n_samples)  # 3分类
                
                # 测试训练时间
                start_time = time.time()
                
                # 使用sklearn模型快速测试
                from sklearn.ensemble import RandomForestClassifier
                from sklearn.model_selection import cross_val_score
                
                # 获取M2优化配置
                ml_config = get_optimal_config('traditional_ml')
                n_jobs = ml_config.get('sklearn_jobs', -1)
                
                # 训练随机森林
                rf = RandomForestClassifier(
                    n_estimators=50,
                    n_jobs=n_jobs,
                    random_state=42
                )
                
                # 交叉验证
                scores = cross_val_score(rf, X, y, cv=3, n_jobs=n_jobs)
                
                training_time = time.time() - start_time
                
                ml_results[test_key] = {
                    'training_time_ms': training_time * 1000,
                    'samples_per_second': n_samples / training_time,
                    'cv_score_mean': scores.mean(),
                    'cv_score_std': scores.std(),
                    'n_jobs_used': n_jobs
                }
                
                logger.info(f"    训练时间: {training_time*1000:.2f}ms, "
                           f"CV分数: {scores.mean():.3f}±{scores.std():.3f}")
        
        self.results['ml_training'] = ml_results
        logger.info("✅ 机器学习训练性能测试完成")
    
    def test_memory_utilization(self):
        """测试内存利用率"""
        logger.info("💾 测试4: 内存利用率测试")
        
        # 获取内存配置
        memory_config = self.config.get_memory_config()
        current_metrics = self.monitor.get_current_metrics()
        
        memory_info = current_metrics.get('memory', {})
        
        memory_results = {
            'total_memory_gb': memory_info.get('total_gb', 0),
            'available_memory_gb': memory_info.get('available_gb', 0),
            'used_memory_gb': memory_info.get('used_gb', 0),
            'memory_usage_percent': memory_info.get('usage_percent', 0),
            'cache_size_gb': memory_config.get('cache_size_gb', 0),
            'max_memory_usage_allowed': memory_config.get('max_memory_usage', 0),
            'memory_optimization_enabled': memory_config.get('enable_memory_mapping', False)
        }
        
        # 内存利用率评估
        available_ratio = memory_results['available_memory_gb'] / memory_results['total_memory_gb']
        utilization_efficiency = 1 - available_ratio
        
        memory_results['utilization_efficiency'] = utilization_efficiency
        memory_results['memory_optimization_score'] = min(100, utilization_efficiency * 100)
        
        self.results['memory_utilization'] = memory_results
        
        logger.info(f"  总内存: {memory_results['total_memory_gb']:.1f}GB")
        logger.info(f"  可用内存: {memory_results['available_memory_gb']:.1f}GB")
        logger.info(f"  内存利用率: {memory_results['memory_usage_percent']:.1f}%")
        logger.info(f"  优化分数: {memory_results['memory_optimization_score']:.1f}/100")
        logger.info("✅ 内存利用率测试完成")
    
    def test_parallel_processing(self):
        """测试并行处理性能"""
        logger.info("⚡ 测试5: 并行处理性能")
        
        from concurrent.futures import ProcessPoolExecutor, ThreadPoolExecutor
        import multiprocessing as mp
        
        # 获取并行配置
        parallel_config = self.config.get_parallel_config()
        max_workers = parallel_config.get('max_workers', 4)
        
        task_sizes = [10000, 50000, 100000]
        parallel_results = {}
        
        for task_size in task_sizes:
            logger.info(f"  测试任务大小: {task_size}")
            
            # 串行执行
            start_time = time.time()
            serial_results = [cpu_intensive_task(task_size) for _ in range(8)]
            serial_time = time.time() - start_time
            
            # 并行执行 (进程池)
            start_time = time.time()
            with ProcessPoolExecutor(max_workers=max_workers) as executor:
                parallel_results_proc = list(executor.map(cpu_intensive_task, [task_size] * 8))
            parallel_time_proc = time.time() - start_time
            
            # 并行执行 (线程池)
            start_time = time.time()
            with ThreadPoolExecutor(max_workers=max_workers) as executor:
                parallel_results_thread = list(executor.map(cpu_intensive_task, [task_size] * 8))
            parallel_time_thread = time.time() - start_time
            
            speedup_proc = serial_time / parallel_time_proc
            speedup_thread = serial_time / parallel_time_thread
            
            parallel_results[task_size] = {
                'serial_time_ms': serial_time * 1000,
                'parallel_time_proc_ms': parallel_time_proc * 1000,
                'parallel_time_thread_ms': parallel_time_thread * 1000,
                'speedup_process': speedup_proc,
                'speedup_thread': speedup_thread,
                'max_workers_used': max_workers,
                'efficiency_process': speedup_proc / max_workers,
                'efficiency_thread': speedup_thread / max_workers
            }
            
            logger.info(f"    进程池加速比: {speedup_proc:.2f}x, 效率: {speedup_proc/max_workers:.2f}")
            logger.info(f"    线程池加速比: {speedup_thread:.2f}x, 效率: {speedup_thread/max_workers:.2f}")
        
        self.results['parallel_processing'] = parallel_results
        logger.info("✅ 并行处理性能测试完成")
    
    def generate_performance_report(self):
        """生成性能报告"""
        logger.info("📊 生成性能报告")
        
        # 获取效率报告
        efficiency_report = self.monitor.get_m2_ultra_efficiency_report()
        
        # 生成综合报告
        report = {
            'benchmark_info': {
                'timestamp': datetime.now().isoformat(),
                'system_info': {
                    'is_m2_ultra': self.config.is_m2_ultra,
                    'cpu_cores': self.config.cpu_count,
                    'available_memory_gb': self.config.available_memory,
                },
                'test_duration_minutes': 10  # 估算
            },
            'performance_results': self.results,
            'efficiency_report': efficiency_report,
            'recommendations': self._generate_recommendations()
        }
        
        # 保存报告
        report_path = f"logs/m2_ultra_benchmark_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
        
        import json
        with open(report_path, 'w', encoding='utf-8') as f:
            json.dump(report, f, ensure_ascii=False, indent=2)
        
        # 打印摘要
        self._print_performance_summary(report)
        
        logger.info(f"✅ 性能报告已保存到: {report_path}")
    
    def _generate_recommendations(self) -> list:
        """生成优化建议"""
        recommendations = []
        
        if self.config.is_m2_ultra:
            recommendations.extend([
                "M2 Ultra已启用，性能优化配置生效",
                "建议使用MPS进行深度学习加速",
                "充分利用192GB大内存优势"
            ])
        else:
            recommendations.extend([
                "当前系统不是M2 Ultra，使用通用优化配置",
                "考虑升级到Apple Silicon获得更好性能",
                "在当前硬件上已应用最优配置"
            ])
        
        # 基于测试结果的建议
        if 'parallel_processing' in self.results:
            avg_efficiency = np.mean([
                result.get('efficiency_process', 0) 
                for result in self.results['parallel_processing'].values()
            ])
            
            if avg_efficiency < 0.5:
                recommendations.append("并行效率较低，建议检查任务粒度和负载均衡")
            elif avg_efficiency > 0.8:
                recommendations.append("并行效率很高，当前配置效果良好")
        
        return recommendations
    
    def _print_performance_summary(self, report: dict):
        """打印性能摘要"""
        print("\n" + "="*60)
        print("🏆 M2 Ultra 性能测试报告摘要")
        print("="*60)
        
        system_info = report['benchmark_info']['system_info']
        print(f"系统类型: {'M2 Ultra' if system_info['is_m2_ultra'] else '其他'}")
        print(f"CPU核心: {system_info['cpu_cores']}")
        print(f"可用内存: {system_info['available_memory_gb']:.1f} GB")
        
        # 性能亮点
        if 'ml_training' in self.results:
            ml_results = self.results['ml_training']
            best_throughput = max([r.get('samples_per_second', 0) for r in ml_results.values()])
            print(f"最大训练吞吐量: {best_throughput:.0f} samples/sec")
        
        if 'parallel_processing' in self.results:
            parallel_results = self.results['parallel_processing']
            best_speedup = max([r.get('speedup_process', 0) for r in parallel_results.values()])
            print(f"最大并行加速比: {best_speedup:.2f}x")
        
        # 效率报告
        efficiency_report = report.get('efficiency_report', {})
        if efficiency_report:
            overall_eff = efficiency_report.get('overall_efficiency', 0)
            print(f"整体效率分数: {overall_eff:.1f}%")
        
        # 建议
        recommendations = report.get('recommendations', [])
        if recommendations:
            print("\n💡 优化建议:")
            for i, rec in enumerate(recommendations[:5], 1):
                print(f"  {i}. {rec}")
        
        print("="*60)

def main():
    """主函数"""
    print("🚀 启动M2 Ultra性能基准测试")
    
    try:
        benchmark = M2UltraBenchmark()
        benchmark.run_all_benchmarks()
        
        print("\n✅ 所有基准测试完成！")
        
    except KeyboardInterrupt:
        print("\n⚠️ 测试被用户中断")
    except Exception as e:
        logger.error(f"基准测试失败: {e}")
        print(f"\n❌ 测试失败: {e}")

if __name__ == "__main__":
    main()