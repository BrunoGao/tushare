"""
M2 Ultra 性能监控工具
实时监控系统资源使用情况和性能指标
"""

import time
import psutil
import threading
from typing import Dict, List, Optional, Callable
from datetime import datetime, timedelta
import json
import logging
from pathlib import Path

logger = logging.getLogger(__name__)

class PerformanceMonitor:
    """性能监控器"""
    
    def __init__(self, log_interval: float = 1.0, save_path: Optional[str] = None):
        self.log_interval = log_interval
        self.save_path = Path(save_path) if save_path else Path("logs/performance.json")
        self.save_path.parent.mkdir(parents=True, exist_ok=True)
        
        self.monitoring = False
        self.monitor_thread = None
        self.performance_data = []
        self.callbacks = []
        
        # 基准性能指标
        self.baseline_metrics = self._get_baseline_metrics()
        
    def _get_baseline_metrics(self) -> Dict:
        """获取基准性能指标"""
        return {
            'cpu_count': psutil.cpu_count(),
            'memory_total_gb': psutil.virtual_memory().total / (1024**3),
            'disk_info': self._get_disk_info(),
            'timestamp': datetime.now().isoformat()
        }
    
    def _get_disk_info(self) -> Dict:
        """获取磁盘信息"""
        try:
            disk_usage = psutil.disk_usage('/')
            return {
                'total_gb': disk_usage.total / (1024**3),
                'free_gb': disk_usage.free / (1024**3),
                'used_gb': disk_usage.used / (1024**3),
                'usage_percent': (disk_usage.used / disk_usage.total) * 100
            }
        except Exception as e:
            logger.error(f"获取磁盘信息失败: {e}")
            return {}
    
    def _get_gpu_info(self) -> Dict:
        """获取GPU信息（MPS）"""
        gpu_info = {
            'mps_available': False,
            'mps_usage': 0.0,
            'gpu_memory_used': 0.0,
            'gpu_memory_total': 0.0
        }
        
        try:
            import torch
            if torch.backends.mps.is_available():
                gpu_info['mps_available'] = True
                # MPS不支持直接获取内存使用情况，但可以检查是否正在使用
                gpu_info['mps_in_use'] = torch.backends.mps.is_built()
        except ImportError:
            pass
        
        return gpu_info
    
    def _collect_metrics(self) -> Dict:
        """收集当前性能指标"""
        try:
            # CPU指标
            cpu_percent = psutil.cpu_percent(interval=0.1)
            cpu_per_core = psutil.cpu_percent(percpu=True, interval=0.1)
            cpu_freq = psutil.cpu_freq()
            
            # 内存指标
            memory = psutil.virtual_memory()
            swap = psutil.swap_memory()
            
            # 磁盘I/O
            disk_io = psutil.disk_io_counters()
            
            # 网络I/O
            net_io = psutil.net_io_counters()
            
            # 进程信息
            current_process = psutil.Process()
            process_info = {
                'cpu_percent': current_process.cpu_percent(),
                'memory_mb': current_process.memory_info().rss / (1024**2),
                'memory_percent': current_process.memory_percent(),
                'num_threads': current_process.num_threads(),
                'open_files': len(current_process.open_files()),
            }
            
            # GPU信息
            gpu_info = self._get_gpu_info()
            
            metrics = {
                'timestamp': datetime.now().isoformat(),
                'cpu': {
                    'usage_percent': cpu_percent,
                    'usage_per_core': cpu_per_core,
                    'frequency_mhz': cpu_freq.current if cpu_freq else 0,
                    'load_average': psutil.getloadavg() if hasattr(psutil, 'getloadavg') else [0, 0, 0]
                },
                'memory': {
                    'total_gb': memory.total / (1024**3),
                    'available_gb': memory.available / (1024**3),
                    'used_gb': memory.used / (1024**3),
                    'usage_percent': memory.percent,
                    'swap_used_gb': swap.used / (1024**3),
                    'swap_percent': swap.percent
                },
                'disk': {
                    'read_mb_per_sec': (disk_io.read_bytes / (1024**2)) if disk_io else 0,
                    'write_mb_per_sec': (disk_io.write_bytes / (1024**2)) if disk_io else 0,
                    'read_count': disk_io.read_count if disk_io else 0,
                    'write_count': disk_io.write_count if disk_io else 0
                },
                'network': {
                    'bytes_sent_mb': net_io.bytes_sent / (1024**2) if net_io else 0,
                    'bytes_recv_mb': net_io.bytes_recv / (1024**2) if net_io else 0,
                    'packets_sent': net_io.packets_sent if net_io else 0,
                    'packets_recv': net_io.packets_recv if net_io else 0
                },
                'process': process_info,
                'gpu': gpu_info
            }
            
            return metrics
            
        except Exception as e:
            logger.error(f"收集性能指标失败: {e}")
            return {'timestamp': datetime.now().isoformat(), 'error': str(e)}
    
    def start_monitoring(self):
        """开始监控"""
        if self.monitoring:
            logger.warning("监控已在运行")
            return
        
        self.monitoring = True
        self.monitor_thread = threading.Thread(target=self._monitor_loop, daemon=True)
        self.monitor_thread.start()
        logger.info(f"✅ 性能监控已启动，记录间隔: {self.log_interval}秒")
    
    def stop_monitoring(self):
        """停止监控"""
        if not self.monitoring:
            return
        
        self.monitoring = False
        if self.monitor_thread:
            self.monitor_thread.join(timeout=5)
        
        # 保存数据
        self.save_data()
        logger.info("✅ 性能监控已停止")
    
    def _monitor_loop(self):
        """监控循环"""
        while self.monitoring:
            try:
                metrics = self._collect_metrics()
                self.performance_data.append(metrics)
                
                # 执行回调函数
                for callback in self.callbacks:
                    try:
                        callback(metrics)
                    except Exception as e:
                        logger.error(f"回调函数执行失败: {e}")
                
                # 限制数据大小，保留最近的数据
                if len(self.performance_data) > 10000:
                    self.performance_data = self.performance_data[-5000:]
                
                time.sleep(self.log_interval)
                
            except Exception as e:
                logger.error(f"监控循环错误: {e}")
                time.sleep(1)
    
    def add_callback(self, callback: Callable[[Dict], None]):
        """添加性能数据回调函数"""
        self.callbacks.append(callback)
    
    def get_current_metrics(self) -> Dict:
        """获取当前性能指标"""
        return self._collect_metrics()
    
    def get_recent_data(self, minutes: int = 5) -> List[Dict]:
        """获取最近N分钟的数据"""
        cutoff_time = datetime.now() - timedelta(minutes=minutes)
        
        recent_data = []
        for data in reversed(self.performance_data):
            try:
                data_time = datetime.fromisoformat(data['timestamp'])
                if data_time >= cutoff_time:
                    recent_data.append(data)
                else:
                    break
            except (KeyError, ValueError):
                continue
        
        return list(reversed(recent_data))
    
    def get_performance_summary(self, minutes: int = 10) -> Dict:
        """获取性能摘要"""
        recent_data = self.get_recent_data(minutes)
        
        if not recent_data:
            return {}
        
        # 计算平均值和峰值
        cpu_usage = [d.get('cpu', {}).get('usage_percent', 0) for d in recent_data]
        memory_usage = [d.get('memory', {}).get('usage_percent', 0) for d in recent_data]
        memory_used_gb = [d.get('memory', {}).get('used_gb', 0) for d in recent_data]
        
        summary = {
            'time_period_minutes': minutes,
            'data_points': len(recent_data),
            'cpu': {
                'avg_usage_percent': sum(cpu_usage) / len(cpu_usage) if cpu_usage else 0,
                'max_usage_percent': max(cpu_usage) if cpu_usage else 0,
                'min_usage_percent': min(cpu_usage) if cpu_usage else 0
            },
            'memory': {
                'avg_usage_percent': sum(memory_usage) / len(memory_usage) if memory_usage else 0,
                'max_usage_percent': max(memory_usage) if memory_usage else 0,
                'peak_used_gb': max(memory_used_gb) if memory_used_gb else 0,
                'avg_used_gb': sum(memory_used_gb) / len(memory_used_gb) if memory_used_gb else 0
            },
            'baseline': self.baseline_metrics,
            'generated_at': datetime.now().isoformat()
        }
        
        return summary
    
    def save_data(self):
        """保存监控数据到文件"""
        try:
            output_data = {
                'baseline_metrics': self.baseline_metrics,
                'performance_data': self.performance_data,
                'summary': self.get_performance_summary(30),  # 30分钟摘要
                'saved_at': datetime.now().isoformat()
            }
            
            with open(self.save_path, 'w', encoding='utf-8') as f:
                json.dump(output_data, f, ensure_ascii=False, indent=2)
            
            logger.info(f"✅ 性能数据已保存到: {self.save_path}")
            
        except Exception as e:
            logger.error(f"保存性能数据失败: {e}")
    
    def print_current_status(self):
        """打印当前状态"""
        metrics = self.get_current_metrics()
        
        print("\n" + "="*50)
        print("📊 当前系统性能状态")
        print("="*50)
        
        if 'cpu' in metrics:
            cpu = metrics['cpu']
            print(f"CPU使用率: {cpu.get('usage_percent', 0):.1f}%")
            print(f"CPU频率: {cpu.get('frequency_mhz', 0):.0f} MHz")
        
        if 'memory' in metrics:
            memory = metrics['memory']
            print(f"内存使用率: {memory.get('usage_percent', 0):.1f}%")
            print(f"内存使用量: {memory.get('used_gb', 0):.1f} GB")
            print(f"可用内存: {memory.get('available_gb', 0):.1f} GB")
        
        if 'gpu' in metrics:
            gpu = metrics['gpu']
            mps_status = "✅ 可用" if gpu.get('mps_available') else "❌ 不可用"
            print(f"MPS状态: {mps_status}")
        
        if 'process' in metrics:
            process = metrics['process']
            print(f"当前进程CPU: {process.get('cpu_percent', 0):.1f}%")
            print(f"当前进程内存: {process.get('memory_mb', 0):.1f} MB")
            print(f"线程数: {process.get('num_threads', 0)}")
        
        print("="*50)

class M2UltraPerformanceMonitor(PerformanceMonitor):
    """M2 Ultra专用性能监控器"""
    
    def __init__(self, log_interval: float = 1.0, save_path: Optional[str] = None):
        super().__init__(log_interval, save_path)
        self.m2_ultra_metrics = {
            'target_cpu_utilization': 0.85,  # M2 Ultra目标CPU利用率
            'target_memory_utilization': 0.8,  # 192GB内存的目标利用率
            'target_gpu_utilization': 0.7,   # MPS目标利用率
        }
        
        # 添加M2 Ultra特定的回调
        self.add_callback(self._m2_ultra_performance_callback)
    
    def _m2_ultra_performance_callback(self, metrics: Dict):
        """M2 Ultra性能回调"""
        try:
            cpu_usage = metrics.get('cpu', {}).get('usage_percent', 0)
            memory_usage = metrics.get('memory', {}).get('usage_percent', 0)
            
            # 性能警告
            if cpu_usage > 95:
                logger.warning(f"⚠️ CPU使用率过高: {cpu_usage:.1f}%")
            elif cpu_usage < 20:
                logger.info(f"💡 CPU利用率较低: {cpu_usage:.1f}%，可以增加并行度")
            
            if memory_usage > 90:
                logger.warning(f"⚠️ 内存使用率过高: {memory_usage:.1f}%")
            elif memory_usage < 40:
                logger.info(f"💡 内存利用率较低: {memory_usage:.1f}%，可以增加batch size")
            
        except Exception as e:
            logger.error(f"M2 Ultra性能回调失败: {e}")
    
    def get_m2_ultra_efficiency_report(self) -> Dict:
        """获取M2 Ultra效率报告"""
        summary = self.get_performance_summary(15)
        
        if not summary:
            return {}
        
        cpu_efficiency = min(100, (summary['cpu']['avg_usage_percent'] / self.m2_ultra_metrics['target_cpu_utilization']) * 100)
        memory_efficiency = min(100, (summary['memory']['avg_usage_percent'] / self.m2_ultra_metrics['target_memory_utilization']) * 100)
        
        report = {
            'overall_efficiency': (cpu_efficiency + memory_efficiency) / 2,
            'cpu_efficiency': cpu_efficiency,
            'memory_efficiency': memory_efficiency,
            'recommendations': [],
            'performance_summary': summary
        }
        
        # 生成建议
        if cpu_efficiency < 50:
            report['recommendations'].append("增加并行worker数量以提高CPU利用率")
        if memory_efficiency < 30:
            report['recommendations'].append("增加batch size或缓存大小以充分利用大内存")
        if cpu_efficiency > 95:
            report['recommendations'].append("CPU接近满载，考虑减少并行度或优化算法")
        if memory_efficiency > 90:
            report['recommendations'].append("内存使用率很高，注意内存泄漏")
        
        return report

# 全局监控实例
performance_monitor = M2UltraPerformanceMonitor()

def start_monitoring():
    """启动性能监控"""
    performance_monitor.start_monitoring()

def stop_monitoring():
    """停止性能监控"""
    performance_monitor.stop_monitoring()

def get_current_performance() -> Dict:
    """获取当前性能状态"""
    return performance_monitor.get_current_metrics()

if __name__ == "__main__":
    # 测试性能监控
    monitor = M2UltraPerformanceMonitor(log_interval=2.0)
    
    print("启动性能监控测试...")
    monitor.start_monitoring()
    
    try:
        time.sleep(10)  # 监控10秒
        monitor.print_current_status()
        
        # 获取效率报告
        if hasattr(monitor, 'get_m2_ultra_efficiency_report'):
            efficiency_report = monitor.get_m2_ultra_efficiency_report()
            if efficiency_report:
                print(f"\n📈 M2 Ultra效率报告:")
                print(f"整体效率: {efficiency_report.get('overall_efficiency', 0):.1f}%")
                print(f"CPU效率: {efficiency_report.get('cpu_efficiency', 0):.1f}%")
                print(f"内存效率: {efficiency_report.get('memory_efficiency', 0):.1f}%")
                
                recommendations = efficiency_report.get('recommendations', [])
                if recommendations:
                    print("\n💡 优化建议:")
                    for i, rec in enumerate(recommendations, 1):
                        print(f"  {i}. {rec}")
        
    finally:
        monitor.stop_monitoring()