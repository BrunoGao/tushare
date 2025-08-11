#!/usr/bin/env python3
"""
缓存和性能监控模块
实现Redis缓存、内存缓存和系统性能监控
"""

import os
import time
import json
import logging
import hashlib
import pickle
import psutil
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Any, Union, Callable
from dataclasses import dataclass, asdict
from functools import wraps
from collections import defaultdict, deque
from threading import Lock
import weakref

try:
    import redis
    REDIS_AVAILABLE = True
except ImportError:
    REDIS_AVAILABLE = False

# 设置日志
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

@dataclass
class CacheStatistics:
    """缓存统计信息"""
    hits: int = 0
    misses: int = 0
    hit_rate: float = 0.0
    total_requests: int = 0
    cache_size: int = 0
    memory_usage: int = 0

@dataclass
class PerformanceMetrics:
    """性能指标"""
    timestamp: str
    cpu_usage: float
    memory_usage: float
    disk_usage: float
    network_io: Dict[str, int]
    database_connections: int
    cache_hit_rate: float
    active_requests: int
    response_time_avg: float

class MemoryCache:
    """内存缓存管理器"""
    
    def __init__(self, max_size: int = 1000, default_ttl: int = 3600):
        """
        初始化内存缓存
        
        Args:
            max_size: 最大缓存条目数
            default_ttl: 默认过期时间（秒）
        """
        self.max_size = max_size
        self.default_ttl = default_ttl
        self._cache = {}
        self._access_times = {}
        self._expire_times = {}
        self._lock = Lock()
        self.stats = CacheStatistics()
    
    def get(self, key: str) -> Optional[Any]:
        """获取缓存值"""
        with self._lock:
            current_time = time.time()
            
            # 检查键是否存在且未过期
            if key in self._cache:
                if key in self._expire_times and current_time > self._expire_times[key]:
                    # 过期，删除
                    self._remove_key(key)
                    self.stats.misses += 1
                    return None
                
                # 更新访问时间
                self._access_times[key] = current_time
                self.stats.hits += 1
                return self._cache[key]
            
            self.stats.misses += 1
            return None
    
    def set(self, key: str, value: Any, ttl: int = None) -> bool:
        """设置缓存值"""
        with self._lock:
            current_time = time.time()
            ttl = ttl or self.default_ttl
            
            # 如果缓存已满，删除最久未访问的项目
            if len(self._cache) >= self.max_size and key not in self._cache:
                self._evict_lru()
            
            self._cache[key] = value
            self._access_times[key] = current_time
            self._expire_times[key] = current_time + ttl
            
            self._update_stats()
            return True
    
    def delete(self, key: str) -> bool:
        """删除缓存值"""
        with self._lock:
            if key in self._cache:
                self._remove_key(key)
                self._update_stats()
                return True
            return False
    
    def clear(self):
        """清空缓存"""
        with self._lock:
            self._cache.clear()
            self._access_times.clear()
            self._expire_times.clear()
            self._update_stats()
    
    def _remove_key(self, key: str):
        """移除键（不加锁）"""
        self._cache.pop(key, None)
        self._access_times.pop(key, None)
        self._expire_times.pop(key, None)
    
    def _evict_lru(self):
        """删除最久未访问的项目"""
        if not self._access_times:
            return
        
        # 找到最久未访问的键
        lru_key = min(self._access_times.items(), key=lambda x: x[1])[0]
        self._remove_key(lru_key)
    
    def _update_stats(self):
        """更新统计信息"""
        self.stats.cache_size = len(self._cache)
        self.stats.total_requests = self.stats.hits + self.stats.misses
        
        if self.stats.total_requests > 0:
            self.stats.hit_rate = self.stats.hits / self.stats.total_requests
        
        # 估算内存使用量
        self.stats.memory_usage = sum(
            len(pickle.dumps(value)) for value in self._cache.values()
        ) if self._cache else 0
    
    def get_stats(self) -> CacheStatistics:
        """获取缓存统计"""
        with self._lock:
            self._update_stats()
            return self.stats
    
    def cleanup_expired(self):
        """清理过期项目"""
        with self._lock:
            current_time = time.time()
            expired_keys = [
                key for key, expire_time in self._expire_times.items()
                if current_time > expire_time
            ]
            
            for key in expired_keys:
                self._remove_key(key)
            
            if expired_keys:
                logger.debug(f"清理了{len(expired_keys)}个过期缓存项目")
                self._update_stats()

class RedisCache:
    """Redis缓存管理器"""
    
    def __init__(self, host: str = 'localhost', port: int = 6379, 
                 db: int = 0, password: str = None, default_ttl: int = 3600):
        """
        初始化Redis缓存
        
        Args:
            host: Redis主机
            port: Redis端口
            db: Redis数据库编号
            password: Redis密码
            default_ttl: 默认过期时间（秒）
        """
        if not REDIS_AVAILABLE:
            raise ImportError("Redis未安装，请运行: pip install redis")
        
        self.default_ttl = default_ttl
        self.stats = CacheStatistics()
        
        try:
            self.client = redis.Redis(
                host=host,
                port=port,
                db=db,
                password=password,
                decode_responses=False,  # 保持二进制数据
                socket_connect_timeout=5,
                socket_timeout=5
            )
            
            # 测试连接
            self.client.ping()
            logger.info(f"Redis连接成功: {host}:{port}")
            
        except Exception as e:
            logger.error(f"Redis连接失败: {e}")
            raise
    
    def get(self, key: str) -> Optional[Any]:
        """获取缓存值"""
        try:
            data = self.client.get(key)
            if data is not None:
                self.stats.hits += 1
                return pickle.loads(data)
            else:
                self.stats.misses += 1
                return None
        except Exception as e:
            logger.error(f"Redis获取失败: {e}")
            self.stats.misses += 1
            return None
    
    def set(self, key: str, value: Any, ttl: int = None) -> bool:
        """设置缓存值"""
        try:
            ttl = ttl or self.default_ttl
            data = pickle.dumps(value)
            result = self.client.setex(key, ttl, data)
            return bool(result)
        except Exception as e:
            logger.error(f"Redis设置失败: {e}")
            return False
    
    def delete(self, key: str) -> bool:
        """删除缓存值"""
        try:
            result = self.client.delete(key)
            return bool(result)
        except Exception as e:
            logger.error(f"Redis删除失败: {e}")
            return False
    
    def clear(self, pattern: str = "*"):
        """清空缓存"""
        try:
            keys = self.client.keys(pattern)
            if keys:
                self.client.delete(*keys)
                logger.info(f"清空了{len(keys)}个Redis键")
        except Exception as e:
            logger.error(f"Redis清空失败: {e}")
    
    def exists(self, key: str) -> bool:
        """检查键是否存在"""
        try:
            return bool(self.client.exists(key))
        except Exception as e:
            logger.error(f"Redis检查失败: {e}")
            return False
    
    def get_stats(self) -> CacheStatistics:
        """获取缓存统计"""
        try:
            info = self.client.info('memory')
            self.stats.memory_usage = info.get('used_memory', 0)
            
            # 获取键数量
            self.stats.cache_size = self.client.dbsize()
            
            # 更新命中率
            self.stats.total_requests = self.stats.hits + self.stats.misses
            if self.stats.total_requests > 0:
                self.stats.hit_rate = self.stats.hits / self.stats.total_requests
            
            return self.stats
            
        except Exception as e:
            logger.error(f"获取Redis统计失败: {e}")
            return self.stats

class CacheManager:
    """统一缓存管理器"""
    
    def __init__(self, config: Dict[str, Any]):
        """
        初始化缓存管理器
        
        Args:
            config: 缓存配置
        """
        self.config = config
        self.memory_cache = None
        self.redis_cache = None
        
        # 初始化内存缓存
        if config.get('memory_cache', {}).get('enabled', True):
            memory_config = config.get('memory_cache', {})
            self.memory_cache = MemoryCache(
                max_size=memory_config.get('max_size', 1000),
                default_ttl=memory_config.get('default_ttl', 3600)
            )
            logger.info("内存缓存已启用")
        
        # 初始化Redis缓存
        if config.get('redis_cache', {}).get('enabled', False) and REDIS_AVAILABLE:
            try:
                redis_config = config.get('redis_cache', {})
                self.redis_cache = RedisCache(
                    host=redis_config.get('host', 'localhost'),
                    port=redis_config.get('port', 6379),
                    db=redis_config.get('db', 0),
                    password=redis_config.get('password'),
                    default_ttl=redis_config.get('default_ttl', 3600)
                )
                logger.info("Redis缓存已启用")
            except Exception as e:
                logger.warning(f"Redis缓存启用失败: {e}")
    
    def get(self, key: str, use_redis: bool = True) -> Optional[Any]:
        """获取缓存值"""
        # 首先尝试内存缓存
        if self.memory_cache:
            value = self.memory_cache.get(key)
            if value is not None:
                return value
        
        # 然后尝试Redis缓存
        if use_redis and self.redis_cache:
            value = self.redis_cache.get(key)
            if value is not None:
                # 回写到内存缓存
                if self.memory_cache:
                    self.memory_cache.set(key, value)
                return value
        
        return None
    
    def set(self, key: str, value: Any, ttl: int = None, 
            use_memory: bool = True, use_redis: bool = True) -> bool:
        """设置缓存值"""
        success = True
        
        # 设置内存缓存
        if use_memory and self.memory_cache:
            if not self.memory_cache.set(key, value, ttl):
                success = False
        
        # 设置Redis缓存
        if use_redis and self.redis_cache:
            if not self.redis_cache.set(key, value, ttl):
                success = False
        
        return success
    
    def delete(self, key: str) -> bool:
        """删除缓存值"""
        success = True
        
        if self.memory_cache:
            if not self.memory_cache.delete(key):
                success = False
        
        if self.redis_cache:
            if not self.redis_cache.delete(key):
                success = False
        
        return success
    
    def clear(self):
        """清空所有缓存"""
        if self.memory_cache:
            self.memory_cache.clear()
        
        if self.redis_cache:
            self.redis_cache.clear()
    
    def get_stats(self) -> Dict[str, CacheStatistics]:
        """获取所有缓存统计"""
        stats = {}
        
        if self.memory_cache:
            stats['memory'] = self.memory_cache.get_stats()
        
        if self.redis_cache:
            stats['redis'] = self.redis_cache.get_stats()
        
        return stats
    
    def cleanup_expired(self):
        """清理过期项目"""
        if self.memory_cache:
            self.memory_cache.cleanup_expired()

def cache_result(cache_manager: CacheManager, ttl: int = 3600, 
                key_prefix: str = "", use_args: bool = True):
    """缓存结果装饰器"""
    def decorator(func: Callable) -> Callable:
        @wraps(func)
        def wrapper(*args, **kwargs):
            # 生成缓存键
            if use_args:
                # 包含参数的缓存键
                key_parts = [key_prefix or func.__name__]
                key_parts.extend(str(arg) for arg in args)
                key_parts.extend(f"{k}={v}" for k, v in sorted(kwargs.items()))
                cache_key = hashlib.md5(":".join(key_parts).encode()).hexdigest()
            else:
                # 仅使用函数名的缓存键
                cache_key = f"{key_prefix or func.__name__}"
            
            # 尝试从缓存获取
            cached_result = cache_manager.get(cache_key)
            if cached_result is not None:
                logger.debug(f"缓存命中: {func.__name__}")
                return cached_result
            
            # 执行函数
            start_time = time.time()
            result = func(*args, **kwargs)
            execution_time = time.time() - start_time
            
            # 缓存结果
            cache_manager.set(cache_key, result, ttl)
            logger.debug(f"缓存设置: {func.__name__} (执行时间: {execution_time:.3f}秒)")
            
            return result
        
        return wrapper
    return decorator

class PerformanceMonitor:
    """性能监控器"""
    
    def __init__(self, max_history: int = 1000):
        """
        初始化性能监控器
        
        Args:
            max_history: 最大历史记录数
        """
        self.max_history = max_history
        self.metrics_history = deque(maxlen=max_history)
        self.request_times = deque(maxlen=1000)
        self.active_requests = 0
        self._lock = Lock()
        
        # 弱引用存储，避免内存泄漏
        self.cache_managers = weakref.WeakSet()
    
    def add_cache_manager(self, cache_manager: CacheManager):
        """添加缓存管理器用于监控"""
        self.cache_managers.add(cache_manager)
    
    def record_request(self, response_time: float):
        """记录请求响应时间"""
        with self._lock:
            self.request_times.append(response_time)
    
    def start_request(self):
        """开始请求计数"""
        with self._lock:
            self.active_requests += 1
    
    def end_request(self):
        """结束请求计数"""
        with self._lock:
            self.active_requests = max(0, self.active_requests - 1)
    
    def collect_metrics(self) -> PerformanceMetrics:
        """收集性能指标"""
        current_time = datetime.now()
        
        # 系统指标
        cpu_usage = psutil.cpu_percent(interval=1)
        memory = psutil.virtual_memory()
        disk = psutil.disk_usage('/')
        
        # 网络IO
        network_io = psutil.net_io_counters()._asdict()
        
        # 数据库连接数（简化实现）
        try:
            # 这里可以根据实际数据库类型获取连接数
            database_connections = len(psutil.pids())  # 简化示例
        except:
            database_connections = 0
        
        # 缓存命中率
        cache_hit_rate = 0.0
        cache_stats = {}
        for cache_manager in self.cache_managers:
            try:
                stats = cache_manager.get_stats()
                for cache_type, stat in stats.items():
                    cache_stats[cache_type] = stat
                    if stat.total_requests > 0:
                        cache_hit_rate = max(cache_hit_rate, stat.hit_rate)
            except:
                continue
        
        # 平均响应时间
        with self._lock:
            if self.request_times:
                response_time_avg = sum(self.request_times) / len(self.request_times)
            else:
                response_time_avg = 0.0
            
            active_requests = self.active_requests
        
        metrics = PerformanceMetrics(
            timestamp=current_time.isoformat(),
            cpu_usage=cpu_usage,
            memory_usage=memory.percent,
            disk_usage=disk.percent,
            network_io=network_io,
            database_connections=database_connections,
            cache_hit_rate=cache_hit_rate,
            active_requests=active_requests,
            response_time_avg=response_time_avg
        )
        
        # 添加到历史记录
        self.metrics_history.append(metrics)
        
        return metrics
    
    def get_metrics_history(self, minutes: int = 60) -> List[PerformanceMetrics]:
        """获取指定时间内的性能指标历史"""
        cutoff_time = datetime.now() - timedelta(minutes=minutes)
        cutoff_str = cutoff_time.isoformat()
        
        return [
            metrics for metrics in self.metrics_history
            if metrics.timestamp >= cutoff_str
        ]
    
    def get_performance_summary(self) -> Dict[str, Any]:
        """获取性能摘要"""
        if not self.metrics_history:
            return {'status': 'no_data'}
        
        recent_metrics = list(self.metrics_history)[-10:]  # 最近10个数据点
        
        if not recent_metrics:
            return {'status': 'no_data'}
        
        # 计算平均值
        avg_cpu = sum(m.cpu_usage for m in recent_metrics) / len(recent_metrics)
        avg_memory = sum(m.memory_usage for m in recent_metrics) / len(recent_metrics)
        avg_response_time = sum(m.response_time_avg for m in recent_metrics) / len(recent_metrics)
        avg_cache_hit_rate = sum(m.cache_hit_rate for m in recent_metrics) / len(recent_metrics)
        
        # 最新指标
        latest = recent_metrics[-1]
        
        return {
            'status': 'healthy',
            'latest_metrics': asdict(latest),
            'averages': {
                'cpu_usage': round(avg_cpu, 2),
                'memory_usage': round(avg_memory, 2),
                'response_time': round(avg_response_time, 3),
                'cache_hit_rate': round(avg_cache_hit_rate, 3)
            },
            'alerts': self._generate_alerts(recent_metrics)
        }
    
    def _generate_alerts(self, metrics: List[PerformanceMetrics]) -> List[str]:
        """生成性能告警"""
        alerts = []
        
        if not metrics:
            return alerts
        
        latest = metrics[-1]
        
        # CPU使用率告警
        if latest.cpu_usage > 80:
            alerts.append(f"CPU使用率过高: {latest.cpu_usage:.1f}%")
        
        # 内存使用率告警
        if latest.memory_usage > 85:
            alerts.append(f"内存使用率过高: {latest.memory_usage:.1f}%")
        
        # 磁盘使用率告警
        if latest.disk_usage > 90:
            alerts.append(f"磁盘使用率过高: {latest.disk_usage:.1f}%")
        
        # 缓存命中率告警
        if latest.cache_hit_rate < 0.5 and latest.cache_hit_rate > 0:
            alerts.append(f"缓存命中率过低: {latest.cache_hit_rate:.1%}")
        
        # 响应时间告警
        if latest.response_time_avg > 2.0:
            alerts.append(f"平均响应时间过长: {latest.response_time_avg:.2f}秒")
        
        return alerts

def monitor_performance(monitor: PerformanceMonitor):
    """性能监控装饰器"""
    def decorator(func: Callable) -> Callable:
        @wraps(func)
        def wrapper(*args, **kwargs):
            monitor.start_request()
            start_time = time.time()
            
            try:
                result = func(*args, **kwargs)
                return result
            finally:
                execution_time = time.time() - start_time
                monitor.record_request(execution_time)
                monitor.end_request()
        
        return wrapper
    return decorator

def create_default_cache_config() -> Dict[str, Any]:
    """创建默认缓存配置"""
    return {
        'memory_cache': {
            'enabled': True,
            'max_size': 1000,
            'default_ttl': 3600
        },
        'redis_cache': {
            'enabled': False,  # 默认关闭Redis
            'host': 'localhost',
            'port': 6379,
            'db': 0,
            'password': None,
            'default_ttl': 3600
        }
    }

def main():
    """主函数 - 缓存和监控测试"""
    print("=== 缓存和性能监控系统测试 ===")
    
    # 创建缓存配置
    config = create_default_cache_config()
    
    # 创建缓存管理器
    cache_manager = CacheManager(config)
    
    # 创建性能监控器
    monitor = PerformanceMonitor()
    monitor.add_cache_manager(cache_manager)
    
    print("\n1. 缓存功能测试:")
    
    # 测试缓存操作
    test_data = {'message': 'Hello, Cache!', 'timestamp': time.time()}
    
    # 设置缓存
    cache_manager.set('test_key', test_data, ttl=60)
    print(f"  设置缓存: test_key")
    
    # 获取缓存
    cached_data = cache_manager.get('test_key')
    print(f"  获取缓存: {cached_data}")
    
    # 测试缓存装饰器
    @cache_result(cache_manager, ttl=30)
    def slow_function(n):
        """模拟耗时函数"""
        time.sleep(0.1)  # 模拟耗时操作
        return n * n
    
    print(f"  首次调用耗时函数...")
    start = time.time()
    result1 = slow_function(10)
    time1 = time.time() - start
    print(f"  结果: {result1}, 耗时: {time1:.3f}秒")
    
    print(f"  第二次调用（应该使用缓存）...")
    start = time.time()
    result2 = slow_function(10)
    time2 = time.time() - start
    print(f"  结果: {result2}, 耗时: {time2:.3f}秒")
    
    # 获取缓存统计
    print(f"\n2. 缓存统计:")
    stats = cache_manager.get_stats()
    for cache_type, stat in stats.items():
        print(f"  {cache_type}缓存:")
        print(f"    命中率: {stat.hit_rate:.1%}")
        print(f"    总请求: {stat.total_requests}")
        print(f"    缓存大小: {stat.cache_size}")
        print(f"    内存使用: {stat.memory_usage} bytes")
    
    # 性能监控测试
    print(f"\n3. 性能监控测试:")
    
    # 模拟一些请求
    @monitor_performance(monitor)
    def test_request(duration=0.1):
        time.sleep(duration)
        return "Request completed"
    
    # 执行几个测试请求
    for i in range(5):
        result = test_request(0.05 + i * 0.01)
    
    # 收集性能指标
    metrics = monitor.collect_metrics()
    print(f"  当前CPU使用率: {metrics.cpu_usage:.1f}%")
    print(f"  当前内存使用率: {metrics.memory_usage:.1f}%")
    print(f"  磁盘使用率: {metrics.disk_usage:.1f}%")
    print(f"  活跃请求数: {metrics.active_requests}")
    print(f"  平均响应时间: {metrics.response_time_avg:.3f}秒")
    print(f"  缓存命中率: {metrics.cache_hit_rate:.1%}")
    
    # 获取性能摘要
    print(f"\n4. 性能摘要:")
    summary = monitor.get_performance_summary()
    if summary['status'] == 'healthy':
        averages = summary['averages']
        print(f"  平均CPU使用率: {averages['cpu_usage']:.1f}%")
        print(f"  平均内存使用率: {averages['memory_usage']:.1f}%")
        print(f"  平均响应时间: {averages['response_time']:.3f}秒")
        print(f"  平均缓存命中率: {averages['cache_hit_rate']:.1%}")
        
        if summary['alerts']:
            print(f"  告警:")
            for alert in summary['alerts']:
                print(f"    - {alert}")
        else:
            print(f"  无告警")
    else:
        print(f"  状态: {summary['status']}")
    
    print(f"\n=== 测试完成 ===")

if __name__ == "__main__":
    main()