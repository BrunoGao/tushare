#!/usr/bin/env python3
"""
Redis缓存管理器
提供智能缓存策略，大幅提升系统响应速度
支持多层缓存、自动失效、预热等功能
"""
import redis
import json
import pickle
import logging
import time
import hashlib
from datetime import datetime, timedelta
from typing import Any, Dict, List, Optional, Union, Callable
from functools import wraps
import threading
from concurrent.futures import ThreadPoolExecutor
import sys
import os
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import config

logger = logging.getLogger(__name__)

class RedisCacheManager:
    """Redis缓存管理器"""
    
    def __init__(self, host='localhost', port=6379, db=0, password=None):
        """初始化Redis连接"""
        try:
            self.redis_client = redis.Redis(
                host=host,
                port=port,
                db=db,
                password=password,
                decode_responses=False,  # 保持二进制模式以支持pickle
                socket_connect_timeout=5,
                socket_timeout=5,
                retry_on_timeout=True,
                health_check_interval=30
            )
            
            # 测试连接
            self.redis_client.ping()
            logger.info("✅ Redis连接成功")
            
        except Exception as e:
            logger.error(f"❌ Redis连接失败: {e}")
            # 使用内存缓存作为降级方案
            self.redis_client = None
            self._memory_cache = {}
            self._memory_cache_ttl = {}
        
        # 缓存配置
        self.cache_config = {
            # 推荐结果缓存 (5分钟)
            'recommendations': {'ttl': 300, 'prefix': 'rec'},
            # 技术指标缓存 (1分钟)
            'indicators': {'ttl': 60, 'prefix': 'ind'},
            # 实时数据缓存 (30秒)
            'realtime': {'ttl': 30, 'prefix': 'rt'},
            # 股票基本信息缓存 (1小时)
            'stock_basic': {'ttl': 3600, 'prefix': 'sb'},
            # 财务数据缓存 (30分钟)
            'financial': {'ttl': 1800, 'prefix': 'fin'},
            # 资金流数据缓存 (2分钟)
            'money_flow': {'ttl': 120, 'prefix': 'mf'},
            # 用户会话缓存 (24小时)
            'session': {'ttl': 86400, 'prefix': 'sess'},
            # 系统状态缓存 (5分钟)
            'system_status': {'ttl': 300, 'prefix': 'sys'}
        }
        
        # 统计信息
        self.stats = {
            'hits': 0,
            'misses': 0,
            'sets': 0,
            'deletes': 0,
            'errors': 0
        }
        
        # 线程池用于异步操作
        self.executor = ThreadPoolExecutor(max_workers=4)
        self.lock = threading.Lock()
    
    def _generate_key(self, category: str, key: str) -> str:
        """生成缓存键"""
        prefix = self.cache_config.get(category, {}).get('prefix', 'cache')
        return f"{prefix}:{key}"
    
    def _serialize_data(self, data: Any) -> bytes:
        """序列化数据"""
        try:
            if isinstance(data, (dict, list)):
                return json.dumps(data, ensure_ascii=False, default=str).encode('utf-8')
            else:
                return pickle.dumps(data)
        except Exception as e:
            logger.error(f"数据序列化失败: {e}")
            return pickle.dumps(data)
    
    def _deserialize_data(self, data: bytes) -> Any:
        """反序列化数据"""
        try:
            # 尝试JSON反序列化
            return json.loads(data.decode('utf-8'))
        except:
            try:
                # 尝试pickle反序列化
                return pickle.loads(data)
            except Exception as e:
                logger.error(f"数据反序列化失败: {e}")
                return None
    
    def get(self, category: str, key: str) -> Optional[Any]:
        """获取缓存数据"""
        cache_key = self._generate_key(category, key)
        
        try:
            if self.redis_client:
                # Redis缓存
                data = self.redis_client.get(cache_key)
                if data:
                    with self.lock:
                        self.stats['hits'] += 1
                    return self._deserialize_data(data)
            else:
                # 内存缓存降级
                if cache_key in self._memory_cache:
                    # 检查TTL
                    if cache_key in self._memory_cache_ttl:
                        if time.time() > self._memory_cache_ttl[cache_key]:
                            del self._memory_cache[cache_key]
                            del self._memory_cache_ttl[cache_key]
                        else:
                            with self.lock:
                                self.stats['hits'] += 1
                            return self._memory_cache[cache_key]
            
            with self.lock:
                self.stats['misses'] += 1
            return None
            
        except Exception as e:
            logger.error(f"缓存获取失败 {cache_key}: {e}")
            with self.lock:
                self.stats['errors'] += 1
            return None
    
    def set(self, category: str, key: str, data: Any, ttl: Optional[int] = None) -> bool:
        """设置缓存数据"""
        cache_key = self._generate_key(category, key)
        
        if ttl is None:
            ttl = self.cache_config.get(category, {}).get('ttl', 300)
        
        try:
            serialized_data = self._serialize_data(data)
            
            if self.redis_client:
                # Redis缓存
                result = self.redis_client.setex(cache_key, ttl, serialized_data)
                if result:
                    with self.lock:
                        self.stats['sets'] += 1
                    return True
            else:
                # 内存缓存降级
                self._memory_cache[cache_key] = data
                self._memory_cache_ttl[cache_key] = time.time() + ttl
                with self.lock:
                    self.stats['sets'] += 1
                return True
            
            return False
            
        except Exception as e:
            logger.error(f"缓存设置失败 {cache_key}: {e}")
            with self.lock:
                self.stats['errors'] += 1
            return False
    
    def delete(self, category: str, key: str) -> bool:
        """删除缓存数据"""
        cache_key = self._generate_key(category, key)
        
        try:
            if self.redis_client:
                result = self.redis_client.delete(cache_key)
                if result:
                    with self.lock:
                        self.stats['deletes'] += 1
                    return True
            else:
                # 内存缓存降级
                if cache_key in self._memory_cache:
                    del self._memory_cache[cache_key]
                    if cache_key in self._memory_cache_ttl:
                        del self._memory_cache_ttl[cache_key]
                    with self.lock:
                        self.stats['deletes'] += 1
                    return True
            
            return False
            
        except Exception as e:
            logger.error(f"缓存删除失败 {cache_key}: {e}")
            with self.lock:
                self.stats['errors'] += 1
            return False
    
    def clear_category(self, category: str) -> int:
        """清空指定类别的所有缓存"""
        try:
            prefix = self.cache_config.get(category, {}).get('prefix', 'cache')
            pattern = f"{prefix}:*"
            
            if self.redis_client:
                keys = self.redis_client.keys(pattern)
                if keys:
                    deleted = self.redis_client.delete(*keys)
                    with self.lock:
                        self.stats['deletes'] += deleted
                    return deleted
            else:
                # 内存缓存降级
                deleted = 0
                keys_to_delete = [k for k in self._memory_cache.keys() if k.startswith(f"{prefix}:")]
                for key in keys_to_delete:
                    del self._memory_cache[key]
                    if key in self._memory_cache_ttl:
                        del self._memory_cache_ttl[key]
                    deleted += 1
                
                with self.lock:
                    self.stats['deletes'] += deleted
                return deleted
            
            return 0
            
        except Exception as e:
            logger.error(f"清空缓存类别失败 {category}: {e}")
            with self.lock:
                self.stats['errors'] += 1
            return 0
    
    def exists(self, category: str, key: str) -> bool:
        """检查缓存是否存在"""
        cache_key = self._generate_key(category, key)
        
        try:
            if self.redis_client:
                return bool(self.redis_client.exists(cache_key))
            else:
                # 内存缓存降级
                if cache_key in self._memory_cache:
                    # 检查TTL
                    if cache_key in self._memory_cache_ttl:
                        if time.time() > self._memory_cache_ttl[cache_key]:
                            del self._memory_cache[cache_key]
                            del self._memory_cache_ttl[cache_key]
                            return False
                    return True
                return False
                
        except Exception as e:
            logger.error(f"缓存存在性检查失败 {cache_key}: {e}")
            return False
    
    def get_ttl(self, category: str, key: str) -> int:
        """获取缓存剩余TTL"""
        cache_key = self._generate_key(category, key)
        
        try:
            if self.redis_client:
                return self.redis_client.ttl(cache_key)
            else:
                # 内存缓存降级
                if cache_key in self._memory_cache_ttl:
                    remaining = self._memory_cache_ttl[cache_key] - time.time()
                    return int(max(0, remaining))
                return -1
                
        except Exception as e:
            logger.error(f"TTL获取失败 {cache_key}: {e}")
            return -1
    
    def extend_ttl(self, category: str, key: str, additional_seconds: int) -> bool:
        """延长缓存TTL"""
        cache_key = self._generate_key(category, key)
        
        try:
            if self.redis_client:
                current_ttl = self.redis_client.ttl(cache_key)
                if current_ttl > 0:
                    return bool(self.redis_client.expire(cache_key, current_ttl + additional_seconds))
            else:
                # 内存缓存降级
                if cache_key in self._memory_cache_ttl:
                    self._memory_cache_ttl[cache_key] += additional_seconds
                    return True
            
            return False
            
        except Exception as e:
            logger.error(f"TTL延长失败 {cache_key}: {e}")
            return False
    
    def get_or_set(self, category: str, key: str, fetch_func: Callable, ttl: Optional[int] = None) -> Any:
        """获取缓存，如果不存在则执行函数并缓存结果"""
        # 先尝试获取缓存
        cached_data = self.get(category, key)
        if cached_data is not None:
            return cached_data
        
        # 缓存不存在，执行函数获取数据
        try:
            data = fetch_func()
            if data is not None:
                self.set(category, key, data, ttl)
            return data
        except Exception as e:
            logger.error(f"数据获取函数执行失败: {e}")
            return None
    
    def batch_get(self, category: str, keys: List[str]) -> Dict[str, Any]:
        """批量获取缓存"""
        results = {}
        
        if self.redis_client:
            # Redis批量获取
            cache_keys = [self._generate_key(category, key) for key in keys]
            try:
                values = self.redis_client.mget(cache_keys)
                for i, value in enumerate(values):
                    if value:
                        results[keys[i]] = self._deserialize_data(value)
                        with self.lock:
                            self.stats['hits'] += 1
                    else:
                        with self.lock:
                            self.stats['misses'] += 1
            except Exception as e:
                logger.error(f"批量获取失败: {e}")
        else:
            # 内存缓存降级
            for key in keys:
                result = self.get(category, key)
                if result is not None:
                    results[key] = result
        
        return results
    
    def batch_set(self, category: str, data_dict: Dict[str, Any], ttl: Optional[int] = None) -> int:
        """批量设置缓存"""
        if ttl is None:
            ttl = self.cache_config.get(category, {}).get('ttl', 300)
        
        success_count = 0
        
        if self.redis_client:
            # Redis批量设置
            try:
                pipe = self.redis_client.pipeline()
                for key, data in data_dict.items():
                    cache_key = self._generate_key(category, key)
                    serialized_data = self._serialize_data(data)
                    pipe.setex(cache_key, ttl, serialized_data)
                
                results = pipe.execute()
                success_count = sum(1 for result in results if result)
                
                with self.lock:
                    self.stats['sets'] += success_count
                    
            except Exception as e:
                logger.error(f"批量设置失败: {e}")
        else:
            # 内存缓存降级
            for key, data in data_dict.items():
                if self.set(category, key, data, ttl):
                    success_count += 1
        
        return success_count
    
    def get_stats(self) -> Dict[str, Any]:
        """获取缓存统计信息"""
        with self.lock:
            stats = self.stats.copy()
        
        # 计算命中率
        total_requests = stats['hits'] + stats['misses']
        hit_rate = (stats['hits'] / total_requests * 100) if total_requests > 0 else 0
        
        stats['hit_rate'] = round(hit_rate, 2)
        stats['total_requests'] = total_requests
        
        # 获取Redis信息
        if self.redis_client:
            try:
                redis_info = self.redis_client.info()
                stats['redis_info'] = {
                    'used_memory': redis_info.get('used_memory_human', 'N/A'),
                    'connected_clients': redis_info.get('connected_clients', 0),
                    'total_commands_processed': redis_info.get('total_commands_processed', 0),
                    'keyspace_hits': redis_info.get('keyspace_hits', 0),
                    'keyspace_misses': redis_info.get('keyspace_misses', 0)
                }
            except:
                stats['redis_info'] = {'status': 'unavailable'}
        else:
            stats['memory_cache_size'] = len(self._memory_cache)
        
        return stats
    
    def health_check(self) -> Dict[str, Any]:
        """健康检查"""
        health = {
            'status': 'healthy',
            'timestamp': datetime.now().isoformat(),
            'issues': []
        }
        
        try:
            if self.redis_client:
                # 测试Redis连接
                start_time = time.time()
                self.redis_client.ping()
                response_time = (time.time() - start_time) * 1000
                
                health['redis_status'] = 'connected'
                health['response_time_ms'] = round(response_time, 2)
                
                # 检查响应时间
                if response_time > 100:
                    health['issues'].append(f'Redis响应时间过长: {response_time:.2f}ms')
                    health['status'] = 'warning'
                
                # 检查内存使用
                redis_info = self.redis_client.info()
                used_memory_mb = redis_info.get('used_memory', 0) / 1024 / 1024
                if used_memory_mb > 1000:  # 超过1GB
                    health['issues'].append(f'Redis内存使用过高: {used_memory_mb:.1f}MB')
                    health['status'] = 'warning'
                
            else:
                health['redis_status'] = 'disconnected'
                health['fallback_mode'] = 'memory_cache'
                health['memory_cache_size'] = len(self._memory_cache)
                health['issues'].append('Redis不可用，使用内存缓存降级')
                health['status'] = 'warning'
            
            # 检查命中率
            stats = self.get_stats()
            if stats['hit_rate'] < 50 and stats['total_requests'] > 100:
                health['issues'].append(f'缓存命中率过低: {stats["hit_rate"]}%')
                health['status'] = 'warning'
            
            if health['issues']:
                health['status'] = 'warning' if health['status'] != 'critical' else 'critical'
            
        except Exception as e:
            health['status'] = 'critical'
            health['error'] = str(e)
            health['issues'].append(f'健康检查失败: {e}')
        
        return health
    
    def preload_cache(self, preload_config: Dict[str, Callable]) -> Dict[str, int]:
        """预热缓存"""
        logger.info("🔥 开始缓存预热...")
        results = {}
        
        for category, fetch_func in preload_config.items():
            try:
                logger.info(f"预热缓存类别: {category}")
                data = fetch_func()
                
                if isinstance(data, dict):
                    # 批量数据
                    success_count = self.batch_set(category, data)
                    results[category] = success_count
                    logger.info(f"✅ {category} 预热完成: {success_count}条")
                else:
                    # 单条数据
                    if self.set(category, 'default', data):
                        results[category] = 1
                        logger.info(f"✅ {category} 预热完成: 1条")
                    else:
                        results[category] = 0
                        logger.warning(f"⚠️ {category} 预热失败")
                        
            except Exception as e:
                logger.error(f"❌ {category} 预热异常: {e}")
                results[category] = 0
        
        logger.info(f"🔥 缓存预热完成: {results}")
        return results
    
    def cleanup_expired(self) -> int:
        """清理过期的内存缓存（仅在降级模式下使用）"""
        if self.redis_client:
            return 0  # Redis自动处理过期
        
        current_time = time.time()
        expired_keys = []
        
        for key, expire_time in self._memory_cache_ttl.items():
            if current_time > expire_time:
                expired_keys.append(key)
        
        for key in expired_keys:
            if key in self._memory_cache:
                del self._memory_cache[key]
            del self._memory_cache_ttl[key]
        
        if expired_keys:
            logger.info(f"🧹 清理过期缓存: {len(expired_keys)}条")
        
        return len(expired_keys)

# 缓存装饰器
def cached(category: str, key_func: Optional[Callable] = None, ttl: Optional[int] = None):
    """缓存装饰器"""
    def decorator(func):
        @wraps(func)
        def wrapper(*args, **kwargs):
            # 生成缓存键
            if key_func:
                cache_key = key_func(*args, **kwargs)
            else:
                # 使用函数名和参数生成键
                key_parts = [func.__name__]
                key_parts.extend(str(arg) for arg in args)
                key_parts.extend(f"{k}={v}" for k, v in sorted(kwargs.items()))
                cache_key = hashlib.md5(":".join(key_parts).encode()).hexdigest()
            
            # 尝试从缓存获取
            cached_result = cache_manager.get(category, cache_key)
            if cached_result is not None:
                return cached_result
            
            # 执行函数并缓存结果
            result = func(*args, **kwargs)
            if result is not None:
                cache_manager.set(category, cache_key, result, ttl)
            
            return result
        return wrapper
    return decorator

# 创建全局缓存管理器实例
try:
    # 尝试从配置文件获取Redis配置
    redis_config = getattr(config, 'REDIS_CONFIG', {
        'host': 'localhost',
        'port': 6379,
        'db': 0,
        'password': None
    })
    cache_manager = RedisCacheManager(**redis_config)
except Exception as e:
    logger.warning(f"使用默认Redis配置: {e}")
    cache_manager = RedisCacheManager()

def main():
    """测试和管理功能"""
    import argparse
    
    parser = argparse.ArgumentParser(description='Redis缓存管理器')
    parser.add_argument('command', choices=['test', 'stats', 'health', 'clear', 'preload'], 
                       help='操作命令')
    parser.add_argument('--category', type=str, help='缓存类别')
    
    args = parser.parse_args()
    
    if args.command == 'test':
        # 测试缓存功能
        print("🧪 测试缓存功能...")
        
        # 设置测试数据
        test_data = {'test': 'data', 'timestamp': datetime.now().isoformat()}
        cache_manager.set('test', 'sample', test_data, 60)
        
        # 获取测试数据
        retrieved = cache_manager.get('test', 'sample')
        print(f"设置数据: {test_data}")
        print(f"获取数据: {retrieved}")
        print(f"数据一致: {test_data == retrieved}")
        
    elif args.command == 'stats':
        # 显示统计信息
        stats = cache_manager.get_stats()
        print("📊 缓存统计信息:")
        for key, value in stats.items():
            print(f"  {key}: {value}")
            
    elif args.command == 'health':
        # 健康检查
        health = cache_manager.health_check()
        print("🏥 缓存健康状态:")
        print(json.dumps(health, indent=2, ensure_ascii=False, default=str))
        
    elif args.command == 'clear':
        # 清空缓存
        if args.category:
            deleted = cache_manager.clear_category(args.category)
            print(f"🧹 清空缓存类别 {args.category}: {deleted}条")
        else:
            print("请指定要清空的缓存类别 --category")
            
    elif args.command == 'preload':
        # 预热缓存示例
        def sample_preload():
            return {'sample_key': 'sample_value', 'timestamp': datetime.now().isoformat()}
        
        preload_config = {'test': sample_preload}
        results = cache_manager.preload_cache(preload_config)
        print(f"🔥 缓存预热结果: {results}")

if __name__ == "__main__":
    main()