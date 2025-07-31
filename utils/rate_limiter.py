"""
请求限流工具
防止API被滥用，控制请求频率
"""
import time
from typing import Dict, Any
from collections import defaultdict, deque
import threading
import logging

logger = logging.getLogger(__name__)

class RateLimiter:
    """请求限流器"""
    
    def __init__(self):
        self.requests = defaultdict(deque)  # IP -> 请求时间队列
        self.lock = threading.Lock()
        self.cleanup_interval = 300  # 5分钟清理一次过期数据
        self.last_cleanup = time.time()
    
    def is_allowed(self, identifier: str, max_requests: int, window_seconds: int) -> bool:
        """检查是否允许请求"""
        with self.lock:
            current_time = time.time()
            
            # 定期清理过期数据
            if current_time - self.last_cleanup > self.cleanup_interval:
                self._cleanup_expired_requests(current_time)
                self.last_cleanup = current_time
            
            # 获取当前标识符的请求队列
            request_queue = self.requests[identifier]
            
            # 移除窗口外的请求
            while request_queue and request_queue[0] <= current_time - window_seconds:
                request_queue.popleft()
            
            # 检查是否超过限制
            if len(request_queue) >= max_requests:
                return False
            
            # 记录当前请求
            request_queue.append(current_time)
            return True
    
    def _cleanup_expired_requests(self, current_time: float):
        """清理过期的请求记录"""
        expired_identifiers = []
        
        for identifier, request_queue in self.requests.items():
            # 移除5分钟前的请求
            while request_queue and request_queue[0] <= current_time - 300:
                request_queue.popleft()
            
            # 如果队列为空，标记为过期
            if not request_queue:
                expired_identifiers.append(identifier)
        
        # 删除过期的标识符
        for identifier in expired_identifiers:
            del self.requests[identifier]
        
        if expired_identifiers:
            logger.info(f"清理了{len(expired_identifiers)}个过期的限流记录")
    
    def get_remaining_requests(self, identifier: str, max_requests: int, window_seconds: int) -> int:
        """获取剩余请求次数"""
        with self.lock:
            current_time = time.time()
            request_queue = self.requests[identifier]
            
            # 移除窗口外的请求
            while request_queue and request_queue[0] <= current_time - window_seconds:
                request_queue.popleft()
            
            return max(0, max_requests - len(request_queue))
    
    def reset_limit(self, identifier: str):
        """重置指定标识符的限制"""
        with self.lock:
            if identifier in self.requests:
                del self.requests[identifier]
    
    def get_stats(self) -> Dict[str, Any]:
        """获取限流统计信息"""
        with self.lock:
            current_time = time.time()
            stats = {
                "total_identifiers": len(self.requests),
                "active_requests": sum(len(queue) for queue in self.requests.values()),
                "top_requesters": []
            }
            
            # 获取请求最多的前10个标识符
            sorted_requesters = sorted(
                self.requests.items(),
                key=lambda x: len(x[1]),
                reverse=True
            )[:10]
            
            for identifier, request_queue in sorted_requesters:
                # 计算最近1分钟的请求数
                recent_requests = sum(1 for req_time in request_queue if req_time > current_time - 60)
                stats["top_requesters"].append({
                    "identifier": identifier,
                    "total_requests": len(request_queue),
                    "recent_requests": recent_requests
                })
            
            return stats

class AdvancedRateLimiter:
    """高级限流器，支持多种限流策略"""
    
    def __init__(self):
        self.token_buckets = defaultdict(dict)  # IP -> {bucket_name: TokenBucket}
        self.sliding_windows = defaultdict(dict)  # IP -> {window_name: SlidingWindow}
        self.lock = threading.Lock()
    
    def check_token_bucket(self, identifier: str, bucket_name: str, capacity: int, refill_rate: float) -> bool:
        """令牌桶限流"""
        with self.lock:
            current_time = time.time()
            
            if bucket_name not in self.token_buckets[identifier]:
                self.token_buckets[identifier][bucket_name] = {
                    'tokens': capacity,
                    'last_refill': current_time,
                    'capacity': capacity,
                    'refill_rate': refill_rate
                }
            
            bucket = self.token_buckets[identifier][bucket_name]
            
            # 添加令牌
            time_passed = current_time - bucket['last_refill']
            tokens_to_add = time_passed * refill_rate
            bucket['tokens'] = min(capacity, bucket['tokens'] + tokens_to_add)
            bucket['last_refill'] = current_time
            
            # 检查是否有足够令牌
            if bucket['tokens'] >= 1:
                bucket['tokens'] -= 1
                return True
            
            return False
    
    def check_sliding_window(self, identifier: str, window_name: str, max_requests: int, window_seconds: int) -> bool:
        """滑动窗口限流"""
        with self.lock:
            current_time = time.time()
            
            if window_name not in self.sliding_windows[identifier]:
                self.sliding_windows[identifier][window_name] = deque()
            
            window = self.sliding_windows[identifier][window_name]
            
            # 移除窗口外的请求
            while window and window[0] <= current_time - window_seconds:
                window.popleft()
            
            # 检查是否超过限制
            if len(window) >= max_requests:
                return False
            
            # 记录当前请求
            window.append(current_time)
            return True