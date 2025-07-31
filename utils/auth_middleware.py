"""
认证中间件
处理API认证、权限验证和用户身份识别
"""
import jwt
import hashlib
import time
from datetime import datetime, timedelta
from typing import Dict, Optional, Any
from flask import Request
import logging

logger = logging.getLogger(__name__)

class AuthMiddleware:
    """认证中间件"""
    
    def __init__(self, secret_key: str = "ljwx_stock_secret_2024"):
        self.secret_key = secret_key
        self.api_keys = {
            # 预设的API密钥
            "ljwx_web_client": {
                "name": "Web客户端",
                "permissions": ["read", "write"],
                "rate_limit": 1000
            },
            "ljwx_harmonyos_client": {
                "name": "鸿蒙客户端", 
                "permissions": ["read"],
                "rate_limit": 500
            },
            "ljwx_miniprogram_client": {
                "name": "小程序客户端",
                "permissions": ["read"],
                "rate_limit": 300
            }
        }
    
    def validate_request(self, request: Request) -> bool:
        """验证请求"""
        try:
            # 检查API密钥
            api_key = request.headers.get('X-API-Key')
            if api_key and self._validate_api_key(api_key):
                return True
            
            # 检查JWT令牌
            auth_header = request.headers.get('Authorization')
            if auth_header and auth_header.startswith('Bearer '):
                token = auth_header.split(' ')[1]
                if self._validate_jwt_token(token):
                    return True
            
            # 检查签名认证
            if self._validate_signature(request):
                return True
            
            # 对部分公开接口不需要认证
            if self._is_public_endpoint(request.endpoint):
                return True
            
            return False
            
        except Exception as e:
            logger.error(f"认证验证失败: {e}")
            return False
    
    def _validate_api_key(self, api_key: str) -> bool:
        """验证API密钥"""
        return api_key in self.api_keys
    
    def _validate_jwt_token(self, token: str) -> bool:
        """验证JWT令牌"""
        try:
            payload = jwt.decode(token, self.secret_key, algorithms=['HS256'])
            # 检查过期时间
            if payload.get('exp', 0) < time.time():
                return False
            return True
        except jwt.InvalidTokenError:
            return False
    
    def _validate_signature(self, request: Request) -> bool:
        """验证请求签名"""
        try:
            timestamp = request.headers.get('X-Timestamp')
            signature = request.headers.get('X-Signature')
            nonce = request.headers.get('X-Nonce')
            
            if not all([timestamp, signature, nonce]):
                return False
            
            # 检查时间戳（5分钟内有效）
            current_time = int(time.time())
            if abs(current_time - int(timestamp)) > 300:
                return False
            
            # 构建签名字符串
            method = request.method
            path = request.path
            query_string = request.query_string.decode('utf-8')
            body = request.get_data().decode('utf-8') if request.get_data() else ''
            
            sign_string = f"{method}|{path}|{query_string}|{body}|{timestamp}|{nonce}"
            expected_signature = hashlib.sha256(
                (sign_string + self.secret_key).encode('utf-8')
            ).hexdigest()
            
            return signature == expected_signature
            
        except Exception as e:
            logger.error(f"签名验证失败: {e}")
            return False
    
    def _is_public_endpoint(self, endpoint: str) -> bool:
        """检查是否为公开接口"""
        public_endpoints = [
            'unified_api.get_system_health',
            'unified_api.search_stocks',
            'unified_api.get_market_overview',
            'unified_api.get_hot_stocks'
        ]
        return endpoint in public_endpoints
    
    def generate_jwt_token(self, user_data: Dict, expires_in: int = 3600) -> str:
        """生成JWT令牌"""
        payload = {
            'user_data': user_data,
            'exp': datetime.utcnow() + timedelta(seconds=expires_in),
            'iat': datetime.utcnow()
        }
        return jwt.encode(payload, self.secret_key, algorithm='HS256')
    
    def get_user_from_token(self, token: str) -> Optional[Dict]:
        """从令牌获取用户信息"""
        try:
            payload = jwt.decode(token, self.secret_key, algorithms=['HS256'])
            return payload.get('user_data')
        except jwt.InvalidTokenError:
            return None