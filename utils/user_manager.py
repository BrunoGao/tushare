"""
用户管理系统
处理用户登录、角色管理和权限验证
"""
import hashlib
import time
from datetime import datetime, timedelta
from typing import Dict, Optional, List
from dataclasses import dataclass
from enum import Enum
import logging

logger = logging.getLogger(__name__)

class UserRole(Enum):
    """用户角色枚举"""
    ADMIN = "admin"
    USER = "user"

@dataclass
class User:
    """用户数据类"""
    username: str
    role: UserRole
    permissions: List[str]
    created_at: datetime
    last_login: Optional[datetime] = None
    is_active: bool = True

class UserManager:
    """用户管理器"""
    
    def __init__(self):
        # 初始化默认用户
        self.users = {
            "admin": {
                "username": "admin",
                "password_hash": self._hash_password("admin123"),  # 默认密码
                "role": UserRole.ADMIN,
                "permissions": [
                    "system:manage",
                    "model:train", 
                    "strategy:edit",
                    "recommendations:generate",
                    "recommendations:view",
                    "market:view",
                    "analysis:view"
                ],
                "created_at": datetime.now(),
                "is_active": True
            },
            "user": {
                "username": "user",
                "password_hash": self._hash_password("user123"),  # 默认密码  
                "role": UserRole.USER,
                "permissions": [
                    "recommendations:view",
                    "market:view",
                    "analysis:view"
                ],
                "created_at": datetime.now(),
                "is_active": True
            }
        }
        
        # 用户会话管理
        self.sessions = {}
        
        # 权限配置
        self.role_permissions = {
            UserRole.ADMIN: [
                "system:manage",
                "model:train",
                "strategy:edit", 
                "recommendations:generate",
                "recommendations:view",
                "market:view",
                "analysis:view",
                "users:manage"
            ],
            UserRole.USER: [
                "recommendations:view",
                "market:view", 
                "analysis:view"
            ]
        }
    
    def _hash_password(self, password: str) -> str:
        """密码哈希"""
        return hashlib.sha256(password.encode()).hexdigest()
    
    def authenticate(self, username: str, password: str) -> Optional[Dict]:
        """用户认证"""
        try:
            if username not in self.users:
                logger.warning(f"用户不存在: {username}")
                return None
            
            user_data = self.users[username]
            
            if not user_data.get("is_active", False):
                logger.warning(f"用户已禁用: {username}")
                return None
            
            password_hash = self._hash_password(password)
            if password_hash != user_data["password_hash"]:
                logger.warning(f"用户密码错误: {username}")
                return None
            
            # 更新最后登录时间
            user_data["last_login"] = datetime.now()
            
            # 创建用户会话
            session_token = self._generate_session_token(username)
            
            user_info = {
                "username": username,
                "role": user_data["role"].value,
                "permissions": user_data["permissions"],
                "session_token": session_token,
                "login_time": datetime.now().isoformat()
            }
            
            logger.info(f"用户登录成功: {username}")
            return user_info
            
        except Exception as e:
            logger.error(f"用户认证失败: {e}")
            return None
    
    def _generate_session_token(self, username: str) -> str:
        """生成会话令牌"""
        timestamp = str(int(time.time()))
        raw_token = f"{username}:{timestamp}:{hashlib.md5(f'{username}{timestamp}'.encode()).hexdigest()}"
        session_token = hashlib.sha256(raw_token.encode()).hexdigest()
        
        # 存储会话信息，设置1小时过期
        self.sessions[session_token] = {
            "username": username,
            "created_at": datetime.now(),
            "expires_at": datetime.now() + timedelta(hours=1)
        }
        
        return session_token
    
    def validate_session(self, session_token: str) -> Optional[Dict]:
        """验证会话令牌"""
        try:
            if not session_token or session_token not in self.sessions:
                return None
            
            session = self.sessions[session_token]
            
            # 检查会话是否过期
            if datetime.now() > session["expires_at"]:
                del self.sessions[session_token]
                return None
            
            username = session["username"]
            if username not in self.users:
                return None
            
            user_data = self.users[username]
            return {
                "username": username,
                "role": user_data["role"].value,
                "permissions": user_data["permissions"]
            }
            
        except Exception as e:
            logger.error(f"会话验证失败: {e}")
            return None
    
    def has_permission(self, username: str, permission: str) -> bool:
        """检查用户权限"""
        try:
            if username not in self.users:
                return False
            
            user_permissions = self.users[username]["permissions"]
            return permission in user_permissions
            
        except Exception as e:
            logger.error(f"权限检查失败: {e}")
            return False
    
    def logout(self, session_token: str) -> bool:
        """用户登出"""
        try:
            if session_token in self.sessions:
                username = self.sessions[session_token]["username"]
                del self.sessions[session_token]
                logger.info(f"用户登出成功: {username}")
                return True
            return False
            
        except Exception as e:
            logger.error(f"用户登出失败: {e}")
            return False
    
    def get_user_info(self, username: str) -> Optional[Dict]:
        """获取用户信息"""
        try:
            if username not in self.users:
                return None
            
            user_data = self.users[username]
            return {
                "username": username,
                "role": user_data["role"].value,
                "permissions": user_data["permissions"],
                "created_at": user_data["created_at"].isoformat(),
                "last_login": user_data.get("last_login").isoformat() if user_data.get("last_login") else None,
                "is_active": user_data.get("is_active", False)
            }
            
        except Exception as e:
            logger.error(f"获取用户信息失败: {e}")
            return None
    
    def create_user(self, username: str, password: str, role: UserRole) -> bool:
        """创建新用户"""
        try:
            if username in self.users:
                logger.warning(f"用户已存在: {username}")
                return False
            
            self.users[username] = {
                "username": username,
                "password_hash": self._hash_password(password),
                "role": role,
                "permissions": self.role_permissions.get(role, []),
                "created_at": datetime.now(),
                "is_active": True
            }
            
            logger.info(f"用户创建成功: {username}")
            return True
            
        except Exception as e:
            logger.error(f"用户创建失败: {e}")
            return False
    
    def update_user_role(self, username: str, new_role: UserRole) -> bool:
        """更新用户角色"""
        try:
            if username not in self.users:
                return False
            
            self.users[username]["role"] = new_role
            self.users[username]["permissions"] = self.role_permissions.get(new_role, [])
            
            logger.info(f"用户角色更新成功: {username} -> {new_role.value}")
            return True
            
        except Exception as e:
            logger.error(f"用户角色更新失败: {e}")
            return False
    
    def list_users(self) -> List[Dict]:
        """列出所有用户"""
        try:
            users_list = []
            for username, user_data in self.users.items():
                users_list.append({
                    "username": username,
                    "role": user_data["role"].value,
                    "created_at": user_data["created_at"].isoformat(),
                    "last_login": user_data.get("last_login").isoformat() if user_data.get("last_login") else None,
                    "is_active": user_data.get("is_active", False)
                })
            return users_list
            
        except Exception as e:
            logger.error(f"用户列表获取失败: {e}")
            return []
    
    def cleanup_expired_sessions(self):
        """清理过期会话"""
        try:
            current_time = datetime.now()
            expired_tokens = [
                token for token, session in self.sessions.items()
                if current_time > session["expires_at"]
            ]
            
            for token in expired_tokens:
                del self.sessions[token]
            
            if expired_tokens:
                logger.info(f"清理过期会话: {len(expired_tokens)}个")
                
        except Exception as e:
            logger.error(f"清理过期会话失败: {e}")

# 全局用户管理器实例
user_manager = UserManager()