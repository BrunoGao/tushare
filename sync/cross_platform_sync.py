"""
跨平台数据同步机制
实现Web端、鸿蒙端、小程序端之间的数据同步
"""
import asyncio
import json
import redis
from datetime import datetime, timedelta
from typing import Dict, List, Any, Optional
from dataclasses import dataclass, asdict
from enum import Enum
import logging
import uuid

# 日志配置
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class SyncEventType(Enum):
    """同步事件类型"""
    WATCHLIST_UPDATE = "watchlist_update"
    USER_PREFERENCES = "user_preferences"
    RECENT_SEARCHES = "recent_searches"
    CUSTOM_ALERTS = "custom_alerts"
    PORTFOLIO_UPDATE = "portfolio_update"
    USER_NOTES = "user_notes"

class ClientType(Enum):
    """客户端类型"""
    WEB = "web"
    HARMONYOS = "harmonyos"
    MINIPROGRAM = "miniprogram"
    MOBILE_APP = "mobile_app"

@dataclass
class SyncEvent:
    """同步事件"""
    event_id: str
    user_id: str
    client_id: str
    client_type: ClientType
    event_type: SyncEventType
    data: Dict[str, Any]
    timestamp: datetime
    version: int = 1

@dataclass
class SyncStatus:
    """同步状态"""
    client_id: str
    client_type: ClientType
    last_sync: datetime
    version: int
    is_online: bool

class CrossPlatformSyncManager:
    """跨平台数据同步管理器"""
    
    def __init__(self, redis_host: str = 'localhost', redis_port: int = 6379):
        self.redis_client = redis.Redis(host=redis_host, port=redis_port, decode_responses=True)
        self.active_connections: Dict[str, Any] = {}  # WebSocket连接
        self.sync_channels = {}  # 同步频道
        
    async def initialize(self):
        """初始化同步管理器"""
        try:
            # 测试Redis连接
            self.redis_client.ping()
            logger.info("跨平台同步管理器初始化成功")
            
            # 订阅同步频道
            await self._setup_sync_channels()
            
        except Exception as e:
            logger.error(f"同步管理器初始化失败: {e}")
            raise
    
    async def _setup_sync_channels(self):
        """设置同步频道"""
        sync_channel = "stock_analysis_sync"
        
        # Redis发布订阅
        pubsub = self.redis_client.pubsub()
        pubsub.subscribe(sync_channel)
        
        # 异步监听消息
        asyncio.create_task(self._listen_sync_messages(pubsub))
    
    async def _listen_sync_messages(self, pubsub):
        """监听同步消息"""
        try:
            async for message in pubsub.listen():
                if message['type'] == 'message':
                    await self._handle_sync_message(json.loads(message['data']))
        except Exception as e:
            logger.error(f"监听同步消息失败: {e}")
    
    async def _handle_sync_message(self, message_data: Dict[str, Any]):
        """处理同步消息"""
        try:
            sync_event = SyncEvent(**message_data)
            
            # 广播给所有相关客户端
            await self._broadcast_to_clients(sync_event)
            
        except Exception as e:
            logger.error(f"处理同步消息失败: {e}")
    
    # ============ 客户端管理 ============
    
    async def register_client(self, user_id: str, client_id: str, client_type: ClientType, websocket=None) -> bool:
        """注册客户端"""
        try:
            # 保存客户端信息
            client_key = f"client:{user_id}:{client_id}"
            client_info = {
                "user_id": user_id,
                "client_id": client_id,
                "client_type": client_type.value,
                "registered_at": datetime.now().isoformat(),
                "is_online": True
            }
            
            self.redis_client.hset(client_key, mapping=client_info)
            self.redis_client.expire(client_key, 3600)  # 1小时过期
            
            # 保存WebSocket连接
            if websocket:
                self.active_connections[client_id] = websocket
            
            # 发送初始同步数据
            await self._send_initial_sync_data(user_id, client_id, client_type)
            
            logger.info(f"客户端注册成功: {client_type.value} - {client_id}")
            return True
            
        except Exception as e:
            logger.error(f"客户端注册失败: {e}")
            return False
    
    async def unregister_client(self, user_id: str, client_id: str):
        """注销客户端"""
        try:
            # 移除客户端信息
            client_key = f"client:{user_id}:{client_id}"
            self.redis_client.delete(client_key)
            
            # 移除WebSocket连接
            if client_id in self.active_connections:
                del self.active_connections[client_id]
                
            logger.info(f"客户端注销成功: {client_id}")
            
        except Exception as e:
            logger.error(f"客户端注销失败: {e}")
    
    async def _send_initial_sync_data(self, user_id: str, client_id: str, client_type: ClientType):
        """发送初始同步数据"""
        try:
            # 获取用户的所有同步数据
            sync_data = await self._get_user_sync_data(user_id)
            
            # 发送给客户端
            if client_id in self.active_connections:
                await self._send_to_client(client_id, {
                    "type": "initial_sync",
                    "data": sync_data
                })
                
        except Exception as e:
            logger.error(f"发送初始同步数据失败: {e}")
    
    # ============ 数据同步操作 ============
    
    async def sync_data(self, user_id: str, client_id: str, client_type: ClientType, 
                       event_type: SyncEventType, data: Dict[str, Any]) -> bool:
        """同步数据"""
        try:
            # 创建同步事件
            sync_event = SyncEvent(
                event_id=str(uuid.uuid4()),
                user_id=user_id,
                client_id=client_id,
                client_type=client_type,
                event_type=event_type,
                data=data,
                timestamp=datetime.now()
            )
            
            # 保存到Redis
            await self._save_sync_event(sync_event)
            
            # 更新用户数据
            await self._update_user_data(user_id, event_type, data)
            
            # 发布同步消息
            await self._publish_sync_event(sync_event)
            
            logger.info(f"数据同步成功: {event_type.value} - {user_id}")
            return True
            
        except Exception as e:
            logger.error(f"数据同步失败: {e}")
            return False
    
    async def _save_sync_event(self, sync_event: SyncEvent):
        """保存同步事件"""
        event_key = f"sync_event:{sync_event.user_id}:{sync_event.event_id}"
        event_data = asdict(sync_event)
        event_data['timestamp'] = sync_event.timestamp.isoformat()
        event_data['client_type'] = sync_event.client_type.value
        event_data['event_type'] = sync_event.event_type.value
        
        self.redis_client.hset(event_key, mapping=event_data)
        self.redis_client.expire(event_key, 86400)  # 24小时过期
    
    async def _update_user_data(self, user_id: str, event_type: SyncEventType, data: Dict[str, Any]):
        """更新用户数据"""
        user_data_key = f"user_data:{user_id}"
        
        # 根据事件类型更新对应数据
        field_name = event_type.value
        self.redis_client.hset(user_data_key, field_name, json.dumps(data))
        
        # 更新版本号
        current_version = self.redis_client.hget(user_data_key, "version") or "0"
        new_version = int(current_version) + 1
        self.redis_client.hset(user_data_key, "version", str(new_version))
        self.redis_client.hset(user_data_key, "last_updated", datetime.now().isoformat())
    
    async def _publish_sync_event(self, sync_event: SyncEvent):
        """发布同步事件"""
        sync_channel = "stock_analysis_sync"
        event_data = asdict(sync_event)
        event_data['timestamp'] = sync_event.timestamp.isoformat()
        event_data['client_type'] = sync_event.client_type.value
        event_data['event_type'] = sync_event.event_type.value
        
        self.redis_client.publish(sync_channel, json.dumps(event_data))
    
    async def _broadcast_to_clients(self, sync_event: SyncEvent):
        """广播给所有相关客户端"""
        try:
            # 获取用户的所有在线客户端
            client_pattern = f"client:{sync_event.user_id}:*"
            client_keys = self.redis_client.keys(client_pattern)
            
            for client_key in client_keys:
                client_info = self.redis_client.hgetall(client_key)
                client_id = client_info.get('client_id')
                
                # 不发送给发起同步的客户端
                if client_id != sync_event.client_id and client_id in self.active_connections:
                    await self._send_to_client(client_id, {
                        "type": "sync_update",
                        "event_type": sync_event.event_type.value,
                        "data": sync_event.data,
                        "timestamp": sync_event.timestamp.isoformat()
                    })
                    
        except Exception as e:
            logger.error(f"广播同步消息失败: {e}")
    
    async def _send_to_client(self, client_id: str, message: Dict[str, Any]):
        """发送消息给指定客户端"""
        try:
            websocket = self.active_connections.get(client_id)
            if websocket:
                await websocket.send_text(json.dumps(message))
        except Exception as e:
            logger.error(f"发送消息给客户端失败: {e}")
            # 移除失效的连接
            if client_id in self.active_connections:
                del self.active_connections[client_id]
    
    # ============ 数据获取 ============
    
    async def _get_user_sync_data(self, user_id: str) -> Dict[str, Any]:
        """获取用户的所有同步数据"""
        user_data_key = f"user_data:{user_id}"
        user_data = self.redis_client.hgetall(user_data_key)
        
        sync_data = {}
        for event_type in SyncEventType:
            field_name = event_type.value
            if field_name in user_data:
                try:
                    sync_data[field_name] = json.loads(user_data[field_name])
                except json.JSONDecodeError:
                    sync_data[field_name] = user_data[field_name]
        
        return sync_data
    
    async def get_user_data(self, user_id: str, event_type: SyncEventType) -> Optional[Dict[str, Any]]:
        """获取用户特定类型的数据"""
        user_data_key = f"user_data:{user_id}"
        field_name = event_type.value
        
        data = self.redis_client.hget(user_data_key, field_name)
        if data:
            try:
                return json.loads(data)
            except json.JSONDecodeError:
                return {"value": data}
        
        return None
    
    # ============ 冲突解决 ============
    
    async def resolve_conflict(self, user_id: str, event_type: SyncEventType, 
                             conflicting_data: List[Dict[str, Any]]) -> Dict[str, Any]:
        """解决数据冲突"""
        try:
            # 简单的冲突解决策略：使用最新时间戳的数据
            if not conflicting_data:
                return {}
            
            # 按时间戳排序，选择最新的
            sorted_data = sorted(conflicting_data, 
                               key=lambda x: x.get('timestamp', ''), 
                               reverse=True)
            
            resolved_data = sorted_data[0]
            
            # 保存解决后的数据
            await self._update_user_data(user_id, event_type, resolved_data)
            
            logger.info(f"冲突解决成功: {event_type.value} - {user_id}")
            return resolved_data
            
        except Exception as e:
            logger.error(f"冲突解决失败: {e}")
            return {}
    
    # ============ 工具方法 ============
    
    async def get_client_status(self, user_id: str) -> List[SyncStatus]:
        """获取用户所有客户端的同步状态"""
        client_pattern = f"client:{user_id}:*"
        client_keys = self.redis_client.keys(client_pattern)
        
        status_list = []
        for client_key in client_keys:
            client_info = self.redis_client.hgetall(client_key)
            if client_info:
                status = SyncStatus(
                    client_id=client_info['client_id'],
                    client_type=ClientType(client_info['client_type']),
                    last_sync=datetime.fromisoformat(client_info.get('registered_at', datetime.now().isoformat())),
                    version=int(client_info.get('version', '0')),
                    is_online=client_info.get('is_online', 'false').lower() == 'true'
                )
                status_list.append(status)
        
        return status_list
    
    async def cleanup_expired_data(self):
        """清理过期数据"""
        try:
            # 清理过期的同步事件
            pattern = "sync_event:*"
            keys = self.redis_client.keys(pattern)
            
            for key in keys:
                ttl = self.redis_client.ttl(key)
                if ttl == -1:  # 没有设置过期时间
                    self.redis_client.expire(key, 86400)  # 设置24小时过期
            
            logger.info("过期数据清理完成")
            
        except Exception as e:
            logger.error(f"清理过期数据失败: {e}")

# 全局同步管理器实例
sync_manager = CrossPlatformSyncManager()

# Flask路由集成
from flask import Blueprint, request, jsonify

sync_api = Blueprint('sync_api', __name__, url_prefix='/api/v1/sync')

@sync_api.route('/register', methods=['POST'])
async def register_client():
    """注册客户端"""
    data = request.get_json()
    user_id = data.get('user_id')
    client_id = data.get('client_id')
    client_type = ClientType(data.get('client_type'))
    
    success = await sync_manager.register_client(user_id, client_id, client_type)
    
    if success:
        return jsonify({"success": True, "message": "客户端注册成功"})
    else:
        return jsonify({"success": False, "message": "客户端注册失败"}), 500

@sync_api.route('/sync', methods=['POST'])
async def sync_data():
    """同步数据"""
    data = request.get_json()
    user_id = data.get('user_id')
    client_id = data.get('client_id')
    client_type = ClientType(data.get('client_type'))
    event_type = SyncEventType(data.get('event_type'))
    sync_data = data.get('data')
    
    success = await sync_manager.sync_data(user_id, client_id, client_type, event_type, sync_data)
    
    if success:
        return jsonify({"success": True, "message": "数据同步成功"})
    else:
        return jsonify({"success": False, "message": "数据同步失败"}), 500

@sync_api.route('/data/<user_id>/<event_type>', methods=['GET'])
async def get_user_data(user_id: str, event_type: str):
    """获取用户数据"""
    try:
        event_type_enum = SyncEventType(event_type)
        data = await sync_manager.get_user_data(user_id, event_type_enum)
        
        return jsonify({
            "success": True,
            "data": data,
            "timestamp": datetime.now().isoformat()
        })
    except Exception as e:
        return jsonify({"success": False, "error": str(e)}), 500

@sync_api.route('/status/<user_id>', methods=['GET'])
async def get_sync_status(user_id: str):
    """获取同步状态"""
    try:
        status_list = await sync_manager.get_client_status(user_id)
        status_data = [asdict(status) for status in status_list]
        
        return jsonify({
            "success": True,
            "data": status_data,
            "timestamp": datetime.now().isoformat()
        })
    except Exception as e:
        return jsonify({"success": False, "error": str(e)}), 500

if __name__ == '__main__':
    import asyncio
    
    async def main():
        await sync_manager.initialize()
        print("跨平台同步系统启动成功")
    
    asyncio.run(main())