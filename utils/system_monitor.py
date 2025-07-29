#!/usr/bin/env python3
"""
系统监控模块
实时监控系统性能、业务指标和异常状态
支持告警通知、性能分析和自动化运维
"""
import psutil
import logging
import time
import json
import threading
import asyncio
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Any, Callable
from dataclasses import dataclass, asdict
from concurrent.futures import ThreadPoolExecutor
import requests
import smtplib
from email.mime.text import MimeText
from email.mime.multipart import MimeMultipart
import sys
import os
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import config
from utils.db_helper import db
from utils.redis_cache_manager import cache_manager
from utils.data_health_checker import data_health_checker
from sqlalchemy import text

logger = logging.getLogger(__name__)

@dataclass
class SystemMetrics:
    """系统指标数据类"""
    timestamp: str
    cpu_percent: float
    memory_percent: float
    memory_used_gb: float
    memory_total_gb: float
    disk_percent: float
    disk_used_gb: float
    disk_total_gb: float
    network_sent_mb: float
    network_recv_mb: float
    process_count: int
    load_average: List[float]

@dataclass
class BusinessMetrics:
    """业务指标数据类"""
    timestamp: str
    api_response_time: float
    api_success_rate: float
    websocket_connections: int
    database_connections: int
    cache_hit_rate: float
    data_health_score: float
    recommendation_count: int
    error_count: int

@dataclass
class AlertRule:
    """告警规则数据类"""
    name: str
    metric: str
    operator: str  # '>', '<', '>=', '<=', '==', '!='
    threshold: float
    duration: int  # 持续时间(秒)
    severity: str  # 'critical', 'warning', 'info'
    enabled: bool = True
    last_triggered: Optional[str] = None

class SystemMonitor:
    """系统监控器"""
    
    def __init__(self):
        self.is_running = False
        self.monitor_thread = None
        self.executor = ThreadPoolExecutor(max_workers=6)
        self.lock = threading.Lock()
        
        # 监控配置
        self.config = {
            'monitor_interval': 30,  # 监控间隔(秒)
            'metrics_retention_days': 7,  # 指标保留天数
            'alert_cooldown': 300,  # 告警冷却时间(秒)
            'max_retries': 3,  # 最大重试次数
        }
        
        # 告警规则
        self.alert_rules = [
            AlertRule('CPU使用率过高', 'cpu_percent', '>', 80.0, 60, 'warning'),
            AlertRule('内存使用率过高', 'memory_percent', '>', 85.0, 60, 'warning'),
            AlertRule('磁盘使用率过高', 'disk_percent', '>', 90.0, 300, 'critical'),
            AlertRule('API响应时间过长', 'api_response_time', '>', 2000.0, 120, 'warning'),
            AlertRule('API成功率过低', 'api_success_rate', '<', 95.0, 180, 'critical'),
            AlertRule('数据健康评分过低', 'data_health_score', '<', 70.0, 300, 'warning'),
            AlertRule('缓存命中率过低', 'cache_hit_rate', '<', 50.0, 300, 'warning'),
        ]
        
        # 告警状态跟踪
        self.alert_states = {}
        
        # 通知配置
        self.notification_config = {
            'email': {
                'enabled': False,
                'smtp_server': 'smtp.gmail.com',
                'smtp_port': 587,
                'username': '',
                'password': '',
                'recipients': []
            },
            'webhook': {
                'enabled': False,
                'url': '',
                'headers': {'Content-Type': 'application/json'}
            }
        }
        
        # 性能基线
        self.performance_baseline = {
            'cpu_percent': 30.0,
            'memory_percent': 60.0,
            'api_response_time': 500.0,
            'cache_hit_rate': 80.0
        }
        
        # 初始化数据库表
        self._init_database_tables()
    
    def _init_database_tables(self):
        """初始化监控数据库表"""
        try:
            with db.engine.connect() as conn:
                # 系统指标表
                conn.execute(text("""
                    CREATE TABLE IF NOT EXISTS system_metrics (
                        id BIGINT AUTO_INCREMENT PRIMARY KEY,
                        timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                        cpu_percent DECIMAL(5,2),
                        memory_percent DECIMAL(5,2),
                        memory_used_gb DECIMAL(8,2),
                        memory_total_gb DECIMAL(8,2),
                        disk_percent DECIMAL(5,2),
                        disk_used_gb DECIMAL(10,2),
                        disk_total_gb DECIMAL(10,2),
                        network_sent_mb DECIMAL(10,2),
                        network_recv_mb DECIMAL(10,2),
                        process_count INT,
                        load_average JSON,
                        INDEX idx_timestamp (timestamp)
                    ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='系统性能指标'
                """))
                
                # 业务指标表
                conn.execute(text("""
                    CREATE TABLE IF NOT EXISTS business_metrics (
                        id BIGINT AUTO_INCREMENT PRIMARY KEY,
                        timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                        api_response_time DECIMAL(8,2),
                        api_success_rate DECIMAL(5,2),
                        websocket_connections INT,
                        database_connections INT,
                        cache_hit_rate DECIMAL(5,2),
                        data_health_score DECIMAL(5,2),
                        recommendation_count INT,
                        error_count INT,
                        INDEX idx_timestamp (timestamp)
                    ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='业务性能指标'
                """))
                
                # 告警记录表
                conn.execute(text("""
                    CREATE TABLE IF NOT EXISTS alert_records (
                        id BIGINT AUTO_INCREMENT PRIMARY KEY,
                        timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                        rule_name VARCHAR(100),
                        metric VARCHAR(50),
                        current_value DECIMAL(10,2),
                        threshold_value DECIMAL(10,2),
                        severity VARCHAR(20),
                        message TEXT,
                        resolved_at TIMESTAMP NULL,
                        INDEX idx_timestamp (timestamp),
                        INDEX idx_severity (severity),
                        INDEX idx_rule_name (rule_name)
                    ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='告警记录'
                """))
                
                conn.commit()
                logger.info("✅ 监控数据库表初始化完成")
                
        except Exception as e:
            logger.error(f"❌ 监控数据库表初始化失败: {e}")
    
    def collect_system_metrics(self) -> SystemMetrics:
        """收集系统性能指标"""
        try:
            # CPU使用率
            cpu_percent = psutil.cpu_percent(interval=1)
            
            # 内存信息
            memory = psutil.virtual_memory()
            memory_percent = memory.percent
            memory_used_gb = memory.used / (1024**3)
            memory_total_gb = memory.total / (1024**3)
            
            # 磁盘信息
            disk = psutil.disk_usage('/')
            disk_percent = disk.percent
            disk_used_gb = disk.used / (1024**3)
            disk_total_gb = disk.total / (1024**3)
            
            # 网络信息
            network = psutil.net_io_counters()
            network_sent_mb = network.bytes_sent / (1024**2)
            network_recv_mb = network.bytes_recv / (1024**2)
            
            # 进程数量
            process_count = len(psutil.pids())
            
            # 系统负载
            try:
                load_average = list(psutil.getloadavg())
            except:
                load_average = [0.0, 0.0, 0.0]
            
            return SystemMetrics(
                timestamp=datetime.now().isoformat(),
                cpu_percent=cpu_percent,
                memory_percent=memory_percent,
                memory_used_gb=round(memory_used_gb, 2),
                memory_total_gb=round(memory_total_gb, 2),
                disk_percent=disk_percent,
                disk_used_gb=round(disk_used_gb, 2),
                disk_total_gb=round(disk_total_gb, 2),
                network_sent_mb=round(network_sent_mb, 2),
                network_recv_mb=round(network_recv_mb, 2),
                process_count=process_count,
                load_average=load_average
            )
            
        except Exception as e:
            logger.error(f"系统指标收集失败: {e}")
            return None
    
    def collect_business_metrics(self) -> BusinessMetrics:
        """收集业务性能指标"""
        try:
            # API响应时间和成功率
            api_metrics = self._test_api_performance()
            
            # WebSocket连接数
            websocket_connections = self._get_websocket_connections()
            
            # 数据库连接数
            database_connections = self._get_database_connections()
            
            # 缓存命中率
            cache_stats = cache_manager.get_stats()
            cache_hit_rate = cache_stats.get('hit_rate', 0)
            
            # 数据健康评分
            health_data = data_health_checker.check_system_health()
            data_health_score = health_data.get('overall_score', 0)
            
            # 推荐数量
            recommendation_count = self._get_recommendation_count()
            
            # 错误数量
            error_count = self._get_error_count()
            
            return BusinessMetrics(
                timestamp=datetime.now().isoformat(),
                api_response_time=api_metrics['response_time'],
                api_success_rate=api_metrics['success_rate'],
                websocket_connections=websocket_connections,
                database_connections=database_connections,
                cache_hit_rate=cache_hit_rate,
                data_health_score=data_health_score,
                recommendation_count=recommendation_count,
                error_count=error_count
            )
            
        except Exception as e:
            logger.error(f"业务指标收集失败: {e}")
            return None
    
    def _test_api_performance(self) -> Dict[str, float]:
        """测试API性能"""
        try:
            # 测试主要API端点
            test_endpoints = [
                'http://localhost:5000/api/health',
                'http://localhost:5000/api/stocks/basic',
                'http://localhost:5000/api/recommendations'
            ]
            
            response_times = []
            success_count = 0
            
            for endpoint in test_endpoints:
                try:
                    start_time = time.time()
                    response = requests.get(endpoint, timeout=5)
                    response_time = (time.time() - start_time) * 1000
                    
                    response_times.append(response_time)
                    if response.status_code == 200:
                        success_count += 1
                        
                except:
                    response_times.append(5000)  # 超时记为5秒
            
            avg_response_time = sum(response_times) / len(response_times) if response_times else 0
            success_rate = (success_count / len(test_endpoints)) * 100 if test_endpoints else 0
            
            return {
                'response_time': round(avg_response_time, 2),
                'success_rate': round(success_rate, 2)
            }
            
        except Exception as e:
            logger.warning(f"API性能测试失败: {e}")
            return {'response_time': 0, 'success_rate': 0}
    
    def _get_websocket_connections(self) -> int:
        """获取WebSocket连接数"""
        try:
            # 这里应该从WebSocket服务器获取连接数
            # 暂时返回模拟数据
            return 0
        except:
            return 0
    
    def _get_database_connections(self) -> int:
        """获取数据库连接数"""
        try:
            with db.engine.connect() as conn:
                result = conn.execute(text("SHOW STATUS LIKE 'Threads_connected'")).fetchone()
                return int(result[1]) if result else 0
        except:
            return 0
    
    def _get_recommendation_count(self) -> int:
        """获取今日推荐数量"""
        try:
            with db.engine.connect() as conn:
                result = conn.execute(text("""
                    SELECT COUNT(*) FROM recommend_result 
                    WHERE DATE(created_at) = CURDATE()
                """)).scalar()
                return result or 0
        except:
            return 0
    
    def _get_error_count(self) -> int:
        """获取今日错误数量"""
        try:
            # 这里应该从日志系统获取错误数量
            # 暂时返回模拟数据
            return 0
        except:
            return 0
    
    def save_metrics(self, system_metrics: SystemMetrics, business_metrics: BusinessMetrics):
        """保存指标到数据库"""
        try:
            with db.engine.connect() as conn:
                # 保存系统指标
                if system_metrics:
                    conn.execute(text("""
                        INSERT INTO system_metrics 
                        (cpu_percent, memory_percent, memory_used_gb, memory_total_gb,
                         disk_percent, disk_used_gb, disk_total_gb, network_sent_mb,
                         network_recv_mb, process_count, load_average)
                        VALUES (:cpu_percent, :memory_percent, :memory_used_gb, :memory_total_gb,
                                :disk_percent, :disk_used_gb, :disk_total_gb, :network_sent_mb,
                                :network_recv_mb, :process_count, :load_average)
                    """), {
                        **asdict(system_metrics),
                        'load_average': json.dumps(system_metrics.load_average)
                    })
                
                # 保存业务指标
                if business_metrics:
                    conn.execute(text("""
                        INSERT INTO business_metrics 
                        (api_response_time, api_success_rate, websocket_connections,
                         database_connections, cache_hit_rate, data_health_score,
                         recommendation_count, error_count)
                        VALUES (:api_response_time, :api_success_rate, :websocket_connections,
                                :database_connections, :cache_hit_rate, :data_health_score,
                                :recommendation_count, :error_count)
                    """), asdict(business_metrics))
                
                conn.commit()
                
        except Exception as e:
            logger.error(f"指标保存失败: {e}")
    
    def check_alerts(self, system_metrics: SystemMetrics, business_metrics: BusinessMetrics):
        """检查告警规则"""
        current_time = datetime.now()
        
        # 合并所有指标
        all_metrics = {}
        if system_metrics:
            all_metrics.update(asdict(system_metrics))
        if business_metrics:
            all_metrics.update(asdict(business_metrics))
        
        for rule in self.alert_rules:
            if not rule.enabled:
                continue
            
            try:
                metric_value = all_metrics.get(rule.metric)
                if metric_value is None:
                    continue
                
                # 检查告警条件
                triggered = self._evaluate_alert_condition(metric_value, rule)
                
                if triggered:
                    # 检查是否在冷却期内
                    if self._is_in_cooldown(rule):
                        continue
                    
                    # 触发告警
                    self._trigger_alert(rule, metric_value, current_time)
                else:
                    # 检查是否需要解除告警
                    self._resolve_alert_if_needed(rule, current_time)
                    
            except Exception as e:
                logger.error(f"告警检查失败 {rule.name}: {e}")
    
    def _evaluate_alert_condition(self, value: float, rule: AlertRule) -> bool:
        """评估告警条件"""
        if rule.operator == '>':
            return value > rule.threshold
        elif rule.operator == '<':
            return value < rule.threshold
        elif rule.operator == '>=':
            return value >= rule.threshold
        elif rule.operator == '<=':
            return value <= rule.threshold
        elif rule.operator == '==':
            return value == rule.threshold
        elif rule.operator == '!=':
            return value != rule.threshold
        return False
    
    def _is_in_cooldown(self, rule: AlertRule) -> bool:
        """检查是否在告警冷却期内"""
        if rule.last_triggered:
            last_time = datetime.fromisoformat(rule.last_triggered)
            cooldown_period = timedelta(seconds=self.config['alert_cooldown'])
            return datetime.now() - last_time < cooldown_period
        return False
    
    def _trigger_alert(self, rule: AlertRule, current_value: float, timestamp: datetime):
        """触发告警"""
        try:
            # 更新告警状态
            rule.last_triggered = timestamp.isoformat()
            self.alert_states[rule.name] = {
                'triggered_at': timestamp.isoformat(),
                'current_value': current_value,
                'resolved': False
            }
            
            # 生成告警消息
            message = f"告警: {rule.name}\n"
            message += f"当前值: {current_value}\n"
            message += f"阈值: {rule.threshold}\n"
            message += f"严重程度: {rule.severity}\n"
            message += f"时间: {timestamp.strftime('%Y-%m-%d %H:%M:%S')}"
            
            # 记录告警到数据库
            self._save_alert_record(rule, current_value, message)
            
            # 发送通知
            self._send_notification(rule, message)
            
            logger.warning(f"🚨 告警触发: {rule.name} - {current_value}")
            
        except Exception as e:
            logger.error(f"告警触发失败: {e}")
    
    def _resolve_alert_if_needed(self, rule: AlertRule, timestamp: datetime):
        """解除告警（如果需要）"""
        if rule.name in self.alert_states and not self.alert_states[rule.name]['resolved']:
            # 标记为已解除
            self.alert_states[rule.name]['resolved'] = True
            self.alert_states[rule.name]['resolved_at'] = timestamp.isoformat()
            
            # 更新数据库记录
            try:
                with db.engine.connect() as conn:
                    conn.execute(text("""
                        UPDATE alert_records 
                        SET resolved_at = NOW() 
                        WHERE rule_name = :rule_name AND resolved_at IS NULL
                    """), {'rule_name': rule.name})
                    conn.commit()
            except Exception as e:
                logger.error(f"告警解除记录更新失败: {e}")
            
            logger.info(f"✅ 告警解除: {rule.name}")
    
    def _save_alert_record(self, rule: AlertRule, current_value: float, message: str):
        """保存告警记录"""
        try:
            with db.engine.connect() as conn:
                conn.execute(text("""
                    INSERT INTO alert_records 
                    (rule_name, metric, current_value, threshold_value, severity, message)
                    VALUES (:rule_name, :metric, :current_value, :threshold_value, :severity, :message)
                """), {
                    'rule_name': rule.name,
                    'metric': rule.metric,
                    'current_value': current_value,
                    'threshold_value': rule.threshold,
                    'severity': rule.severity,
                    'message': message
                })
                conn.commit()
        except Exception as e:
            logger.error(f"告警记录保存失败: {e}")
    
    def _send_notification(self, rule: AlertRule, message: str):
        """发送告警通知"""
        try:
            # 邮件通知
            if self.notification_config['email']['enabled']:
                self._send_email_notification(rule, message)
            
            # Webhook通知
            if self.notification_config['webhook']['enabled']:
                self._send_webhook_notification(rule, message)
                
        except Exception as e:
            logger.error(f"通知发送失败: {e}")
    
    def _send_email_notification(self, rule: AlertRule, message: str):
        """发送邮件通知"""
        try:
            config = self.notification_config['email']
            
            msg = MimeMultipart()
            msg['From'] = config['username']
            msg['To'] = ', '.join(config['recipients'])
            msg['Subject'] = f"[{rule.severity.upper()}] 系统告警: {rule.name}"
            
            msg.attach(MimeText(message, 'plain', 'utf-8'))
            
            server = smtplib.SMTP(config['smtp_server'], config['smtp_port'])
            server.starttls()
            server.login(config['username'], config['password'])
            server.send_message(msg)
            server.quit()
            
            logger.info(f"📧 邮件告警发送成功: {rule.name}")
            
        except Exception as e:
            logger.error(f"邮件告警发送失败: {e}")
    
    def _send_webhook_notification(self, rule: AlertRule, message: str):
        """发送Webhook通知"""
        try:
            config = self.notification_config['webhook']
            
            payload = {
                'rule_name': rule.name,
                'metric': rule.metric,
                'severity': rule.severity,
                'message': message,
                'timestamp': datetime.now().isoformat()
            }
            
            response = requests.post(
                config['url'],
                json=payload,
                headers=config['headers'],
                timeout=10
            )
            
            if response.status_code == 200:
                logger.info(f"🔗 Webhook告警发送成功: {rule.name}")
            else:
                logger.error(f"Webhook告警发送失败: {response.status_code}")
                
        except Exception as e:
            logger.error(f"Webhook告警发送失败: {e}")
    
    def start_monitoring(self):
        """启动监控"""
        if self.is_running:
            logger.warning("监控已在运行中")
            return
        
        self.is_running = True
        self.monitor_thread = threading.Thread(target=self._monitoring_loop, daemon=True)
        self.monitor_thread.start()
        
        logger.info("🔍 系统监控已启动")
    
    def stop_monitoring(self):
        """停止监控"""
        self.is_running = False
        if self.monitor_thread:
            self.monitor_thread.join(timeout=10)
        
        logger.info("🛑 系统监控已停止")
    
    def _monitoring_loop(self):
        """监控主循环"""
        logger.info(f"🔄 监控循环启动，间隔: {self.config['monitor_interval']}秒")
        
        while self.is_running:
            try:
                # 收集指标
                system_metrics = self.collect_system_metrics()
                business_metrics = self.collect_business_metrics()
                
                # 保存指标
                self.save_metrics(system_metrics, business_metrics)
                
                # 检查告警
                self.check_alerts(system_metrics, business_metrics)
                
                # 缓存最新指标
                if system_metrics:
                    cache_manager.set('system_status', 'latest_system_metrics', asdict(system_metrics), 300)
                if business_metrics:
                    cache_manager.set('system_status', 'latest_business_metrics', asdict(business_metrics), 300)
                
                # 清理过期数据
                self._cleanup_old_data()
                
                time.sleep(self.config['monitor_interval'])
                
            except Exception as e:
                logger.error(f"监控循环异常: {e}")
                time.sleep(10)  # 出错后短暂等待
    
    def _cleanup_old_data(self):
        """清理过期监控数据"""
        try:
            retention_days = self.config['metrics_retention_days']
            cutoff_date = datetime.now() - timedelta(days=retention_days)
            
            with db.engine.connect() as conn:
                # 清理系统指标
                conn.execute(text("""
                    DELETE FROM system_metrics 
                    WHERE timestamp < :cutoff_date
                """), {'cutoff_date': cutoff_date})
                
                # 清理业务指标
                conn.execute(text("""
                    DELETE FROM business_metrics 
                    WHERE timestamp < :cutoff_date
                """), {'cutoff_date': cutoff_date})
                
                # 清理已解决的告警记录（保留30天）
                alert_cutoff = datetime.now() - timedelta(days=30)
                conn.execute(text("""
                    DELETE FROM alert_records 
                    WHERE resolved_at IS NOT NULL AND resolved_at < :cutoff_date
                """), {'cutoff_date': alert_cutoff})
                
                conn.commit()
                
        except Exception as e:
            logger.error(f"数据清理失败: {e}")
    
    def get_current_status(self) -> Dict[str, Any]:
        """获取当前系统状态"""
        try:
            # 获取最新指标
            system_metrics = self.collect_system_metrics()
            business_metrics = self.collect_business_metrics()
            
            # 获取活跃告警
            active_alerts = [
                {
                    'name': name,
                    'triggered_at': state['triggered_at'],
                    'current_value': state['current_value']
                }
                for name, state in self.alert_states.items()
                if not state['resolved']
            ]
            
            # 计算整体健康状态
            health_score = self._calculate_overall_health(system_metrics, business_metrics)
            
            return {
                'timestamp': datetime.now().isoformat(),
                'health_score': health_score,
                'status': 'healthy' if health_score >= 80 else 'warning' if health_score >= 60 else 'critical',
                'system_metrics': asdict(system_metrics) if system_metrics else {},
                'business_metrics': asdict(business_metrics) if business_metrics else {},
                'active_alerts': active_alerts,
                'monitoring_status': 'running' if self.is_running else 'stopped'
            }
            
        except Exception as e:
            logger.error(f"状态获取失败: {e}")
            return {
                'timestamp': datetime.now().isoformat(),
                'health_score': 0,
                'status': 'error',
                'error': str(e)
            }
    
    def _calculate_overall_health(self, system_metrics: SystemMetrics, business_metrics: BusinessMetrics) -> float:
        """计算整体健康评分"""
        try:
            score = 100.0
            
            if system_metrics:
                # 系统指标权重 40%
                if system_metrics.cpu_percent > 80:
                    score -= 15
                elif system_metrics.cpu_percent > 60:
                    score -= 8
                
                if system_metrics.memory_percent > 85:
                    score -= 15
                elif system_metrics.memory_percent > 70:
                    score -= 8
                
                if system_metrics.disk_percent > 90:
                    score -= 10
                elif system_metrics.disk_percent > 80:
                    score -= 5
            
            if business_metrics:
                # 业务指标权重 60%
                if business_metrics.api_response_time > 2000:
                    score -= 20
                elif business_metrics.api_response_time > 1000:
                    score -= 10
                
                if business_metrics.api_success_rate < 95:
                    score -= 20
                elif business_metrics.api_success_rate < 98:
                    score -= 10
                
                if business_metrics.data_health_score < 70:
                    score -= 15
                elif business_metrics.data_health_score < 85:
                    score -= 8
                
                if business_metrics.cache_hit_rate < 50:
                    score -= 5
            
            return max(0.0, score)
            
        except Exception as e:
            logger.error(f"健康评分计算失败: {e}")
            return 0.0
    
    def get_performance_report(self, hours: int = 24) -> Dict[str, Any]:
        """生成性能报告"""
        try:
            end_time = datetime.now()
            start_time = end_time - timedelta(hours=hours)
            
            with db.engine.connect() as conn:
                # 系统指标统计
                system_stats = conn.execute(text("""
                    SELECT
                        AVG(cpu_percent) as avg_cpu,
                        MAX(cpu_percent) as max_cpu,
                        AVG(memory_percent) as avg_memory,
                        MAX(memory_percent) as max_memory,
                        AVG(disk_percent) as avg_disk,
                        COUNT(*) as sample_count
                    FROM system_metrics
                    WHERE timestamp BETWEEN :start_time AND :end_time
                """), {'start_time': start_time, 'end_time': end_time}).fetchone()
                
                # 业务指标统计
                business_stats = conn.execute(text("""
                    SELECT
                        AVG(api_response_time) as avg_response_time,
                        MAX(api_response_time) as max_response_time,
                        AVG(api_success_rate) as avg_success_rate,
                        MIN(api_success_rate) as min_success_rate,
                        AVG(cache_hit_rate) as avg_cache_hit_rate,
                        AVG(data_health_score) as avg_health_score
                    FROM business_metrics
                    WHERE timestamp BETWEEN :start_time AND :end_time
                """), {'start_time': start_time, 'end_time': end_time}).fetchone()
                
                # 告警统计
                alert_stats = conn.execute(text("""
                    SELECT
                        COUNT(*) as total_alerts,
                        SUM(CASE WHEN severity = 'critical' THEN 1 ELSE 0 END) as critical_alerts,
                        SUM(CASE WHEN severity = 'warning' THEN 1 ELSE 0 END) as warning_alerts
                    FROM alert_records
                    WHERE timestamp BETWEEN :start_time AND :end_time
                """), {'start_time': start_time, 'end_time': end_time}).fetchone()
                
                # 生成报告
                report = {
                    'period': {
                        'start_time': start_time.isoformat(),
                        'end_time': end_time.isoformat(),
                        'duration_hours': hours
                    },
                    'system_performance': {
                        'avg_cpu_percent': round(system_stats.avg_cpu or 0, 2),
                        'max_cpu_percent': round(system_stats.max_cpu or 0, 2),
                        'avg_memory_percent': round(system_stats.avg_memory or 0, 2),
                        'max_memory_percent': round(system_stats.max_memory or 0, 2),
                        'avg_disk_percent': round(system_stats.avg_disk or 0, 2),
                        'sample_count': system_stats.sample_count or 0
                    },
                    'business_performance': {
                        'avg_response_time': round(business_stats.avg_response_time or 0, 2),
                        'max_response_time': round(business_stats.max_response_time or 0, 2),
                        'avg_success_rate': round(business_stats.avg_success_rate or 0, 2),
                        'min_success_rate': round(business_stats.min_success_rate or 0, 2),
                        'avg_cache_hit_rate': round(business_stats.avg_cache_hit_rate or 0, 2),
                        'avg_health_score': round(business_stats.avg_health_score or 0, 2)
                    },
                    'alerts': {
                        'total_alerts': alert_stats.total_alerts or 0,
                        'critical_alerts': alert_stats.critical_alerts or 0,
                        'warning_alerts': alert_stats.warning_alerts or 0
                    },
                    'recommendations': self._generate_performance_recommendations(system_stats, business_stats)
                }
                
                return report
                
        except Exception as e:
            logger.error(f"性能报告生成失败: {e}")
            return {
                'error': str(e),
                'timestamp': datetime.now().isoformat()
            }
    
    def _generate_performance_recommendations(self, system_stats, business_stats) -> List[str]:
        """生成性能优化建议"""
        recommendations = []
        
        try:
            # 系统性能建议
            if system_stats.avg_cpu and system_stats.avg_cpu > 70:
                recommendations.append("CPU使用率偏高，建议优化计算密集型任务或增加CPU资源")
            
            if system_stats.avg_memory and system_stats.avg_memory > 80:
                recommendations.append("内存使用率偏高，建议优化内存使用或增加内存容量")
            
            if system_stats.avg_disk and system_stats.avg_disk > 85:
                recommendations.append("磁盘使用率偏高，建议清理无用文件或扩容磁盘")
            
            # 业务性能建议
            if business_stats.avg_response_time and business_stats.avg_response_time > 1000:
                recommendations.append("API响应时间偏长，建议优化数据库查询和缓存策略")
            
            if business_stats.avg_success_rate and business_stats.avg_success_rate < 98:
                recommendations.append("API成功率偏低，建议检查错误日志和异常处理")
            
            if business_stats.avg_cache_hit_rate and business_stats.avg_cache_hit_rate < 70:
                recommendations.append("缓存命中率偏低，建议优化缓存策略和预热机制")
            
            if business_stats.avg_health_score and business_stats.avg_health_score < 80:
                recommendations.append("数据健康评分偏低，建议检查数据质量和完整性")
            
            if not recommendations:
                recommendations.append("系统运行良好，继续保持当前配置")
                
        except Exception as e:
            logger.error(f"性能建议生成失败: {e}")
            recommendations.append("性能建议生成失败，请检查监控数据")
        
        return recommendations

# 创建全局监控实例
system_monitor = SystemMonitor()

def main():
    """主函数"""
    import argparse
    
    parser = argparse.ArgumentParser(description='系统监控模块')
    parser.add_argument('command', choices=['start', 'stop', 'status', 'report', 'test'],
                       help='操作命令')
    parser.add_argument('--hours', type=int, default=24,
                       help='报告时间范围(小时)')
    
    args = parser.parse_args()
    
    # 配置日志
    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s - %(levelname)s - %(message)s',
        handlers=[
            logging.StreamHandler(),
            logging.FileHandler('logs/system_monitor.log', encoding='utf-8')
        ]
    )
    
    try:
        if args.command == 'start':
            system_monitor.start_monitoring()
            print("✅ 系统监控已启动")
            
            # 保持运行
            try:
                while True:
                    time.sleep(1)
            except KeyboardInterrupt:
                system_monitor.stop_monitoring()
                print("🛑 系统监控已停止")
                
        elif args.command == 'stop':
            system_monitor.stop_monitoring()
            print("🛑 系统监控已停止")
            
        elif args.command == 'status':
            status = system_monitor.get_current_status()
            print("📊 当前系统状态:")
            print(json.dumps(status, indent=2, ensure_ascii=False, default=str))
            
        elif args.command == 'report':
            report = system_monitor.get_performance_report(args.hours)
            print(f"📈 {args.hours}小时性能报告:")
            print(json.dumps(report, indent=2, ensure_ascii=False, default=str))
            
        elif args.command == 'test':
            print("🧪 测试监控功能...")
            
            # 测试指标收集
            system_metrics = system_monitor.collect_system_metrics()
            business_metrics = system_monitor.collect_business_metrics()
            
            print("系统指标:", asdict(system_metrics) if system_metrics else "收集失败")
            print("业务指标:", asdict(business_metrics) if business_metrics else "收集失败")
            
            # 测试告警检查
            system_monitor.check_alerts(system_metrics, business_metrics)
            print("告警检查完成")
            
    except Exception as e:
        logger.error(f"❌ 程序执行失败: {e}")

if __name__ == "__main__":
    main()