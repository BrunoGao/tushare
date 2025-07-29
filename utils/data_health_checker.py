#!/usr/bin/env python3
"""
数据健康检查器 - 增强版
自动监控数据完整性，检测数据质量问题，并触发自动修复
支持多维度数据质量评估和智能修复策略
"""
import logging
import pandas as pd
from datetime import datetime, timedelta
from typing import Dict, List, Tuple, Optional
import json
import time
import threading
import asyncio
from concurrent.futures import ThreadPoolExecutor
from sqlalchemy import text
import sys
import os
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import config
from utils.db_helper import db
from scripts.fetch_comprehensive_data import ComprehensiveDataFetcher

logger = logging.getLogger(__name__)

class DataHealthChecker:
    """数据健康检查器"""
    
    def __init__(self):
        self.fetcher = ComprehensiveDataFetcher()
        self.health_report = {}
        self.executor = ThreadPoolExecutor(max_workers=4)
        self.lock = threading.Lock()
        
        # 扩展关键表列表
        self.critical_tables = [
            'stock_basic', 't_stock_basic', 'stock_indicators',
            't_financial_indicators', 't_money_flow', 'recommend_result',
            't_daily_basic', 't_concept', 't_concept_detail', 't_index_basic'
        ]
        
        # 增强数据质量标准
        self.quality_standards = {
            'stock_basic': {
                'min_records': 4000,
                'required_fields': ['ts_code', 'name', 'industry', 'market'],
                'data_types': {'ts_code': 'str', 'name': 'str'},
                'null_tolerance': 0.01  # 允许1%的空值
            },
            't_stock_basic': {
                'min_records': 4000,
                'required_fields': ['ts_code', 'name', 'industry', 'market'],
                'data_types': {'ts_code': 'str', 'name': 'str'},
                'null_tolerance': 0.01
            },
            'stock_indicators': {
                'min_records': 100000,
                'required_fields': ['ts_code', 'trade_date', 'close', 'volume'],
                'data_types': {'close': 'float', 'volume': 'int'},
                'null_tolerance': 0.05
            },
            't_financial_indicators': {
                'min_records': 50000,
                'required_fields': ['ts_code', 'end_date', 'total_revenue'],
                'data_types': {'total_revenue': 'float'},
                'null_tolerance': 0.1
            },
            't_money_flow': {
                'min_records': 10000,
                'required_fields': ['ts_code', 'trade_date', 'net_mf_amount'],
                'data_types': {'net_mf_amount': 'float'},
                'null_tolerance': 0.05
            },
            'recommend_result': {
                'min_records': 10,
                'required_fields': ['ts_code', 'score', 'strategy'],
                'data_types': {'score': 'float'},
                'null_tolerance': 0.0
            },
            't_daily_basic': {
                'min_records': 50000,
                'required_fields': ['ts_code', 'trade_date', 'pe', 'pb'],
                'data_types': {'pe': 'float', 'pb': 'float'},
                'null_tolerance': 0.1
            },
            't_concept': {
                'min_records': 100,
                'required_fields': ['concept_code', 'concept_name'],
                'data_types': {'concept_code': 'str'},
                'null_tolerance': 0.0
            }
        }
        
        # 数据质量检查规则
        self.quality_rules = {
            'duplicate_check': True,
            'outlier_detection': True,
            'consistency_check': True,
            'completeness_check': True,
            'timeliness_check': True
        }
        
    def check_table_health(self, table_name: str) -> Dict:
        """检查单个表的健康状态 - 增强版"""
        try:
            with db.engine.connect() as conn:
                # 检查表是否存在
                table_exists = conn.execute(text(f"SHOW TABLES LIKE '{table_name}'")).fetchone()
                if not table_exists:
                    return {
                        'table': table_name,
                        'status': 'MISSING',
                        'record_count': 0,
                        'issues': ['表不存在'],
                        'health_score': 0,
                        'quality_metrics': {}
                    }
                
                # 基础统计信息
                record_count = conn.execute(text(f"SELECT COUNT(*) FROM {table_name}")).scalar()
                
                # 多维度质量检查
                quality_metrics = self._comprehensive_quality_check(table_name, conn)
                
                # 检查必需字段
                missing_fields = self._check_required_fields(table_name, conn)
                
                # 检查数据新鲜度
                data_freshness = self._check_data_freshness(table_name, conn)
                
                # 检查数据完整性
                completeness_score = self._check_data_completeness(table_name, conn)
                
                # 检查数据一致性
                consistency_score = self._check_data_consistency(table_name, conn)
                
                # 计算综合健康评分
                health_score = self._calculate_enhanced_health_score(
                    table_name, record_count, missing_fields, data_freshness,
                    completeness_score, consistency_score, quality_metrics
                )
                
                # 生成详细问题列表
                issues = self._generate_detailed_issues(
                    table_name, record_count, missing_fields, data_freshness,
                    completeness_score, consistency_score, quality_metrics
                )
                
                status = self._determine_health_status(health_score, issues)
                
                return {
                    'table': table_name,
                    'status': status,
                    'record_count': record_count,
                    'missing_fields': missing_fields,
                    'data_freshness': data_freshness,
                    'completeness_score': completeness_score,
                    'consistency_score': consistency_score,
                    'quality_metrics': quality_metrics,
                    'health_score': health_score,
                    'issues': issues,
                    'check_timestamp': datetime.now().isoformat()
                }
                
        except Exception as e:
            logger.error(f"检查表 {table_name} 健康状态失败: {e}")
            return {
                'table': table_name,
                'status': 'ERROR',
                'record_count': 0,
                'issues': [f'检查失败: {str(e)}'],
                'health_score': 0,
                'quality_metrics': {},
                'error': str(e)
            }
    
    def _check_data_freshness(self, table_name: str, conn) -> Dict:
        """检查数据新鲜度"""
        try:
            # 尝试不同的日期字段
            date_fields = ['trade_date', 'ann_date', 'end_date', 'created_at', 'updated_at']
            
            for field in date_fields:
                try:
                    result = conn.execute(text(f"SELECT MAX({field}) FROM {table_name}")).scalar()
                    if result:
                        if isinstance(result, str):
                            latest_date = pd.to_datetime(result).date()
                        else:
                            latest_date = result.date() if hasattr(result, 'date') else result
                        
                        days_old = (datetime.now().date() - latest_date).days
                        return {
                            'latest_date': latest_date.isoformat(),
                            'days_old': days_old,
                            'date_field': field
                        }
                except:
                    continue
            
            return {'latest_date': None, 'days_old': 999, 'date_field': None}
            
        except Exception as e:
            return {'latest_date': None, 'days_old': 999, 'date_field': None, 'error': str(e)}
    
    def _calculate_health_score(self, table_name: str, record_count: int, missing_fields: List, data_freshness: Dict) -> int:
        """计算健康评分 (0-100)"""
        score = 100
        
        # 记录数量评分 (40分)
        if table_name in self.quality_standards:
            min_records = self.quality_standards[table_name]['min_records']
            if record_count == 0:
                score -= 40
            elif record_count < min_records * 0.5:
                score -= 30
            elif record_count < min_records:
                score -= 20
        
        # 字段完整性评分 (30分)
        if missing_fields:
            score -= len(missing_fields) * 10
        
        # 数据新鲜度评分 (30分)
        days_old = data_freshness.get('days_old', 999)
        if days_old > 30:
            score -= 30
        elif days_old > 7:
            score -= 20
        elif days_old > 3:
            score -= 10
        
        return max(0, score)
    
    def check_system_health(self) -> Dict:
        """检查整个系统的健康状态"""
        logger.info("🔍 开始系统健康检查...")
        
        system_health = {
            'check_time': datetime.now().isoformat(),
            'tables': {},
            'overall_status': 'HEALTHY',
            'critical_issues': [],
            'warnings': [],
            'recommendations': []
        }
        
        critical_issues = []
        warnings = []
        total_score = 0
        
        # 检查关键表
        for table_name in self.critical_tables:
            table_health = self.check_table_health(table_name)
            system_health['tables'][table_name] = table_health
            
            if table_health['status'] == 'CRITICAL' or table_health['status'] == 'MISSING':
                critical_issues.extend(table_health['issues'])
            elif table_health['status'] == 'WARNING':
                warnings.extend(table_health['issues'])
            
            total_score += table_health['health_score']
        
        # 计算整体健康评分
        avg_score = total_score / len(self.critical_tables) if self.critical_tables else 0
        
        if avg_score >= 80:
            system_health['overall_status'] = 'HEALTHY'
        elif avg_score >= 60:
            system_health['overall_status'] = 'WARNING'
        else:
            system_health['overall_status'] = 'CRITICAL'
        
        system_health['overall_score'] = round(avg_score, 2)
        system_health['critical_issues'] = critical_issues
        system_health['warnings'] = warnings
        
        # 生成修复建议
        system_health['recommendations'] = self._generate_recommendations(system_health)
        
        logger.info(f"✅ 系统健康检查完成，整体评分: {avg_score:.1f}")
        return system_health
    
    def _generate_recommendations(self, health_report: Dict) -> List[str]:
        """生成修复建议"""
        recommendations = []
        
        for table_name, table_health in health_report['tables'].items():
            if table_health['status'] in ['CRITICAL', 'MISSING']:
                if table_health['record_count'] == 0:
                    recommendations.append(f"执行数据初始化: python3 scripts/fetch_comprehensive_data.py --category basic")
                
                if 'stock_basic' in table_name and table_health['record_count'] < 1000:
                    recommendations.append(f"获取股票基本信息: python3 scripts/fetch_stock_basic.py")
                
                if 'financial' in table_name and table_health['record_count'] < 10000:
                    recommendations.append(f"获取财务数据: python3 run.py fetch-comprehensive --category financial")
                
                if 'money_flow' in table_name and table_health['record_count'] < 5000:
                    recommendations.append(f"获取资金流数据: python3 run.py fetch-comprehensive --category money_flow")
        
        # 通用建议
        if health_report['overall_score'] < 70:
            recommendations.append("运行完整数据获取: python3 run.py fetch-comprehensive --mode full")
            recommendations.append("启动数据调度器: python3 utils/data_scheduler.py start")
        
        return recommendations
    
    def auto_repair(self, health_report: Dict) -> Dict:
        """自动修复数据问题"""
        logger.info("🔧 开始自动修复数据问题...")
        
        repair_results = {
            'start_time': datetime.now().isoformat(),
            'actions_taken': [],
            'success_count': 0,
            'failure_count': 0,
            'details': []
        }
        
        try:
            # 执行推荐的修复操作
            for recommendation in health_report.get('recommendations', []):
                try:
                    if 'fetch_stock_basic' in recommendation:
                        logger.info("📊 执行股票基本信息获取...")
                        result = self.fetcher.fetch_basic_info_data('incremental')
                        repair_results['actions_taken'].append('股票基本信息获取')
                        repair_results['success_count'] += 1
                        repair_results['details'].append(f"股票基本信息获取: {result}")
                        
                    elif 'fetch-comprehensive' in recommendation and 'financial' in recommendation:
                        logger.info("💼 执行财务数据获取...")
                        result = self.fetcher.fetch_financial_data('incremental')
                        repair_results['actions_taken'].append('财务数据获取')
                        repair_results['success_count'] += 1
                        repair_results['details'].append(f"财务数据获取: {result}")
                        
                    elif 'fetch-comprehensive' in recommendation and 'money_flow' in recommendation:
                        logger.info("💸 执行资金流数据获取...")
                        result = self.fetcher.fetch_money_flow_data('incremental')
                        repair_results['actions_taken'].append('资金流数据获取')
                        repair_results['success_count'] += 1
                        repair_results['details'].append(f"资金流数据获取: {result}")
                        
                    time.sleep(2)  # 避免API频率限制
                    
                except Exception as e:
                    logger.error(f"修复操作失败: {e}")
                    repair_results['failure_count'] += 1
                    repair_results['details'].append(f"修复失败: {str(e)}")
        
        except Exception as e:
            logger.error(f"自动修复过程异常: {e}")
            repair_results['details'].append(f"修复过程异常: {str(e)}")
        
        repair_results['end_time'] = datetime.now().isoformat()
        logger.info(f"🔧 自动修复完成，成功: {repair_results['success_count']}, 失败: {repair_results['failure_count']}")
        
        return repair_results
    
    def generate_health_report(self, save_to_file: bool = True) -> str:
        """生成健康检查报告"""
        health_data = self.check_system_health()
        
        report = f"""
# 📊 数据健康检查报告

**检查时间**: {health_data['check_time']}
**整体状态**: {health_data['overall_status']} ({health_data['overall_score']}/100)

## 📋 表健康状态

| 表名 | 状态 | 记录数 | 健康评分 | 主要问题 |
|------|------|--------|----------|----------|
"""
        
        for table_name, table_health in health_data['tables'].items():
            issues = '; '.join(table_health['issues'][:2]) if table_health['issues'] else '无'
            report += f"| {table_name} | {table_health['status']} | {table_health['record_count']:,} | {table_health['health_score']}/100 | {issues} |\n"
        
        if health_data['critical_issues']:
            report += f"\n## 🚨 严重问题\n"
            for issue in health_data['critical_issues']:
                report += f"- {issue}\n"
        
        if health_data['warnings']:
            report += f"\n## ⚠️ 警告\n"
            for warning in health_data['warnings']:
                report += f"- {warning}\n"
        
        if health_data['recommendations']:
            report += f"\n## 🔧 修复建议\n"
            for rec in health_data['recommendations']:
                report += f"- {rec}\n"
        
        report += f"\n---\n*报告生成时间: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}*\n"
        
        if save_to_file:
            report_file = f"logs/health_report_{datetime.now().strftime('%Y%m%d_%H%M%S')}.md"
            os.makedirs('logs', exist_ok=True)
            with open(report_file, 'w', encoding='utf-8') as f:
                f.write(report)
            logger.info(f"📄 健康报告已保存: {report_file}")
        
        return report
    
    def monitor_continuous(self, interval_minutes: int = 30):
        """持续监控模式"""
        logger.info(f"🔄 启动持续监控模式，检查间隔: {interval_minutes}分钟")
        
        while True:
            try:
                health_data = self.check_system_health()
                
                # 如果发现严重问题，尝试自动修复
                if health_data['overall_status'] == 'CRITICAL':
                    logger.warning("🚨 检测到严重问题，启动自动修复...")
                    repair_result = self.auto_repair(health_data)
                    logger.info(f"🔧 自动修复完成: {repair_result}")
                
                # 记录监控结果
                self._log_monitoring_result(health_data)
                
                time.sleep(interval_minutes * 60)
                
            except KeyboardInterrupt:
                logger.info("🛑 监控已停止")
                break
            except Exception as e:
                logger.error(f"❌ 监控过程异常: {e}")
                time.sleep(60)  # 出错后等待1分钟再继续
    
    def _log_monitoring_result(self, health_data: Dict):
        """记录监控结果到数据库"""
        try:
            with db.engine.connect() as conn:
                # 创建监控日志表（如果不存在）
                conn.execute(text("""
                    CREATE TABLE IF NOT EXISTS data_health_log (
                        id BIGINT AUTO_INCREMENT PRIMARY KEY,
                        check_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                        overall_status VARCHAR(20),
                        overall_score DECIMAL(5,2),
                        critical_issues_count INT,
                        warnings_count INT,
                        details JSON,
                        INDEX idx_check_time (check_time),
                        INDEX idx_status (overall_status)
                    ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='数据健康监控日志'
                """))
                
                # 插入监控记录
                conn.execute(text("""
                    INSERT INTO data_health_log 
                    (overall_status, overall_score, critical_issues_count, warnings_count, details)
                    VALUES (:status, :score, :critical_count, :warning_count, :details)
                """), {
                    'status': health_data['overall_status'],
                    'score': health_data['overall_score'],
                    'critical_count': len(health_data['critical_issues']),
                    'warning_count': len(health_data['warnings']),
                    'details': json.dumps(health_data, ensure_ascii=False, default=str)
                })
                conn.commit()
                
        except Exception as e:
            logger.error(f"记录监控结果失败: {e}")
    
    def _comprehensive_quality_check(self, table_name: str, conn) -> Dict:
        """综合数据质量检查"""
        metrics = {}
        
        try:
            # 重复数据检查
            if self.quality_rules['duplicate_check']:
                metrics['duplicate_rate'] = self._check_duplicate_rate(table_name, conn)
            
            # 空值率检查
            metrics['null_rates'] = self._check_null_rates(table_name, conn)
            
            # 数据类型一致性
            metrics['type_consistency'] = self._check_type_consistency(table_name, conn)
            
            # 数据分布异常检查
            if self.quality_rules['outlier_detection']:
                metrics['outlier_detection'] = self._detect_outliers(table_name, conn)
            
            # 数据增长趋势
            metrics['growth_trend'] = self._analyze_growth_trend(table_name, conn)
            
        except Exception as e:
            logger.warning(f"质量检查异常 {table_name}: {e}")
            metrics['error'] = str(e)
        
        return metrics
    
    def _check_required_fields(self, table_name: str, conn) -> List[str]:
        """检查必需字段"""
        missing_fields = []
        
        if table_name in self.quality_standards:
            required_fields = self.quality_standards[table_name]['required_fields']
            for field in required_fields:
                try:
                    conn.execute(text(f"SELECT {field} FROM {table_name} LIMIT 1"))
                except:
                    missing_fields.append(field)
        
        return missing_fields
    
    def _check_data_completeness(self, table_name: str, conn) -> float:
        """检查数据完整性评分"""
        try:
            if table_name not in self.quality_standards:
                return 100.0
            
            required_fields = self.quality_standards[table_name]['required_fields']
            total_score = 0
            
            for field in required_fields:
                try:
                    # 计算非空率
                    total_count = conn.execute(text(f"SELECT COUNT(*) FROM {table_name}")).scalar()
                    non_null_count = conn.execute(text(f"SELECT COUNT({field}) FROM {table_name}")).scalar()
                    
                    if total_count > 0:
                        completeness = (non_null_count / total_count) * 100
                        total_score += completeness
                except:
                    total_score += 0
            
            return total_score / len(required_fields) if required_fields else 100.0
            
        except Exception as e:
            logger.warning(f"完整性检查失败 {table_name}: {e}")
            return 0.0
    
    def _check_data_consistency(self, table_name: str, conn) -> float:
        """检查数据一致性评分"""
        try:
            consistency_score = 100.0
            
            # 检查ts_code格式一致性
            if 'ts_code' in self.quality_standards.get(table_name, {}).get('required_fields', []):
                try:
                    invalid_codes = conn.execute(text(f"""
                        SELECT COUNT(*) FROM {table_name}
                        WHERE ts_code NOT REGEXP '^[0-9]{{6}}\\.(SH|SZ)$'
                    """)).scalar()
                    
                    total_codes = conn.execute(text(f"SELECT COUNT(*) FROM {table_name} WHERE ts_code IS NOT NULL")).scalar()
                    
                    if total_codes > 0:
                        consistency_score -= (invalid_codes / total_codes) * 30
                except:
                    pass
            
            # 检查日期格式一致性
            date_fields = ['trade_date', 'end_date', 'ann_date']
            for field in date_fields:
                try:
                    invalid_dates = conn.execute(text(f"""
                        SELECT COUNT(*) FROM {table_name}
                        WHERE {field} IS NOT NULL AND {field} NOT REGEXP '^[0-9]{{8}}$'
                    """)).scalar()
                    
                    total_dates = conn.execute(text(f"SELECT COUNT(*) FROM {table_name} WHERE {field} IS NOT NULL")).scalar()
                    
                    if total_dates > 0:
                        consistency_score -= (invalid_dates / total_dates) * 10
                except:
                    continue
            
            return max(0.0, consistency_score)
            
        except Exception as e:
            logger.warning(f"一致性检查失败 {table_name}: {e}")
            return 50.0
    
    def _check_duplicate_rate(self, table_name: str, conn) -> float:
        """检查重复数据率"""
        try:
            total_count = conn.execute(text(f"SELECT COUNT(*) FROM {table_name}")).scalar()
            
            # 根据表类型选择去重字段
            if 'ts_code' in self.quality_standards.get(table_name, {}).get('required_fields', []):
                if 'trade_date' in self.quality_standards.get(table_name, {}).get('required_fields', []):
                    unique_count = conn.execute(text(f"SELECT COUNT(DISTINCT ts_code, trade_date) FROM {table_name}")).scalar()
                else:
                    unique_count = conn.execute(text(f"SELECT COUNT(DISTINCT ts_code) FROM {table_name}")).scalar()
            else:
                unique_count = total_count
            
            if total_count > 0:
                return ((total_count - unique_count) / total_count) * 100
            return 0.0
            
        except Exception as e:
            logger.warning(f"重复率检查失败 {table_name}: {e}")
            return 0.0
    
    def _check_null_rates(self, table_name: str, conn) -> Dict:
        """检查各字段空值率"""
        null_rates = {}
        
        try:
            if table_name in self.quality_standards:
                required_fields = self.quality_standards[table_name]['required_fields']
                total_count = conn.execute(text(f"SELECT COUNT(*) FROM {table_name}")).scalar()
                
                for field in required_fields:
                    try:
                        null_count = conn.execute(text(f"SELECT COUNT(*) FROM {table_name} WHERE {field} IS NULL")).scalar()
                        null_rates[field] = (null_count / total_count) * 100 if total_count > 0 else 0
                    except:
                        null_rates[field] = 100.0
        
        except Exception as e:
            logger.warning(f"空值率检查失败 {table_name}: {e}")
        
        return null_rates
    
    def _check_type_consistency(self, table_name: str, conn) -> Dict:
        """检查数据类型一致性"""
        type_consistency = {}
        
        try:
            if table_name in self.quality_standards:
                data_types = self.quality_standards[table_name].get('data_types', {})
                
                for field, expected_type in data_types.items():
                    try:
                        # 简单的类型检查
                        if expected_type == 'float':
                            invalid_count = conn.execute(text(f"""
                                SELECT COUNT(*) FROM {table_name}
                                WHERE {field} IS NOT NULL AND {field} NOT REGEXP '^-?[0-9]+(\\.[0-9]+)?$'
                            """)).scalar()
                        elif expected_type == 'int':
                            invalid_count = conn.execute(text(f"""
                                SELECT COUNT(*) FROM {table_name}
                                WHERE {field} IS NOT NULL AND {field} NOT REGEXP '^-?[0-9]+$'
                            """)).scalar()
                        else:
                            invalid_count = 0
                        
                        total_count = conn.execute(text(f"SELECT COUNT(*) FROM {table_name} WHERE {field} IS NOT NULL")).scalar()
                        
                        type_consistency[field] = {
                            'expected_type': expected_type,
                            'invalid_count': invalid_count,
                            'consistency_rate': ((total_count - invalid_count) / total_count * 100) if total_count > 0 else 100
                        }
                    except:
                        type_consistency[field] = {'error': 'check_failed'}
        
        except Exception as e:
            logger.warning(f"类型一致性检查失败 {table_name}: {e}")
        
        return type_consistency
    
    def _detect_outliers(self, table_name: str, conn) -> Dict:
        """检测数据异常值"""
        outliers = {}
        
        try:
            # 检查数值字段的异常值
            numeric_fields = ['close', 'volume', 'amount', 'pe', 'pb', 'total_revenue']
            
            for field in numeric_fields:
                try:
                    # 使用统计方法检测异常值
                    result = conn.execute(text(f"""
                        SELECT
                            COUNT(*) as total,
                            AVG({field}) as mean_val,
                            STD({field}) as std_val,
                            MIN({field}) as min_val,
                            MAX({field}) as max_val
                        FROM {table_name}
                        WHERE {field} IS NOT NULL AND {field} > 0
                    """)).fetchone()
                    
                    if result and result.total > 0:
                        outliers[field] = {
                            'total_records': result.total,
                            'mean': float(result.mean_val) if result.mean_val else 0,
                            'std': float(result.std_val) if result.std_val else 0,
                            'min': float(result.min_val) if result.min_val else 0,
                            'max': float(result.max_val) if result.max_val else 0
                        }
                except:
                    continue
        
        except Exception as e:
            logger.warning(f"异常值检测失败 {table_name}: {e}")
        
        return outliers
    
    def _analyze_growth_trend(self, table_name: str, conn) -> Dict:
        """分析数据增长趋势"""
        try:
            # 检查最近7天的数据增长
            if 'trade_date' in self.quality_standards.get(table_name, {}).get('required_fields', []):
                result = conn.execute(text(f"""
                    SELECT
                        DATE(STR_TO_DATE(trade_date, '%Y%m%d')) as date,
                        COUNT(*) as daily_count
                    FROM {table_name}
                    WHERE trade_date >= DATE_FORMAT(DATE_SUB(NOW(), INTERVAL 7 DAY), '%Y%m%d')
                    GROUP BY DATE(STR_TO_DATE(trade_date, '%Y%m%d'))
                    ORDER BY date DESC
                    LIMIT 7
                """)).fetchall()
                
                if result:
                    daily_counts = [row.daily_count for row in result]
                    return {
                        'recent_days': len(daily_counts),
                        'daily_counts': daily_counts,
                        'avg_daily_growth': sum(daily_counts) / len(daily_counts),
                        'trend': 'increasing' if len(daily_counts) > 1 and daily_counts[0] > daily_counts[-1] else 'stable'
                    }
            
            return {'status': 'no_date_field'}
            
        except Exception as e:
            logger.warning(f"增长趋势分析失败 {table_name}: {e}")
            return {'error': str(e)}
    
    def _calculate_enhanced_health_score(self, table_name: str, record_count: int, missing_fields: List,
                                       data_freshness: Dict, completeness_score: float,
                                       consistency_score: float, quality_metrics: Dict) -> int:
        """计算增强的健康评分"""
        score = 100
        
        # 记录数量评分 (25分)
        if table_name in self.quality_standards:
            min_records = self.quality_standards[table_name]['min_records']
            if record_count == 0:
                score -= 25
            elif record_count < min_records * 0.5:
                score -= 20
            elif record_count < min_records:
                score -= 10
        
        # 字段完整性评分 (20分)
        if missing_fields:
            score -= len(missing_fields) * 5
        
        # 数据新鲜度评分 (20分)
        days_old = data_freshness.get('days_old', 999)
        if days_old > 30:
            score -= 20
        elif days_old > 7:
            score -= 15
        elif days_old > 3:
            score -= 5
        
        # 数据完整性评分 (15分)
        score -= (100 - completeness_score) * 0.15
        
        # 数据一致性评分 (15分)
        score -= (100 - consistency_score) * 0.15
        
        # 质量指标评分 (5分)
        duplicate_rate = quality_metrics.get('duplicate_rate', 0)
        if duplicate_rate > 10:
            score -= 5
        elif duplicate_rate > 5:
            score -= 3
        elif duplicate_rate > 1:
            score -= 1
        
        return max(0, int(score))
    
    def _generate_detailed_issues(self, table_name: str, record_count: int, missing_fields: List,
                                data_freshness: Dict, completeness_score: float,
                                consistency_score: float, quality_metrics: Dict) -> List[str]:
        """生成详细问题列表"""
        issues = []
        
        # 记录数量问题
        if table_name in self.quality_standards:
            min_records = self.quality_standards[table_name]['min_records']
            if record_count < min_records:
                issues.append(f"记录数不足: {record_count:,} < {min_records:,}")
        
        # 字段缺失问题
        if missing_fields:
            issues.append(f"缺少必需字段: {', '.join(missing_fields)}")
        
        # 数据新鲜度问题
        days_old = data_freshness.get('days_old', 999)
        if days_old > 7:
            issues.append(f"数据过期: {days_old}天前")
        
        # 完整性问题
        if completeness_score < 90:
            issues.append(f"数据完整性不足: {completeness_score:.1f}%")
        
        # 一致性问题
        if consistency_score < 90:
            issues.append(f"数据一致性问题: {consistency_score:.1f}%")
        
        # 重复数据问题
        duplicate_rate = quality_metrics.get('duplicate_rate', 0)
        if duplicate_rate > 5:
            issues.append(f"重复数据率过高: {duplicate_rate:.1f}%")
        
        # 空值率问题
        null_rates = quality_metrics.get('null_rates', {})
        for field, rate in null_rates.items():
            tolerance = self.quality_standards.get(table_name, {}).get('null_tolerance', 0.05) * 100
            if rate > tolerance:
                issues.append(f"字段{field}空值率过高: {rate:.1f}% > {tolerance}%")
        
        return issues
    
    def _determine_health_status(self, health_score: int, issues: List[str]) -> str:
        """确定健康状态"""
        if health_score >= 85:
            return 'HEALTHY'
        elif health_score >= 70:
            return 'WARNING'
        else:
            return 'CRITICAL'

# 创建全局实例
data_health_checker = DataHealthChecker()

def main():
    """主函数"""
    import argparse
    
    parser = argparse.ArgumentParser(description='数据健康检查器')
    parser.add_argument('command', choices=['check', 'repair', 'report', 'monitor'], 
                       help='操作命令')
    parser.add_argument('--interval', type=int, default=30,
                       help='监控间隔(分钟)')
    parser.add_argument('--auto-repair', action='store_true',
                       help='自动修复问题')
    
    args = parser.parse_args()
    
    # 配置日志
    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s - %(levelname)s - %(message)s',
        handlers=[
            logging.StreamHandler(),
            logging.FileHandler('logs/data_health.log', encoding='utf-8')
        ]
    )
    
    try:
        if args.command == 'check':
            health_data = data_health_checker.check_system_health()
            print(json.dumps(health_data, indent=2, ensure_ascii=False, default=str))
            
            if args.auto_repair and health_data['overall_status'] in ['CRITICAL', 'WARNING']:
                repair_result = data_health_checker.auto_repair(health_data)
                print("修复结果:", json.dumps(repair_result, indent=2, ensure_ascii=False, default=str))
                
        elif args.command == 'repair':
            health_data = data_health_checker.check_system_health()
            repair_result = data_health_checker.auto_repair(health_data)
            print("修复结果:", json.dumps(repair_result, indent=2, ensure_ascii=False, default=str))
            
        elif args.command == 'report':
            report = data_health_checker.generate_health_report()
            print(report)
            
        elif args.command == 'monitor':
            data_health_checker.monitor_continuous(args.interval)
            
    except Exception as e:
        logger.error(f"❌ 程序执行失败: {e}")

if __name__ == "__main__":
    main()