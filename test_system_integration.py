#!/usr/bin/env python3
"""
系统集成测试脚本
测试所有优化组件的集成和功能
"""
import sys
import os
import time
import json
import asyncio
import logging
from datetime import datetime
from typing import Dict, List, Any

# 添加项目路径
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

# 配置日志
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    handlers=[
        logging.StreamHandler(),
        logging.FileHandler('logs/integration_test.log', encoding='utf-8')
    ]
)
logger = logging.getLogger(__name__)

class SystemIntegrationTester:
    """系统集成测试器"""
    
    def __init__(self):
        self.test_results = {}
        self.start_time = datetime.now()
        
    def run_all_tests(self):
        """运行所有集成测试"""
        logger.info("🚀 开始系统集成测试")
        logger.info("=" * 80)
        
        test_methods = [
            self.test_database_connection,
            self.test_redis_cache_manager,
            self.test_data_health_checker,
            self.test_system_monitor,
            self.test_data_scheduler,
            self.test_multi_strategy_recommender,
            self.test_advanced_technical_indicators,
            self.test_api_endpoints,
            self.test_frontend_integration
        ]
        
        for test_method in test_methods:
            try:
                logger.info(f"🧪 执行测试: {test_method.__name__}")
                result = test_method()
                self.test_results[test_method.__name__] = result
                
                if result['success']:
                    logger.info(f"✅ {test_method.__name__} - 通过")
                else:
                    logger.error(f"❌ {test_method.__name__} - 失败: {result.get('error', 'Unknown error')}")
                    
            except Exception as e:
                logger.error(f"❌ {test_method.__name__} - 异常: {e}")
                self.test_results[test_method.__name__] = {
                    'success': False,
                    'error': str(e),
                    'timestamp': datetime.now().isoformat()
                }
            
            logger.info("-" * 60)
        
        self.generate_test_report()
    
    def test_database_connection(self) -> Dict:
        """测试数据库连接"""
        try:
            from utils.db_helper import db
            from sqlalchemy import text
            
            with db.engine.connect() as conn:
                # 测试基本连接
                result = conn.execute(text("SELECT 1")).scalar()
                assert result == 1
                
                # 测试关键表存在
                tables_to_check = [
                    'stock_basic', 't_stock_basic', 'stock_indicators',
                    't_financial_indicators', 't_money_flow', 'recommend_result'
                ]
                
                for table in tables_to_check:
                    table_exists = conn.execute(text(f"SHOW TABLES LIKE '{table}'")).fetchone()
                    if not table_exists:
                        return {
                            'success': False,
                            'error': f'表 {table} 不存在',
                            'timestamp': datetime.now().isoformat()
                        }
                
                # 测试数据量
                stock_count = conn.execute(text("SELECT COUNT(*) FROM stock_basic")).scalar()
                
                return {
                    'success': True,
                    'details': {
                        'connection': 'OK',
                        'tables_checked': len(tables_to_check),
                        'stock_count': stock_count
                    },
                    'timestamp': datetime.now().isoformat()
                }
                
        except Exception as e:
            return {
                'success': False,
                'error': str(e),
                'timestamp': datetime.now().isoformat()
            }
    
    def test_redis_cache_manager(self) -> Dict:
        """测试Redis缓存管理器"""
        try:
            from utils.redis_cache_manager import cache_manager
            
            # 测试基本缓存操作
            test_key = 'integration_test'
            test_data = {'test': 'data', 'timestamp': datetime.now().isoformat()}
            
            # 设置缓存
            set_result = cache_manager.set('test', test_key, test_data, 60)
            assert set_result, "缓存设置失败"
            
            # 获取缓存
            cached_data = cache_manager.get('test', test_key)
            assert cached_data == test_data, "缓存数据不匹配"
            
            # 检查缓存存在
            exists = cache_manager.exists('test', test_key)
            assert exists, "缓存存在性检查失败"
            
            # 删除缓存
            delete_result = cache_manager.delete('test', test_key)
            assert delete_result, "缓存删除失败"
            
            # 获取统计信息
            stats = cache_manager.get_stats()
            
            # 健康检查
            health = cache_manager.health_check()
            
            return {
                'success': True,
                'details': {
                    'basic_operations': 'OK',
                    'stats': stats,
                    'health_status': health['status']
                },
                'timestamp': datetime.now().isoformat()
            }
            
        except Exception as e:
            return {
                'success': False,
                'error': str(e),
                'timestamp': datetime.now().isoformat()
            }
    
    def test_data_health_checker(self) -> Dict:
        """测试数据健康检查器"""
        try:
            from utils.data_health_checker import data_health_checker
            
            # 执行系统健康检查
            health_report = data_health_checker.check_system_health()
            
            # 检查关键指标
            assert 'overall_status' in health_report
            assert 'overall_score' in health_report
            assert 'tables' in health_report
            
            # 检查是否有表健康信息
            tables_checked = len(health_report['tables'])
            assert tables_checked > 0, "未检查到任何表"
            
            return {
                'success': True,
                'details': {
                    'overall_status': health_report['overall_status'],
                    'overall_score': health_report['overall_score'],
                    'tables_checked': tables_checked,
                    'critical_issues': len(health_report.get('critical_issues', [])),
                    'warnings': len(health_report.get('warnings', []))
                },
                'timestamp': datetime.now().isoformat()
            }
            
        except Exception as e:
            return {
                'success': False,
                'error': str(e),
                'timestamp': datetime.now().isoformat()
            }
    
    def test_system_monitor(self) -> Dict:
        """测试系统监控模块"""
        try:
            from utils.system_monitor import system_monitor
            
            # 收集系统指标
            system_metrics = system_monitor.collect_system_metrics()
            business_metrics = system_monitor.collect_business_metrics()
            
            # 获取当前状态
            current_status = system_monitor.get_current_status()
            
            # 检查指标完整性
            assert system_metrics is not None, "系统指标收集失败"
            assert business_metrics is not None, "业务指标收集失败"
            assert 'health_score' in current_status, "状态信息不完整"
            
            return {
                'success': True,
                'details': {
                    'system_metrics': 'OK',
                    'business_metrics': 'OK',
                    'health_score': current_status['health_score'],
                    'status': current_status['status'],
                    'monitoring_status': current_status['monitoring_status']
                },
                'timestamp': datetime.now().isoformat()
            }
            
        except Exception as e:
            return {
                'success': False,
                'error': str(e),
                'timestamp': datetime.now().isoformat()
            }
    
    def test_data_scheduler(self) -> Dict:
        """测试数据调度器"""
        try:
            from utils.data_scheduler import scheduler
            
            # 获取调度状态报告
            status_report = scheduler.get_status_report()
            
            # 健康检查
            health_check = scheduler.health_check()
            
            # 检查任务配置
            task_count = len(scheduler.task_config)
            assert task_count > 0, "未配置任何调度任务"
            
            return {
                'success': True,
                'details': {
                    'scheduler_running': status_report['scheduler_running'],
                    'total_tasks': status_report['total_tasks'],
                    'database_connected': health_check['database_connected'],
                    'tushare_connected': health_check['tushare_connected'],
                    'disk_space_sufficient': health_check['disk_space_sufficient']
                },
                'timestamp': datetime.now().isoformat()
            }
            
        except Exception as e:
            return {
                'success': False,
                'error': str(e),
                'timestamp': datetime.now().isoformat()
            }
    
    def test_multi_strategy_recommender(self) -> Dict:
        """测试多策略推荐引擎"""
        try:
            from utils.multi_strategy_recommender import multi_strategy_recommender
            
            # 测试单股分析
            test_stock = '000001.SZ'
            analysis_result = multi_strategy_recommender.analyze_single_stock(test_stock)
            
            # 检查分析结果
            assert 'total_score' in analysis_result, "分析结果缺少评分"
            assert 'recommendation' in analysis_result, "分析结果缺少推荐"
            assert 'strategies' in analysis_result, "分析结果缺少策略详情"
            
            # 测试推荐生成（限制数量以加快测试）
            recommendations = multi_strategy_recommender.generate_recommendations(
                strategy_weights=None,
                limit=5,
                min_score=50
            )
            
            return {
                'success': True,
                'details': {
                    'single_analysis': 'OK',
                    'test_stock_score': analysis_result['total_score'],
                    'test_stock_recommendation': analysis_result['recommendation'],
                    'recommendations_generated': len(recommendations),
                    'strategies_available': len(multi_strategy_recommender.strategies)
                },
                'timestamp': datetime.now().isoformat()
            }
            
        except Exception as e:
            return {
                'success': False,
                'error': str(e),
                'timestamp': datetime.now().isoformat()
            }
    
    def test_advanced_technical_indicators(self) -> Dict:
        """测试高级技术指标"""
        try:
            from utils.advanced_technical_indicators import AdvancedTechnicalIndicators
            from utils.db_helper import db
            from datetime import datetime, timedelta
            
            # 获取测试数据
            test_stock = '000001.SZ'
            end_date = datetime.now().strftime('%Y%m%d')
            start_date = (datetime.now() - timedelta(days=60)).strftime('%Y%m%d')
            df = db.query_stock_data(test_stock, start_date, end_date)
            
            if df.empty:
                return {
                    'success': False,
                    'error': f'无法获取测试股票 {test_stock} 的数据',
                    'timestamp': datetime.now().isoformat()
                }
            
            # 测试高级技术指标计算
            advanced_indicators = AdvancedTechnicalIndicators()
            enhanced_df = advanced_indicators.calculate_enhanced_indicators(df)
            
            # 检查指标是否计算成功
            expected_columns = ['adx', 'williams_r', 'obv', 'vwap']
            missing_columns = [col for col in expected_columns if col not in enhanced_df.columns]
            
            if missing_columns:
                return {
                    'success': False,
                    'error': f'缺少指标列: {missing_columns}',
                    'timestamp': datetime.now().isoformat()
                }
            
            # 测试智能信号生成
            smart_signals = advanced_indicators.generate_smart_signals(enhanced_df)
            
            return {
                'success': True,
                'details': {
                    'data_rows': len(df),
                    'enhanced_columns': len(enhanced_df.columns),
                    'indicators_calculated': len(expected_columns),
                    'smart_signals_generated': len(smart_signals) if not smart_signals.empty else 0
                },
                'timestamp': datetime.now().isoformat()
            }
            
        except Exception as e:
            return {
                'success': False,
                'error': str(e),
                'timestamp': datetime.now().isoformat()
            }
    
    def test_api_endpoints(self) -> Dict:
        """测试API端点"""
        try:
            import requests
            import time
            
            # 启动API服务器（如果未运行）
            base_url = 'http://localhost:5000'
            
            # 测试健康检查端点
            try:
                response = requests.get(f'{base_url}/api/health', timeout=5)
                health_ok = response.status_code == 200
            except:
                health_ok = False
            
            # 测试系统健康端点
            try:
                response = requests.get(f'{base_url}/api/system/health', timeout=10)
                system_health_ok = response.status_code == 200
            except:
                system_health_ok = False
            
            # 测试推荐生成端点
            try:
                payload = {
                    'strategy': 'multi_strategy',
                    'limit': 5,
                    'min_score': 50
                }
                response = requests.post(
                    f'{base_url}/api/recommendations/generate',
                    json=payload,
                    timeout=30
                )
                recommendations_ok = response.status_code == 200
            except:
                recommendations_ok = False
            
            return {
                'success': health_ok or system_health_ok,  # 至少一个端点可用
                'details': {
                    'base_url': base_url,
                    'health_endpoint': health_ok,
                    'system_health_endpoint': system_health_ok,
                    'recommendations_endpoint': recommendations_ok,
                    'note': 'API服务器可能未启动' if not any([health_ok, system_health_ok]) else 'API服务器运行正常'
                },
                'timestamp': datetime.now().isoformat()
            }
            
        except Exception as e:
            return {
                'success': False,
                'error': str(e),
                'timestamp': datetime.now().isoformat()
            }
    
    def test_frontend_integration(self) -> Dict:
        """测试前端集成"""
        try:
            import os
            from pathlib import Path
            
            # 检查前端文件是否存在
            frontend_files = [
                'frontend/recommendation_dashboard.html',
                'frontend/realtime_dashboard.html'
            ]
            
            existing_files = []
            missing_files = []
            
            for file_path in frontend_files:
                if os.path.exists(file_path):
                    existing_files.append(file_path)
                    
                    # 检查文件大小
                    file_size = os.path.getsize(file_path)
                    if file_size < 1000:  # 小于1KB可能是空文件
                        missing_files.append(f"{file_path} (文件过小)")
                else:
                    missing_files.append(file_path)
            
            # 检查前端文件内容完整性
            dashboard_path = 'frontend/recommendation_dashboard.html'
            if os.path.exists(dashboard_path):
                with open(dashboard_path, 'r', encoding='utf-8') as f:
                    content = f.read()
                    
                # 检查关键功能
                required_features = [
                    'generateRecommendations',
                    'strategy-btn',
                    'recommendation-card',
                    'api/recommendations/generate'
                ]
                
                missing_features = [feature for feature in required_features if feature not in content]
            else:
                missing_features = ['文件不存在']
            
            return {
                'success': len(existing_files) > 0 and len(missing_features) == 0,
                'details': {
                    'existing_files': existing_files,
                    'missing_files': missing_files,
                    'missing_features': missing_features,
                    'total_files_checked': len(frontend_files)
                },
                'timestamp': datetime.now().isoformat()
            }
            
        except Exception as e:
            return {
                'success': False,
                'error': str(e),
                'timestamp': datetime.now().isoformat()
            }
    
    def generate_test_report(self):
        """生成测试报告"""
        end_time = datetime.now()
        duration = (end_time - self.start_time).total_seconds()
        
        # 统计测试结果
        total_tests = len(self.test_results)
        passed_tests = sum(1 for result in self.test_results.values() if result['success'])
        failed_tests = total_tests - passed_tests
        
        # 生成报告
        report = {
            'test_summary': {
                'total_tests': total_tests,
                'passed_tests': passed_tests,
                'failed_tests': failed_tests,
                'success_rate': f"{(passed_tests/total_tests*100):.1f}%" if total_tests > 0 else "0%",
                'duration_seconds': duration,
                'start_time': self.start_time.isoformat(),
                'end_time': end_time.isoformat()
            },
            'test_results': self.test_results
        }
        
        # 保存报告到文件
        os.makedirs('logs', exist_ok=True)
        report_file = f"logs/integration_test_report_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
        
        with open(report_file, 'w', encoding='utf-8') as f:
            json.dump(report, f, ensure_ascii=False, indent=2, default=str)
        
        # 打印摘要
        logger.info("=" * 80)
        logger.info("🎯 系统集成测试完成")
        logger.info("=" * 80)
        logger.info(f"📊 测试总数: {total_tests}")
        logger.info(f"✅ 通过测试: {passed_tests}")
        logger.info(f"❌ 失败测试: {failed_tests}")
        logger.info(f"📈 成功率: {(passed_tests/total_tests*100):.1f}%")
        logger.info(f"⏱️ 总耗时: {duration:.2f}秒")
        logger.info(f"📄 详细报告: {report_file}")
        
        if failed_tests > 0:
            logger.warning("⚠️ 存在失败的测试，请检查详细报告")
            for test_name, result in self.test_results.items():
                if not result['success']:
                    logger.error(f"❌ {test_name}: {result.get('error', 'Unknown error')}")
        else:
            logger.info("🎉 所有测试通过！系统集成成功！")
        
        logger.info("=" * 80)
        
        return report

def main():
    """主函数"""
    try:
        # 确保日志目录存在
        os.makedirs('logs', exist_ok=True)
        
        # 创建测试器并运行测试
        tester = SystemIntegrationTester()
        report = tester.run_all_tests()
        
        # 返回退出码
        failed_tests = report['test_summary']['failed_tests']
        sys.exit(0 if failed_tests == 0 else 1)
        
    except KeyboardInterrupt:
        logger.info("🛑 测试被用户中断")
        sys.exit(1)
    except Exception as e:
        logger.error(f"❌ 测试执行异常: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main()