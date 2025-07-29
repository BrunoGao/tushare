#!/usr/bin/env python3
"""
数据初始化和修复脚本
自动检测数据库状态，按正确顺序执行数据获取，智能错误处理和重试
"""
import sys
import os
import logging
import time
from datetime import datetime, timedelta
from typing import Dict, List, Tuple
import subprocess
import json

# 添加项目根目录到路径
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import config
from utils.db_helper import db
from sqlalchemy import text
import tushare as ts

# 配置日志
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    handlers=[
        logging.StreamHandler(),
        logging.FileHandler('logs/data_initialization.log', encoding='utf-8')
    ]
)
logger = logging.getLogger(__name__)

class DataInitializer:
    """数据初始化管理器"""
    
    def __init__(self):
        self.ts_token = config.TS_TOKEN
        self.status_file = "logs/initialization_status.json"
        self.initialization_steps = [
            ('database_setup', '数据库初始化', self.setup_database),
            ('stock_basic', '股票基本信息', self.fetch_stock_basic),
            ('stock_daily', '股票历史数据', self.fetch_stock_daily),
            ('comprehensive_data', '全维度数据', self.fetch_comprehensive_data),
            ('data_validation', '数据验证', self.validate_data)
        ]
        
    def print_banner(self):
        """打印启动横幅"""
        print("""
╔══════════════════════════════════════════════════════════════╗
║                🚀 领京万象股票分析系统                        ║
║                   数据初始化和修复工具                        ║
║                                                              ║
║  功能：自动检测数据库状态，智能修复数据获取问题               ║
║  版本：v2.0                                                  ║
╚══════════════════════════════════════════════════════════════╝
        """)
        
    def load_status(self) -> Dict:
        """加载初始化状态"""
        try:
            if os.path.exists(self.status_file):
                with open(self.status_file, 'r', encoding='utf-8') as f:
                    return json.load(f)
        except Exception as e:
            logger.warning(f"加载状态文件失败: {e}")
        return {}
        
    def save_status(self, status: Dict):
        """保存初始化状态"""
        try:
            os.makedirs(os.path.dirname(self.status_file), exist_ok=True)
            with open(self.status_file, 'w', encoding='utf-8') as f:
                json.dump(status, f, ensure_ascii=False, indent=2, default=str)
        except Exception as e:
            logger.error(f"保存状态文件失败: {e}")
            
    def check_prerequisites(self) -> bool:
        """检查前置条件"""
        logger.info("🔍 检查系统前置条件...")
        
        # 检查TuShare Token
        if not self.ts_token or self.ts_token == 'your_tushare_token':
            logger.error("❌ TuShare Token未配置，请在config.py中设置TS_TOKEN")
            return False
            
        # 检查数据库连接
        try:
            with db.engine.connect() as conn:
                conn.execute(text("SELECT 1"))
            logger.info("✅ 数据库连接正常")
        except Exception as e:
            logger.error(f"❌ 数据库连接失败: {e}")
            return False
            
        # 检查TuShare API
        try:
            ts.set_token(self.ts_token)
            pro = ts.pro_api()
            df = pro.trade_cal(start_date='20240101', end_date='20240101')
            if df.empty:
                logger.warning("⚠️ TuShare API返回空数据，可能是权限问题")
            else:
                logger.info("✅ TuShare API连接正常")
        except Exception as e:
            logger.error(f"❌ TuShare API连接失败: {e}")
            return False
            
        return True
        
    def setup_database(self) -> bool:
        """数据库初始化"""
        logger.info("🔧 初始化数据库表结构...")
        
        try:
            # 创建基础表
            db.create_tables()
            
            # 创建全维度数据表
            from utils.comprehensive_data_schema import ComprehensiveDataSchema
            ComprehensiveDataSchema.create_all_tables(db)
            
            logger.info("✅ 数据库表结构初始化完成")
            return True
            
        except Exception as e:
            logger.error(f"❌ 数据库初始化失败: {e}")
            return False
            
    def fetch_stock_basic(self) -> bool:
        """获取股票基本信息"""
        logger.info("📋 获取股票基本信息...")
        
        try:
            # 检查是否已有数据
            with db.engine.connect() as conn:
                result = conn.execute(text("SELECT COUNT(*) FROM stock_basic")).scalar()
                if result > 0:
                    logger.info(f"📊 股票基本信息已存在 {result} 条记录")
                    return True
                    
            # 执行获取脚本
            cmd = [sys.executable, 'scripts/fetch_stock_basic.py']
            result = subprocess.run(cmd, capture_output=True, text=True, cwd=os.path.dirname(os.path.dirname(__file__)))
            
            if result.returncode == 0:
                logger.info("✅ 股票基本信息获取成功")
                return True
            else:
                logger.error(f"❌ 股票基本信息获取失败: {result.stderr}")
                return False
                
        except Exception as e:
            logger.error(f"❌ 获取股票基本信息异常: {e}")
            return False
            
    def fetch_stock_daily(self) -> bool:
        """获取股票历史数据"""
        logger.info("📈 获取股票历史数据...")
        
        try:
            # 检查是否已有数据
            with db.engine.connect() as conn:
                # 检查最近的月表
                current_month = datetime.now().strftime('%Y%m')
                table_name = f"stock_daily_{current_month}"
                
                try:
                    result = conn.execute(text(f"SELECT COUNT(*) FROM {table_name}")).scalar()
                    if result > 1000:  # 如果有足够的数据
                        logger.info(f"📊 股票历史数据已存在，{table_name} 表有 {result} 条记录")
                        return True
                except:
                    pass  # 表不存在，继续获取数据
                    
            # 执行快速测试获取
            logger.info("🧪 执行快速数据获取测试...")
            cmd = [sys.executable, 'scripts/fetch_stock_daily.py', 'test', '20']
            result = subprocess.run(cmd, capture_output=True, text=True, cwd=os.path.dirname(os.path.dirname(__file__)))
            
            if result.returncode == 0:
                logger.info("✅ 股票历史数据获取成功")
                return True
            else:
                logger.error(f"❌ 股票历史数据获取失败: {result.stderr}")
                return False
                
        except Exception as e:
            logger.error(f"❌ 获取股票历史数据异常: {e}")
            return False
            
    def fetch_comprehensive_data(self) -> bool:
        """获取全维度数据"""
        logger.info("🗄️ 获取全维度数据...")
        
        try:
            # 执行基本面数据获取
            cmd = [sys.executable, 'run.py', 'fetch-comprehensive', '--category', 'basic', '--mode', 'incremental']
            result = subprocess.run(cmd, capture_output=True, text=True, cwd=os.path.dirname(os.path.dirname(__file__)))
            
            if result.returncode == 0:
                logger.info("✅ 基本面数据获取成功")
                
                # 尝试获取财务数据
                cmd = [sys.executable, 'run.py', 'fetch-comprehensive', '--category', 'financial', '--mode', 'incremental']
                result = subprocess.run(cmd, capture_output=True, text=True, cwd=os.path.dirname(os.path.dirname(__file__)))
                
                if result.returncode == 0:
                    logger.info("✅ 财务数据获取成功")
                else:
                    logger.warning("⚠️ 财务数据获取失败，可能需要更高权限")
                    
                return True
            else:
                logger.error(f"❌ 全维度数据获取失败: {result.stderr}")
                return False
                
        except Exception as e:
            logger.error(f"❌ 获取全维度数据异常: {e}")
            return False
            
    def validate_data(self) -> bool:
        """数据验证"""
        logger.info("🔍 验证数据完整性...")
        
        validation_results = {}
        
        try:
            with db.engine.connect() as conn:
                # 检查股票基本信息
                stock_count = conn.execute(text("SELECT COUNT(*) FROM stock_basic")).scalar()
                validation_results['stock_basic'] = stock_count
                logger.info(f"📊 股票基本信息: {stock_count} 条")
                
                # 检查历史数据表
                current_month = datetime.now().strftime('%Y%m')
                table_name = f"stock_daily_{current_month}"
                try:
                    daily_count = conn.execute(text(f"SELECT COUNT(*) FROM {table_name}")).scalar()
                    validation_results['stock_daily'] = daily_count
                    logger.info(f"📈 股票历史数据: {daily_count} 条")
                except:
                    validation_results['stock_daily'] = 0
                    logger.warning(f"⚠️ 历史数据表 {table_name} 不存在")
                
                # 检查技术指标表
                try:
                    indicator_count = conn.execute(text("SELECT COUNT(*) FROM stock_indicators")).scalar()
                    validation_results['indicators'] = indicator_count
                    logger.info(f"📊 技术指标数据: {indicator_count} 条")
                except:
                    validation_results['indicators'] = 0
                    logger.info("📊 技术指标表未创建")
                
                # 检查推荐结果表
                try:
                    recommend_count = conn.execute(text("SELECT COUNT(*) FROM recommend_result")).scalar()
                    validation_results['recommendations'] = recommend_count
                    logger.info(f"⭐ 推荐结果: {recommend_count} 条")
                except:
                    validation_results['recommendations'] = 0
                    logger.info("⭐ 推荐结果表未创建")
                    
            # 评估数据完整性
            if validation_results['stock_basic'] > 0:
                logger.info("✅ 数据验证通过，系统可以正常使用")
                return True
            else:
                logger.error("❌ 数据验证失败，缺少基础数据")
                return False
                
        except Exception as e:
            logger.error(f"❌ 数据验证异常: {e}")
            return False
            
    def run_initialization(self, force_refresh: bool = False) -> bool:
        """运行完整初始化流程"""
        self.print_banner()
        
        start_time = datetime.now()
        logger.info(f"🚀 开始数据初始化流程 - {start_time.strftime('%Y-%m-%d %H:%M:%S')}")
        
        # 检查前置条件
        if not self.check_prerequisites():
            logger.error("❌ 前置条件检查失败，初始化终止")
            return False
            
        # 加载状态
        status = self.load_status() if not force_refresh else {}
        
        # 执行初始化步骤
        success_count = 0
        total_steps = len(self.initialization_steps)
        
        for step_id, step_name, step_func in self.initialization_steps:
            logger.info(f"📋 步骤 {success_count + 1}/{total_steps}: {step_name}")
            
            # 检查是否已完成
            if not force_refresh and status.get(step_id, {}).get('completed', False):
                logger.info(f"⏭️ {step_name} 已完成，跳过")
                success_count += 1
                continue
                
            # 执行步骤
            step_start = datetime.now()
            try:
                if step_func():
                    step_duration = (datetime.now() - step_start).total_seconds()
                    status[step_id] = {
                        'completed': True,
                        'timestamp': datetime.now().isoformat(),
                        'duration': step_duration
                    }
                    self.save_status(status)
                    success_count += 1
                    logger.info(f"✅ {step_name} 完成，耗时 {step_duration:.1f} 秒")
                else:
                    logger.error(f"❌ {step_name} 失败")
                    break
                    
            except Exception as e:
                logger.error(f"❌ {step_name} 异常: {e}")
                break
                
            # 步骤间休息
            time.sleep(2)
            
        # 总结结果
        total_duration = (datetime.now() - start_time).total_seconds()
        
        if success_count == total_steps:
            logger.info(f"🎉 数据初始化完成！成功执行 {success_count}/{total_steps} 个步骤")
            logger.info(f"⏱️ 总耗时: {total_duration:.1f} 秒")
            logger.info("🚀 系统已准备就绪，可以启动实时分析服务")
            return True
        else:
            logger.error(f"❌ 数据初始化失败！仅完成 {success_count}/{total_steps} 个步骤")
            logger.info("💡 建议检查网络连接和TuShare权限后重试")
            return False
            
    def quick_fix(self) -> bool:
        """快速修复常见问题"""
        logger.info("🔧 执行快速修复...")
        
        try:
            # 修复1: 重新创建缺失的表
            logger.info("🔧 检查并创建缺失的数据表...")
            db.create_tables()
            
            # 修复2: 清理无效数据
            logger.info("🔧 清理无效数据...")
            with db.engine.connect() as conn:
                # 删除无效的股票记录
                conn.execute(text("DELETE FROM stock_basic WHERE ts_code IS NULL OR ts_code = ''"))
                conn.commit()
                
            # 修复3: 重建索引
            logger.info("🔧 优化数据库索引...")
            with db.engine.connect() as conn:
                conn.execute(text("ANALYZE TABLE stock_basic"))
                conn.commit()
                
            logger.info("✅ 快速修复完成")
            return True
            
        except Exception as e:
            logger.error(f"❌ 快速修复失败: {e}")
            return False

def main():
    """主函数"""
    import argparse
    
    parser = argparse.ArgumentParser(description='数据初始化和修复工具')
    parser.add_argument('--force', action='store_true', help='强制重新初始化所有数据')
    parser.add_argument('--quick-fix', action='store_true', help='执行快速修复')
    parser.add_argument('--validate-only', action='store_true', help='仅执行数据验证')
    
    args = parser.parse_args()
    
    initializer = DataInitializer()
    
    try:
        if args.quick_fix:
            success = initializer.quick_fix()
        elif args.validate_only:
            success = initializer.validate_data()
        else:
            success = initializer.run_initialization(force_refresh=args.force)
            
        sys.exit(0 if success else 1)
        
    except KeyboardInterrupt:
        logger.info("🛑 用户中断初始化流程")
        sys.exit(1)
    except Exception as e:
        logger.error(f"❌ 初始化流程异常: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main()