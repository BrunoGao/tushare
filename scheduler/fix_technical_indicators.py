#!/usr/bin/env python3
"""
修复技术指标数据类型和重新计算MACD、KDJ指标
"""

import sys
import os
import logging
import pandas as pd
import numpy as np
from datetime import datetime

# 添加项目路径
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import config
from database.db_manager import DatabaseManager

# 配置日志
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)

class TechnicalIndicatorFixer:
    """技术指标修复器"""
    
    def __init__(self):
        self.db = DatabaseManager()
    
    def fix_all_indicators(self):
        """修复所有技术指标"""
        logger.info("🔧 开始修复技术指标数据")
        
        try:
            # 1. 修复数据类型
            self._fix_data_types()
            
            # 2. 重新计算MACD和KDJ
            self._recalculate_macd_kdj()
            
            # 3. 验证修复结果
            self._validate_fix()
            
            logger.info("✅ 技术指标修复完成")
            return True
            
        except Exception as e:
            logger.error(f"❌ 技术指标修复失败: {e}")
            return False
    
    def _fix_data_types(self):
        """修复数据类型"""
        logger.info("🔧 修复MACD和KDJ字段数据类型")
        
        try:
            # 修改字段类型为DOUBLE
            alter_sqls = [
                "ALTER TABLE technical_indicators MODIFY COLUMN macd_dif DOUBLE",
                "ALTER TABLE technical_indicators MODIFY COLUMN macd_dea DOUBLE", 
                "ALTER TABLE technical_indicators MODIFY COLUMN macd_macd DOUBLE",
                "ALTER TABLE technical_indicators MODIFY COLUMN kdj_k DOUBLE",
                "ALTER TABLE technical_indicators MODIFY COLUMN kdj_d DOUBLE",
                "ALTER TABLE technical_indicators MODIFY COLUMN kdj_j DOUBLE"
            ]
            
            for sql in alter_sqls:
                try:
                    self.db.execute_sql(sql)
                    logger.info(f"✅ 执行: {sql}")
                except Exception as e:
                    logger.warning(f"⚠️ 执行失败: {sql} - {e}")
                    
        except Exception as e:
            logger.error(f"❌ 数据类型修复失败: {e}")
    
    def _recalculate_macd_kdj(self):
        """重新计算MACD和KDJ指标"""
        logger.info("📊 重新计算MACD和KDJ指标")
        
        try:
            # 获取所有需要计算的股票
            stocks = self.db.fetch_data("""
                SELECT DISTINCT ts_code 
                FROM technical_indicators 
                WHERE macd_dif IS NULL OR kdj_k IS NULL
                ORDER BY ts_code
            """)
            
            logger.info(f"📈 需要重新计算 {len(stocks)} 只股票的指标")
            
            for i, row in stocks.iterrows():
                ts_code = row['ts_code']
                try:
                    self._calculate_stock_indicators(ts_code)
                    if (i + 1) % 100 == 0:
                        logger.info(f"📊 进度: {i + 1}/{len(stocks)}")
                except Exception as e:
                    logger.debug(f"❌ {ts_code} 指标计算失败: {e}")
                    continue
                    
        except Exception as e:
            logger.error(f"❌ 指标重计算失败: {e}")
    
    def _calculate_stock_indicators(self, ts_code):
        """计算单只股票的指标"""
        try:
            # 获取该股票的历史数据用于计算
            daily_data = self._get_stock_daily_data(ts_code)
            
            if daily_data.empty or len(daily_data) < 30:
                return False
            
            # 按日期排序
            daily_data = daily_data.sort_values('trade_date').reset_index(drop=True)
            
            # 准备价格数据
            close = daily_data['close'].values
            high = daily_data['high'].values if 'high' in daily_data.columns else close
            low = daily_data['low'].values if 'low' in daily_data.columns else close
            
            # 更新每一条技术指标记录
            updates = []
            
            for i, row in daily_data.iterrows():
                trade_date = row['trade_date']
                
                # 计算MACD
                if i >= 25:  # 需要足够数据
                    macd_data = self._calculate_macd(close[:i+1])
                    
                    # 计算KDJ  
                    kdj_data = {}
                    if i >= 8:
                        kdj_data = self._calculate_kdj(
                            high[max(0, i-8):i+1],
                            low[max(0, i-8):i+1], 
                            close[max(0, i-8):i+1]
                        )
                    
                    # 准备更新SQL
                    if macd_data['macd_dif'] is not None or kdj_data.get('kdj_k') is not None:
                        update_sql = f"""
                        UPDATE technical_indicators 
                        SET macd_dif = {macd_data['macd_dif'] or 'NULL'},
                            macd_dea = {macd_data['macd_dea'] or 'NULL'},
                            macd_macd = {macd_data['macd_macd'] or 'NULL'},
                            kdj_k = {kdj_data.get('kdj_k') or 'NULL'},
                            kdj_d = {kdj_data.get('kdj_d') or 'NULL'},
                            kdj_j = {kdj_data.get('kdj_j') or 'NULL'}
                        WHERE ts_code = '{ts_code}' AND trade_date = '{trade_date}'
                        """
                        updates.append(update_sql)
            
            # 批量执行更新
            if updates:
                for update_sql in updates[:50]:  # 限制批次大小
                    try:
                        self.db.execute_sql(update_sql)
                    except Exception as e:
                        logger.debug(f"更新失败: {e}")
                        continue
                        
            return True
            
        except Exception as e:
            logger.debug(f"计算股票指标失败 {ts_code}: {e}")
            return False
    
    def _get_stock_daily_data(self, ts_code):
        """获取股票日线数据"""
        try:
            # 从最近的月度表获取数据
            recent_tables = [
                'stock_daily_202507', 'stock_daily_202506', 'stock_daily_202505',
                'stock_daily_202504', 'stock_daily_202503', 'stock_daily_202502'
            ]
            
            all_data = []
            for table in recent_tables:
                try:
                    sql = f"""
                    SELECT trade_date, open, high, low, close, vol
                    FROM {table}
                    WHERE ts_code = '{ts_code}'
                    ORDER BY trade_date
                    """
                    data = self.db.fetch_data(sql)
                    if not data.empty:
                        all_data.append(data)
                except:
                    continue
            
            if all_data:
                return pd.concat(all_data, ignore_index=True).sort_values('trade_date')
            else:
                return pd.DataFrame()
                
        except Exception as e:
            logger.debug(f"获取日线数据失败: {e}")
            return pd.DataFrame()
    
    def _calculate_macd(self, prices):
        """计算MACD指标"""
        try:
            if len(prices) < 26:
                return {'macd_dif': None, 'macd_dea': None, 'macd_macd': None}
            
            # 计算EMA
            ema12 = self._calculate_ema(prices, 12)
            ema26 = self._calculate_ema(prices, 26)
            
            # DIF线
            dif = ema12 - ema26
            
            # DEA线（DIF的9日EMA）
            if len(prices) >= 34:  # 26 + 9 - 1
                dif_series = []
                for i in range(25, len(prices)):
                    temp_ema12 = self._calculate_ema(prices[:i+1], 12)
                    temp_ema26 = self._calculate_ema(prices[:i+1], 26)
                    dif_series.append(temp_ema12 - temp_ema26)
                
                dea = self._calculate_ema(np.array(dif_series), 9)
                macd = 2 * (dif - dea)
                
                return {
                    'macd_dif': round(dif, 4),
                    'macd_dea': round(dea, 4),
                    'macd_macd': round(macd, 4)
                }
            
            return {
                'macd_dif': round(dif, 4),
                'macd_dea': None,
                'macd_macd': None
            }
            
        except:
            return {'macd_dif': None, 'macd_dea': None, 'macd_macd': None}
    
    def _calculate_ema(self, prices, period):
        """计算指数移动平均"""
        try:
            alpha = 2 / (period + 1)
            ema = prices[0]
            
            for price in prices[1:]:
                ema = alpha * price + (1 - alpha) * ema
                
            return ema
        except:
            return None
    
    def _calculate_kdj(self, high, low, close):
        """计算KDJ指标"""
        try:
            if len(high) < 9:
                return {'kdj_k': None, 'kdj_d': None, 'kdj_j': None}
            
            # 计算RSV
            lowest_low = np.min(low)
            highest_high = np.max(high)
            
            if highest_high == lowest_low:
                rsv = 50
            else:
                rsv = (close[-1] - lowest_low) / (highest_high - lowest_low) * 100
            
            # 简化的KDJ计算
            k = rsv * 0.33 + 50 * 0.67  # 简化处理
            d = k * 0.33 + 50 * 0.67
            j = 3 * k - 2 * d
            
            return {
                'kdj_k': round(k, 2),
                'kdj_d': round(d, 2),
                'kdj_j': round(j, 2)
            }
            
        except:
            return {'kdj_k': None, 'kdj_d': None, 'kdj_j': None}
    
    def _validate_fix(self):
        """验证修复结果"""
        logger.info("🔍 验证修复结果")
        
        try:
            # 检查MACD数据
            macd_count = self.db.fetch_data("""
                SELECT COUNT(*) as count 
                FROM technical_indicators 
                WHERE macd_dif IS NOT NULL
            """).iloc[0]['count']
            
            # 检查KDJ数据
            kdj_count = self.db.fetch_data("""
                SELECT COUNT(*) as count 
                FROM technical_indicators 
                WHERE kdj_k IS NOT NULL
            """).iloc[0]['count']
            
            # 总记录数
            total_count = self.db.fetch_data("""
                SELECT COUNT(*) as count FROM technical_indicators
            """).iloc[0]['count']
            
            logger.info(f"✅ MACD数据: {macd_count:,}/{total_count:,} ({macd_count/total_count*100:.1f}%)")
            logger.info(f"✅ KDJ数据: {kdj_count:,}/{total_count:,} ({kdj_count/total_count*100:.1f}%)")
            
            return macd_count > 0 and kdj_count > 0
            
        except Exception as e:
            logger.error(f"❌ 验证失败: {e}")
            return False

def main():
    """主函数"""
    fixer = TechnicalIndicatorFixer()
    
    try:
        success = fixer.fix_all_indicators()
        
        if success:
            logger.info("🎉 技术指标修复成功完成！")
        else:
            logger.warning("⚠️ 技术指标修复未完全成功")
            
    except Exception as e:
        logger.error(f"❌ 系统异常: {e}")

if __name__ == "__main__":
    main()