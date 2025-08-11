#!/usr/bin/env python3
"""
TuShare数据获取管理器
实现全面的TuShare数据获取、验证和同步功能
"""

import os
import time
import logging
import pandas as pd
import mysql.connector
import sqlite3
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Any, Tuple
from dataclasses import dataclass
from concurrent.futures import ThreadPoolExecutor, as_completed
import tushare as ts
from ratelimit import limits, sleep_and_retry
import json

# 设置日志
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

@dataclass
class DataSyncResult:
    """数据同步结果"""
    success: bool
    records_processed: int
    records_success: int
    records_failed: int
    error_messages: List[str]
    start_time: str
    end_time: str
    duration: float

class RateLimiter:
    """API限流器"""
    def __init__(self, calls: int = 200, period: int = 60):
        self.calls = calls
        self.period = period
        
    def limit(self, func):
        """限流装饰器"""
        return sleep_and_retry(limits(calls=self.calls, period=self.period)(func))

class TuShareDataManager:
    """TuShare数据获取管理器"""
    
    def __init__(self, token: str, db_config: Dict[str, Any]):
        """
        初始化TuShare数据管理器
        
        Args:
            token: TuShare API Token
            db_config: 数据库配置
        """
        self.token = token
        self.db_config = db_config
        self.db_type = db_config.get('type', 'mysql')
        
        # 初始化TuShare Pro API
        try:
            self.pro = ts.pro_api(token)
            logger.info("TuShare Pro API初始化成功")
        except Exception as e:
            logger.error(f"TuShare Pro API初始化失败: {e}")
            raise
        
        # 初始化限流器
        self.rate_limiter = RateLimiter(calls=200, period=60)
        
        # 数据同步日志
        self.sync_logs = []
        
    def get_connection(self):
        """获取数据库连接"""
        if self.db_type == 'mysql':
            return mysql.connector.connect(
                host=self.db_config['host'],
                port=self.db_config['port'],
                user=self.db_config['user'],
                password=self.db_config['password'],
                database=self.db_config['database']
            )
        else:
            return sqlite3.connect(self.db_config['database'])
    
    @sleep_and_retry
    @limits(calls=200, period=60)
    def _api_call_with_retry(self, func, *args, **kwargs):
        """带重试的API调用"""
        max_retries = 3
        retry_delay = 1
        
        for attempt in range(max_retries):
            try:
                return func(*args, **kwargs)
            except Exception as e:
                if attempt == max_retries - 1:
                    raise e
                logger.warning(f"API调用失败，{retry_delay}秒后重试 (第{attempt + 1}次): {e}")
                time.sleep(retry_delay)
                retry_delay *= 2
    
    def sync_stock_basic_info(self, include_hk: bool = True) -> DataSyncResult:
        """
        同步股票基础信息（支持A股和港股）
        
        Args:
            include_hk: 是否包含港股数据
        """
        start_time = datetime.now()
        records_processed = 0
        records_success = 0
        records_failed = 0
        error_messages = []
        
        logger.info(f"开始同步股票基础信息 (包含港股: {include_hk})...")
        
        try:
            # 获取A股基础信息
            logger.info("正在获取A股基础信息...")
            a_stock_df = self._api_call_with_retry(
                self.pro.stock_basic,
                exchange='',  # 空值表示获取所有A股
                list_status='L',
                fields='ts_code,symbol,name,area,industry,market,list_date,delist_date'
            )
            
            dfs_to_process = []
            if not a_stock_df.empty:
                a_stock_df['stock_type'] = 'A股'
                dfs_to_process.append(a_stock_df)
                logger.info(f"获取到{len(a_stock_df)}只A股")
            
            # 获取港股基础信息
            if include_hk:
                logger.info("正在获取港股基础信息...")
                try:
                    hk_stock_df = self._api_call_with_retry(
                        self.pro.hk_basic,
                        list_status='L',
                        fields='ts_code,symbol,name,market,list_date,delist_date'
                    )
                    
                    if not hk_stock_df.empty:
                        # 港股数据结构适配
                        hk_stock_df['area'] = '香港'
                        hk_stock_df['industry'] = ''  # 港股基础信息中没有行业字段
                        hk_stock_df['stock_type'] = '港股'
                        dfs_to_process.append(hk_stock_df)
                        logger.info(f"获取到{len(hk_stock_df)}只港股")
                except Exception as e:
                    logger.warning(f"获取港股基础信息失败: {e}")
                    error_messages.append(f"港股数据获取失败: {e}")
            
            if not dfs_to_process:
                logger.warning("未获取到任何股票基础信息")
                return self._create_sync_result(False, 0, 0, 0, ["未获取到数据"], start_time)
            
            # 合并所有数据
            df = pd.concat(dfs_to_process, ignore_index=True)
            
            # 数据预处理
            df = self._preprocess_stock_basic(df)
            records_processed = len(df)
            
            # 批量插入数据库
            success_count = self._bulk_insert_stock_basic(df)
            records_success = success_count
            records_failed = records_processed - records_success
            
            logger.info(f"股票基础信息同步完成: 处理{records_processed}条，成功{records_success}条，失败{records_failed}条")
            
        except Exception as e:
            error_msg = f"股票基础信息同步失败: {e}"
            logger.error(error_msg)
            error_messages.append(error_msg)
            
        return self._create_sync_result(
            records_failed == 0, records_processed, records_success, records_failed, 
            error_messages, start_time
        )
    
    def sync_daily_data(self, trade_date: str = None, incremental: bool = True, include_hk: bool = True) -> DataSyncResult:
        """
        同步日线行情数据（支持A股和港股）
        
        Args:
            trade_date: 交易日期，如果为空则同步最新数据
            incremental: 是否增量同步
            include_hk: 是否包含港股数据
        """
        start_time = datetime.now()
        records_processed = 0
        records_success = 0
        records_failed = 0
        error_messages = []
        
        logger.info(f"开始同步日线行情数据 (交易日期: {trade_date or '最新'}, 增量模式: {incremental}, 包含港股: {include_hk})...")
        
        try:
            # 分别获取A股和港股列表，分开处理
            results_to_merge = []
            
            # 同步A股数据
            logger.info("正在同步A股日线数据...")
            a_stock_list = self._get_active_stocks(stock_type='A股')
            if a_stock_list:
                a_result = self._sync_daily_data_by_type(a_stock_list, trade_date, incremental, 'A股')
                results_to_merge.append(a_result)
                records_processed += a_result.records_processed
                records_success += a_result.records_success
                records_failed += a_result.records_failed
                error_messages.extend(a_result.error_messages)
            
            # 同步港股数据
            if include_hk:
                logger.info("正在同步港股日线数据...")
                hk_stock_list = self._get_active_stocks(stock_type='港股')
                if hk_stock_list:
                    hk_result = self._sync_hk_daily_data(hk_stock_list, trade_date, incremental)
                    results_to_merge.append(hk_result)
                    records_processed += hk_result.records_processed
                    records_success += hk_result.records_success
                    records_failed += hk_result.records_failed
                    error_messages.extend(hk_result.error_messages)
            
            if not results_to_merge:
                logger.warning("未获取到任何股票列表")
                return self._create_sync_result(False, 0, 0, 0, ["未获取到股票列表"], start_time)
            
            # 确定同步日期范围
            if not trade_date:
                trade_date = self._get_last_trade_date()
            
            if incremental:
                last_sync_date = self._get_last_sync_date('daily')
                start_date = last_sync_date if last_sync_date else trade_date
            else:
                start_date = trade_date
            
            logger.info(f"同步日期范围: {start_date} 到 {trade_date}")
            
            logger.info(f"日线数据同步完成: 处理{records_processed}条，成功{records_success}条，失败{records_failed}条")
            
            # 更新同步日志
            self._update_sync_log('daily', trade_date, records_processed, records_success, records_failed)
            
            logger.info(f"日线数据同步完成: 处理{records_processed}条，成功{records_success}条，失败{records_failed}条")
            
        except Exception as e:
            error_msg = f"日线数据同步失败: {e}"
            logger.error(error_msg)
            error_messages.append(error_msg)
        
        return self._create_sync_result(
            records_failed == 0, records_processed, records_success, records_failed,
            error_messages, start_time
        )
    
    def sync_financial_data(self, period: str = '') -> DataSyncResult:
        """
        同步财务数据
        
        Args:
            period: 报告期，格式：YYYYMMDD，空表示最新报告期
        """
        start_time = datetime.now()
        records_processed = 0
        records_success = 0
        records_failed = 0
        error_messages = []
        
        logger.info(f"开始同步财务数据 (报告期: {period or '最新'})...")
        
        try:
            # 获取活跃股票列表
            stock_list = self._get_active_stocks()
            if not stock_list:
                logger.warning("未获取到活跃股票列表")
                return self._create_sync_result(False, 0, 0, 0, ["未获取到股票列表"], start_time)
            
            # 使用线程池并发同步
            with ThreadPoolExecutor(max_workers=3) as executor:  # 财务数据限制并发数
                futures = []
                batch_size = 50  # 财务数据批次较小
                
                for i in range(0, len(stock_list), batch_size):
                    batch_stocks = stock_list[i:i + batch_size]
                    future = executor.submit(
                        self._sync_financial_data_batch,
                        batch_stocks, period
                    )
                    futures.append(future)
                
                # 收集结果
                for future in as_completed(futures):
                    try:
                        batch_result = future.result()
                        records_processed += batch_result['processed']
                        records_success += batch_result['success']
                        records_failed += batch_result['failed']
                        error_messages.extend(batch_result['errors'])
                    except Exception as e:
                        error_msg = f"财务数据批次同步失败: {e}"
                        logger.error(error_msg)
                        error_messages.append(error_msg)
            
            # 更新同步日志
            self._update_sync_log('financial', period or 'latest', records_processed, records_success, records_failed)
            
            logger.info(f"财务数据同步完成: 处理{records_processed}条，成功{records_success}条，失败{records_failed}条")
            
        except Exception as e:
            error_msg = f"财务数据同步失败: {e}"
            logger.error(error_msg)
            error_messages.append(error_msg)
        
        return self._create_sync_result(
            records_failed == 0, records_processed, records_success, records_failed,
            error_messages, start_time
        )
    
    def sync_market_data(self, trade_date: str = None) -> DataSyncResult:
        """
        同步市场数据(每日基本面数据)
        
        Args:
            trade_date: 交易日期
        """
        start_time = datetime.now()
        records_processed = 0
        records_success = 0
        records_failed = 0
        error_messages = []
        
        if not trade_date:
            trade_date = self._get_last_trade_date()
        
        logger.info(f"开始同步市场数据 (交易日期: {trade_date})...")
        
        try:
            # 获取每日基本面数据
            df = self._api_call_with_retry(
                self.pro.daily_basic,
                trade_date=trade_date.replace('-', ''),
                fields='ts_code,trade_date,close,turnover_rate,volume_ratio,pe,pb,ps,dv_ratio,dv_ttm,total_share,float_share,free_share,total_mv,circ_mv'
            )
            
            if df.empty:
                logger.warning(f"未获取到{trade_date}的市场数据")
                return self._create_sync_result(False, 0, 0, 0, ["未获取到数据"], start_time)
            
            # 数据预处理
            df = self._preprocess_market_data(df)
            records_processed = len(df)
            
            # 更新stock_daily表的市场数据字段
            success_count = self._update_daily_market_data(df)
            records_success = success_count
            records_failed = records_processed - records_success
            
            logger.info(f"市场数据同步完成: 处理{records_processed}条，成功{records_success}条，失败{records_failed}条")
            
        except Exception as e:
            error_msg = f"市场数据同步失败: {e}"
            logger.error(error_msg)
            error_messages.append(error_msg)
        
        return self._create_sync_result(
            records_failed == 0, records_processed, records_success, records_failed,
            error_messages, start_time
        )
    
    def get_stock_data_for_backtest(self, stock_code: str, start_date: str, end_date: str) -> pd.DataFrame:
        """
        获取回测用的股票数据
        
        Args:
            stock_code: 股票代码
            start_date: 开始日期
            end_date: 结束日期
            
        Returns:
            股票数据DataFrame
        """
        try:
            # 转换股票代码格式
            ts_code = self._convert_stock_code(stock_code)
            
            # 获取日线数据
            df = self._api_call_with_retry(
                self.pro.daily,
                ts_code=ts_code,
                start_date=start_date.replace('-', ''),
                end_date=end_date.replace('-', ''),
                fields='ts_code,trade_date,open,high,low,close,pre_close,change,pct_chg,vol,amount'
            )
            
            if df.empty:
                logger.warning(f"未获取到股票{stock_code}的数据")
                return pd.DataFrame()
            
            # 数据预处理
            df = self._preprocess_daily_data_for_backtest(df)
            
            return df
            
        except Exception as e:
            logger.error(f"获取股票{stock_code}数据失败: {e}")
            return pd.DataFrame()
    
    def _sync_daily_data_batch(self, stock_codes: List[str], start_date: str, end_date: str) -> Dict[str, Any]:
        """同步一批股票的日线数据"""
        result = {'processed': 0, 'success': 0, 'failed': 0, 'errors': []}
        
        for stock_code in stock_codes:
            try:
                # 获取日线数据
                df = self._api_call_with_retry(
                    self.pro.daily,
                    ts_code=stock_code,
                    start_date=start_date.replace('-', ''),
                    end_date=end_date.replace('-', ''),
                    fields='ts_code,trade_date,open,high,low,close,pre_close,change,pct_chg,vol,amount'
                )
                
                if not df.empty:
                    # 数据预处理
                    df = self._preprocess_daily_data(df)
                    result['processed'] += len(df)
                    
                    # 插入数据库
                    success_count = self._bulk_insert_daily_data(df)
                    result['success'] += success_count
                    result['failed'] += len(df) - success_count
                
                # API限流延时
                time.sleep(0.1)
                
            except Exception as e:
                error_msg = f"同步{stock_code}失败: {e}"
                result['errors'].append(error_msg)
                result['failed'] += 1
                logger.warning(error_msg)
        
        return result
    
    def _sync_financial_data_batch(self, stock_codes: List[str], period: str) -> Dict[str, Any]:
        """同步一批股票的财务数据"""
        result = {'processed': 0, 'success': 0, 'failed': 0, 'errors': []}
        
        for stock_code in stock_codes:
            try:
                # 获取资产负债表
                balancesheet = self._api_call_with_retry(
                    self.pro.balancesheet,
                    ts_code=stock_code,
                    period=period,
                    fields='ts_code,end_date,ann_date,report_type,total_assets,total_hldr_eqy_exc_min_int,total_liab,monetary_cap,accounts_receiv,inventories,fixed_assets'
                )
                
                # 获取利润表
                income = self._api_call_with_retry(
                    self.pro.income,
                    ts_code=stock_code,
                    period=period,
                    fields='ts_code,end_date,ann_date,report_type,revenue,operate_profit,total_profit,n_income,n_income_attr_p,ebit,ebitda'
                )
                
                # 获取现金流量表
                cashflow = self._api_call_with_retry(
                    self.pro.cashflow,
                    ts_code=stock_code,
                    period=period,
                    fields='ts_code,end_date,ann_date,report_type,c_fr_sale_sg,c_paid_goods_s,n_cashflow_act,n_cashflow_inv_act,n_cashflow_fin_act,c_cash_equ_end_period'
                )
                
                # 获取财务指标
                fina_indicator = self._api_call_with_retry(
                    self.pro.fina_indicator,
                    ts_code=stock_code,
                    period=period,
                    fields='ts_code,end_date,ann_date,roe,roa,gross_margin,net_margin,debt_to_assets,current_ratio,quick_ratio'
                )
                
                # 合并财务数据
                financial_data = self._merge_financial_data(balancesheet, income, cashflow, fina_indicator)
                
                if not financial_data.empty:
                    result['processed'] += len(financial_data)
                    
                    # 插入数据库
                    success_count = self._bulk_insert_financial_data(financial_data)
                    result['success'] += success_count
                    result['failed'] += len(financial_data) - success_count
                
                # API限流延时
                time.sleep(0.2)  # 财务数据限流更严格
                
            except Exception as e:
                error_msg = f"同步{stock_code}财务数据失败: {e}"
                result['errors'].append(error_msg)
                result['failed'] += 1
                logger.warning(error_msg)
        
        return result
    
    def _preprocess_stock_basic(self, df: pd.DataFrame) -> pd.DataFrame:
        """预处理股票基础信息"""
        df = df.copy()
        
        # 添加股票代码(去掉后缀)
        df['stock_code'] = df['ts_code'].str.split('.').str[0]
        
        # 处理上市日期
        df['list_date'] = pd.to_datetime(df['list_date'], format='%Y%m%d', errors='coerce')
        
        # 处理退市日期
        df['delist_date'] = pd.to_datetime(df['delist_date'], format='%Y%m%d', errors='coerce')
        
        # 判断是否活跃
        df['is_active'] = df['delist_date'].isna()
        
        # 处理市场类型
        market_mapping = {
            '主板': 'main',
            '创业板': 'gem', 
            '科创板': 'star',
            '北交所': 'bse'
        }
        df['market'] = df['market'].map(market_mapping).fillna('main')
        
        return df
    
    def _preprocess_daily_data(self, df: pd.DataFrame) -> pd.DataFrame:
        """预处理日线数据"""
        df = df.copy()
        
        # 添加股票代码(去掉后缀)
        df['stock_code'] = df['ts_code'].str.split('.').str[0]
        
        # 转换交易日期
        df['trade_date'] = pd.to_datetime(df['trade_date'], format='%Y%m%d')
        
        # 重命名列
        column_mapping = {
            'vol': 'volume',
            'pct_chg': 'change_pct',
            'change': 'change_amt'
        }
        df.rename(columns=column_mapping, inplace=True)
        
        # 处理空值
        numeric_columns = ['open', 'high', 'low', 'close', 'pre_close', 'change_amt', 'change_pct', 'volume', 'amount']
        df[numeric_columns] = df[numeric_columns].fillna(0)
        
        # 数据验证
        df = df[df['close'] > 0]  # 过滤无效价格
        df = df.drop_duplicates(subset=['stock_code', 'trade_date'])  # 去重
        
        return df
    
    def _preprocess_daily_data_for_backtest(self, df: pd.DataFrame) -> pd.DataFrame:
        """为回测预处理日线数据"""
        df = self._preprocess_daily_data(df)
        
        # 设置日期为索引
        df.set_index('trade_date', inplace=True)
        df.sort_index(inplace=True)
        
        # 重命名列以匹配回测引擎需求
        df.rename(columns={'volume': 'vol'}, inplace=True)
        
        return df[['open', 'high', 'low', 'close', 'vol', 'amount']]
    
    def _preprocess_market_data(self, df: pd.DataFrame) -> pd.DataFrame:
        """预处理市场数据"""
        df = df.copy()
        
        # 添加股票代码
        df['stock_code'] = df['ts_code'].str.split('.').str[0]
        
        # 转换日期
        df['trade_date'] = pd.to_datetime(df['trade_date'], format='%Y%m%d')
        
        # 处理空值
        numeric_columns = ['turnover_rate', 'volume_ratio', 'pe', 'pb', 'total_share', 'float_share', 'free_share', 'total_mv', 'circ_mv']
        df[numeric_columns] = df[numeric_columns].fillna(0)
        
        return df
    
    def _merge_financial_data(self, balancesheet: pd.DataFrame, income: pd.DataFrame, 
                             cashflow: pd.DataFrame, indicator: pd.DataFrame) -> pd.DataFrame:
        """合并财务数据"""
        # 以资产负债表为基础进行合并
        if balancesheet.empty:
            return pd.DataFrame()
        
        merged = balancesheet.copy()
        
        # 合并利润表
        if not income.empty:
            merged = merged.merge(
                income[['ts_code', 'end_date', 'revenue', 'operate_profit', 'total_profit', 'n_income', 'n_income_attr_p', 'ebit', 'ebitda']], 
                on=['ts_code', 'end_date'], 
                how='left'
            )
        
        # 合并现金流量表
        if not cashflow.empty:
            merged = merged.merge(
                cashflow[['ts_code', 'end_date', 'c_fr_sale_sg', 'c_paid_goods_s', 'n_cashflow_act', 'n_cashflow_inv_act', 'n_cashflow_fin_act', 'c_cash_equ_end_period']],
                on=['ts_code', 'end_date'],
                how='left'
            )
        
        # 合并财务指标
        if not indicator.empty:
            merged = merged.merge(
                indicator[['ts_code', 'end_date', 'roe', 'roa', 'gross_margin', 'net_margin', 'debt_to_assets', 'current_ratio', 'quick_ratio']],
                on=['ts_code', 'end_date'],
                how='left'
            )
        
        # 添加股票代码
        merged['stock_code'] = merged['ts_code'].str.split('.').str[0]
        
        # 转换日期
        merged['end_date'] = pd.to_datetime(merged['end_date'], format='%Y%m%d')
        merged['ann_date'] = pd.to_datetime(merged['ann_date'], format='%Y%m%d', errors='coerce')
        
        return merged
    
    def _bulk_insert_stock_basic(self, df: pd.DataFrame) -> int:
        """批量插入股票基础信息"""
        try:
            connection = self.get_connection()
            cursor = connection.cursor()
            
            if self.db_type == 'mysql':
                sql = """
                INSERT INTO stock_basic 
                (stock_code, stock_name, industry, sector, market, stock_type, area, list_date, delist_date, is_active)
                VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s)
                ON DUPLICATE KEY UPDATE
                stock_name = VALUES(stock_name),
                industry = VALUES(industry),
                sector = VALUES(sector),
                market = VALUES(market),
                stock_type = VALUES(stock_type),
                area = VALUES(area),
                list_date = VALUES(list_date),
                delist_date = VALUES(delist_date),
                is_active = VALUES(is_active),
                updated_at = CURRENT_TIMESTAMP
                """
            else:
                sql = """
                INSERT OR REPLACE INTO stock_basic 
                (stock_code, stock_name, industry, sector, market, stock_type, area, list_date, delist_date, is_active)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                """
            
            success_count = 0
            for _, row in df.iterrows():
                try:
                    cursor.execute(sql, (
                        row['stock_code'],
                        row['name'],
                        row.get('industry', ''),
                        row.get('area', ''),  # 这个作为sector字段
                        row['market'],
                        row.get('stock_type', 'A股'),  # 新增的股票类型字段
                        row.get('area', ''),  # 新增的地区字段
                        row['list_date'] if pd.notna(row['list_date']) else None,
                        row['delist_date'] if pd.notna(row['delist_date']) else None,
                        row['is_active']
                    ))
                    success_count += 1
                except Exception as e:
                    logger.warning(f"插入股票{row['stock_code']}基础信息失败: {e}")
            
            connection.commit()
            return success_count
            
        except Exception as e:
            logger.error(f"批量插入股票基础信息失败: {e}")
            return 0
        finally:
            if 'connection' in locals():
                connection.close()
    
    def _bulk_insert_daily_data(self, df: pd.DataFrame) -> int:
        """批量插入日线数据"""
        try:
            connection = self.get_connection()
            cursor = connection.cursor()
            
            if self.db_type == 'mysql':
                sql = """
                INSERT INTO stock_daily 
                (stock_code, trade_date, open_price, high_price, low_price, close_price, 
                 pre_close, change_amt, change_pct, volume, amount)
                VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s)
                ON DUPLICATE KEY UPDATE
                open_price = VALUES(open_price),
                high_price = VALUES(high_price),
                low_price = VALUES(low_price),
                close_price = VALUES(close_price),
                pre_close = VALUES(pre_close),
                change_amt = VALUES(change_amt),
                change_pct = VALUES(change_pct),
                volume = VALUES(volume),
                amount = VALUES(amount)
                """
            else:
                sql = """
                INSERT OR REPLACE INTO stock_daily 
                (stock_code, trade_date, open_price, high_price, low_price, close_price,
                 pre_close, change_amt, change_pct, volume, amount)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                """
            
            success_count = 0
            for _, row in df.iterrows():
                try:
                    cursor.execute(sql, (
                        row['stock_code'],
                        row['trade_date'],
                        row['open'],
                        row['high'],
                        row['low'],
                        row['close'],
                        row['pre_close'],
                        row['change_amt'],
                        row['change_pct'],
                        int(row['volume']) if pd.notna(row['volume']) else 0,
                        float(row['amount']) if pd.notna(row['amount']) else 0.0
                    ))
                    success_count += 1
                except Exception as e:
                    logger.warning(f"插入{row['stock_code']} {row['trade_date']}行情数据失败: {e}")
            
            connection.commit()
            return success_count
            
        except Exception as e:
            logger.error(f"批量插入日线数据失败: {e}")
            return 0
        finally:
            if 'connection' in locals():
                connection.close()
    
    def _bulk_insert_financial_data(self, df: pd.DataFrame) -> int:
        """批量插入财务数据"""
        try:
            connection = self.get_connection()
            cursor = connection.cursor()
            
            if self.db_type == 'mysql':
                sql = """
                INSERT INTO financial_data 
                (stock_code, end_date, ann_date, report_type, total_assets, total_hldr_eqy_exc_min_int, 
                 total_liab, monetary_cap, accounts_receiv, inventories, fixed_assets,
                 revenue, operate_profit, total_profit, n_income, n_income_attr_p, ebit, ebitda,
                 c_fr_sale_sg, c_paid_goods_s, n_cashflow_act, n_cashflow_inv_act, n_cashflow_fin_act, c_cash_equ_end_period,
                 roe, roa, gross_margin, net_margin, debt_to_assets, current_ratio, quick_ratio)
                VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s)
                ON DUPLICATE KEY UPDATE
                ann_date = VALUES(ann_date),
                report_type = VALUES(report_type),
                total_assets = VALUES(total_assets),
                revenue = VALUES(revenue),
                roe = VALUES(roe),
                updated_at = CURRENT_TIMESTAMP
                """
            else:
                # SQLite简化版
                sql = """
                INSERT OR REPLACE INTO financial_data 
                (stock_code, end_date, report_type, revenue, n_income, roe, net_margin)
                VALUES (?, ?, ?, ?, ?, ?, ?)
                """
            
            success_count = 0
            for _, row in df.iterrows():
                try:
                    if self.db_type == 'mysql':
                        cursor.execute(sql, tuple(row[col] if pd.notna(row[col]) else None for col in [
                            'stock_code', 'end_date', 'ann_date', 'report_type',
                            'total_assets', 'total_hldr_eqy_exc_min_int', 'total_liab',
                            'monetary_cap', 'accounts_receiv', 'inventories', 'fixed_assets',
                            'revenue', 'operate_profit', 'total_profit', 'n_income', 'n_income_attr_p', 'ebit', 'ebitda',
                            'c_fr_sale_sg', 'c_paid_goods_s', 'n_cashflow_act', 'n_cashflow_inv_act', 'n_cashflow_fin_act', 'c_cash_equ_end_period',
                            'roe', 'roa', 'gross_margin', 'net_margin', 'debt_to_assets', 'current_ratio', 'quick_ratio'
                        ]))
                    else:
                        cursor.execute(sql, (
                            row['stock_code'],
                            row['end_date'],
                            row.get('report_type', ''),
                            row.get('revenue', 0),
                            row.get('n_income', 0),
                            row.get('roe', 0),
                            row.get('net_margin', 0)
                        ))
                    success_count += 1
                except Exception as e:
                    logger.warning(f"插入{row['stock_code']} {row['end_date']}财务数据失败: {e}")
            
            connection.commit()
            return success_count
            
        except Exception as e:
            logger.error(f"批量插入财务数据失败: {e}")
            return 0
        finally:
            if 'connection' in locals():
                connection.close()
    
    def _update_daily_market_data(self, df: pd.DataFrame) -> int:
        """更新日线数据的市场字段"""
        try:
            connection = self.get_connection()
            cursor = connection.cursor()
            
            if self.db_type == 'mysql':
                sql = """
                UPDATE stock_daily SET
                turnover_rate = %s, volume_ratio = %s, pe = %s, pb = %s,
                total_share = %s, float_share = %s, free_share = %s, 
                total_mv = %s, circ_mv = %s
                WHERE stock_code = %s AND trade_date = %s
                """
            else:
                sql = """
                UPDATE stock_daily SET
                turnover_rate = ?, volume_ratio = ?, pe = ?, pb = ?
                WHERE stock_code = ? AND trade_date = ?
                """
            
            success_count = 0
            for _, row in df.iterrows():
                try:
                    if self.db_type == 'mysql':
                        cursor.execute(sql, (
                            row['turnover_rate'], row['volume_ratio'], row['pe'], row['pb'],
                            int(row['total_share']), int(row['float_share']), int(row['free_share']),
                            row['total_mv'], row['circ_mv'],
                            row['stock_code'], row['trade_date']
                        ))
                    else:
                        cursor.execute(sql, (
                            row['turnover_rate'], row['volume_ratio'], row['pe'], row['pb'],
                            row['stock_code'], row['trade_date']
                        ))
                    if cursor.rowcount > 0:
                        success_count += 1
                except Exception as e:
                    logger.warning(f"更新{row['stock_code']} {row['trade_date']}市场数据失败: {e}")
            
            connection.commit()
            return success_count
            
        except Exception as e:
            logger.error(f"更新市场数据失败: {e}")
            return 0
        finally:
            if 'connection' in locals():
                connection.close()
    
    def _get_active_stocks(self, stock_type: str = None) -> List[str]:
        """
        获取活跃股票列表（支持A股和港股）
        
        Args:
            stock_type: 股票类型 ('A股', '港股', None=全部)
        """
        try:
            connection = self.get_connection()
            cursor = connection.cursor()
            
            if self.db_type == 'mysql':
                base_query = """
                    SELECT CONCAT(stock_code, CASE 
                        WHEN stock_type = '港股' THEN ''
                        WHEN market IN ('gem', 'star') THEN '.SZ'
                        WHEN SUBSTRING(stock_code, 1, 1) IN ('0', '3') THEN '.SZ'
                        ELSE '.SH'
                    END) as ts_code,
                    stock_type
                    FROM stock_basic 
                    WHERE is_active = 1
                """
                
                if stock_type:
                    base_query += " AND stock_type = %s"
                    cursor.execute(base_query + " ORDER BY stock_code", (stock_type,))
                else:
                    cursor.execute(base_query + " ORDER BY stock_code")
                    
            else:
                base_query = """
                    SELECT stock_code || 
                           CASE 
                               WHEN stock_type = '港股' THEN ''
                               WHEN substr(stock_code, 1, 1) IN ('0', '3') THEN '.SZ'
                               ELSE '.SH'
                           END as ts_code,
                           stock_type
                    FROM stock_basic 
                    WHERE is_active = 1
                """
                
                if stock_type:
                    base_query += " AND stock_type = ?"
                    cursor.execute(base_query + " ORDER BY stock_code", (stock_type,))
                else:
                    cursor.execute(base_query + " ORDER BY stock_code")
            
            result = [row[0] for row in cursor.fetchall()]
            return result
            
        except Exception as e:
            logger.error(f"获取活跃股票列表失败: {e}")
            return []
        finally:
            if 'connection' in locals():
                connection.close()
    
    def _get_last_trade_date(self) -> str:
        """获取最后交易日"""
        try:
            # 从TuShare获取最新交易日历
            cal_df = self._api_call_with_retry(
                self.pro.trade_cal,
                exchange='SSE',
                start_date=(datetime.now() - timedelta(days=10)).strftime('%Y%m%d'),
                end_date=datetime.now().strftime('%Y%m%d')
            )
            
            if not cal_df.empty:
                last_trade = cal_df[cal_df['is_open'] == 1]['cal_date'].max()
                return pd.to_datetime(last_trade, format='%Y%m%d').strftime('%Y-%m-%d')
            
        except Exception as e:
            logger.warning(f"获取交易日历失败: {e}")
        
        # 备选方案：返回前一工作日
        today = datetime.now()
        if today.weekday() == 0:  # 周一
            last_trade = today - timedelta(days=3)
        elif today.weekday() == 6:  # 周日
            last_trade = today - timedelta(days=2)
        else:
            last_trade = today - timedelta(days=1)
        
        return last_trade.strftime('%Y-%m-%d')
    
    def _get_last_sync_date(self, sync_type: str) -> Optional[str]:
        """获取最后同步日期"""
        try:
            connection = self.get_connection()
            cursor = connection.cursor()
            
            cursor.execute("""
                SELECT sync_date FROM data_sync_log 
                WHERE sync_type = ? AND status = 'SUCCESS'
                ORDER BY created_at DESC LIMIT 1
            """, (sync_type,))
            
            result = cursor.fetchone()
            return result[0] if result else None
            
        except Exception as e:
            logger.warning(f"获取最后同步日期失败: {e}")
            return None
        finally:
            if 'connection' in locals():
                connection.close()
    
    def _update_sync_log(self, sync_type: str, sync_date: str, processed: int, success: int, failed: int):
        """更新同步日志"""
        try:
            connection = self.get_connection()
            cursor = connection.cursor()
            
            if self.db_type == 'mysql':
                sql = """
                INSERT INTO data_sync_log 
                (sync_type, sync_date, status, records_processed, records_success, records_failed, created_at)
                VALUES (%s, %s, %s, %s, %s, %s, NOW())
                """
            else:
                sql = """
                INSERT INTO data_sync_log 
                (sync_type, sync_date, status, records_processed, records_success, records_failed, created_at)
                VALUES (?, ?, ?, ?, ?, ?, datetime('now'))
                """
            
            status = 'SUCCESS' if failed == 0 else 'PARTIAL'
            cursor.execute(sql, (sync_type, sync_date, status, processed, success, failed))
            connection.commit()
            
        except Exception as e:
            logger.error(f"更新同步日志失败: {e}")
        finally:
            if 'connection' in locals():
                connection.close()
    
    def _convert_stock_code(self, stock_code: str) -> str:
        """转换股票代码格式"""
        if '.' in stock_code:
            return stock_code
        
        # 根据股票代码判断交易所
        if stock_code.startswith(('00', '30')):  # 深交所
            return f"{stock_code}.SZ"
        else:  # 上交所
            return f"{stock_code}.SH"
    
    def _create_sync_result(self, success: bool, processed: int, success_count: int, 
                          failed_count: int, errors: List[str], start_time: datetime) -> DataSyncResult:
        """创建同步结果对象"""
        end_time = datetime.now()
        duration = (end_time - start_time).total_seconds()
        
        return DataSyncResult(
            success=success,
            records_processed=processed,
            records_success=success_count,
            records_failed=failed_count,
            error_messages=errors,
            start_time=start_time.isoformat(),
            end_time=end_time.isoformat(),
            duration=duration
        )
    
    def get_sync_status(self) -> Dict[str, Any]:
        """获取同步状态"""
        try:
            connection = self.get_connection()
            cursor = connection.cursor()
            
            # 获取最近的同步记录
            cursor.execute("""
                SELECT sync_type, sync_date, status, records_processed, records_success, records_failed, created_at
                FROM data_sync_log 
                ORDER BY created_at DESC 
                LIMIT 10
            """)
            
            recent_syncs = []
            for row in cursor.fetchall():
                recent_syncs.append({
                    'sync_type': row[0],
                    'sync_date': row[1],
                    'status': row[2],
                    'records_processed': row[3],
                    'records_success': row[4],
                    'records_failed': row[5],
                    'created_at': row[6]
                })
            
            # 统计各类数据的记录数
            stats = {}
            tables = ['stock_basic', 'stock_daily', 'financial_data']
            
            for table in tables:
                try:
                    cursor.execute(f"SELECT COUNT(*) FROM {table}")
                    count = cursor.fetchone()[0]
                    stats[table] = count
                except Exception as e:
                    stats[table] = 0
                    logger.warning(f"获取{table}表统计失败: {e}")
            
            return {
                'recent_syncs': recent_syncs,
                'table_stats': stats,
                'last_update': datetime.now().isoformat()
            }
            
        except Exception as e:
            logger.error(f"获取同步状态失败: {e}")
            return {'error': str(e)}
        finally:
            if 'connection' in locals():
                connection.close()

    def _sync_daily_data_by_type(self, stock_list: List[str], trade_date: str, incremental: bool, stock_type: str) -> DataSyncResult:
        """
        按股票类型同步日线数据
        
        Args:
            stock_list: 股票列表
            trade_date: 交易日期
            incremental: 是否增量同步
            stock_type: 股票类型
        """
        start_time = datetime.now()
        records_processed = 0
        records_success = 0
        records_failed = 0
        error_messages = []
        
        try:
            # 确定同步日期范围
            if incremental:
                last_sync_date = self._get_last_sync_date('daily')
                start_date = last_sync_date if last_sync_date else trade_date
            else:
                start_date = trade_date
            
            # 使用线程池并发同步
            with ThreadPoolExecutor(max_workers=5) as executor:
                futures = []
                batch_size = 100  # 日线数据批次大小
                
                for i in range(0, len(stock_list), batch_size):
                    batch = stock_list[i:i + batch_size]
                    future = executor.submit(
                        self._sync_daily_data_batch, 
                        batch, start_date, trade_date
                    )
                    futures.append(future)
                
                # 收集结果
                for future in as_completed(futures):
                    try:
                        batch_result = future.result()
                        records_processed += batch_result['processed']
                        records_success += batch_result['success']
                        records_failed += batch_result['failed']
                        if batch_result['errors']:
                            error_messages.extend(batch_result['errors'])
                    except Exception as e:
                        error_msg = f"{stock_type}数据同步批次失败: {e}"
                        logger.error(error_msg)
                        error_messages.append(error_msg)
                        records_failed += batch_size
            
            logger.info(f"{stock_type}日线数据同步完成: 处理{records_processed}条，成功{records_success}条，失败{records_failed}条")
            
        except Exception as e:
            error_msg = f"{stock_type}日线数据同步失败: {e}"
            logger.error(error_msg)
            error_messages.append(error_msg)
            
        return self._create_sync_result(
            records_failed == 0, records_processed, records_success, records_failed, 
            error_messages, start_time
        )
    
    def _sync_hk_daily_data(self, hk_stock_list: List[str], trade_date: str, incremental: bool) -> DataSyncResult:
        """
        同步港股日线数据
        
        Args:
            hk_stock_list: 港股列表
            trade_date: 交易日期
            incremental: 是否增量同步
        """
        start_time = datetime.now()
        records_processed = 0
        records_success = 0
        records_failed = 0
        error_messages = []
        
        logger.info(f"开始同步港股日线数据，股票数量: {len(hk_stock_list)}")
        
        try:
            # 确定同步日期范围
            if incremental:
                last_sync_date = self._get_last_sync_date('daily')
                start_date = last_sync_date if last_sync_date else trade_date
            else:
                start_date = trade_date
            
            # 港股使用专门的API接口
            with ThreadPoolExecutor(max_workers=3) as executor:  # 港股API限制并发数
                futures = []
                batch_size = 50  # 港股批次较小
                
                for i in range(0, len(hk_stock_list), batch_size):
                    batch = hk_stock_list[i:i + batch_size]
                    future = executor.submit(
                        self._sync_hk_daily_batch, 
                        batch, start_date, trade_date
                    )
                    futures.append(future)
                
                # 收集结果
                for future in as_completed(futures):
                    try:
                        batch_result = future.result()
                        records_processed += batch_result['processed']
                        records_success += batch_result['success']
                        records_failed += batch_result['failed']
                        if batch_result['errors']:
                            error_messages.extend(batch_result['errors'])
                    except Exception as e:
                        error_msg = f"港股数据同步批次失败: {e}"
                        logger.error(error_msg)
                        error_messages.append(error_msg)
                        records_failed += batch_size
            
            logger.info(f"港股日线数据同步完成: 处理{records_processed}条，成功{records_success}条，失败{records_failed}条")
            
        except Exception as e:
            error_msg = f"港股日线数据同步失败: {e}"
            logger.error(error_msg)
            error_messages.append(error_msg)
            
        return self._create_sync_result(
            records_failed == 0, records_processed, records_success, records_failed, 
            error_messages, start_time
        )
    
    def _sync_hk_daily_batch(self, stock_codes: List[str], start_date: str, end_date: str) -> Dict[str, Any]:
        """
        批量同步港股日线数据
        
        Args:
            stock_codes: 港股代码列表
            start_date: 开始日期
            end_date: 结束日期
        """
        processed = 0
        success = 0
        failed = 0
        errors = []
        
        try:
            for ts_code in stock_codes:
                try:
                    # 使用港股日线数据接口
                    df = self._api_call_with_retry(
                        self.pro.hk_daily,
                        ts_code=ts_code,
                        start_date=start_date.replace('-', ''),
                        end_date=end_date.replace('-', ''),
                        fields='ts_code,trade_date,open,high,low,close,pre_close,change,pct_chg,vol,amount,turnover_rate'
                    )
                    
                    if not df.empty:
                        # 港股数据字段映射
                        df = df.rename(columns={
                            'ts_code': 'stock_code',
                            'open': 'open_price',
                            'high': 'high_price', 
                            'low': 'low_price',
                            'close': 'close_price',
                            'change': 'change_amt',
                            'pct_chg': 'change_pct',
                            'vol': 'volume',
                            'amount': 'amount'
                        })
                        
                        # 格式化日期
                        df['trade_date'] = pd.to_datetime(df['trade_date']).dt.strftime('%Y-%m-%d')
                        df['stock_code'] = df['stock_code'].str.replace('.HK', '')  # 移除.HK后缀
                        
                        # 插入数据库
                        insert_count = self._bulk_insert_daily_data(df)
                        success += insert_count
                        processed += len(df)
                    else:
                        processed += 1
                        success += 1
                    
                except Exception as e:
                    error_msg = f"港股{ts_code}数据获取失败: {e}"
                    logger.warning(error_msg)
                    errors.append(error_msg)
                    failed += 1
                    processed += 1
                
                # 增加延迟避免API限制
                time.sleep(0.1)
                
        except Exception as e:
            error_msg = f"港股批量数据同步失败: {e}"
            logger.error(error_msg)
            errors.append(error_msg)
            
        return {
            'processed': processed,
            'success': success, 
            'failed': failed,
            'errors': errors
        }

def main():
    """主函数 - 测试TuShare数据管理器"""
    # 配置
    token = os.getenv('TUSHARE_TOKEN', 'your_token_here')
    
    db_config = {
        'type': 'sqlite',
        'database': 'data/stock_analysis.db'
    }
    
    # 创建数据管理器
    data_manager = TuShareDataManager(token, db_config)
    
    # 测试基础信息同步
    print("测试股票基础信息同步...")
    result = data_manager.sync_stock_basic_info()
    print(f"同步结果: 成功={result.success}, 处理={result.records_processed}, 成功={result.records_success}, 失败={result.records_failed}")
    
    # 测试日线数据同步
    print("\n测试日线数据同步...")
    result = data_manager.sync_daily_data(trade_date='2025-01-15', incremental=False)
    print(f"同步结果: 成功={result.success}, 处理={result.records_processed}, 成功={result.records_success}, 失败={result.records_failed}")
    
    # 获取同步状态
    print("\n获取同步状态...")
    status = data_manager.get_sync_status()
    print(json.dumps(status, indent=2, ensure_ascii=False, default=str))

if __name__ == "__main__":
    main()