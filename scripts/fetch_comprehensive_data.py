#!/usr/bin/env python3
"""
A股全维度数据获取系统
基于TuShare Pro接口获取基本面、财务、资金流、股东、公告等多维度信息
"""
import pandas as pd
import tushare as ts
from datetime import datetime, timedelta
from typing import List, Dict, Optional
import logging
import time
import sys, os
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import config
from utils.db_helper import db
from utils.comprehensive_data_schema import ComprehensiveDataSchema
from tqdm import tqdm
import threading
from concurrent.futures import ThreadPoolExecutor, as_completed

logger = logging.getLogger(__name__)

class ComprehensiveDataFetcher:
    """全维度数据获取器"""
    
    def __init__(self):
        ts.set_token(config.TS_TOKEN)
        self.pro = ts.pro_api()
        self.rate_limit_delay = 60 / config.TS_RATE_LIMIT  # 限频延迟
        self.schema = ComprehensiveDataSchema()
        
        # 数据更新频率配置
        self.update_frequency = {
            'stock_basic': 'weekly',      # 基本信息每周更新
            'financial': 'quarterly',    # 财务数据每季度更新  
            'money_flow': 'daily',       # 资金流向每日更新
            'holders': 'quarterly',      # 股东信息每季度更新
            'announcements': 'daily',    # 公告每日更新
            'market_ext': 'daily',       # 行情扩展每日更新
            'macro': 'monthly'           # 宏观数据每月更新
        }
        
    def _clean_ann_date_data(self, df, required_cols=['ann_date']):
        """通用数据清洗方法：处理ann_date等必需字段为空的问题
        
        Args:
            df: 原始DataFrame
            required_cols: 必需非空的列名列表
            
        Returns:
            tuple: (清洗后的DataFrame, 是否有效数据)
        """
        if df.empty:
            return df, False
            
        # 处理日期字段
        date_cols = [col for col in df.columns if 'date' in col.lower()]
        for col in date_cols:
            df[col] = pd.to_datetime(df[col], errors='coerce')
            
        # 过滤掉必需字段为空的记录
        initial_count = len(df)
        df_clean = df.dropna(subset=required_cols)
        final_count = len(df_clean)
        
        if initial_count > final_count:
            logger.warning(f"过滤掉{initial_count - final_count}条必需字段为空的记录")
            
        return df_clean, not df_clean.empty
            
    def fetch_all_comprehensive_data(self, mode='incremental'):
        """获取所有维度数据
        
        Args:
            mode: 'full' 全量获取, 'incremental' 增量更新
        """
        logger.info(f"🚀 开始获取A股全维度数据 - 模式: {mode}")
        
        try:
            # 1. 初始化数据库表结构
            logger.info("🔧 初始化数据库表结构...")
            self.schema.create_all_tables(db)
            
            # 2. 获取股票基本信息(优先级最高)
            self.fetch_basic_info_data(mode)
            
            # 3. 并行获取其他维度数据
            with ThreadPoolExecutor(max_workers=config.TS_THREAD_COUNT) as executor:
                futures = []
                
                # 提交各类数据获取任务
                futures.append(executor.submit(self.fetch_financial_data, mode))
                futures.append(executor.submit(self.fetch_money_flow_data, mode))
                futures.append(executor.submit(self.fetch_shareholder_data, mode))
                futures.append(executor.submit(self.fetch_announcement_data, mode))
                futures.append(executor.submit(self.fetch_market_extension_data, mode))
                futures.append(executor.submit(self.fetch_macro_data, mode))
                
                # 等待所有任务完成
                for future in as_completed(futures):
                    try:
                        result = future.result()
                        logger.info(f"✅ 数据获取任务完成: {result}")
                    except Exception as e:
                        logger.error(f"❌ 数据获取任务失败: {e}")
                        
            logger.info("🎉 全维度数据获取完成!")
            
        except Exception as e:
            logger.error(f"❌ 全维度数据获取失败: {e}")
            raise
            
    def fetch_basic_info_data(self, mode='incremental'):
        """获取基本面信息数据"""
        logger.info("📊 开始获取基本面信息数据...")
        
        try:
            # 1. 股票基本信息（免费接口）
            self._fetch_stock_basic()
            
            # 2. 停复牌信息（免费接口）
            self._fetch_suspend_data()
            
            # 3. 行业分类信息（使用备用方案）
            self._fetch_industry_classification()
            
            # 4. 跳过股本结构（太耗时，已有27000+记录）
            logger.info("⏭️ 跳过股本结构数据获取（已有充足数据，避免长时间运行）")
            
            logger.info("✅ 基本面信息数据获取完成")
            return "基本面信息数据获取完成"
            
        except Exception as e:
            logger.error(f"❌ 基本面信息数据获取失败: {e}")
            raise
            
    def _fetch_stock_basic(self):
        """获取股票基本信息"""
        logger.info("📋 获取股票基本信息...")
        
        try:
            # 获取基本信息
            df = self.pro.stock_basic()  # 不指定fields，获取所有字段
            
            if not df.empty:
                # TuShare实际返回的字段: ts_code, symbol, name, area, industry, cnspell, market, list_date, act_name, act_ent_type
                # 选择表中存在且接口返回的字段
                available_columns = list(df.columns)
                table_columns = ['ts_code', 'symbol', 'name', 'area', 'industry', 'market', 'list_date']
                
                # 只保留存在的字段
                df_filtered = df[table_columns].copy()
                
                # 转换数据格式
                df_filtered['list_date'] = pd.to_datetime(df_filtered['list_date'], errors='coerce')
                
                # 添加缺失的字段设为默认值
                df_filtered['fullname'] = None
                df_filtered['enname'] = None  
                df_filtered['exchange'] = None
                df_filtered['curr_type'] = None
                df_filtered['list_status'] = 'L'  # 默认为上市
                df_filtered['delist_date'] = None
                df_filtered['is_hs'] = None
                
                # 批量插入数据库
                success_count = db.upsert_dataframe(
                    df_filtered, 't_stock_basic', 
                    unique_cols=['ts_code'],
                    update_cols=['name', 'industry', 'area']
                )
                
                logger.info(f"✅ 股票基本信息插入成功: {success_count} 条")
            
            time.sleep(self.rate_limit_delay)
            
        except Exception as e:
            logger.error(f"❌ 获取股票基本信息失败: {e}")
            import traceback
            logger.error(traceback.format_exc())
            
    def _fetch_suspend_data(self):
        """获取停复牌信息（免费接口）"""
        logger.info("🛑 获取停复牌信息...")
        
        try:
            from datetime import datetime, timedelta
            
            # 获取最近1年的停复牌数据
            end_date = datetime.now()
            start_date = end_date - timedelta(days=365)
            
            df = self.pro.suspend_d(
                start_date=start_date.strftime('%Y%m%d'),
                end_date=end_date.strftime('%Y%m%d')
            )
            
            if not df.empty:
                # 转换数据格式，匹配表结构
                df['suspend_date'] = pd.to_datetime(df['trade_date'], format='%Y%m%d', errors='coerce')
                df['resume_date'] = None  # TuShare接口没有复牌日期
                df['ann_date'] = df['suspend_date']  # 使用停牌日期作为公告日期
                df['suspend_reason'] = df['suspend_type']  # 停牌类型作为原因
                df['reason_type'] = df['suspend_timing']  # 停牌时点
                
                # 选择需要的字段
                df_insert = df[['ts_code', 'suspend_date', 'resume_date', 'ann_date', 'suspend_reason', 'reason_type']].copy()
                
                success_count = db.upsert_dataframe(
                    df_insert, 't_suspend',
                    unique_cols=['ts_code', 'suspend_date'],
                    update_cols=['resume_date', 'suspend_reason', 'reason_type']
                )
                
                logger.info(f"✅ 停复牌数据插入: {success_count} 条")
            else:
                logger.warning("⚠️ 停复牌数据为空")
                
        except Exception as e:
            logger.error(f"❌ 获取停复牌信息失败: {e}")
            import traceback
            logger.error(traceback.format_exc())
            
    def _fetch_industry_classification(self):
        """获取行业分类信息"""
        logger.info("🏭 获取行业分类信息...")
        
        try:
            # 使用hs_const接口获取行业分类数据
            logger.info("📋 获取申万行业分类...")
            
            # 获取申万一级行业分类
            df_sw_l1 = self.pro.hs_const(hs_type='SW_L1')
            if not df_sw_l1.empty:
                df_sw_l1['classifier'] = 'SW2021'
                df_sw_l1['level'] = 'L1'
                df_sw_l1 = df_sw_l1.rename(columns={'code': 'industry_code', 'name': 'industry_name'})
                
                success_count = db.upsert_dataframe(
                    df_sw_l1[['ts_code', 'classifier', 'level', 'industry_code', 'industry_name']],
                    't_industry_classification',
                    unique_cols=['ts_code', 'classifier', 'level', 'industry_code']
                )
                logger.info(f"✅ 申万一级行业插入: {success_count} 条")
                return  # 成功获取到数据就返回
                
            # 如果申万接口返回空数据，使用备用方案
            logger.warning("⚠️ 申万行业分类返回空数据，使用备用方案")
            self._use_basic_industry_as_fallback()
                
        except Exception as e:
            logger.error(f"❌ 获取行业分类信息失败: {e}")
            # 使用备用方案
            self._use_basic_industry_as_fallback()
            
    def _use_basic_industry_as_fallback(self):
        """使用股票基本信息中的行业作为备用分类方案"""
        logger.info("🔄 使用基本信息中的行业数据作为备用方案...")
        
        try:
            from sqlalchemy import text
            import pandas as pd
            
            # 从stock_basic表提取行业信息
            with db.engine.connect() as conn:
                sql = """
                SELECT ts_code, industry as industry_name, 
                       CONCAT('IND_', SUBSTRING(MD5(industry), 1, 8)) as industry_code
                FROM stock_basic 
                WHERE industry IS NOT NULL AND industry != ''
                """
                result = conn.execute(text(sql))
                rows = result.fetchall()
                
            if rows:
                # 构建DataFrame，包含所有必需字段
                df_industry = pd.DataFrame(rows, columns=['ts_code', 'industry_name', 'industry_code'])
                df_industry['classifier'] = 'BASIC'
                df_industry['level'] = 'L1'
                df_industry['src'] = 'stock_basic'  # 添加数据源字段
                df_industry['start_date'] = None  # 可选字段设为None
                df_industry['end_date'] = None
                df_industry['is_new'] = 'Y'
                
                success_count = db.upsert_dataframe(
                    df_industry, 't_industry_classification',
                    unique_cols=['ts_code', 'classifier', 'level', 'industry_code']
                )
                logger.info(f"✅ 基本行业分类插入: {success_count} 条")
            else:
                logger.warning("⚠️ 基本信息中也无行业数据")
                
        except Exception as e:
            logger.error(f"❌ 备用行业分类方案失败: {e}")
            import traceback
            logger.error(traceback.format_exc())
            
    def _fetch_index_components(self):
        """获取指数成分股"""
        logger.info("📈 获取指数成分股信息...")
        
        try:
            # 暂时跳过指数成分股，因为可能需要更高权限
            logger.info("⏭️ 暂时跳过指数成分股获取（需要更高API权限）")
            return
                
        except Exception as e:
            logger.error(f"❌ 获取指数成分股失败: {e}")
            
    def _fetch_share_structure(self):
        """获取股本结构"""
        logger.info("📈 获取股本结构信息...")
        
        try:
            stock_codes = self._get_stock_codes()
            
            for i in tqdm(range(0, len(stock_codes), config.TS_BATCH_SIZE), 
                         desc="股本结构"):
                batch_codes = stock_codes[i:i+config.TS_BATCH_SIZE]
                
                for ts_code in batch_codes:
                    try:
                        df = self.pro.share_float(
                            ts_code=ts_code,
                            fields='ts_code,ann_date,float_date,float_share,float_ratio,holder_num'
                        )
                        
                        if not df.empty:
                            # 数据清洗：确保ann_date不为空
                            df['ann_date'] = pd.to_datetime(df['ann_date'], errors='coerce')
                            df['float_date'] = pd.to_datetime(df['float_date'], errors='coerce')
                            
                            # 过滤掉ann_date为空的记录
                            df = df.dropna(subset=['ann_date'])
                            
                            if not df.empty:  # 确保过滤后还有数据
                                db.upsert_dataframe(
                                    df, 't_share_structure',
                                    unique_cols=['ts_code', 'ann_date']
                                )
                            else:
                                logger.warning(f"{ts_code}股本结构数据ann_date全为空，跳过")
                            
                    except Exception as e:
                        logger.warning(f"获取{ts_code}股本结构失败: {e}")
                        
                    time.sleep(self.rate_limit_delay)
                    
        except Exception as e:
            logger.error(f"❌ 获取股本结构失败: {e}")
            
    def _fetch_company_managers(self):
        """获取公司高管信息"""
        logger.info("👥 获取公司高管信息...")
        
        try:
            # 暂时跳过公司高管信息，因为可能需要更高权限
            logger.info("⏭️ 暂时跳过公司高管信息获取（需要更高API权限）")
            return
                    
        except Exception as e:
            logger.error(f"❌ 获取公司高管信息失败: {e}")
            
    def fetch_financial_data(self, mode='incremental'):
        """获取财务类信息数据"""
        logger.info("💼 开始获取财务类信息数据...")
        
        try:
            # 1. 资产负债表
            self._fetch_balance_sheet()
            
            # 2. 利润表
            self._fetch_income_statement()
            
            # 3. 现金流量表
            self._fetch_cashflow_statement()
            
            # 4. 财务指标
            self._fetch_financial_indicators()
            
            # 5. 业绩预告
            self._fetch_performance_forecast()
            
            # 6. 业绩快报
            self._fetch_performance_express()
            
            return "财务类信息数据获取完成"
            
        except Exception as e:
            logger.error(f"❌ 财务类信息数据获取失败: {e}")
            raise
            
    def _fetch_balance_sheet(self):
        """获取资产负债表"""
        logger.info("💰 获取资产负债表...")
        
        try:
            # 获取最近8个季度的资产负债表
            for i in range(8):
                period = self._get_period_by_offset(i)
                logger.info(f"📊 获取{period}期资产负债表...")
                
                df = self.pro.balancesheet(
                    period=period,
                    fields='ts_code,ann_date,end_date,total_share,cap_rese,undistr_porfit,'
                           'surplus_rese,special_rese,money_cap,trad_asset,notes_receiv,'
                           'accounts_receiv,oth_receiv,prepayment,div_receiv,int_receiv,'
                           'inventories,amor_exp,nca_within_1y,sett_rsrv,loanto_oth_bank_fi'
                )
                
                if not df.empty:
                    # 使用通用数据清洗方法
                    df_clean, has_data = self._clean_ann_date_data(df, ['ann_date'])
                    
                    if has_data:
                        success_count = db.upsert_dataframe(
                            df_clean, 't_balance_sheet',
                            unique_cols=['ts_code', 'ann_date', 'end_date']
                        )
                        
                        logger.info(f"✅ {period}资产负债表插入: {success_count} 条")
                    else:
                        logger.warning(f"{period}资产负债表数据无效，跳过")
                        
                time.sleep(self.rate_limit_delay)
                
        except Exception as e:
            logger.error(f"❌ 获取资产负债表失败: {e}")
            
    def _fetch_income_statement(self):
        """获取利润表"""
        logger.info("📈 获取利润表...")
        
        try:
            # 获取最近8个季度的利润表
            for i in range(8):
                period = self._get_period_by_offset(i)
                logger.info(f"📊 获取{period}期利润表...")
                
                df = self.pro.income(
                    period=period,
                    fields='ts_code,ann_date,end_date,basic_eps,total_revenue,revenue,'
                           'int_income,prem_earned,comm_income,n_commis_income,n_oth_income,'
                           'n_oth_b_income,prem_income,out_prem,une_prem_reser,reins_income'
                )
                
                if not df.empty:
                    # 使用通用数据清洗方法
                    df_clean, has_data = self._clean_ann_date_data(df, ['ann_date'])
                    
                    if has_data:
                        success_count = db.upsert_dataframe(
                            df_clean, 't_income_statement',
                            unique_cols=['ts_code', 'ann_date', 'end_date']
                        )
                        
                        logger.info(f"✅ {period}利润表插入: {success_count} 条")
                    else:
                        logger.warning(f"{period}利润表数据无效，跳过")
                        
                time.sleep(self.rate_limit_delay)
                
        except Exception as e:
            logger.error(f"❌ 获取利润表失败: {e}")
            
    def _fetch_cashflow_statement(self):
        """获取现金流量表"""
        logger.info("💧 获取现金流量表...")
        
        try:
            # 获取最近8个季度的现金流量表
            for i in range(8):
                period = self._get_period_by_offset(i)
                logger.info(f"📊 获取{period}期现金流量表...")
                
                df = self.pro.cashflow(
                    period=period,
                    fields='ts_code,ann_date,end_date,net_profit,finan_exp,c_fr_sale_sg,'
                          'c_paid_goods_s,c_paid_to_for_empl,c_paid_for_taxes,n_cashflow_act,'
                          'c_paid_invest,n_cashflow_inv_act,c_recp_borrow,free_cashflow,'
                          'n_cash_flows_fnc_act,n_incr_cash_cash_equ'
                )
                
                if not df.empty:
                    # 使用通用数据清洗方法
                    df_clean, has_data = self._clean_ann_date_data(df, ['ann_date'])
                    
                    if has_data:
                        success_count = db.upsert_dataframe(
                            df_clean, 't_cashflow_statement',
                            unique_cols=['ts_code', 'ann_date', 'end_date']
                        )
                        
                        logger.info(f"✅ {period}现金流量表插入: {success_count} 条")
                    else:
                        logger.warning(f"{period}现金流量表数据无效，跳过")
                    
                time.sleep(self.rate_limit_delay)
                
        except Exception as e:
            logger.error(f"❌ 获取现金流量表失败: {e}")
            import traceback
            logger.error(traceback.format_exc())
            
    def _fetch_financial_indicators(self):
        """获取财务指标"""
        logger.info("📈 获取财务指标...")
        
        try:
            periods = self._get_recent_periods(8)
            
            for period in periods:
                logger.info(f"获取{period}财务指标...")
                
                df = self.pro.fina_indicator(
                    period=period,
                    fields='ts_code,ann_date,end_date,eps,dt_eps,revenue_ps,bps,ocfps,'
                          'netprofit_margin,grossprofit_margin,current_ratio,quick_ratio,'
                          'ar_turn,ca_turn,fa_turn,assets_turn,roe,roe_waa,roa,roic,'
                          'debt_to_assets,assets_to_eqt,debt_to_eqt,current_exint,'
                          'noncurrent_exint,interestdebt,netdebt'
                )
                
                if not df.empty:
                    df['ann_date'] = pd.to_datetime(df['ann_date'], errors='coerce')
                    df['end_date'] = pd.to_datetime(df['end_date'], errors='coerce')
                    
                    success_count = db.upsert_dataframe(
                        df, 't_financial_indicators',
                        unique_cols=['ts_code', 'end_date']
                    )
                    
                    logger.info(f"✅ {period}财务指标插入: {success_count} 条")
                    
                time.sleep(self.rate_limit_delay)
                
        except Exception as e:
            logger.error(f"❌ 获取财务指标失败: {e}")
            
    def _fetch_performance_forecast(self):
        """获取业绩预告"""
        logger.info("📢 获取业绩预告...")
        
        try:
            # 获取最近两年的业绩预告
            start_date = (datetime.now() - timedelta(days=730)).strftime('%Y%m%d')
            end_date = datetime.now().strftime('%Y%m%d')
            
            df = self.pro.forecast(
                start_date=start_date,
                end_date=end_date,
                fields='ts_code,ann_date,end_date,type,p_change_min,p_change_max,'
                      'net_profit_min,net_profit_max,last_parent_net,first_ann_date,summary'
            )
            
            if not df.empty:
                df['ann_date'] = pd.to_datetime(df['ann_date'], errors='coerce')
                df['end_date'] = pd.to_datetime(df['end_date'], errors='coerce')
                df['first_ann_date'] = pd.to_datetime(df['first_ann_date'], errors='coerce')
                
                success_count = db.upsert_dataframe(
                    df, 't_performance_forecast',
                    unique_cols=['ts_code', 'ann_date', 'end_date']
                )
                
                logger.info(f"✅ 业绩预告插入: {success_count} 条")
                
        except Exception as e:
            logger.error(f"❌ 获取业绩预告失败: {e}")
            
    def _fetch_performance_express(self):
        """获取业绩快报"""
        logger.info("⚡ 获取业绩快报...")
        
        try:
            periods = self._get_recent_periods(4)  # 最近4个季度
            
            for period in periods:
                logger.info(f"获取{period}业绩快报...")
                
                df = self.pro.express(
                    period=period,
                    fields='ts_code,ann_date,end_date,revenue,operate_profit,total_profit,'
                          'n_income,total_assets,total_hldr_eqy_exc_min_int,diluted_eps,'
                          'diluted_roe,yoy_net_profit,bps,yoy_sales,yoy_op'
                )
                
                if not df.empty:
                    df['ann_date'] = pd.to_datetime(df['ann_date'], errors='coerce')
                    df['end_date'] = pd.to_datetime(df['end_date'], errors='coerce')
                    
                    success_count = db.upsert_dataframe(
                        df, 't_performance_express',
                        unique_cols=['ts_code', 'ann_date', 'end_date']
                    )
                    
                    logger.info(f"✅ {period}业绩快报插入: {success_count} 条")
                    
                time.sleep(self.rate_limit_delay)
                
        except Exception as e:
            logger.error(f"❌ 获取业绩快报失败: {e}")
            
    def fetch_money_flow_data(self, mode='incremental'):
        """获取资金流向数据"""
        logger.info("💸 开始获取资金流向数据...")
        
        try:
            # 1. 个股资金流向
            self._fetch_money_flow()
            
            # 2. 龙虎榜数据
            self._fetch_dragon_tiger_list()
            
            return "资金流向数据获取完成"
            
        except Exception as e:
            logger.error(f"❌ 资金流向数据获取失败: {e}")
            raise
            
    def _fetch_money_flow(self):
        """获取个股资金流向"""
        logger.info("🔄 获取个股资金流向...")
        
        try:
            # 获取最近30个交易日的资金流向
            start_date = (datetime.now() - timedelta(days=60)).strftime('%Y%m%d')
            end_date = datetime.now().strftime('%Y%m%d')
            
            stock_codes = self._get_active_stock_codes(500)  # 获取活跃股票
            
            for i in tqdm(range(0, len(stock_codes), config.TS_BATCH_SIZE), 
                         desc="个股资金流向"):
                batch_codes = stock_codes[i:i+config.TS_BATCH_SIZE]
                
                for ts_code in batch_codes:
                    try:
                        df = self.pro.moneyflow(
                            ts_code=ts_code,
                            start_date=start_date,
                            end_date=end_date,
                            fields='ts_code,trade_date,buy_sm_vol,buy_sm_amount,sell_sm_vol,'
                                  'sell_sm_amount,buy_md_vol,buy_md_amount,sell_md_vol,'
                                  'sell_md_amount,buy_lg_vol,buy_lg_amount,sell_lg_vol,'
                                  'sell_lg_amount,buy_elg_vol,buy_elg_amount,sell_elg_vol,'
                                  'sell_elg_amount,net_mf_vol,net_mf_amount'
                        )
                        
                        if not df.empty:
                            df['trade_date'] = pd.to_datetime(df['trade_date'], errors='coerce')
                            
                            db.upsert_dataframe(
                                df, 't_money_flow',
                                unique_cols=['ts_code', 'trade_date']
                            )
                            
                    except Exception as e:
                        logger.warning(f"获取{ts_code}资金流向失败: {e}")
                        
                    time.sleep(self.rate_limit_delay)
                    
        except Exception as e:
            logger.error(f"❌ 获取个股资金流向失败: {e}")
            
    def _fetch_dragon_tiger_list(self):
        """获取龙虎榜数据"""
        logger.info("🐉 获取龙虎榜数据...")
        
        try:
            # 获取最近30个交易日的龙虎榜
            start_date = (datetime.now() - timedelta(days=60)).strftime('%Y%m%d')
            end_date = datetime.now().strftime('%Y%m%d')
            
            df = self.pro.top_list(
                start_date=start_date,
                end_date=end_date,
                fields='trade_date,ts_code,name,close,pct_chg,turnover_rate,amount,'
                      'l_sell,l_buy,l_amount,net_amount,net_rate,amount_rate,reason'
            )
            
            if not df.empty:
                df['trade_date'] = pd.to_datetime(df['trade_date'], errors='coerce')
                
                success_count = db.upsert_dataframe(
                    df, 't_dragon_tiger_list',
                    unique_cols=['trade_date', 'ts_code']
                )
                
                logger.info(f"✅ 龙虎榜数据插入: {success_count} 条")
                
        except Exception as e:
            logger.error(f"❌ 获取龙虎榜数据失败: {e}")
            
    def fetch_shareholder_data(self, mode='incremental'):
        """获取股东及股权变动数据"""
        logger.info("👥 开始获取股东及股权变动数据...")
        
        try:
            # 1. 十大股东
            self._fetch_top10_holders()
            
            # 2. 十大流通股东
            self._fetch_top10_float_holders()
            
            # 3. 股东户数
            self._fetch_holder_number()
            
            # 4. 限售解禁
            self._fetch_share_float()
            
            return "股东及股权变动数据获取完成"
            
        except Exception as e:
            logger.error(f"❌ 股东及股权变动数据获取失败: {e}")
            raise
            
    def _fetch_top10_holders(self):
        """获取十大股东"""
        logger.info("🔟 获取十大股东...")
        
        try:
            periods = self._get_recent_periods(4)  # 最近4个季度
            
            for period in periods:
                logger.info(f"获取{period}十大股东...")
                
                df = self.pro.top10_holders(
                    period=period,
                    fields='ts_code,ann_date,end_date,holder_name,hold_amount,hold_ratio'
                )
                
                if not df.empty:
                    df['ann_date'] = pd.to_datetime(df['ann_date'], errors='coerce')
                    df['end_date'] = pd.to_datetime(df['end_date'], errors='coerce')
                    
                    success_count = db.upsert_dataframe(
                        df, 't_top10_holders',
                        unique_cols=['ts_code', 'end_date', 'holder_name']
                    )
                    
                    logger.info(f"✅ {period}十大股东插入: {success_count} 条")
                    
                time.sleep(self.rate_limit_delay)
                
        except Exception as e:
            logger.error(f"❌ 获取十大股东失败: {e}")
            
    def _fetch_top10_float_holders(self):
        """获取十大流通股东"""
        logger.info("🔄 获取十大流通股东...")
        
        try:
            periods = self._get_recent_periods(4)
            
            for period in periods:
                logger.info(f"获取{period}十大流通股东...")
                
                df = self.pro.top10_floatholders(
                    period=period,
                    fields='ts_code,ann_date,end_date,holder_name,hold_amount,hold_ratio'
                )
                
                if not df.empty:
                    df['ann_date'] = pd.to_datetime(df['ann_date'], errors='coerce')
                    df['end_date'] = pd.to_datetime(df['end_date'], errors='coerce')
                    
                    success_count = db.upsert_dataframe(
                        df, 't_top10_float_holders',
                        unique_cols=['ts_code', 'end_date', 'holder_name']
                    )
                    
                    logger.info(f"✅ {period}十大流通股东插入: {success_count} 条")
                    
                time.sleep(self.rate_limit_delay)
                
        except Exception as e:
            logger.error(f"❌ 获取十大流通股东失败: {e}")
            
    def _fetch_holder_number(self):
        """获取股东户数"""
        logger.info("🏠 获取股东户数...")
        
        try:
            # 获取最近两年的股东户数
            start_date = (datetime.now() - timedelta(days=730)).strftime('%Y%m%d')
            end_date = datetime.now().strftime('%Y%m%d')
            
            stock_codes = self._get_stock_codes()
            
            for i in tqdm(range(0, len(stock_codes), config.TS_BATCH_SIZE), 
                         desc="股东户数"):
                batch_codes = stock_codes[i:i+config.TS_BATCH_SIZE]
                
                for ts_code in batch_codes:
                    try:
                        df = self.pro.stk_holdernumber(
                            ts_code=ts_code,
                            start_date=start_date,
                            end_date=end_date,
                            fields='ts_code,ann_date,end_date,holder_num'
                        )
                        
                        if not df.empty:
                            df['ann_date'] = pd.to_datetime(df['ann_date'], errors='coerce')
                            df['end_date'] = pd.to_datetime(df['end_date'], errors='coerce')
                            
                            db.upsert_dataframe(
                                df, 't_holder_number',
                                unique_cols=['ts_code', 'end_date']
                            )
                            
                    except Exception as e:
                        logger.warning(f"获取{ts_code}股东户数失败: {e}")
                        
                    time.sleep(self.rate_limit_delay)
                    
        except Exception as e:
            logger.error(f"❌ 获取股东户数失败: {e}")
            
    def _fetch_share_float(self):
        """获取限售解禁"""
        logger.info("🔓 获取限售解禁...")
        
        try:
            # 获取未来一年的解禁数据
            start_date = datetime.now().strftime('%Y%m%d')
            end_date = (datetime.now() + timedelta(days=365)).strftime('%Y%m%d')
            
            df = self.pro.share_float(
                start_date=start_date,
                end_date=end_date,
                fields='ts_code,ann_date,float_date,float_share,float_ratio,holder_name,share_type'
            )
            
            if not df.empty:
                # 数据清洗：确保ann_date不为空
                df['ann_date'] = pd.to_datetime(df['ann_date'], errors='coerce')
                df['float_date'] = pd.to_datetime(df['float_date'], errors='coerce')
                
                # 过滤掉ann_date为空的记录
                df = df.dropna(subset=['ann_date'])
                
                if not df.empty:  # 确保过滤后还有数据
                    success_count = db.upsert_dataframe(
                        df, 't_share_float',
                        unique_cols=['ts_code', 'ann_date']
                    )
                    
                    logger.info(f"✅ 限售解禁插入: {success_count} 条")
                else:
                    logger.warning("限售解禁数据ann_date全为空，跳过")
                    
        except Exception as e:
            logger.error(f"❌ 获取限售解禁失败: {e}")
            
    def fetch_announcement_data(self, mode='incremental'):
        """获取公告类信息数据"""
        logger.info("📢 开始获取公告类信息数据...")
        
        try:
            # 1. 分红送股
            self._fetch_dividend()
            
            # 2. 股票回购
            self._fetch_repurchase()
            
            return "公告类信息数据获取完成"
            
        except Exception as e:
            logger.error(f"❌ 公告类信息数据获取失败: {e}")
            raise
            
    def _fetch_dividend(self):
        """获取分红送股"""
        logger.info("💰 获取分红送股...")
        
        try:
            # 获取最近三年的分红数据
            start_date = (datetime.now() - timedelta(days=1095)).strftime('%Y%m%d')
            end_date = datetime.now().strftime('%Y%m%d')
            
            df = self.pro.dividend(
                ann_date=start_date,
                end_date=end_date,
                fields='ts_code,end_date,ann_date,div_proc,stk_div,stk_bo_rate,'
                      'stk_co_rate,cash_div,cash_div_tax,record_date,ex_date,pay_date'
            )
            
            if not df.empty:
                df['end_date'] = pd.to_datetime(df['end_date'], errors='coerce')
                df['ann_date'] = pd.to_datetime(df['ann_date'], errors='coerce')
                df['record_date'] = pd.to_datetime(df['record_date'], errors='coerce')
                df['ex_date'] = pd.to_datetime(df['ex_date'], errors='coerce')
                df['pay_date'] = pd.to_datetime(df['pay_date'], errors='coerce')
                
                success_count = db.upsert_dataframe(
                    df, 't_dividend',
                    unique_cols=['ts_code', 'end_date', 'ann_date']
                )
                
                logger.info(f"✅ 分红送股插入: {success_count} 条")
                
        except Exception as e:
            logger.error(f"❌ 获取分红送股失败: {e}")
            
    def _fetch_repurchase(self):
        """获取股票回购"""
        logger.info("🔄 获取股票回购...")
        
        try:
            # 获取最近两年的回购数据
            start_date = (datetime.now() - timedelta(days=730)).strftime('%Y%m%d')
            end_date = datetime.now().strftime('%Y%m%d')
            
            df = self.pro.repurchase(
                ann_date=start_date,
                end_date=end_date,
                fields='ts_code,ann_date,end_date,proc,exp_date,vol,amount,high_limit,low_limit'
            )
            
            if not df.empty:
                df['ann_date'] = pd.to_datetime(df['ann_date'], errors='coerce')
                df['end_date'] = pd.to_datetime(df['end_date'], errors='coerce')
                df['exp_date'] = pd.to_datetime(df['exp_date'], errors='coerce')
                
                success_count = db.upsert_dataframe(
                    df, 't_repurchase',
                    unique_cols=['ts_code', 'ann_date']
                )
                
                logger.info(f"✅ 股票回购插入: {success_count} 条")
                
        except Exception as e:
            logger.error(f"❌ 获取股票回购失败: {e}")
            
    def fetch_market_extension_data(self, mode='incremental'):
        """获取行情扩展数据"""
        logger.info("📊 开始获取行情扩展数据...")
        
        try:
            # 1. 停复牌
            self._fetch_suspend()
            
            # 2. 涨跌停价格
            self._fetch_limit_price()
            
            # 3. 概念分类
            self._fetch_concept()
            
            return "行情扩展数据获取完成"
            
        except Exception as e:
            logger.error(f"❌ 行情扩展数据获取失败: {e}")
            raise
            
    def _fetch_suspend(self):
        """获取停复牌"""
        logger.info("⏸️ 获取停复牌...")
        
        try:
            # 获取最近一年的停复牌数据
            start_date = (datetime.now() - timedelta(days=365)).strftime('%Y%m%d')
            end_date = datetime.now().strftime('%Y%m%d')
            
            df = self.pro.suspend_d(
                start_date=start_date,
                end_date=end_date,
                fields='ts_code,suspend_date,resume_date,ann_date,suspend_reason,reason_type'
            )
            
            if not df.empty:
                df['suspend_date'] = pd.to_datetime(df['suspend_date'], errors='coerce')
                df['resume_date'] = pd.to_datetime(df['resume_date'], errors='coerce')
                df['ann_date'] = pd.to_datetime(df['ann_date'], errors='coerce')
                
                success_count = db.upsert_dataframe(
                    df, 't_suspend',
                    unique_cols=['ts_code', 'suspend_date']
                )
                
                logger.info(f"✅ 停复牌插入: {success_count} 条")
                
        except Exception as e:
            logger.error(f"❌ 获取停复牌失败: {e}")
            
    def _fetch_limit_price(self):
        """获取涨跌停价格"""
        logger.info("🔺 获取涨跌停价格...")
        
        try:
            # 获取最近30个交易日的涨跌停价格
            start_date = (datetime.now() - timedelta(days=60)).strftime('%Y%m%d')
            end_date = datetime.now().strftime('%Y%m%d')
            
            stock_codes = self._get_active_stock_codes(200)  # 获取活跃股票
            
            for i in tqdm(range(0, len(stock_codes), config.TS_BATCH_SIZE), 
                         desc="涨跌停价格"):
                batch_codes = stock_codes[i:i+config.TS_BATCH_SIZE]
                
                for ts_code in batch_codes:
                    try:
                        df = self.pro.stk_limit(
                            ts_code=ts_code,
                            start_date=start_date,
                            end_date=end_date,
                            fields='ts_code,trade_date,up_limit,down_limit'
                        )
                        
                        if not df.empty:
                            df['trade_date'] = pd.to_datetime(df['trade_date'], errors='coerce')
                            
                            db.upsert_dataframe(
                                df, 't_limit_price',
                                unique_cols=['ts_code', 'trade_date']
                            )
                            
                    except Exception as e:
                        logger.warning(f"获取{ts_code}涨跌停价格失败: {e}")
                        
                    time.sleep(self.rate_limit_delay)
                    
        except Exception as e:
            logger.error(f"❌ 获取涨跌停价格失败: {e}")
            
    def _fetch_concept(self):
        """获取概念分类"""
        logger.info("💡 获取概念分类...")
        
        try:
            # 1. 获取概念分类列表
            concept_df = self.pro.concept(
                fields='code,name,src'
            )
            
            if not concept_df.empty:
                db.upsert_dataframe(
                    concept_df, 't_concept',
                    unique_cols=['code']
                )
                
                logger.info(f"✅ 概念分类插入: {len(concept_df)} 条")
                
            time.sleep(self.rate_limit_delay)
            
            # 2. 获取概念股明细
            for _, concept in concept_df.iterrows():
                try:
                    detail_df = self.pro.concept_detail(
                        id=concept['code'],
                        fields='id,concept_name,ts_code,name,in_date,out_date'
                    )
                    
                    if not detail_df.empty:
                        detail_df['concept_code'] = concept['code']
                        detail_df['in_date'] = pd.to_datetime(detail_df['in_date'], errors='coerce')
                        detail_df['out_date'] = pd.to_datetime(detail_df['out_date'], errors='coerce')
                        
                        db.upsert_dataframe(
                            detail_df, 't_concept_detail',
                            unique_cols=['concept_code', 'ts_code']
                        )
                        
                except Exception as e:
                    logger.warning(f"获取概念{concept['code']}明细失败: {e}")
                    
                time.sleep(self.rate_limit_delay)
                
        except Exception as e:
            logger.error(f"❌ 获取概念分类失败: {e}")
            
    def fetch_macro_data(self, mode='incremental'):
        """获取宏观经济数据"""
        logger.info("🌍 开始获取宏观经济数据...")
        
        try:
            # 1. 沪深港通资金流向
            self._fetch_money_flow_hsgt()
            
            # 2. 融资融券明细
            self._fetch_margin_detail()
            
            # 3. 宏观经济指标
            self._fetch_macro_indicators()
            
            return "宏观经济数据获取完成"
            
        except Exception as e:
            logger.error(f"❌ 宏观经济数据获取失败: {e}")
            raise
            
    def _fetch_money_flow_hsgt(self):
        """获取沪深港通资金流向"""
        logger.info("🔄 获取沪深港通资金流向...")
        
        try:
            # 获取最近一年的沪深港通数据
            start_date = (datetime.now() - timedelta(days=365)).strftime('%Y%m%d')
            end_date = datetime.now().strftime('%Y%m%d')
            
            df = self.pro.moneyflow_hsgt(
                start_date=start_date,
                end_date=end_date,
                fields='trade_date,ggt_ss,ggt_sz,hgt,sgt,north_money,south_money'
            )
            
            if not df.empty:
                df['trade_date'] = pd.to_datetime(df['trade_date'], errors='coerce')
                
                success_count = db.upsert_dataframe(
                    df, 't_money_flow_hsgt',
                    unique_cols=['trade_date']
                )
                
                logger.info(f"✅ 沪深港通资金流向插入: {success_count} 条")
                
        except Exception as e:
            logger.error(f"❌ 获取沪深港通资金流向失败: {e}")
            
    def _fetch_margin_detail(self):
        """获取融资融券明细"""
        logger.info("📊 获取融资融券明细...")
        
        try:
            # 获取最近一年的融资融券数据
            start_date = (datetime.now() - timedelta(days=365)).strftime('%Y%m%d')
            end_date = datetime.now().strftime('%Y%m%d')
            
            df = self.pro.margin_detail(
                start_date=start_date,
                end_date=end_date,
                fields='trade_date,ts_code,rzye,rqye,rzmre,rqmcl,rzche,rqchl,rqyl,rzrqye'
            )
            
            if not df.empty:
                df['trade_date'] = pd.to_datetime(df['trade_date'], errors='coerce')
                
                success_count = db.upsert_dataframe(
                    df, 't_margin_detail',
                    unique_cols=['trade_date', 'ts_code']
                )
                
                logger.info(f"✅ 融资融券明细插入: {success_count} 条")
                
        except Exception as e:
            logger.error(f"❌ 获取融资融券明细失败: {e}")
            
    def _fetch_macro_indicators(self):
        """获取宏观经济指标"""
        logger.info("📈 获取宏观经济指标...")
        
        try:
            # 宏观指标列表
            macro_indicators = [
                ('shibor', 'SHIBOR利率'),
                ('libor', 'LIBOR利率'),
                ('hibor', 'HIBOR利率'),
                ('cpi', 'CPI'),
                ('ppi', 'PPI'),
                ('gdp', 'GDP'),
                ('us_tltyield', '美国国债收益率'),
                ('cn_gdp', '中国GDP'),
                ('cn_ppi', '中国PPI'),
                ('cn_cpi', '中国CPI'),
                ('cn_m', '中国货币供应量'),
            ]
            
            for indicator_code, indicator_name in macro_indicators:
                try:
                    # 获取最近两年的宏观数据
                    start_m = (datetime.now() - timedelta(days=730)).strftime('%Y%m')
                    end_m = datetime.now().strftime('%Y%m')
                    
                    # 动态调用不同的宏观指标接口
                    if hasattr(self.pro, indicator_code):
                        func = getattr(self.pro, indicator_code)
                        df = func(start_m=start_m, end_m=end_m)
                        
                        if not df.empty:
                            # 标准化数据格式
                            df['indicator_name'] = indicator_name
                            df['indicator_code'] = indicator_code
                            
                            # 获取数值列
                            value_cols = [col for col in df.columns 
                                        if col not in ['m', 'indicator_name', 'indicator_code']]
                            
                            # 转换为长格式
                            long_df = pd.melt(df, 
                                            id_vars=['m', 'indicator_name', 'indicator_code'],
                                            value_vars=value_cols,
                                            var_name='sub_indicator',
                                            value_name='value')
                            
                            long_df['m'] = pd.to_datetime(long_df['m'], format='%Y%m', errors='coerce')
                            
                            db.upsert_dataframe(
                                long_df, 't_macro_indicators',
                                unique_cols=['m', 'indicator_code']
                            )
                            
                            logger.info(f"✅ {indicator_name}插入: {len(long_df)} 条")
                            
                except Exception as e:
                    logger.warning(f"获取{indicator_name}失败: {e}")
                    
                time.sleep(self.rate_limit_delay)
                
        except Exception as e:
            logger.error(f"❌ 获取宏观经济指标失败: {e}")
            
    # ===================辅助方法===================
    
    def _get_stock_codes(self) -> List[str]:
        """获取所有股票代码"""
        try:
            df = self.pro.stock_basic(list_status='L', fields='ts_code')
            return df['ts_code'].tolist()
        except Exception as e:
            logger.error(f"获取股票代码失败: {e}")
            return []
            
    def _get_active_stock_codes(self, limit: int = 500) -> List[str]:
        """获取活跃股票代码"""
        try:
            # 可以根据成交额、换手率等筛选活跃股票
            df = self.pro.stock_basic(list_status='L', fields='ts_code')
            return df['ts_code'].head(limit).tolist()
        except Exception as e:
            logger.error(f"获取活跃股票代码失败: {e}")
            return []
            
    def _get_recent_periods(self, count: int = 8) -> List[str]:
        """获取最近的报告期"""
        periods = []
        current_date = datetime.now()
        
        for i in range(count):
            # 计算季度
            year = current_date.year
            month = current_date.month
            
            if month <= 3:
                quarter = 1
            elif month <= 6:
                quarter = 2
            elif month <= 9:
                quarter = 3
            else:
                quarter = 4
                
            # 向前推移季度
            quarter -= i
            while quarter <= 0:
                quarter += 4
                year -= 1
                
            # 转换为期末日期
            if quarter == 1:
                period_end = f"{year}0331"
            elif quarter == 2:
                period_end = f"{year}0630"
            elif quarter == 3:
                period_end = f"{year}0930"
            else:
                period_end = f"{year}1231"
                
            periods.append(period_end)
            
        return periods
        
    def _get_period_by_offset(self, offset=0):
        """根据偏移量获取报告期（0为最近一期）"""
        current_date = datetime.now()
        year = current_date.year
        month = current_date.month
        
        if month <= 3:
            quarter = 1
        elif month <= 6:
            quarter = 2
        elif month <= 9:
            quarter = 3
        else:
            quarter = 4
            
        # 向前推移季度
        quarter -= offset
        while quarter <= 0:
            quarter += 4
            year -= 1
            
        # 转换为期末日期
        if quarter == 1:
            return f"{year}0331"
        elif quarter == 2:
            return f"{year}0630"
        elif quarter == 3:
            return f"{year}0930"
        else:
            return f"{year}1231"
        
    def _get_stock_name(self, ts_code: str) -> str:
        """获取股票名称"""
        try:
            df = self.pro.stock_basic(ts_code=ts_code, fields='name')
            return df['name'].iloc[0] if not df.empty else ts_code
        except:
            return ts_code
            
    def _get_index_name(self, index_code: str) -> str:
        """获取指数名称"""
        index_names = {
            '000001.SH': '上证指数',
            '000300.SH': '沪深300',
            '000905.SH': '中证500',
            '399001.SZ': '深证成指',
            '399006.SZ': '创业板指',
            '000016.SH': '上证50',
        }
        return index_names.get(index_code, index_code)

def main():
    """主函数"""
    import argparse
    
    parser = argparse.ArgumentParser(description='A股全维度数据获取系统')
    parser.add_argument('--mode', choices=['full', 'incremental'], default='incremental',
                       help='获取模式: full=全量获取, incremental=增量更新')
    parser.add_argument('--category', choices=['basic', 'financial', 'money_flow', 
                                              'shareholder', 'announcement', 'market_ext', 'macro', 'all'],
                       default='all', help='数据类别')
    
    args = parser.parse_args()
    
    # 配置日志
    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s - %(levelname)s - %(message)s',
        handlers=[
            logging.StreamHandler(),
            logging.FileHandler('logs/comprehensive_data_fetch.log', encoding='utf-8')
        ]
    )
    
    fetcher = ComprehensiveDataFetcher()
    
    try:
        if args.category == 'all':
            fetcher.fetch_all_comprehensive_data(args.mode)
        elif args.category == 'basic':
            fetcher.fetch_basic_info_data(args.mode)
        elif args.category == 'financial':
            fetcher.fetch_financial_data(args.mode)
        elif args.category == 'money_flow':
            fetcher.fetch_money_flow_data(args.mode)
        elif args.category == 'shareholder':
            fetcher.fetch_shareholder_data(args.mode)
        elif args.category == 'announcement':
            fetcher.fetch_announcement_data(args.mode)
        elif args.category == 'market_ext':
            fetcher.fetch_market_extension_data(args.mode)
        elif args.category == 'macro':
            fetcher.fetch_macro_data(args.mode)
            
    except KeyboardInterrupt:
        logger.info("🛑 用户中断，程序退出")
    except Exception as e:
        logger.error(f"❌ 程序执行失败: {e}")

if __name__ == "__main__":
    main() 