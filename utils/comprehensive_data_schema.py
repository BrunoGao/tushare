#!/usr/bin/env python3
"""
A股全维度数据表结构设计
涵盖基本面、财务、资金流、股东、公告等多维度信息
"""
from sqlalchemy import text
import logging

logger = logging.getLogger(__name__)

class ComprehensiveDataSchema:
    """A股全维度数据表结构"""
    
    @staticmethod
    def create_all_tables(db_helper):
        """创建所有多维度数据表"""
        tables = [
            # 基本面信息表
            ComprehensiveDataSchema.create_stock_basic_table,
            ComprehensiveDataSchema.create_industry_classification_table,
            ComprehensiveDataSchema.create_index_components_table,
            ComprehensiveDataSchema.create_share_structure_table,
            ComprehensiveDataSchema.create_company_managers_table,
            
            # 财务类信息表
            ComprehensiveDataSchema.create_balance_sheet_table,
            ComprehensiveDataSchema.create_income_statement_table,
            ComprehensiveDataSchema.create_cashflow_statement_table,
            ComprehensiveDataSchema.create_financial_indicators_table,
            ComprehensiveDataSchema.create_performance_forecast_table,
            ComprehensiveDataSchema.create_performance_express_table,
            
            # 资金流向表
            ComprehensiveDataSchema.create_money_flow_table,
            ComprehensiveDataSchema.create_dragon_tiger_list_table,
            
            # 股东及股权变动表
            ComprehensiveDataSchema.create_top10_holders_table,
            ComprehensiveDataSchema.create_top10_float_holders_table,
            ComprehensiveDataSchema.create_holder_number_table,
            ComprehensiveDataSchema.create_share_float_table,
            
            # 公告类信息表
            ComprehensiveDataSchema.create_dividend_table,
            ComprehensiveDataSchema.create_repurchase_table,
            
            # 行情扩展表
            ComprehensiveDataSchema.create_suspend_table,
            ComprehensiveDataSchema.create_limit_price_table,
            ComprehensiveDataSchema.create_concept_table,
            ComprehensiveDataSchema.create_concept_detail_table,
            
            # 宏观经济表
            ComprehensiveDataSchema.create_money_flow_hsgt_table,
            ComprehensiveDataSchema.create_margin_detail_table,
            ComprehensiveDataSchema.create_macro_indicators_table,
        ]
        
        success_count = 0
        for create_func in tables:
            try:
                create_func(db_helper)
                success_count += 1
            except Exception as e:
                logger.error(f"创建表失败 {create_func.__name__}: {e}")
                
        logger.info(f"✅ 成功创建 {success_count}/{len(tables)} 个数据表")
        return success_count == len(tables)
    
    # ===================基本面信息表===================
    
    @staticmethod
    def create_stock_basic_table(db_helper):
        """股票基本信息表(增强版)"""
        sql = """
        CREATE TABLE IF NOT EXISTS t_stock_basic (
            ts_code VARCHAR(10) PRIMARY KEY COMMENT '股票代码',
            symbol VARCHAR(10) NOT NULL COMMENT '股票简称代码',
            name VARCHAR(50) NOT NULL COMMENT '股票名称',
            fullname VARCHAR(200) COMMENT '公司全称',
            enname VARCHAR(200) COMMENT '英文全称',
            area VARCHAR(20) COMMENT '所在地域',
            industry VARCHAR(50) COMMENT '所属行业',
            sector VARCHAR(50) COMMENT '所属板块',
            market VARCHAR(10) COMMENT '市场类型(主板/中小板/创业板)',
            exchange VARCHAR(10) COMMENT '交易所代码',
            curr_type VARCHAR(5) COMMENT '交易货币',
            list_status VARCHAR(2) DEFAULT 'L' COMMENT '上市状态',
            list_date DATE COMMENT '上市日期',
            delist_date DATE COMMENT '退市日期',
            is_hs VARCHAR(2) COMMENT '是否沪深港通标的',
            act_name VARCHAR(100) COMMENT '实控人名称',
            act_ent_type VARCHAR(20) COMMENT '实控人企业性质',
            business_scope TEXT COMMENT '经营范围',
            main_business TEXT COMMENT '主营业务',
            setup_date DATE COMMENT '成立日期',
            reg_capital DECIMAL(15,2) COMMENT '注册资本(万元)',
            office_addr VARCHAR(200) COMMENT '办公地址',
            employees INT COMMENT '员工总数',
            website VARCHAR(200) COMMENT '公司网址',
            email VARCHAR(100) COMMENT '电子邮箱',
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
            INDEX idx_industry (industry),
            INDEX idx_area (area),
            INDEX idx_list_status (list_status),
            INDEX idx_list_date (list_date)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='股票基本信息表(增强版)'
        """
        db_helper.execute_sql(sql)
        
    @staticmethod
    def create_industry_classification_table(db_helper):
        """行业分类表"""
        sql = """
        CREATE TABLE IF NOT EXISTS t_industry_classification (
            id BIGINT PRIMARY KEY AUTO_INCREMENT,
            ts_code VARCHAR(10) NOT NULL COMMENT '股票代码',
            classifier VARCHAR(20) NOT NULL COMMENT '分类标准(SW/ZJH/THS)',
            level VARCHAR(10) NOT NULL COMMENT '分类级别(L1/L2/L3)',
            industry_code VARCHAR(20) NOT NULL COMMENT '行业代码',
            industry_name VARCHAR(100) NOT NULL COMMENT '行业名称',
            src VARCHAR(20) COMMENT '数据来源',
            start_date DATE COMMENT '纳入日期',
            end_date DATE COMMENT '剔除日期',
            is_new VARCHAR(2) COMMENT '是否最新',
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            UNIQUE KEY uk_stock_classifier (ts_code, classifier, level, industry_code),
            INDEX idx_ts_code (ts_code),
            INDEX idx_classifier (classifier),
            INDEX idx_industry_code (industry_code)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='行业分类表'
        """
        db_helper.execute_sql(sql)
        
    @staticmethod
    def create_index_components_table(db_helper):
        """指数成分股表"""
        sql = """
        CREATE TABLE IF NOT EXISTS t_index_components (
            id BIGINT PRIMARY KEY AUTO_INCREMENT,
            index_code VARCHAR(10) NOT NULL COMMENT '指数代码',
            index_name VARCHAR(50) COMMENT '指数名称',
            con_code VARCHAR(10) NOT NULL COMMENT '成分股代码',
            con_name VARCHAR(50) COMMENT '成分股名称',
            trade_date DATE NOT NULL COMMENT '交易日期',
            weight DECIMAL(10,6) COMMENT '权重',
            in_date DATE COMMENT '纳入日期',
            out_date DATE COMMENT '剔除日期',
            is_new VARCHAR(2) COMMENT '是否最新',
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            UNIQUE KEY uk_index_stock_date (index_code, con_code, trade_date),
            INDEX idx_index_code (index_code),
            INDEX idx_con_code (con_code),
            INDEX idx_trade_date (trade_date)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='指数成分股表'
        """
        db_helper.execute_sql(sql)
        
    @staticmethod
    def create_share_structure_table(db_helper):
        """股本结构表"""
        sql = """
        CREATE TABLE IF NOT EXISTS t_share_structure (
            id BIGINT PRIMARY KEY AUTO_INCREMENT,
            ts_code VARCHAR(10) NOT NULL COMMENT '股票代码',
            ann_date DATE NOT NULL COMMENT '公告日期',
            float_date DATE COMMENT '流通日期',
            float_share DECIMAL(20,2) COMMENT '流通股本(万股)',
            float_ratio DECIMAL(8,4) COMMENT '流通股本占总股本比例',
            holder_num INT COMMENT '股东户数',
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            UNIQUE KEY uk_code_ann_date (ts_code, ann_date),
            INDEX idx_ts_code (ts_code),
            INDEX idx_float_date (float_date)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='股本结构表'
        """
        db_helper.execute_sql(sql)
        
    @staticmethod
    def create_company_managers_table(db_helper):
        """公司高管表"""
        sql = """
        CREATE TABLE IF NOT EXISTS t_company_managers (
            id BIGINT PRIMARY KEY AUTO_INCREMENT,
            ts_code VARCHAR(10) NOT NULL COMMENT '股票代码',
            ann_date DATE NOT NULL COMMENT '公告日期',
            name VARCHAR(50) NOT NULL COMMENT '姓名',
            gender VARCHAR(2) COMMENT '性别',
            lev VARCHAR(20) COMMENT '岗位类别',
            title VARCHAR(100) COMMENT '岗位',
            edu VARCHAR(20) COMMENT '学历',
            national VARCHAR(20) COMMENT '国籍',
            birthday DATE COMMENT '出生年月',
            begin_date DATE COMMENT '上任日期',
            end_date DATE COMMENT '离任日期',
            resume TEXT COMMENT '个人简历',
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            UNIQUE KEY uk_code_ann_name (ts_code, ann_date, name),
            INDEX idx_ts_code (ts_code),
            INDEX idx_ann_date (ann_date)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='公司高管表'
        """
        db_helper.execute_sql(sql)
        
    # ===================财务类信息表===================
    
    @staticmethod
    def create_balance_sheet_table(db_helper):
        """资产负债表"""
        sql = """
        CREATE TABLE IF NOT EXISTS t_balance_sheet (
            id BIGINT PRIMARY KEY AUTO_INCREMENT,
            ts_code VARCHAR(10) NOT NULL COMMENT '股票代码',
            ann_date DATE NOT NULL COMMENT '公告日期',
            f_ann_date DATE COMMENT '实际公告日期',
            end_date DATE NOT NULL COMMENT '报告期',
            report_type VARCHAR(10) COMMENT '报告类型',
            comp_type VARCHAR(10) COMMENT '公司类型',
            total_share DECIMAL(20,2) COMMENT '期末总股本',
            cap_rese DECIMAL(20,2) COMMENT '资本公积金',
            undistr_porfit DECIMAL(20,2) COMMENT '未分配利润',
            surplus_rese DECIMAL(20,2) COMMENT '盈余公积金',
            special_rese DECIMAL(20,2) COMMENT '专项储备',
            money_cap DECIMAL(20,2) COMMENT '货币资金',
            trad_asset DECIMAL(20,2) COMMENT '交易性金融资产',
            notes_receiv DECIMAL(20,2) COMMENT '应收票据',
            accounts_receiv DECIMAL(20,2) COMMENT '应收账款',
            oth_receiv DECIMAL(20,2) COMMENT '其他应收款',
            prepayment DECIMAL(20,2) COMMENT '预付款项',
            div_receiv DECIMAL(20,2) COMMENT '应收股利',
            int_receiv DECIMAL(20,2) COMMENT '应收利息',
            inventories DECIMAL(20,2) COMMENT '存货',
            amor_exp DECIMAL(20,2) COMMENT '长期待摊费用',
            nca_within_1y DECIMAL(20,2) COMMENT '一年内到期的非流动资产',
            sett_rsrv DECIMAL(20,2) COMMENT '结算备付金',
            loanto_oth_bank_fi DECIMAL(20,2) COMMENT '拆出资金',
            premium_receiv DECIMAL(20,2) COMMENT '应收保费',
            reinsur_receiv DECIMAL(20,2) COMMENT '应收分保账款',
            reinsur_res_receiv DECIMAL(20,2) COMMENT '应收分保合同准备金',
            pur_resale_fa DECIMAL(20,2) COMMENT '买入返售金融资产',
            oth_cur_assets DECIMAL(20,2) COMMENT '其他流动资产',
            total_cur_assets DECIMAL(20,2) COMMENT '流动资产合计',
            fa_avail_for_sale DECIMAL(20,2) COMMENT '可供出售金融资产',
            htm_invest DECIMAL(20,2) COMMENT '持有至到期投资',
            lt_eqt_invest DECIMAL(20,2) COMMENT '长期股权投资',
            invest_real_estate DECIMAL(20,2) COMMENT '投资性房地产',
            time_deposits DECIMAL(20,2) COMMENT '定期存款',
            oth_assets DECIMAL(20,2) COMMENT '其他资产',
            lt_rec DECIMAL(20,2) COMMENT '长期应收款',
            fix_assets DECIMAL(20,2) COMMENT '固定资产',
            cip DECIMAL(20,2) COMMENT '在建工程',
            const_materials DECIMAL(20,2) COMMENT '工程物资',
            fixed_assets_disp DECIMAL(20,2) COMMENT '固定资产清理',
            produc_bio_assets DECIMAL(20,2) COMMENT '生产性生物资产',
            oil_and_gas_assets DECIMAL(20,2) COMMENT '油气资产',
            intan_assets DECIMAL(20,2) COMMENT '无形资产',
            r_and_d DECIMAL(20,2) COMMENT '开发支出',
            goodwill DECIMAL(20,2) COMMENT '商誉',
            lt_amor_exp DECIMAL(20,2) COMMENT '长期待摊费用',
            defer_tax_assets DECIMAL(20,2) COMMENT '递延所得税资产',
            decr_in_disbur DECIMAL(20,2) COMMENT '发放贷款及垫款',
            oth_nca DECIMAL(20,2) COMMENT '其他非流动资产',
            total_nca DECIMAL(20,2) COMMENT '非流动资产合计',
            cash_reser_cb DECIMAL(20,2) COMMENT '现金及存放央行款项',
            depos_in_oth_bfi DECIMAL(20,2) COMMENT '存放同业和其它金融机构款项',
            prec_metals DECIMAL(20,2) COMMENT '贵金属',
            deriv_assets DECIMAL(20,2) COMMENT '衍生金融资产',
            rr_reins_une_prem DECIMAL(20,2) COMMENT '应收分保未到期责任准备金',
            rr_reins_outstd_cla DECIMAL(20,2) COMMENT '应收分保未决赔款准备金',
            rr_reins_lins_liab DECIMAL(20,2) COMMENT '应收分保寿险责任准备金',
            rr_reins_lthins_liab DECIMAL(20,2) COMMENT '应收分保长期健康险责任准备金',
            refund_depos DECIMAL(20,2) COMMENT '存出保证金',
            ph_pledge_loans DECIMAL(20,2) COMMENT '保户质押贷款',
            receiv_invest DECIMAL(20,2) COMMENT '应收款项类投资',
            receiv_cap_steels DECIMAL(20,2) COMMENT '应收保单红利',
            fa_avail_for_sale_assets DECIMAL(20,2) COMMENT '可供出售金融资产',
            other_debt_invest DECIMAL(20,2) COMMENT '其他债权投资',
            other_equity_invest DECIMAL(20,2) COMMENT '其他权益工具投资',
            other_illiq_fin_assets DECIMAL(20,2) COMMENT '其他非流动金融资产',
            total_assets DECIMAL(20,2) COMMENT '资产总计',
            lt_borr DECIMAL(20,2) COMMENT '长期借款',
            st_borr DECIMAL(20,2) COMMENT '短期借款',
            cb_borr DECIMAL(20,2) COMMENT '向央行借款',
            depos_ib_deposits DECIMAL(20,2) COMMENT '吸收存款及同业存放',
            loan_oth_bank DECIMAL(20,2) COMMENT '拆入资金',
            trading_fl DECIMAL(20,2) COMMENT '交易性金融负债',
            notes_payable DECIMAL(20,2) COMMENT '应付票据',
            acct_payable DECIMAL(20,2) COMMENT '应付账款',
            adv_receipts DECIMAL(20,2) COMMENT '预收款项',
            sold_for_repur_fa DECIMAL(20,2) COMMENT '卖出回购金融资产款',
            comm_payable DECIMAL(20,2) COMMENT '应付手续费及佣金',
            payroll_payable DECIMAL(20,2) COMMENT '应付职工薪酬',
            taxes_payable DECIMAL(20,2) COMMENT '应交税费',
            int_payable DECIMAL(20,2) COMMENT '应付利息',
            div_payable DECIMAL(20,2) COMMENT '应付股利',
            oth_payable DECIMAL(20,2) COMMENT '其他应付款',
            acc_exp DECIMAL(20,2) COMMENT '预提费用',
            deferred_inc DECIMAL(20,2) COMMENT '递延收益',
            st_bonds_payable DECIMAL(20,2) COMMENT '应付短期债券',
            payable_to_reinsurer DECIMAL(20,2) COMMENT '应付分保账款',
            rsrv_insur_cont DECIMAL(20,2) COMMENT '保险合同准备金',
            acting_trading_sec DECIMAL(20,2) COMMENT '代理买卖证券款',
            acting_uw_sec DECIMAL(20,2) COMMENT '代理承销证券款',
            non_cur_liab_due_1y DECIMAL(20,2) COMMENT '一年内到期的非流动负债',
            oth_cur_liab DECIMAL(20,2) COMMENT '其他流动负债',
            total_cur_liab DECIMAL(20,2) COMMENT '流动负债合计',
            bond_payable DECIMAL(20,2) COMMENT '应付债券',
            lt_payable DECIMAL(20,2) COMMENT '长期应付款',
            specific_payables DECIMAL(20,2) COMMENT '专项应付款',
            estimated_liab DECIMAL(20,2) COMMENT '预计负债',
            defer_tax_liab DECIMAL(20,2) COMMENT '递延所得税负债',
            defer_inc_non_cur_liab DECIMAL(20,2) COMMENT '递延收益-非流动负债',
            oth_ncl DECIMAL(20,2) COMMENT '其他非流动负债',
            total_ncl DECIMAL(20,2) COMMENT '非流动负债合计',
            depos_oth_bfi DECIMAL(20,2) COMMENT '同业和其它金融机构存放款项',
            deriv_liab DECIMAL(20,2) COMMENT '衍生金融负债',
            depos DECIMAL(20,2) COMMENT '吸收存款',
            agency_bus_liab DECIMAL(20,2) COMMENT '代理业务负债',
            oth_liab DECIMAL(20,2) COMMENT '其他负债',
            prem_receiv_adva DECIMAL(20,2) COMMENT '预收保费',
            depos_received DECIMAL(20,2) COMMENT '存入保证金',
            ph_invest DECIMAL(20,2) COMMENT '保户储金及投资款',
            reser_une_prem DECIMAL(20,2) COMMENT '未到期责任准备金',
            reser_outstd_claims DECIMAL(20,2) COMMENT '未决赔款准备金',
            reser_lins_liab DECIMAL(20,2) COMMENT '寿险责任准备金',
            reser_lthins_liab DECIMAL(20,2) COMMENT '长期健康险责任准备金',
            indept_acc_liab DECIMAL(20,2) COMMENT '独立账户负债',
            pledge_borr DECIMAL(20,2) COMMENT '其中:质押借款',
            indem_payable DECIMAL(20,2) COMMENT '应付赔付款',
            policy_div_payable DECIMAL(20,2) COMMENT '应付保单红利',
            total_liab DECIMAL(20,2) COMMENT '负债合计',
            treasury_share DECIMAL(20,2) COMMENT '减:库存股',
            ordin_risk_reser DECIMAL(20,2) COMMENT '一般风险准备',
            forex_differ DECIMAL(20,2) COMMENT '外币报表折算差额',
            invest_loss_unconf DECIMAL(20,2) COMMENT '未确认的投资损失',
            minority_int DECIMAL(20,2) COMMENT '少数股东权益',
            total_hldr_eqy_exc_min_int DECIMAL(20,2) COMMENT '股东权益合计(不含少数股东权益)',
            total_hldr_eqy_inc_min_int DECIMAL(20,2) COMMENT '股东权益合计(含少数股东权益)',
            total_liab_hldr_eqy DECIMAL(20,2) COMMENT '负债及股东权益总计',
            lt_payroll_payable DECIMAL(20,2) COMMENT '长期应付职工薪酬',
            oth_comp_income DECIMAL(20,2) COMMENT '其他综合收益',
            oth_eqt_tools DECIMAL(20,2) COMMENT '其他权益工具',
            oth_eqt_tools_p_shr DECIMAL(20,2) COMMENT '其他权益工具(优先股)',
            lending_funds DECIMAL(20,2) COMMENT '融出资金',
            acc_receivable DECIMAL(20,2) COMMENT '应收款项',
            st_fin_payable DECIMAL(20,2) COMMENT '应付短期融资款',
            payables DECIMAL(20,2) COMMENT '应付款项',
            hfs_assets DECIMAL(20,2) COMMENT '持有待售资产',
            hfs_sales DECIMAL(20,2) COMMENT '持有待售负债',
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            UNIQUE KEY uk_code_end_date (ts_code, end_date),
            INDEX idx_ts_code (ts_code),
            INDEX idx_end_date (end_date),
            INDEX idx_ann_date (ann_date)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='资产负债表'
        """
        db_helper.execute_sql(sql)
        
    @staticmethod  
    def create_income_statement_table(db_helper):
        """利润表"""
        sql = """
        CREATE TABLE IF NOT EXISTS t_income_statement (
            id BIGINT PRIMARY KEY AUTO_INCREMENT,
            ts_code VARCHAR(10) NOT NULL COMMENT '股票代码',
            ann_date DATE NOT NULL COMMENT '公告日期',
            f_ann_date DATE COMMENT '实际公告日期',
            end_date DATE NOT NULL COMMENT '报告期',
            report_type VARCHAR(10) COMMENT '报告类型',
            comp_type VARCHAR(10) COMMENT '公司类型',
            basic_eps DECIMAL(10,4) COMMENT '基本每股收益',
            diluted_eps DECIMAL(10,4) COMMENT '稀释每股收益',
            total_revenue DECIMAL(20,2) COMMENT '营业总收入',
            revenue DECIMAL(20,2) COMMENT '营业收入',
            int_income DECIMAL(20,2) COMMENT '利息收入',
            prem_earned DECIMAL(20,2) COMMENT '已赚保费',
            comm_income DECIMAL(20,2) COMMENT '手续费及佣金收入',
            n_commis_income DECIMAL(20,2) COMMENT '手续费及佣金净收入',
            n_oth_income DECIMAL(20,2) COMMENT '其他经营净收益',
            n_oth_b_income DECIMAL(20,2) COMMENT '加:其他业务净收益',
            prem_income DECIMAL(20,2) COMMENT '保险业务收入',
            out_prem DECIMAL(20,2) COMMENT '减:分出保费',
            une_prem_reser DECIMAL(20,2) COMMENT '提取未到期责任准备金',
            reins_income DECIMAL(20,2) COMMENT '其中:分保费收入',
            n_sec_tb_income DECIMAL(20,2) COMMENT '代理买卖证券业务净收入',
            n_sec_uw_income DECIMAL(20,2) COMMENT '证券承销业务净收入',
            n_asset_mg_income DECIMAL(20,2) COMMENT '受托客户资产管理业务净收入',
            oth_b_income DECIMAL(20,2) COMMENT '其他业务收入',
            fv_value_chg_gain DECIMAL(20,2) COMMENT '加:公允价值变动净收益',
            invest_income DECIMAL(20,2) COMMENT '加:投资净收益',
            ass_invest_income DECIMAL(20,2) COMMENT '其中:对联营企业和合营企业的投资收益',
            forex_gain DECIMAL(20,2) COMMENT '加:汇兑净收益',
            total_cogs DECIMAL(20,2) COMMENT '营业总成本',
            oper_cost DECIMAL(20,2) COMMENT '减:营业成本',
            int_exp DECIMAL(20,2) COMMENT '减:利息支出',
            comm_exp DECIMAL(20,2) COMMENT '减:手续费及佣金支出',
            biz_tax_surchg DECIMAL(20,2) COMMENT '减:营业税金及附加',
            sell_exp DECIMAL(20,2) COMMENT '减:销售费用',
            admin_exp DECIMAL(20,2) COMMENT '减:管理费用',
            fin_exp DECIMAL(20,2) COMMENT '减:财务费用',
            assets_impair_loss DECIMAL(20,2) COMMENT '减:资产减值损失',
            prem_refund DECIMAL(20,2) COMMENT '退保金',
            compens_payout DECIMAL(20,2) COMMENT '赔付总支出',
            reser_insur_liab DECIMAL(20,2) COMMENT '提取保险责任准备金',
            div_payt DECIMAL(20,2) COMMENT '保户红利支出',
            reins_exp DECIMAL(20,2) COMMENT '分保费用',
            oper_exp DECIMAL(20,2) COMMENT '营业支出',
            compens_payout_refu DECIMAL(20,2) COMMENT '减:摊回赔付支出',
            insur_reser_refu DECIMAL(20,2) COMMENT '减:摊回保险责任准备金',
            reins_cost_refund DECIMAL(20,2) COMMENT '减:摊回分保费用',
            other_bus_cost DECIMAL(20,2) COMMENT '其他业务成本',
            operate_profit DECIMAL(20,2) COMMENT '营业利润',
            non_oper_income DECIMAL(20,2) COMMENT '加:营业外收入',
            non_oper_exp DECIMAL(20,2) COMMENT '减:营业外支出',
            nca_disploss DECIMAL(20,2) COMMENT '其中:减:非流动资产处置净损失',
            total_profit DECIMAL(20,2) COMMENT '利润总额',
            income_tax DECIMAL(20,2) COMMENT '减:所得税费用',
            n_income DECIMAL(20,2) COMMENT '净利润(含少数股东损益)',
            n_income_attr_p DECIMAL(20,2) COMMENT '净利润(不含少数股东损益)',
            minority_gain DECIMAL(20,2) COMMENT '少数股东损益',
            oth_compr_income DECIMAL(20,2) COMMENT '其他综合收益',
            t_compr_income DECIMAL(20,2) COMMENT '综合收益总额',
            compr_inc_attr_p DECIMAL(20,2) COMMENT '归属于母公司(或股东)的综合收益总额',
            compr_inc_attr_m_s DECIMAL(20,2) COMMENT '归属于少数股东的综合收益总额',
            ebit DECIMAL(20,2) COMMENT '息税前利润',
            ebitda DECIMAL(20,2) COMMENT '息税折旧摊销前利润',
            insurance_exp DECIMAL(20,2) COMMENT '保险业务支出',
            undist_profit DECIMAL(20,2) COMMENT '年初未分配利润',
            distable_profit DECIMAL(20,2) COMMENT '可分配利润',
            rd_exp DECIMAL(20,2) COMMENT '研发费用',
            fin_exp_int_exp DECIMAL(20,2) COMMENT '财务费用:利息费用',
            fin_exp_int_inc DECIMAL(20,2) COMMENT '财务费用:利息收入',
            transfer_surplus_rese DECIMAL(20,2) COMMENT '提取盈余公积',
            transfer_housin_imprest DECIMAL(20,2) COMMENT '提取住房公积金',
            transfer_oth DECIMAL(20,2) COMMENT '提取一般风险准备',
            adj_lossgain DECIMAL(20,2) COMMENT '调整以前年度损益',
            withdra_legal_surplus DECIMAL(20,2) COMMENT '提取法定盈余公积',
            withdra_legal_pubfund DECIMAL(20,2) COMMENT '提取法定公益金',
            withdra_biz_devfund DECIMAL(20,2) COMMENT '提取企业发展基金',
            withdra_rese_fund DECIMAL(20,2) COMMENT '提取储备基金',
            withdra_oth_ersu DECIMAL(20,2) COMMENT '提取任意盈余公积金',
            workers_welfare DECIMAL(20,2) COMMENT '职工奖金福利',
            distr_profit_shrhder DECIMAL(20,2) COMMENT '可供股东分配的利润',
            prfshare_payable_dvd DECIMAL(20,2) COMMENT '应付优先股股利',
            comshare_payable_dvd DECIMAL(20,2) COMMENT '应付普通股股利',
            capit_comstock_div DECIMAL(20,2) COMMENT '转作股本的普通股股利',
            net_after_nr_lp_correct DECIMAL(20,2) COMMENT '扣除非经常性损益后的净利润',
            credit_impa_loss DECIMAL(20,2) COMMENT '信用减值损失',
            net_expo_hedging_benefits DECIMAL(20,2) COMMENT '净敞口套期收益',
            oth_impair_loss_assets DECIMAL(20,2) COMMENT '其他资产减值损失',
            total_opcost DECIMAL(20,2) COMMENT '营业总成本',
            amodcost_fin_assets DECIMAL(20,2) COMMENT '以摊余成本计量的金融资产终止确认收益',
            oth_income DECIMAL(20,2) COMMENT '其他收益',
            asset_disp_income DECIMAL(20,2) COMMENT '资产处置收益',
            continued_net_profit DECIMAL(20,2) COMMENT '持续经营净利润',
            end_net_profit DECIMAL(20,2) COMMENT '终止经营净利润',
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            UNIQUE KEY uk_code_end_date (ts_code, end_date),
            INDEX idx_ts_code (ts_code),
            INDEX idx_end_date (end_date),
            INDEX idx_ann_date (ann_date)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='利润表'
        """
        db_helper.execute_sql(sql)
        
    @staticmethod
    def create_cashflow_statement_table(db_helper):
        """现金流量表"""
        sql = """
        CREATE TABLE IF NOT EXISTS t_cashflow_statement (
            id BIGINT PRIMARY KEY AUTO_INCREMENT,
            ts_code VARCHAR(10) NOT NULL COMMENT '股票代码',
            ann_date DATE NOT NULL COMMENT '公告日期', 
            f_ann_date DATE COMMENT '实际公告日期',
            end_date DATE NOT NULL COMMENT '报告期',
            report_type VARCHAR(10) COMMENT '报告类型',
            comp_type VARCHAR(10) COMMENT '公司类型',
            net_profit DECIMAL(20,2) COMMENT '净利润',
            finan_exp DECIMAL(20,2) COMMENT '财务费用',
            c_fr_sale_sg DECIMAL(20,2) COMMENT '销售商品、提供劳务收到的现金',
            recp_tax_rends DECIMAL(20,2) COMMENT '收到的税费返还',
            n_depos_incr_fi DECIMAL(20,2) COMMENT '客户存款和同业存放款项净增加额',
            n_incr_loans_cb DECIMAL(20,2) COMMENT '向央行借款净增加额',
            n_inc_borr_oth_fi DECIMAL(20,2) COMMENT '向其他金融机构拆入资金净增加额',
            prem_fr_orig_contr DECIMAL(20,2) COMMENT '收到原保险合同保费取得的现金',
            n_incr_insured_dep DECIMAL(20,2) COMMENT '保户储金净增加额',
            n_reinsur_prem DECIMAL(20,2) COMMENT '收到再保业务现金净额',
            n_incr_disp_tfa DECIMAL(20,2) COMMENT '处置交易性金融资产净增加额',
            ifc_cash_incr DECIMAL(20,2) COMMENT '收取利息和手续费净增加额',
            n_incr_disp_faas DECIMAL(20,2) COMMENT '处置可供出售金融资产净增加额',
            n_incr_loans_oth_bank DECIMAL(20,2) COMMENT '拆入资金净增加额',
            n_cap_incr_repur DECIMAL(20,2) COMMENT '回购业务资金净增加额',
            c_fr_oth_operate_a DECIMAL(20,2) COMMENT '收到其他与经营活动有关的现金',
            c_inf_fr_operate_a DECIMAL(20,2) COMMENT '经营活动现金流入小计',
            c_paid_goods_s DECIMAL(20,2) COMMENT '购买商品、接受劳务支付的现金',
            c_paid_to_for_empl DECIMAL(20,2) COMMENT '支付给职工以及为职工支付的现金',
            c_paid_for_taxes DECIMAL(20,2) COMMENT '支付的各项税费',
            n_incr_clt_loan_adv DECIMAL(20,2) COMMENT '客户贷款及垫款净增加额',
            n_incr_dep_cbob DECIMAL(20,2) COMMENT '存放央行和同业款项净增加额',
            c_pay_claims_orig_inco DECIMAL(20,2) COMMENT '支付原保险合同赔付款项的现金',
            pay_handling_chrg DECIMAL(20,2) COMMENT '支付手续费的现金',
            pay_comm_insur_plcy DECIMAL(20,2) COMMENT '支付保单红利的现金',
            c_paid_oth_operate_a DECIMAL(20,2) COMMENT '支付其他与经营活动有关的现金',
            c_outf_fr_operate_a DECIMAL(20,2) COMMENT '经营活动现金流出小计',
            n_cashflow_act DECIMAL(20,2) COMMENT '经营活动产生的现金流量净额',
            oth_recp_ral_inv_act DECIMAL(20,2) COMMENT '收到其他与投资活动有关的现金',
            c_disp_withdrwl_invest DECIMAL(20,2) COMMENT '收回投资收到的现金',
            c_recp_return_invest DECIMAL(20,2) COMMENT '取得投资收益收到的现金',
            n_recp_disp_fiolta DECIMAL(20,2) COMMENT '处置固定资产无形资产和其他长期资产收回的现金净额',
            n_recp_disp_sobu DECIMAL(20,2) COMMENT '处置子公司及其他营业单位收到的现金净额',
            stot_inflows_inv_act DECIMAL(20,2) COMMENT '投资活动现金流入小计',
            c_pay_acq_const_fiolta DECIMAL(20,2) COMMENT '购建固定资产无形资产和其他长期资产支付的现金',
            c_paid_invest DECIMAL(20,2) COMMENT '投资支付的现金',
            n_disp_subs_oth_biz DECIMAL(20,2) COMMENT '取得子公司及其他营业单位支付的现金净额',
            oth_pay_ral_inv_act DECIMAL(20,2) COMMENT '支付其他与投资活动有关的现金',
            n_incr_pledge_loan DECIMAL(20,2) COMMENT '质押贷款净增加额',
            stot_out_inv_act DECIMAL(20,2) COMMENT '投资活动现金流出小计',
            n_cashflow_inv_act DECIMAL(20,2) COMMENT '投资活动产生的现金流量净额',
            c_recp_borrow DECIMAL(20,2) COMMENT '取得借款收到的现金',
            proc_issue_bonds DECIMAL(20,2) COMMENT '发行债券收到的现金',
            oth_cash_recp_ral_fnc_act DECIMAL(20,2) COMMENT '收到其他与筹资活动有关的现金',
            stot_cash_inflows_fnc_act DECIMAL(20,2) COMMENT '筹资活动现金流入小计',
            free_cashflow DECIMAL(20,2) COMMENT '企业自由现金流量',
            c_prepay_amt_borr DECIMAL(20,2) COMMENT '偿还债务支付的现金',
            c_pay_dist_dpcp_int_exp DECIMAL(20,2) COMMENT '分配股利利润或偿付利息支付的现金',
            incl_dvd_profit_paid_sc_ms DECIMAL(20,2) COMMENT '其中:子公司支付给少数股东的股利利润',
            oth_cashpay_ral_fnc_act DECIMAL(20,2) COMMENT '支付其他与筹资活动有关的现金',
            stot_cashout_fnc_act DECIMAL(20,2) COMMENT '筹资活动现金流出小计',
            n_cash_flows_fnc_act DECIMAL(20,2) COMMENT '筹资活动产生的现金流量净额',
            eff_fx_flu_cash DECIMAL(20,2) COMMENT '汇率变动对现金的影响',
            n_incr_cash_cash_equ DECIMAL(20,2) COMMENT '现金及现金等价物净增加额',
            c_cash_equ_beg_period DECIMAL(20,2) COMMENT '期初现金及现金等价物余额',
            c_cash_equ_end_period DECIMAL(20,2) COMMENT '期末现金及现金等价物余额',
            c_recp_cap_contrib DECIMAL(20,2) COMMENT '吸收投资收到的现金',
            incl_cash_rec_saims DECIMAL(20,2) COMMENT '其中:子公司吸收少数股东投资收到的现金',
            uncon_invest_loss DECIMAL(20,2) COMMENT '未确认投资损失',
            prov_depr_assets DECIMAL(20,2) COMMENT '加:资产减值准备',
            depr_fa_coga_dpba DECIMAL(20,2) COMMENT '固定资产折旧油气资产折耗生产性生物资产折旧',
            amort_intang_assets DECIMAL(20,2) COMMENT '无形资产摊销',
            lt_amort_deferred_exp DECIMAL(20,2) COMMENT '长期待摊费用摊销',
            decr_deferred_exp DECIMAL(20,2) COMMENT '待摊费用减少',
            incr_acc_exp DECIMAL(20,2) COMMENT '预提费用增加',
            loss_disp_fiolta DECIMAL(20,2) COMMENT '处置固定、无形资产和其他长期资产的损失',
            loss_scr_fa DECIMAL(20,2) COMMENT '固定资产报废损失',
            loss_fv_chg DECIMAL(20,2) COMMENT '公允价值变动损失',
            invest_loss DECIMAL(20,2) COMMENT '投资损失',
            decr_def_inc_tax_assets DECIMAL(20,2) COMMENT '递延所得税资产减少',
            incr_def_inc_tax_liab DECIMAL(20,2) COMMENT '递延所得税负债增加',
            decr_inventories DECIMAL(20,2) COMMENT '存货的减少',
            decr_oper_payable DECIMAL(20,2) COMMENT '经营性应收项目的减少',
            incr_oper_payable DECIMAL(20,2) COMMENT '经营性应付项目的增加',
            others DECIMAL(20,2) COMMENT '其他',
            im_net_cashflow_oper_act DECIMAL(20,2) COMMENT '经营活动产生的现金流量净额(间接法)',
            conv_debt_into_cap DECIMAL(20,2) COMMENT '债务转为资本',
            conv_copbonds_due_within_1y DECIMAL(20,2) COMMENT '一年内到期的可转换公司债券',
            fa_fnc_leases DECIMAL(20,2) COMMENT '融资租入固定资产',
            end_bal_cash DECIMAL(20,2) COMMENT '现金的期末余额',
            beg_bal_cash DECIMAL(20,2) COMMENT '减:现金的期初余额',
            end_bal_cash_equ DECIMAL(20,2) COMMENT '加:现金等价物的期末余额',
            beg_bal_cash_equ DECIMAL(20,2) COMMENT '减:现金等价物的期初余额',
            im_n_incr_cash_equ DECIMAL(20,2) COMMENT '现金及现金等价物净增加额(间接法)',
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            UNIQUE KEY uk_code_end_date (ts_code, end_date),
            INDEX idx_ts_code (ts_code),
            INDEX idx_end_date (end_date),
            INDEX idx_ann_date (ann_date)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='现金流量表'
        """
        db_helper.execute_sql(sql)
        
    @staticmethod
    def create_financial_indicators_table(db_helper):
        """财务指标表"""
        sql = """
        CREATE TABLE IF NOT EXISTS t_financial_indicators (
            id BIGINT PRIMARY KEY AUTO_INCREMENT,
            ts_code VARCHAR(10) NOT NULL COMMENT '股票代码',
            ann_date DATE NOT NULL COMMENT '公告日期',
            end_date DATE NOT NULL COMMENT '报告期',
            eps DECIMAL(10,4) COMMENT '基本每股收益',
            dt_eps DECIMAL(10,4) COMMENT '稀释每股收益',
            total_revenue_ps DECIMAL(10,4) COMMENT '每股营业总收入',
            revenue_ps DECIMAL(10,4) COMMENT '每股营业收入',
            capital_rese_ps DECIMAL(10,4) COMMENT '每股资本公积',
            surplus_rese_ps DECIMAL(10,4) COMMENT '每股盈余公积',
            undist_profit_ps DECIMAL(10,4) COMMENT '每股未分配利润',
            extra_item DECIMAL(20,2) COMMENT '非经常性损益',
            profit_dedt DECIMAL(20,2) COMMENT '扣除非经常性损益后的净利润',
            gross_margin DECIMAL(10,4) COMMENT '毛利',
            current_ratio DECIMAL(10,4) COMMENT '流动比率',
            quick_ratio DECIMAL(10,4) COMMENT '速动比率',
            cash_ratio DECIMAL(10,4) COMMENT '保守速动比率',
            invturn_days DECIMAL(10,2) COMMENT '存货周转天数',
            arturn_days DECIMAL(10,2) COMMENT '应收账款周转天数',
            inv_turn DECIMAL(10,4) COMMENT '存货周转率',
            ar_turn DECIMAL(10,4) COMMENT '应收账款周转率',
            ca_turn DECIMAL(10,4) COMMENT '流动资产周转率',
            fa_turn DECIMAL(10,4) COMMENT '固定资产周转率',
            assets_turn DECIMAL(10,4) COMMENT '总资产周转率',
            op_income DECIMAL(20,2) COMMENT '经营活动净收益',
            valuechange_income DECIMAL(20,2) COMMENT '价值变动净收益',
            interst_income DECIMAL(20,2) COMMENT '利息费用',
            daa DECIMAL(20,2) COMMENT '折旧与摊销',
            ebit DECIMAL(20,2) COMMENT '息税前利润',
            ebitda DECIMAL(20,2) COMMENT '息税折旧摊销前利润',
            fcff DECIMAL(20,2) COMMENT '企业自由现金流量',
            fcfe DECIMAL(20,2) COMMENT '股权自由现金流量',
            current_exint DECIMAL(20,2) COMMENT '无息流动负债',
            noncurrent_exint DECIMAL(20,2) COMMENT '无息非流动负债',
            interestdebt DECIMAL(20,2) COMMENT '带息债务',
            netdebt DECIMAL(20,2) COMMENT '净债务',
            tangible_asset DECIMAL(20,2) COMMENT '有形资产',
            working_capital DECIMAL(20,2) COMMENT '营运资金',
            networking_capital DECIMAL(20,2) COMMENT '营运流动资本',
            invest_capital DECIMAL(20,2) COMMENT '全部投入资本',
            retained_earnings DECIMAL(20,2) COMMENT '留存收益',
            diluted2_eps DECIMAL(10,4) COMMENT '期末摊薄每股收益',
            bps DECIMAL(10,4) COMMENT '每股净资产',
            ocfps DECIMAL(10,4) COMMENT '每股经营活动产生的现金流量净额',
            retainedps DECIMAL(10,4) COMMENT '每股留存收益',
            cfps DECIMAL(10,4) COMMENT '每股现金流量净额',
            ebit_ps DECIMAL(10,4) COMMENT '每股息税前利润',
            fcff_ps DECIMAL(10,4) COMMENT '每股企业自由现金流量',
            fcfe_ps DECIMAL(10,4) COMMENT '每股股东自由现金流量',
            netprofit_margin DECIMAL(10,4) COMMENT '销售净利率',
            grossprofit_margin DECIMAL(10,4) COMMENT '销售毛利率',
            cogs_of_sales DECIMAL(10,4) COMMENT '销售成本率',
            expense_of_sales DECIMAL(10,4) COMMENT '销售期间费用率',
            profit_to_gr DECIMAL(10,4) COMMENT '净利润/营业总收入',
            saleexp_to_gr DECIMAL(10,4) COMMENT '销售费用/营业总收入',
            adminexp_of_gr DECIMAL(10,4) COMMENT '管理费用/营业总收入',
            finaexp_of_gr DECIMAL(10,4) COMMENT '财务费用/营业总收入',
            impai_ttm DECIMAL(20,2) COMMENT '资产减值损失/营业总收入',
            gc_of_gr DECIMAL(10,4) COMMENT '营业总成本/营业总收入',
            op_of_gr DECIMAL(10,4) COMMENT '营业利润/营业总收入',
            ebit_of_gr DECIMAL(10,4) COMMENT '息税前利润/营业总收入',
            roe DECIMAL(10,4) COMMENT '净资产收益率',
            roe_waa DECIMAL(10,4) COMMENT '加权平均净资产收益率',
            roe_dt DECIMAL(10,4) COMMENT '净资产收益率(扣除非经常损益)',
            roa DECIMAL(10,4) COMMENT '总资产报酬率',
            npta DECIMAL(10,4) COMMENT '总资产净利润率',
            roic DECIMAL(10,4) COMMENT '投入资本回报率',
            roe_yearly DECIMAL(10,4) COMMENT '年化净资产收益率',
            roa2_yearly DECIMAL(10,4) COMMENT '年化总资产报酬率',
            roe_avg DECIMAL(10,4) COMMENT '平均净资产收益率(增发条件)',
            opincome_of_ebt DECIMAL(10,4) COMMENT '经营活动净收益/利润总额',
            investincome_of_ebt DECIMAL(10,4) COMMENT '价值变动净收益/利润总额',
            n_op_profit_of_ebt DECIMAL(10,4) COMMENT '营业外收支净额/利润总额',
            tax_to_ebt DECIMAL(10,4) COMMENT '所得税/利润总额',
            dtprofit_to_profit DECIMAL(10,4) COMMENT '扣除非经常损益后的净利润/净利润',
            salescash_to_or DECIMAL(10,4) COMMENT '销售商品提供劳务收到的现金/营业收入',
            ocf_to_or DECIMAL(10,4) COMMENT '经营活动产生的现金流量净额/营业收入',
            ocf_to_opincome DECIMAL(10,4) COMMENT '经营活动产生的现金流量净额/经营活动净收益',
            capitalized_to_da DECIMAL(10,4) COMMENT '资本支出/折旧和摊销',
            debt_to_assets DECIMAL(10,4) COMMENT '资产负债率',
            assets_to_eqt DECIMAL(10,4) COMMENT '权益乘数',
            dp_assets_to_eqt DECIMAL(10,4) COMMENT '权益乘数(杜邦分析)',
            ca_to_assets DECIMAL(10,4) COMMENT '流动资产/总资产',
            nca_to_assets DECIMAL(10,4) COMMENT '非流动资产/总资产',
            tbassets_to_totalassets DECIMAL(10,4) COMMENT '有形资产/总资产',
            int_to_talcap DECIMAL(10,4) COMMENT '带息债务/全部投入资本',
            eqt_to_talcapital DECIMAL(10,4) COMMENT '归属于母公司的股东权益/全部投入资本',
            currentdebt_to_debt DECIMAL(10,4) COMMENT '流动负债/负债合计',
            longdeb_to_debt DECIMAL(10,4) COMMENT '非流动负债/负债合计',
            ocf_to_shortdebt DECIMAL(10,4) COMMENT '经营活动产生的现金流量净额/流动负债',
            debt_to_eqt DECIMAL(10,4) COMMENT '产权比率',
            eqt_to_debt DECIMAL(10,4) COMMENT '归属于母公司的股东权益/负债合计',
            eqt_to_interestdebt DECIMAL(10,4) COMMENT '归属于母公司的股东权益/带息债务',
            tangibleasset_to_debt DECIMAL(10,4) COMMENT '有形资产/负债合计',
            tangasset_to_intdebt DECIMAL(10,4) COMMENT '有形资产/带息债务',
            tangibleasset_to_netdebt DECIMAL(10,4) COMMENT '有形资产/净债务',
            ocf_to_debt DECIMAL(10,4) COMMENT '经营活动产生的现金流量净额/负债合计',
            ocf_to_interestdebt DECIMAL(10,4) COMMENT '经营活动产生的现金流量净额/带息债务',
            ocf_to_netdebt DECIMAL(10,4) COMMENT '经营活动产生的现金流量净额/净债务',
            ebit_to_interest DECIMAL(10,4) COMMENT '已获利息倍数(EBIT/利息费用)',
            longdebt_to_workingcapital DECIMAL(10,4) COMMENT '长期债务与营运资金比率',
            ebitda_to_debt DECIMAL(10,4) COMMENT 'EBITDA/负债合计',
            turn_days DECIMAL(10,2) COMMENT '营业周期',
            roa_yearly DECIMAL(10,4) COMMENT '年化总资产净利率',
            roa_dp DECIMAL(10,4) COMMENT '总资产净利率(杜邦分析)',
            fixed_assets DECIMAL(20,2) COMMENT '固定资产合计',
            profit_prefin_exp DECIMAL(20,2) COMMENT '扣除财务费用前营业利润',
            non_op_profit DECIMAL(20,2) COMMENT '非营业利润',
            op_to_ebt DECIMAL(10,4) COMMENT '营业利润／利润总额',
            nop_to_ebt DECIMAL(10,4) COMMENT '非营业利润／利润总额',
            ocf_to_profit DECIMAL(10,4) COMMENT '经营活动产生的现金流量净额／营业利润',
            cash_to_liqdebt DECIMAL(10,4) COMMENT '货币资金／流动负债',
            cash_to_liqdebt_withinterest DECIMAL(10,4) COMMENT '货币资金／带息流动负债',
            op_to_liqdebt DECIMAL(10,4) COMMENT '营业利润／流动负债',
            op_to_debt DECIMAL(10,4) COMMENT '营业利润／负债合计',
            roic_yearly DECIMAL(10,4) COMMENT '年化投入资本回报率',
            total_fa_trun DECIMAL(10,4) COMMENT '固定资产合计周转率',
            profit_to_op DECIMAL(10,4) COMMENT '利润总额／营业收入',
            q_opincome DECIMAL(20,2) COMMENT '经营活动单季度净收益',
            q_investincome DECIMAL(20,2) COMMENT '价值变动单季度净收益',
            q_dtprofit DECIMAL(20,2) COMMENT '扣除非经常损益后的单季度净利润',
            q_eps DECIMAL(10,4) COMMENT '每股收益(单季度)',
            q_netprofit_margin DECIMAL(10,4) COMMENT '销售净利率(单季度)',
            q_gsprofit_margin DECIMAL(10,4) COMMENT '销售毛利率(单季度)',
            q_exp_to_sales DECIMAL(10,4) COMMENT '销售期间费用率(单季度)',
            q_profit_to_gr DECIMAL(10,4) COMMENT '净利润／营业总收入(单季度)',
            q_saleexp_to_gr DECIMAL(10,4) COMMENT '销售费用／营业总收入(单季度)',
            q_adminexp_to_gr DECIMAL(10,4) COMMENT '管理费用／营业总收入(单季度)',
            q_finaexp_to_gr DECIMAL(10,4) COMMENT '财务费用／营业总收入(单季度)',
            q_impair_to_gr_ttm DECIMAL(10,4) COMMENT '资产减值损失／营业总收入(单季度)',
            q_gc_to_gr DECIMAL(10,4) COMMENT '营业总成本／营业总收入(单季度)',
            q_op_to_gr DECIMAL(10,4) COMMENT '营业利润／营业总收入(单季度)',
            q_roe DECIMAL(10,4) COMMENT '净资产收益率(单季度)',
            q_dt_roe DECIMAL(10,4) COMMENT '净资产单季度收益率(扣除非经常损益)',
            q_npta DECIMAL(10,4) COMMENT '总资产净利润率(单季度)',
            q_opincome_to_ebt DECIMAL(10,4) COMMENT '经营活动净收益／利润总额(单季度)',
            q_investincome_to_ebt DECIMAL(10,4) COMMENT '价值变动净收益／利润总额(单季度)',
            q_dtprofit_to_profit DECIMAL(10,4) COMMENT '扣除非经常损益后的净利润／净利润(单季度)',
            q_salescash_to_or DECIMAL(10,4) COMMENT '销售商品提供劳务收到的现金／营业收入(单季度)',
            q_ocf_to_sales DECIMAL(10,4) COMMENT '经营活动产生的现金流量净额／营业收入(单季度)',
            q_ocf_to_or DECIMAL(10,4) COMMENT '经营活动产生的现金流量净额／经营活动净收益(单季度)',
            basic_eps_yoy DECIMAL(10,4) COMMENT '基本每股收益同比增长率(%)',
            dt_eps_yoy DECIMAL(10,4) COMMENT '稀释每股收益同比增长率(%)',
            cfps_yoy DECIMAL(10,4) COMMENT '每股经营活动产生的现金流量净额同比增长率(%)',
            op_yoy DECIMAL(10,4) COMMENT '营业利润同比增长率(%)',
            ebt_yoy DECIMAL(10,4) COMMENT '利润总额同比增长率(%)',
            netprofit_yoy DECIMAL(10,4) COMMENT '归属母公司股东的净利润同比增长率(%)',
            dt_netprofit_yoy DECIMAL(10,4) COMMENT '归属母公司股东的净利润-扣除非经常损益同比增长率(%)',
            ocf_yoy DECIMAL(10,4) COMMENT '经营活动产生的现金流量净额同比增长率(%)',
            roe_yoy DECIMAL(10,4) COMMENT '净资产收益率(摊薄)同比增长率(%)',
            bps_yoy DECIMAL(10,4) COMMENT '每股净资产相对年初增长率(%)',
            assets_yoy DECIMAL(10,4) COMMENT '资产总计相对年初增长率(%)',
            eqt_yoy DECIMAL(10,4) COMMENT '归属母公司的股东权益相对年初增长率(%)',
            tr_yoy DECIMAL(10,4) COMMENT '营业总收入同比增长率(%)',
            or_yoy DECIMAL(10,4) COMMENT '营业收入同比增长率(%)',
            q_gr_yoy DECIMAL(10,4) COMMENT '营业总收入同比增长率(%)(单季度)',
            q_gr_qoq DECIMAL(10,4) COMMENT '营业总收入环比增长率(%)(单季度)',
            q_sales_yoy DECIMAL(10,4) COMMENT '营业收入同比增长率(%)(单季度)',
            q_sales_qoq DECIMAL(10,4) COMMENT '营业收入环比增长率(%)(单季度)',
            q_op_yoy DECIMAL(10,4) COMMENT '营业利润同比增长率(%)(单季度)',
            q_op_qoq DECIMAL(10,4) COMMENT '营业利润环比增长率(%)(单季度)',
            q_profit_yoy DECIMAL(10,4) COMMENT '净利润同比增长率(%)(单季度)',
            q_profit_qoq DECIMAL(10,4) COMMENT '净利润环比增长率(%)(单季度)',
            q_netprofit_yoy DECIMAL(10,4) COMMENT '归属母公司股东的净利润同比增长率(%)(单季度)',
            q_netprofit_qoq DECIMAL(10,4) COMMENT '归属母公司股东的净利润环比增长率(%)(单季度)',
            equity_yoy DECIMAL(10,4) COMMENT '净资产同比增长率',
            rd_exp DECIMAL(20,2) COMMENT '研发费用',
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            UNIQUE KEY uk_code_end_date (ts_code, end_date),
            INDEX idx_ts_code (ts_code),
            INDEX idx_end_date (end_date),
            INDEX idx_ann_date (ann_date),
            INDEX idx_roe (roe),
            INDEX idx_roa (roa),
            INDEX idx_eps (eps)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='财务指标表'
        """
        db_helper.execute_sql(sql)
        
    @staticmethod
    def create_performance_forecast_table(db_helper):
        """业绩预告表"""
        sql = """
        CREATE TABLE IF NOT EXISTS t_performance_forecast (
            id BIGINT PRIMARY KEY AUTO_INCREMENT,
            ts_code VARCHAR(10) NOT NULL COMMENT '股票代码',
            ann_date DATE NOT NULL COMMENT '公告日期',
            end_date DATE NOT NULL COMMENT '报告期',
            type VARCHAR(20) COMMENT '业绩预告类型',
            p_change_min DECIMAL(10,2) COMMENT '预告净利润变动幅度下限(%)',
            p_change_max DECIMAL(10,2) COMMENT '预告净利润变动幅度上限(%)',
            net_profit_min DECIMAL(20,2) COMMENT '预告净利润下限(万元)',
            net_profit_max DECIMAL(20,2) COMMENT '预告净利润上限(万元)',
            last_parent_net DECIMAL(20,2) COMMENT '上年同期归属母公司净利润',
            first_ann_date DATE COMMENT '首次公告日',
            summary TEXT COMMENT '业绩预告摘要',
            change_reason TEXT COMMENT '业绩变动原因',
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            UNIQUE KEY uk_code_ann_end (ts_code, ann_date, end_date),
            INDEX idx_ts_code (ts_code),
            INDEX idx_end_date (end_date),
            INDEX idx_ann_date (ann_date)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='业绩预告表'
        """
        db_helper.execute_sql(sql)
        
    @staticmethod
    def create_performance_express_table(db_helper):
        """业绩快报表"""
        sql = """
        CREATE TABLE IF NOT EXISTS t_performance_express (
            id BIGINT PRIMARY KEY AUTO_INCREMENT,
            ts_code VARCHAR(10) NOT NULL COMMENT '股票代码',
            ann_date DATE NOT NULL COMMENT '公告日期',
            end_date DATE NOT NULL COMMENT '报告期',
            revenue DECIMAL(20,2) COMMENT '营业收入(元)',
            operate_profit DECIMAL(20,2) COMMENT '营业利润(元)',
            total_profit DECIMAL(20,2) COMMENT '利润总额(元)',
            n_income DECIMAL(20,2) COMMENT '净利润(元)',
            total_assets DECIMAL(20,2) COMMENT '总资产(元)',
            total_hldr_eqy_exc_min_int DECIMAL(20,2) COMMENT '股东权益合计(不含少数股东权益)(元)',
            diluted_eps DECIMAL(10,4) COMMENT '每股收益(元)',
            diluted_roe DECIMAL(10,4) COMMENT '净资产收益率',
            yoy_net_profit DECIMAL(10,2) COMMENT '去年同期修正后净利润',
            bps DECIMAL(10,4) COMMENT '每股净资产',
            yoy_sales DECIMAL(10,2) COMMENT '同比增长率:营业收入',
            yoy_op DECIMAL(10,2) COMMENT '同比增长率:营业利润',
            yoy_tp DECIMAL(10,2) COMMENT '同比增长率:利润总额',
            yoy_dedu_np DECIMAL(10,2) COMMENT '同比增长率:归属母公司股东的净利润',
            yoy_eps DECIMAL(10,2) COMMENT '同比增长率:基本每股收益',
            yoy_roe DECIMAL(10,2) COMMENT '同比增减:加权平均净资产收益率',
            growth_assets DECIMAL(10,2) COMMENT '比年初增长率:总资产',
            yoy_equity DECIMAL(10,2) COMMENT '比年初增长率:归属母公司的股东权益',
            growth_bps DECIMAL(10,2) COMMENT '比年初增长率:归属于母公司股东的每股净资产',
            or_last_year DECIMAL(20,2) COMMENT '去年同期营业收入',
            op_last_year DECIMAL(20,2) COMMENT '去年同期营业利润',
            tp_last_year DECIMAL(20,2) COMMENT '去年同期利润总额',
            np_last_year DECIMAL(20,2) COMMENT '去年同期净利润',
            eps_last_year DECIMAL(10,4) COMMENT '去年同期每股收益',
            open_net_assets DECIMAL(20,2) COMMENT '期初净资产',
            open_bps DECIMAL(10,4) COMMENT '期初每股净资产',
            perf_summary TEXT COMMENT '业绩简要说明',
            is_audit VARCHAR(2) COMMENT '是否审计',
            remark TEXT COMMENT '备注',
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            UNIQUE KEY uk_code_ann_end (ts_code, ann_date, end_date),
            INDEX idx_ts_code (ts_code),
            INDEX idx_end_date (end_date),
            INDEX idx_ann_date (ann_date)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='业绩快报表'
        """
        db_helper.execute_sql(sql)
        
    # ===================资金流向表===================
    
    @staticmethod
    def create_money_flow_table(db_helper):
        """个股资金流向表"""
        sql = """
        CREATE TABLE IF NOT EXISTS t_money_flow (
            id BIGINT PRIMARY KEY AUTO_INCREMENT,
            ts_code VARCHAR(10) NOT NULL COMMENT '股票代码',
            trade_date DATE NOT NULL COMMENT '交易日期',
            buy_sm_vol INT COMMENT '小单买入量(手)',
            buy_sm_amount DECIMAL(20,2) COMMENT '小单买入金额(万元)',
            sell_sm_vol INT COMMENT '小单卖出量(手)',
            sell_sm_amount DECIMAL(20,2) COMMENT '小单卖出金额(万元)',
            buy_md_vol INT COMMENT '中单买入量(手)',
            buy_md_amount DECIMAL(20,2) COMMENT '中单买入金额(万元)',
            sell_md_vol INT COMMENT '中单卖出量(手)',
            sell_md_amount DECIMAL(20,2) COMMENT '中单卖出金额(万元)',
            buy_lg_vol INT COMMENT '大单买入量(手)',
            buy_lg_amount DECIMAL(20,2) COMMENT '大单买入金额(万元)',
            sell_lg_vol INT COMMENT '大单卖出量(手)',
            sell_lg_amount DECIMAL(20,2) COMMENT '大单卖出金额(万元)',
            buy_elg_vol INT COMMENT '特大单买入量(手)',
            buy_elg_amount DECIMAL(20,2) COMMENT '特大单买入金额(万元)',
            sell_elg_vol INT COMMENT '特大单卖出量(手)',
            sell_elg_amount DECIMAL(20,2) COMMENT '特大单卖出金额(万元)',
            net_mf_vol INT COMMENT '净流入量(手)',
            net_mf_amount DECIMAL(20,2) COMMENT '净流入额(万元)',
            trade_count INT COMMENT '交易笔数',
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            UNIQUE KEY uk_code_date (ts_code, trade_date),
            INDEX idx_ts_code (ts_code),
            INDEX idx_trade_date (trade_date),
            INDEX idx_net_mf_amount (net_mf_amount)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='个股资金流向表'
        """
        db_helper.execute_sql(sql)
        
    @staticmethod
    def create_dragon_tiger_list_table(db_helper):
        """龙虎榜数据表"""
        sql = """
        CREATE TABLE IF NOT EXISTS t_dragon_tiger_list (
            id BIGINT PRIMARY KEY AUTO_INCREMENT,
            trade_date DATE NOT NULL COMMENT '交易日期',
            ts_code VARCHAR(10) NOT NULL COMMENT '股票代码',
            name VARCHAR(50) COMMENT '股票名称',
            close DECIMAL(10,2) COMMENT '收盘价',
            pct_chg DECIMAL(10,2) COMMENT '涨跌幅',
            turnover_rate DECIMAL(10,2) COMMENT '换手率',
            amount DECIMAL(20,2) COMMENT '总成交额',
            l_sell DECIMAL(20,2) COMMENT '龙虎榜卖出额',
            l_buy DECIMAL(20,2) COMMENT '龙虎榜买入额',
            l_amount DECIMAL(20,2) COMMENT '龙虎榜成交额',
            net_amount DECIMAL(20,2) COMMENT '龙虎榜净买入额',
            net_rate DECIMAL(10,4) COMMENT '龙虎榜净买入额占比',
            amount_rate DECIMAL(10,4) COMMENT '龙虎榜成交额占比',
            float_values DECIMAL(20,2) COMMENT '当日流通市值',
            reason VARCHAR(200) COMMENT '上榜原因',
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            UNIQUE KEY uk_date_code (trade_date, ts_code),
            INDEX idx_ts_code (ts_code),
            INDEX idx_trade_date (trade_date),
            INDEX idx_net_amount (net_amount)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='龙虎榜数据表'
        """
        db_helper.execute_sql(sql)
        
    # ===================股东及股权变动表===================
    
    @staticmethod
    def create_top10_holders_table(db_helper):
        """十大股东表"""
        sql = """
        CREATE TABLE IF NOT EXISTS t_top10_holders (
            id BIGINT PRIMARY KEY AUTO_INCREMENT,
            ts_code VARCHAR(10) NOT NULL COMMENT '股票代码',
            ann_date DATE NOT NULL COMMENT '公告日期',
            end_date DATE NOT NULL COMMENT '报告期',
            holder_name VARCHAR(200) NOT NULL COMMENT '股东名称',
            hold_amount DECIMAL(20,2) COMMENT '持有数量(股)',
            hold_ratio DECIMAL(10,4) COMMENT '持有比例',
            is_holdorg VARCHAR(2) COMMENT '是否为机构',
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            UNIQUE KEY uk_code_end_holder (ts_code, end_date, holder_name),
            INDEX idx_ts_code (ts_code),
            INDEX idx_end_date (end_date),
            INDEX idx_ann_date (ann_date)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='十大股东表'
        """
        db_helper.execute_sql(sql)
        
    @staticmethod
    def create_top10_float_holders_table(db_helper):
        """十大流通股东表"""
        sql = """
        CREATE TABLE IF NOT EXISTS t_top10_float_holders (
            id BIGINT PRIMARY KEY AUTO_INCREMENT,
            ts_code VARCHAR(10) NOT NULL COMMENT '股票代码',
            ann_date DATE NOT NULL COMMENT '公告日期',
            end_date DATE NOT NULL COMMENT '报告期',
            holder_name VARCHAR(200) NOT NULL COMMENT '股东名称',
            hold_amount DECIMAL(20,2) COMMENT '持有数量(股)',
            hold_ratio DECIMAL(10,4) COMMENT '持有比例',
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            UNIQUE KEY uk_code_end_holder (ts_code, end_date, holder_name),
            INDEX idx_ts_code (ts_code),
            INDEX idx_end_date (end_date),
            INDEX idx_ann_date (ann_date)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='十大流通股东表'
        """
        db_helper.execute_sql(sql)
        
    @staticmethod
    def create_holder_number_table(db_helper):
        """股东户数表"""
        sql = """
        CREATE TABLE IF NOT EXISTS t_holder_number (
            id BIGINT PRIMARY KEY AUTO_INCREMENT,
            ts_code VARCHAR(10) NOT NULL COMMENT '股票代码',
            ann_date DATE NOT NULL COMMENT '公告日期',
            end_date DATE NOT NULL COMMENT '截止日期',
            holder_num INT COMMENT '股东户数',
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            UNIQUE KEY uk_code_end_date (ts_code, end_date),
            INDEX idx_ts_code (ts_code),
            INDEX idx_end_date (end_date),
            INDEX idx_ann_date (ann_date)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='股东户数表'
        """
        db_helper.execute_sql(sql)
        
    @staticmethod
    def create_share_float_table(db_helper):
        """限售解禁表"""
        sql = """
        CREATE TABLE IF NOT EXISTS t_share_float (
            id BIGINT PRIMARY KEY AUTO_INCREMENT,
            ts_code VARCHAR(10) NOT NULL COMMENT '股票代码',
            ann_date DATE NOT NULL COMMENT '公告日期',
            float_date DATE NOT NULL COMMENT '解禁日期',
            float_share DECIMAL(20,2) COMMENT '解禁数量(万股)',
            float_ratio DECIMAL(10,4) COMMENT '解禁数量占总股本比例',
            holder_name VARCHAR(200) COMMENT '股东名称',
            share_type VARCHAR(50) COMMENT '股份类型',
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            UNIQUE KEY uk_code_float_date_holder (ts_code, float_date, holder_name),
            INDEX idx_ts_code (ts_code),
            INDEX idx_float_date (float_date),
            INDEX idx_ann_date (ann_date)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='限售解禁表'
        """
        db_helper.execute_sql(sql)
        
    # ===================公告类信息表===================
    
    @staticmethod
    def create_dividend_table(db_helper):
        """分红送股表"""
        sql = """
        CREATE TABLE IF NOT EXISTS t_dividend (
            id BIGINT PRIMARY KEY AUTO_INCREMENT,
            ts_code VARCHAR(10) NOT NULL COMMENT '股票代码',
            end_date DATE NOT NULL COMMENT '分红年度',
            ann_date DATE NOT NULL COMMENT '预案公告日',
            div_proc VARCHAR(20) COMMENT '实施进度',
            stk_div DECIMAL(10,4) COMMENT '每股送红股数',
            stk_bo_rate DECIMAL(10,4) COMMENT '每股送红股比例',
            stk_co_rate DECIMAL(10,4) COMMENT '每股转增比例',
            cash_div DECIMAL(10,4) COMMENT '每股分红(税前)',
            cash_div_tax DECIMAL(10,4) COMMENT '每股分红(税后)',
            record_date DATE COMMENT '股权登记日',
            ex_date DATE COMMENT '除权除息日',
            pay_date DATE COMMENT '派息日',
            div_listdate DATE COMMENT '红股上市日',
            imp_ann_date DATE COMMENT '实施公告日',
            base_date DATE COMMENT '基准日',
            base_share DECIMAL(20,2) COMMENT '基准股本(万)',
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            UNIQUE KEY uk_code_end_ann (ts_code, end_date, ann_date),
            INDEX idx_ts_code (ts_code),
            INDEX idx_end_date (end_date),
            INDEX idx_ex_date (ex_date)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='分红送股表'
        """
        db_helper.execute_sql(sql)
        
    @staticmethod
    def create_repurchase_table(db_helper):
        """股票回购表"""
        sql = """
        CREATE TABLE IF NOT EXISTS t_repurchase (
            id BIGINT PRIMARY KEY AUTO_INCREMENT,
            ts_code VARCHAR(10) NOT NULL COMMENT '股票代码',
            ann_date DATE NOT NULL COMMENT '公告日期',
            end_date DATE COMMENT '截止日期',
            proc VARCHAR(20) COMMENT '进展',
            exp_date DATE COMMENT '过期日期',
            vol DECIMAL(20,2) COMMENT '回购数量(万股)',
            amount DECIMAL(20,2) COMMENT '回购金额(万元)',
            high_limit DECIMAL(10,2) COMMENT '回购最高价',
            low_limit DECIMAL(10,2) COMMENT '回购最低价',
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            UNIQUE KEY uk_code_ann_date (ts_code, ann_date),
            INDEX idx_ts_code (ts_code),
            INDEX idx_ann_date (ann_date),
            INDEX idx_end_date (end_date)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='股票回购表'
        """
        db_helper.execute_sql(sql)
        
    # ===================行情扩展表===================
    
    @staticmethod
    def create_suspend_table(db_helper):
        """停复牌表"""
        sql = """
        CREATE TABLE IF NOT EXISTS t_suspend (
            id BIGINT PRIMARY KEY AUTO_INCREMENT,
            ts_code VARCHAR(10) NOT NULL COMMENT '股票代码',
            suspend_date DATE NOT NULL COMMENT '停牌日期',
            resume_date DATE COMMENT '复牌日期',
            ann_date DATE COMMENT '公告日期',
            suspend_reason VARCHAR(200) COMMENT '停牌原因',
            reason_type VARCHAR(50) COMMENT '停牌原因类别',
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            UNIQUE KEY uk_code_suspend_date (ts_code, suspend_date),
            INDEX idx_ts_code (ts_code),
            INDEX idx_suspend_date (suspend_date),
            INDEX idx_resume_date (resume_date)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='停复牌表'
        """
        db_helper.execute_sql(sql)
        
    @staticmethod
    def create_limit_price_table(db_helper):
        """涨跌停价格表"""
        sql = """
        CREATE TABLE IF NOT EXISTS t_limit_price (
            id BIGINT PRIMARY KEY AUTO_INCREMENT,
            ts_code VARCHAR(10) NOT NULL COMMENT '股票代码',
            trade_date DATE NOT NULL COMMENT '交易日期',
            up_limit DECIMAL(10,2) COMMENT '涨停价',
            down_limit DECIMAL(10,2) COMMENT '跌停价',
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            UNIQUE KEY uk_code_date (ts_code, trade_date),
            INDEX idx_ts_code (ts_code),
            INDEX idx_trade_date (trade_date)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='涨跌停价格表'
        """
        db_helper.execute_sql(sql)
        
    @staticmethod
    def create_concept_table(db_helper):
        """概念分类表"""
        sql = """
        CREATE TABLE IF NOT EXISTS t_concept (
            id BIGINT PRIMARY KEY AUTO_INCREMENT,
            code VARCHAR(20) NOT NULL COMMENT '概念分类ID',
            name VARCHAR(100) NOT NULL COMMENT '概念分类名称',
            src VARCHAR(20) COMMENT '来源',
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            UNIQUE KEY uk_code (code),
            INDEX idx_name (name)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='概念分类表'
        """
        db_helper.execute_sql(sql)
        
    @staticmethod
    def create_concept_detail_table(db_helper):
        """概念股明细表"""
        sql = """
        CREATE TABLE IF NOT EXISTS t_concept_detail (
            id BIGINT PRIMARY KEY AUTO_INCREMENT,
            concept_code VARCHAR(20) NOT NULL COMMENT '概念代码',
            concept_name VARCHAR(100) COMMENT '概念名称',
            ts_code VARCHAR(10) NOT NULL COMMENT '股票代码',
            name VARCHAR(50) COMMENT '股票名称',
            in_date DATE COMMENT '纳入日期',
            out_date DATE COMMENT '剔除日期',
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            UNIQUE KEY uk_concept_stock (concept_code, ts_code),
            INDEX idx_concept_code (concept_code),
            INDEX idx_ts_code (ts_code)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='概念股明细表'
        """
        db_helper.execute_sql(sql)
        
    # ===================宏观经济表===================
    
    @staticmethod
    def create_money_flow_hsgt_table(db_helper):
        """沪深港通资金流向表"""
        sql = """
        CREATE TABLE IF NOT EXISTS t_money_flow_hsgt (
            id BIGINT PRIMARY KEY AUTO_INCREMENT,
            trade_date DATE NOT NULL COMMENT '交易日期',
            ggt_ss DECIMAL(20,2) COMMENT '港股通(上海)当日成交金额(万元)',
            ggt_sz DECIMAL(20,2) COMMENT '港股通(深圳)当日成交金额(万元)',
            hgt DECIMAL(20,2) COMMENT '沪股通当日成交金额(万元)',
            sgt DECIMAL(20,2) COMMENT '深股通当日成交金额(万元)',
            north_money DECIMAL(20,2) COMMENT '北向资金(万元)',
            south_money DECIMAL(20,2) COMMENT '南向资金(万元)',
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            PRIMARY KEY (trade_date),
            INDEX idx_trade_date (trade_date)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='沪深港通资金流向表'
        """
        db_helper.execute_sql(sql)
        
    @staticmethod
    def create_margin_detail_table(db_helper):
        """融资融券明细表"""
        sql = """
        CREATE TABLE IF NOT EXISTS t_margin_detail (
            id BIGINT PRIMARY KEY AUTO_INCREMENT,
            trade_date DATE NOT NULL COMMENT '交易日期',
            ts_code VARCHAR(10) NOT NULL COMMENT '股票代码',
            rzye DECIMAL(20,2) COMMENT '融资余额(万元)',
            rqye DECIMAL(20,2) COMMENT '融券余额(万元)',
            rzmre DECIMAL(20,2) COMMENT '融资买入额(万元)',
            rqmcl DECIMAL(20,2) COMMENT '融券卖出量(股)',
            rzche DECIMAL(20,2) COMMENT '融资偿还额(万元)',
            rqchl DECIMAL(20,2) COMMENT '融券偿还量(股)',
            rqyl DECIMAL(20,2) COMMENT '融券余量(股)',
            rzrqye DECIMAL(20,2) COMMENT '融资融券余额(万元)',
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            UNIQUE KEY uk_date_code (trade_date, ts_code),
            INDEX idx_ts_code (ts_code),
            INDEX idx_trade_date (trade_date),
            INDEX idx_rzrqye (rzrqye)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='融资融券明细表'
        """
        db_helper.execute_sql(sql)
        
    @staticmethod
    def create_macro_indicators_table(db_helper):
        """宏观经济指标表"""
        sql = """
        CREATE TABLE IF NOT EXISTS t_macro_indicators (
            id BIGINT PRIMARY KEY AUTO_INCREMENT,
            m DATE NOT NULL COMMENT '月份',
            indicator_name VARCHAR(50) NOT NULL COMMENT '指标名称',
            indicator_code VARCHAR(20) NOT NULL COMMENT '指标代码',
            value DECIMAL(15,4) COMMENT '指标值',
            unit VARCHAR(20) COMMENT '单位',
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            UNIQUE KEY uk_month_indicator (m, indicator_code),
            INDEX idx_m (m),
            INDEX idx_indicator_code (indicator_code)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='宏观经济指标表'
        """
        db_helper.execute_sql(sql)
        
    def execute_sql(self, sql):
        """执行SQL语句"""
        try:
            with self.engine.connect() as conn:
                conn.execute(text(sql))
                conn.commit()
        except Exception as e:
            logger.error(f"执行SQL失败: {e}")
            raise 