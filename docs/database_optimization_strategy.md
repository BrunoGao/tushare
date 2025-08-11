# 投资策略系统数据库优化方案

## 概述

基于我们实现的市场主流投资策略，本文档详细介绍数据库设计优化方案、TuShare数据获取策略，以及相应的性能优化措施。

## 1. 数据库架构设计

### 1.1 核心数据表设计

#### 1.1.1 股票基础信息表 (stock_basic)
```sql
CREATE TABLE stock_basic (
    stock_code VARCHAR(10) PRIMARY KEY,    -- 股票代码
    stock_name VARCHAR(50) NOT NULL,       -- 股票名称
    industry VARCHAR(50),                  -- 所属行业
    sector VARCHAR(50),                    -- 板块
    market VARCHAR(10),                    -- 市场(主板/创业板/科创板)
    list_date DATE,                        -- 上市日期
    delist_date DATE,                      -- 退市日期
    is_active BOOLEAN DEFAULT TRUE,        -- 是否活跃
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    
    INDEX idx_industry (industry),
    INDEX idx_sector (sector),
    INDEX idx_market (market),
    INDEX idx_active (is_active),
    INDEX idx_list_date (list_date)
);
```

#### 1.1.2 日线行情数据表 (stock_daily)
```sql
CREATE TABLE stock_daily (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    stock_code VARCHAR(10) NOT NULL,       -- 股票代码
    trade_date DATE NOT NULL,              -- 交易日期
    open_price DECIMAL(10,3),              -- 开盘价
    high_price DECIMAL(10,3),              -- 最高价
    low_price DECIMAL(10,3),               -- 最低价
    close_price DECIMAL(10,3),             -- 收盘价
    pre_close DECIMAL(10,3),               -- 前收盘价
    change_amt DECIMAL(10,3),              -- 涨跌额
    change_pct DECIMAL(6,3),               -- 涨跌幅
    volume BIGINT,                         -- 成交量(手)
    amount DECIMAL(15,2),                  -- 成交额(千元)
    turnover_rate DECIMAL(6,3),           -- 换手率
    volume_ratio DECIMAL(6,3),             -- 量比
    pe DECIMAL(8,2),                       -- 市盈率
    pb DECIMAL(6,3),                       -- 市净率
    total_share BIGINT,                    -- 总股本
    float_share BIGINT,                    -- 流通股本
    free_share BIGINT,                     -- 自由流通股本
    total_mv DECIMAL(15,2),                -- 总市值
    circ_mv DECIMAL(15,2),                 -- 流通市值
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    
    UNIQUE KEY uk_stock_date (stock_code, trade_date),
    INDEX idx_trade_date (trade_date),
    INDEX idx_stock_code (stock_code),
    INDEX idx_volume (volume),
    INDEX idx_amount (amount),
    INDEX idx_change_pct (change_pct)
) PARTITION BY RANGE (YEAR(trade_date)) (
    PARTITION p2020 VALUES LESS THAN (2021),
    PARTITION p2021 VALUES LESS THAN (2022),
    PARTITION p2022 VALUES LESS THAN (2023),
    PARTITION p2023 VALUES LESS THAN (2024),
    PARTITION p2024 VALUES LESS THAN (2025),
    PARTITION p2025 VALUES LESS THAN (2026),
    PARTITION pmax VALUES LESS THAN MAXVALUE
);
```

#### 1.1.3 技术指标缓存表 (technical_indicators)
```sql
CREATE TABLE technical_indicators (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    stock_code VARCHAR(10) NOT NULL,
    trade_date DATE NOT NULL,
    
    -- 移动平均线
    ma5 DECIMAL(10,3),
    ma10 DECIMAL(10,3),
    ma20 DECIMAL(10,3),
    ma30 DECIMAL(10,3),
    ma60 DECIMAL(10,3),
    ma120 DECIMAL(10,3),
    ma250 DECIMAL(10,3),
    
    -- RSI指标
    rsi6 DECIMAL(6,2),
    rsi12 DECIMAL(6,2),
    rsi24 DECIMAL(6,2),
    
    -- MACD指标
    macd_dif DECIMAL(8,4),
    macd_dea DECIMAL(8,4),
    macd_macd DECIMAL(8,4),
    
    -- KDJ指标
    kdj_k DECIMAL(6,2),
    kdj_d DECIMAL(6,2),
    kdj_j DECIMAL(6,2),
    
    -- 布林带
    boll_upper DECIMAL(10,3),
    boll_mid DECIMAL(10,3),
    boll_lower DECIMAL(10,3),
    
    -- 其他指标
    atr DECIMAL(8,4),
    cci DECIMAL(8,2),
    williams_r DECIMAL(6,2),
    obv BIGINT,
    vwap DECIMAL(10,3),
    
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    
    UNIQUE KEY uk_stock_date (stock_code, trade_date),
    INDEX idx_trade_date (trade_date),
    INDEX idx_rsi (rsi6, rsi12, rsi24),
    INDEX idx_kdj (kdj_k, kdj_d, kdj_j)
) PARTITION BY RANGE (YEAR(trade_date)) (
    PARTITION p2020 VALUES LESS THAN (2021),
    PARTITION p2021 VALUES LESS THAN (2022),
    PARTITION p2022 VALUES LESS THAN (2023),
    PARTITION p2023 VALUES LESS THAN (2024),
    PARTITION p2024 VALUES LESS THAN (2025),
    PARTITION p2025 VALUES LESS THAN (2026),
    PARTITION pmax VALUES LESS THAN MAXVALUE
);
```

#### 1.1.4 财务数据表 (financial_data)
```sql
CREATE TABLE financial_data (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    stock_code VARCHAR(10) NOT NULL,
    end_date DATE NOT NULL,                -- 报告期
    ann_date DATE,                         -- 公告日期
    report_type VARCHAR(10),               -- 报告类型(年报/半年报/季报)
    
    -- 资产负债表
    total_assets DECIMAL(15,2),            -- 总资产
    total_hldr_eqy_exc_min_int DECIMAL(15,2), -- 股东权益合计(不含少数股东权益)
    total_liab DECIMAL(15,2),              -- 负债合计
    monetary_cap DECIMAL(15,2),            -- 货币资金
    accounts_receiv DECIMAL(15,2),         -- 应收账款
    inventories DECIMAL(15,2),             -- 存货
    fixed_assets DECIMAL(15,2),            -- 固定资产
    
    -- 利润表
    revenue DECIMAL(15,2),                 -- 营业收入
    operate_profit DECIMAL(15,2),          -- 营业利润
    total_profit DECIMAL(15,2),            -- 利润总额
    n_income DECIMAL(15,2),                -- 净利润
    n_income_attr_p DECIMAL(15,2),         -- 归属于母公司所有者的净利润
    ebit DECIMAL(15,2),                    -- 息税前利润
    ebitda DECIMAL(15,2),                  -- 息税折旧摊销前利润
    
    -- 现金流量表
    c_fr_sale_sg DECIMAL(15,2),            -- 销售商品、提供劳务收到的现金
    c_paid_goods_s DECIMAL(15,2),          -- 购买商品、接受劳务支付的现金
    n_cashflow_act DECIMAL(15,2),          -- 经营活动产生的现金流量净额
    n_cashflow_inv_act DECIMAL(15,2),      -- 投资活动产生的现金流量净额
    n_cashflow_fin_act DECIMAL(15,2),      -- 筹资活动产生的现金流量净额
    c_cash_equ_end_period DECIMAL(15,2),   -- 期末现金及现金等价物余额
    
    -- 财务比率
    roe DECIMAL(6,3),                      -- 净资产收益率
    roa DECIMAL(6,3),                      -- 总资产收益率
    gross_margin DECIMAL(6,3),             -- 毛利率
    net_margin DECIMAL(6,3),               -- 净利率
    debt_to_assets DECIMAL(6,3),           -- 资产负债率
    current_ratio DECIMAL(6,3),            -- 流动比率
    quick_ratio DECIMAL(6,3),              -- 速动比率
    
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    
    UNIQUE KEY uk_stock_enddate (stock_code, end_date),
    INDEX idx_end_date (end_date),
    INDEX idx_ann_date (ann_date),
    INDEX idx_roe (roe),
    INDEX idx_net_margin (net_margin),
    INDEX idx_debt_ratio (debt_to_assets)
);
```

#### 1.1.5 策略信号表 (strategy_signals)
```sql
CREATE TABLE strategy_signals (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    strategy_id VARCHAR(50) NOT NULL,      -- 策略ID
    stock_code VARCHAR(10) NOT NULL,       -- 股票代码
    signal_date DATE NOT NULL,             -- 信号日期
    signal_type ENUM('BUY', 'SELL', 'HOLD') NOT NULL, -- 信号类型
    signal_strength DECIMAL(3,2),          -- 信号强度(0-1)
    signal_price DECIMAL(10,3),            -- 信号价格
    
    -- 信号详情
    trigger_conditions JSON,               -- 触发条件详情
    indicator_values JSON,                 -- 指标值快照
    risk_metrics JSON,                     -- 风险指标
    
    -- 执行状态
    execution_status ENUM('PENDING', 'EXECUTED', 'CANCELLED', 'EXPIRED') DEFAULT 'PENDING',
    execution_price DECIMAL(10,3),         -- 实际执行价格
    execution_time TIMESTAMP NULL,         -- 执行时间
    
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    
    INDEX idx_strategy_date (strategy_id, signal_date),
    INDEX idx_stock_date (stock_code, signal_date),
    INDEX idx_signal_type (signal_type),
    INDEX idx_execution_status (execution_status),
    INDEX idx_signal_strength (signal_strength)
);
```

#### 1.1.6 回测结果表 (backtest_results)
```sql
CREATE TABLE backtest_results (
    id VARCHAR(50) PRIMARY KEY,           -- 回测ID
    strategy_id VARCHAR(50) NOT NULL,     -- 策略ID
    stock_code VARCHAR(10),               -- 股票代码(可为空表示组合回测)
    start_date DATE NOT NULL,             -- 回测开始日期
    end_date DATE NOT NULL,               -- 回测结束日期
    
    -- 基础参数
    initial_capital DECIMAL(15,2),        -- 初始资金
    commission DECIMAL(6,4),              -- 手续费率
    slippage DECIMAL(6,4),                -- 滑点
    
    -- 收益指标
    total_return DECIMAL(8,4),            -- 总收益率
    annual_return DECIMAL(8,4),           -- 年化收益率
    excess_return DECIMAL(8,4),           -- 超额收益率
    
    -- 风险指标
    volatility DECIMAL(8,4),              -- 波动率
    max_drawdown DECIMAL(8,4),            -- 最大回撤
    sharpe_ratio DECIMAL(6,3),            -- 夏普比率
    sortino_ratio DECIMAL(6,3),           -- 索提诺比率
    calmar_ratio DECIMAL(6,3),            -- 卡玛比率
    
    -- 交易统计
    total_trades INT,                     -- 总交易次数
    win_trades INT,                       -- 盈利交易次数
    lose_trades INT,                      -- 亏损交易次数
    win_rate DECIMAL(5,3),                -- 胜率
    avg_win DECIMAL(8,4),                 -- 平均盈利
    avg_lose DECIMAL(8,4),                -- 平均亏损
    profit_factor DECIMAL(6,3),           -- 盈亏比
    
    -- 持仓统计
    max_position_size DECIMAL(5,3),       -- 最大仓位
    avg_holding_days DECIMAL(6,1),        -- 平均持股天数
    turnover_rate DECIMAL(6,3),           -- 换手率
    
    -- 详细数据
    equity_curve JSON,                    -- 净值曲线
    trade_records JSON,                   -- 交易记录
    daily_returns JSON,                   -- 日收益率
    position_details JSON,               -- 持仓明细
    
    -- 性能指标
    backtest_duration DECIMAL(8,2),      -- 回测耗时(秒)
    data_quality_score DECIMAL(3,2),     -- 数据质量评分
    
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    
    INDEX idx_strategy_date (strategy_id, start_date, end_date),
    INDEX idx_stock_date (stock_code, start_date, end_date),
    INDEX idx_annual_return (annual_return),
    INDEX idx_sharpe_ratio (sharpe_ratio),
    INDEX idx_max_drawdown (max_drawdown)
);
```

### 1.2 索引优化策略

#### 1.2.1 复合索引设计
```sql
-- 查询特定股票特定日期范围的行情数据
CREATE INDEX idx_stock_date_range ON stock_daily (stock_code, trade_date, close_price);

-- 按涨跌幅和成交量筛选股票
CREATE INDEX idx_change_volume ON stock_daily (trade_date, change_pct, volume);

-- 财务数据按指标筛选
CREATE INDEX idx_financial_metrics ON financial_data (end_date, roe, net_margin, debt_to_assets);

-- 技术指标组合查询
CREATE INDEX idx_tech_combo ON technical_indicators (stock_code, trade_date, rsi6, kdj_k, macd_dif);
```

#### 1.2.2 分区策略
```sql
-- 按年份分区提高查询性能
ALTER TABLE stock_daily PARTITION BY RANGE (YEAR(trade_date));
ALTER TABLE technical_indicators PARTITION BY RANGE (YEAR(trade_date));
ALTER TABLE strategy_signals PARTITION BY RANGE (YEAR(signal_date));
```

### 1.3 数据存储优化

#### 1.3.1 数据压缩
- 使用 `ROW_FORMAT=COMPRESSED` 压缩历史数据
- 对JSON字段使用 `COMPRESSION='zlib'`
- 定期归档超过3年的历史数据

#### 1.3.2 读写分离
```python
# 数据库连接配置
DATABASE_CONFIG = {
    'master': {
        'host': 'master-db.example.com',
        'port': 3306,
        'database': 'stock_analysis',
        'pool_size': 20,
        'max_overflow': 30
    },
    'slaves': [
        {
            'host': 'slave1-db.example.com',
            'port': 3306,
            'database': 'stock_analysis',
            'pool_size': 10,
            'max_overflow': 20
        },
        {
            'host': 'slave2-db.example.com', 
            'port': 3306,
            'database': 'stock_analysis',
            'pool_size': 10,
            'max_overflow': 20
        }
    ]
}
```

## 2. TuShare数据获取策略

### 2.1 数据获取架构

#### 2.1.1 数据获取任务调度
```python
from apscheduler.schedulers.blocking import BlockingScheduler
from apscheduler.jobstores.sqlalchemy import SQLAlchemyJobStore
import tushare as ts

class TuShareDataManager:
    def __init__(self, token: str):
        self.pro = ts.pro_api(token)
        self.scheduler = BlockingScheduler(
            jobstores={'default': SQLAlchemyJobStore(url='sqlite:///jobs.sqlite')}
        )
    
    def setup_data_sync_jobs(self):
        """设置数据同步任务"""
        # 股票基本信息 - 每日更新一次
        self.scheduler.add_job(
            func=self.sync_stock_basic,
            trigger='cron',
            hour=8, minute=30,
            id='sync_stock_basic'
        )
        
        # 日线行情数据 - 交易日收盘后更新
        self.scheduler.add_job(
            func=self.sync_daily_data,
            trigger='cron',
            hour=17, minute=0,
            id='sync_daily_data'
        )
        
        # 财务数据 - 每季度财报发布后更新
        self.scheduler.add_job(
            func=self.sync_financial_data,
            trigger='cron',
            day='1,15', hour=9, minute=0,
            id='sync_financial_data'
        )
        
        # 技术指标计算 - 每日收盘后计算
        self.scheduler.add_job(
            func=self.calculate_technical_indicators,
            trigger='cron',
            hour=18, minute=0,
            id='calc_indicators'
        )
```

#### 2.1.2 增量数据同步
```python
def sync_daily_data_incremental(self, last_trade_date: str = None):
    """增量同步日线数据"""
    if not last_trade_date:
        # 获取数据库中最新的交易日期
        last_trade_date = self.get_last_trade_date()
    
    # 获取股票列表
    stock_list = self.get_active_stocks()
    
    for stock_code in stock_list:
        try:
            # 获取增量数据
            df = self.pro.daily(
                ts_code=stock_code,
                start_date=last_trade_date,
                end_date=None
            )
            
            if not df.empty:
                # 数据预处理
                df = self.preprocess_daily_data(df)
                
                # 批量插入数据库
                self.bulk_insert_daily_data(df)
                
        except Exception as e:
            logger.error(f"同步 {stock_code} 数据失败: {e}")
            continue
```

### 2.2 数据获取API映射

#### 2.2.1 基础数据获取
```python
class TuShareAPIManager:
    """TuShare API管理器"""
    
    def __init__(self, token: str):
        self.pro = ts.pro_api(token)
        self.rate_limiter = RateLimiter(calls=200, period=60)  # API限流
    
    @rate_limiter.limit
    def get_stock_basic(self):
        """获取股票基本信息"""
        return self.pro.stock_basic(
            exchange='',
            list_status='L',
            fields='ts_code,symbol,name,area,industry,market,list_date'
        )
    
    @rate_limiter.limit
    def get_daily_data(self, ts_code: str, start_date: str, end_date: str):
        """获取日线行情数据"""
        return self.pro.daily(
            ts_code=ts_code,
            start_date=start_date,
            end_date=end_date,
            fields='ts_code,trade_date,open,high,low,close,pre_close,change,pct_chg,vol,amount'
        )
    
    @rate_limiter.limit
    def get_daily_basic(self, trade_date: str):
        """获取每日基本面数据"""
        return self.pro.daily_basic(
            trade_date=trade_date,
            fields='ts_code,trade_date,close,turnover_rate,volume_ratio,pe,pb,ps,dv_ratio,dv_ttm,total_share,float_share,free_share,total_mv,circ_mv'
        )
    
    @rate_limiter.limit
    def get_financial_data(self, ts_code: str, period: str = ''):
        """获取财务数据"""
        # 资产负债表
        balancesheet = self.pro.balancesheet(
            ts_code=ts_code,
            period=period,
            fields='ts_code,end_date,report_type,total_assets,total_hldr_eqy_exc_min_int,total_liab'
        )
        
        # 利润表
        income = self.pro.income(
            ts_code=ts_code,
            period=period,
            fields='ts_code,end_date,report_type,revenue,operate_profit,total_profit,n_income,n_income_attr_p'
        )
        
        # 现金流量表
        cashflow = self.pro.cashflow(
            ts_code=ts_code,
            period=period,
            fields='ts_code,end_date,report_type,n_cashflow_act,n_cashflow_inv_act,n_cashflow_fin_act'
        )
        
        return balancesheet, income, cashflow
    
    @rate_limiter.limit
    def get_financial_indicator(self, ts_code: str, period: str = ''):
        """获取财务指标"""
        return self.pro.fina_indicator(
            ts_code=ts_code,
            period=period,
            fields='ts_code,end_date,roe,roa,gross_margin,net_margin,debt_to_assets,current_ratio,quick_ratio'
        )
```

#### 2.2.2 特殊数据获取策略
```python
def get_industry_classification(self):
    """获取行业分类数据"""
    # 申万行业分类
    sw_l1 = self.pro.index_classify(level='L1', src='SW2021')
    sw_l2 = self.pro.index_classify(level='L2', src='SW2021')
    
    # 中证行业分类  
    csi_l1 = self.pro.index_classify(level='L1', src='CSI')
    
    return sw_l1, sw_l2, csi_l1

def get_market_sentiment_data(self, trade_date: str):
    """获取市场情绪数据"""
    # 限售股解禁
    share_float = self.pro.share_float(trade_date=trade_date)
    
    # 融资融券数据
    margin = self.pro.margin(trade_date=trade_date)
    
    # 北向资金
    hsgt_top10 = self.pro.hsgt_top10(trade_date=trade_date)
    
    return share_float, margin, hsgt_top10

def get_macro_economic_data(self):
    """获取宏观经济数据"""
    # GDP数据
    gdp = self.pro.cn_gdp()
    
    # CPI数据
    cpi = self.pro.cn_cpi()
    
    # PMI数据
    pmi = self.pro.cn_pmi()
    
    # 利率数据
    shibor = self.pro.shibor(start_date='20200101')
    
    return gdp, cpi, pmi, shibor
```

### 2.3 数据质量保证

#### 2.3.1 数据验证
```python
class DataValidator:
    """数据验证器"""
    
    @staticmethod
    def validate_daily_data(df: pd.DataFrame) -> tuple:
        """验证日线数据质量"""
        issues = []
        
        # 检查必要字段
        required_fields = ['ts_code', 'trade_date', 'open', 'high', 'low', 'close', 'vol']
        missing_fields = [field for field in required_fields if field not in df.columns]
        if missing_fields:
            issues.append(f"缺少必要字段: {missing_fields}")
        
        # 检查价格合理性
        if not df.empty:
            # 检查是否有负价格
            price_fields = ['open', 'high', 'low', 'close']
            for field in price_fields:
                if field in df.columns and (df[field] <= 0).any():
                    issues.append(f"{field} 存在负值或零值")
            
            # 检查高低价关系
            if 'high' in df.columns and 'low' in df.columns:
                if (df['high'] < df['low']).any():
                    issues.append("存在最高价小于最低价的异常数据")
            
            # 检查开收盘价合理性
            if all(field in df.columns for field in ['open', 'high', 'low', 'close']):
                invalid_ohlc = (
                    (df['open'] > df['high']) | 
                    (df['open'] < df['low']) |
                    (df['close'] > df['high']) | 
                    (df['close'] < df['low'])
                ).any()
                if invalid_ohlc:
                    issues.append("存在开收盘价超出最高最低价范围的数据")
        
        # 检查数据完整性
        completeness = 1 - (df.isnull().sum().sum() / (len(df) * len(df.columns)))
        
        return len(issues) == 0, issues, completeness
    
    @staticmethod
    def validate_financial_data(df: pd.DataFrame) -> tuple:
        """验证财务数据质量"""
        issues = []
        
        # 检查必要字段
        required_fields = ['ts_code', 'end_date', 'report_type']
        missing_fields = [field for field in required_fields if field not in df.columns]
        if missing_fields:
            issues.append(f"缺少必要字段: {missing_fields}")
        
        # 检查财务数据合理性
        if not df.empty:
            # 检查资产负债表平衡
            if all(field in df.columns for field in ['total_assets', 'total_liab', 'total_hldr_eqy_exc_min_int']):
                balance_check = abs(df['total_assets'] - df['total_liab'] - df['total_hldr_eqy_exc_min_int']) > 1000
                if balance_check.any():
                    issues.append("资产负债表不平衡")
        
        completeness = 1 - (df.isnull().sum().sum() / (len(df) * len(df.columns)))
        
        return len(issues) == 0, issues, completeness
```

#### 2.3.2 异常数据处理
```python
class DataCleaner:
    """数据清洗器"""
    
    @staticmethod
    def clean_daily_data(df: pd.DataFrame) -> pd.DataFrame:
        """清洗日线数据"""
        df = df.copy()
        
        # 删除重复数据
        df = df.drop_duplicates(subset=['ts_code', 'trade_date'])
        
        # 处理异常价格
        price_fields = ['open', 'high', 'low', 'close', 'pre_close']
        for field in price_fields:
            if field in df.columns:
                # 删除负价格和零价格
                df = df[df[field] > 0]
                
                # 处理异常涨跌幅(>50%认为异常)
                if field == 'close' and 'pre_close' in df.columns:
                    change_pct = abs((df[field] - df['pre_close']) / df['pre_close'])
                    df = df[change_pct <= 0.5]  # 过滤掉涨跌幅超过50%的数据
        
        # 处理成交量异常
        if 'vol' in df.columns:
            # 成交量为负的设为0
            df.loc[df['vol'] < 0, 'vol'] = 0
        
        # 按日期排序
        if 'trade_date' in df.columns:
            df = df.sort_values('trade_date')
        
        return df
    
    @staticmethod
    def fill_missing_data(df: pd.DataFrame, method: str = 'forward') -> pd.DataFrame:
        """填充缺失数据"""
        df = df.copy()
        
        if method == 'forward':
            # 前向填充
            df = df.fillna(method='ffill')
        elif method == 'backward':
            # 后向填充
            df = df.fillna(method='bfill')
        elif method == 'interpolate':
            # 线性插值
            numeric_columns = df.select_dtypes(include=[np.number]).columns
            df[numeric_columns] = df[numeric_columns].interpolate()
        
        return df
```

## 3. 性能优化策略

### 3.1 查询优化

#### 3.1.1 SQL优化示例
```sql
-- 优化前：获取最近20个交易日RSI小于30的股票
SELECT s.stock_code, s.stock_name, t.trade_date, t.rsi6
FROM stock_basic s
JOIN technical_indicators t ON s.stock_code = t.stock_code
WHERE t.trade_date >= DATE_SUB(CURDATE(), INTERVAL 30 DAY)
  AND t.rsi6 < 30;

-- 优化后：使用索引和限制条件
SELECT s.stock_code, s.stock_name, t.trade_date, t.rsi6
FROM technical_indicators t
JOIN stock_basic s ON t.stock_code = s.stock_code
WHERE t.trade_date >= DATE_SUB(CURDATE(), INTERVAL 30 DAY)
  AND t.rsi6 < 30
  AND s.is_active = 1
ORDER BY t.trade_date DESC, t.rsi6 ASC
LIMIT 100;
```

#### 3.1.2 缓存策略
```python
import redis
from functools import wraps

class CacheManager:
    def __init__(self):
        self.redis_client = redis.StrictRedis(
            host='localhost', 
            port=6379, 
            db=0,
            decode_responses=True
        )
    
    def cache_result(self, expiration: int = 3600):
        """缓存查询结果装饰器"""
        def decorator(func):
            @wraps(func)
            def wrapper(*args, **kwargs):
                # 生成缓存key
                cache_key = f"{func.__name__}:{hash(str(args) + str(kwargs))}"
                
                # 尝试从缓存获取
                cached_result = self.redis_client.get(cache_key)
                if cached_result:
                    return json.loads(cached_result)
                
                # 执行查询
                result = func(*args, **kwargs)
                
                # 缓存结果
                self.redis_client.setex(
                    cache_key, 
                    expiration, 
                    json.dumps(result, default=str)
                )
                
                return result
            return wrapper
        return decorator
    
    @cache_result(expiration=1800)  # 30分钟缓存
    def get_stock_technical_indicators(self, stock_code: str, start_date: str, end_date: str):
        """获取技术指标数据（带缓存）"""
        # 数据库查询逻辑
        pass
```

### 3.2 数据预计算

#### 3.2.1 技术指标预计算
```python
class TechnicalIndicatorCalculator:
    """技术指标计算器"""
    
    def __init__(self):
        self.batch_size = 1000
    
    def calculate_all_indicators(self, stock_codes: list = None, trade_date: str = None):
        """批量计算技术指标"""
        if not stock_codes:
            stock_codes = self.get_active_stock_codes()
        
        if not trade_date:
            trade_date = self.get_last_trade_date()
        
        # 分批处理
        for i in range(0, len(stock_codes), self.batch_size):
            batch_codes = stock_codes[i:i + self.batch_size]
            self.calculate_batch_indicators(batch_codes, trade_date)
    
    def calculate_batch_indicators(self, stock_codes: list, trade_date: str):
        """批量计算指标"""
        # 获取基础数据
        daily_data = self.get_daily_data_batch(stock_codes, trade_date)
        
        indicators_data = []
        
        for stock_code in stock_codes:
            stock_data = daily_data[daily_data['stock_code'] == stock_code]
            if len(stock_data) < 250:  # 数据不足一年
                continue
            
            # 计算各种指标
            indicators = {
                'stock_code': stock_code,
                'trade_date': trade_date,
                # 移动平均线
                'ma5': stock_data['close'].rolling(5).mean().iloc[-1],
                'ma10': stock_data['close'].rolling(10).mean().iloc[-1],
                'ma20': stock_data['close'].rolling(20).mean().iloc[-1],
                'ma30': stock_data['close'].rolling(30).mean().iloc[-1],
                'ma60': stock_data['close'].rolling(60).mean().iloc[-1],
                'ma120': stock_data['close'].rolling(120).mean().iloc[-1],
                'ma250': stock_data['close'].rolling(250).mean().iloc[-1],
                
                # RSI
                'rsi6': self.calculate_rsi(stock_data['close'], 6),
                'rsi12': self.calculate_rsi(stock_data['close'], 12),
                'rsi24': self.calculate_rsi(stock_data['close'], 24),
                
                # MACD
                **self.calculate_macd(stock_data['close']),
                
                # KDJ
                **self.calculate_kdj(stock_data['high'], stock_data['low'], stock_data['close']),
                
                # 布林带
                **self.calculate_bollinger_bands(stock_data['close']),
                
                # 其他指标
                'atr': self.calculate_atr(stock_data['high'], stock_data['low'], stock_data['close']),
                'obv': self.calculate_obv(stock_data['close'], stock_data['volume']),
                'vwap': self.calculate_vwap(stock_data['close'], stock_data['volume'])
            }
            
            indicators_data.append(indicators)
        
        # 批量插入数据库
        if indicators_data:
            self.bulk_insert_indicators(indicators_data)
```

### 3.3 监控和告警

#### 3.3.1 性能监控
```python
import time
import logging
from functools import wraps

class PerformanceMonitor:
    """性能监控器"""
    
    def __init__(self):
        self.logger = logging.getLogger('performance')
        self.metrics = {}
    
    def monitor_execution_time(self, func_name: str = None):
        """监控函数执行时间"""
        def decorator(func):
            @wraps(func)
            def wrapper(*args, **kwargs):
                start_time = time.time()
                try:
                    result = func(*args, **kwargs)
                    execution_time = time.time() - start_time
                    
                    name = func_name or func.__name__
                    self.logger.info(f"{name} 执行时间: {execution_time:.2f}秒")
                    
                    # 记录性能指标
                    if name not in self.metrics:
                        self.metrics[name] = []
                    self.metrics[name].append(execution_time)
                    
                    return result
                    
                except Exception as e:
                    execution_time = time.time() - start_time
                    self.logger.error(f"{func.__name__} 执行失败 (耗时{execution_time:.2f}秒): {e}")
                    raise
                    
            return wrapper
        return decorator
    
    def get_performance_report(self):
        """获取性能报告"""
        report = {}
        for func_name, times in self.metrics.items():
            report[func_name] = {
                'calls': len(times),
                'avg_time': sum(times) / len(times),
                'max_time': max(times),
                'min_time': min(times)
            }
        return report
```

## 4. 数据备份和恢复

### 4.1 备份策略
```bash
#!/bin/bash
# 数据库备份脚本

BACKUP_DIR="/data/backup/mysql"
DATE=$(date +%Y%m%d_%H%M%S)
DB_NAME="stock_analysis"

# 创建备份目录
mkdir -p $BACKUP_DIR/$DATE

# 全量备份
mysqldump --single-transaction --routines --triggers --events \
  --master-data=2 --flush-logs \
  $DB_NAME > $BACKUP_DIR/$DATE/full_backup_$DATE.sql

# 压缩备份文件
gzip $BACKUP_DIR/$DATE/full_backup_$DATE.sql

# 删除7天前的备份
find $BACKUP_DIR -type d -mtime +7 -exec rm -rf {} \;

echo "备份完成: $BACKUP_DIR/$DATE/full_backup_$DATE.sql.gz"
```

### 4.2 数据恢复
```bash
#!/bin/bash
# 数据恢复脚本

BACKUP_FILE=$1
DB_NAME="stock_analysis"

if [ -z "$BACKUP_FILE" ]; then
    echo "用法: $0 <backup_file>"
    exit 1
fi

# 解压备份文件
if [[ $BACKUP_FILE == *.gz ]]; then
    gunzip -c $BACKUP_FILE | mysql $DB_NAME
else
    mysql $DB_NAME < $BACKUP_FILE
fi

echo "数据恢复完成"
```

## 5. 总结

本数据库优化方案包含了：

1. **完整的数据表设计** - 涵盖股票基础信息、行情数据、技术指标、财务数据、策略信号和回测结果
2. **索引优化策略** - 针对常用查询场景设计复合索引和分区策略
3. **TuShare数据获取** - 完整的数据同步、验证、清洗流程
4. **性能优化** - 包括查询优化、缓存策略、预计算等
5. **监控告警** - 性能监控和异常告警机制
6. **备份恢复** - 完整的数据备份和恢复策略

这套方案能够支撑大规模的投资策略回测和实时交易信号生成，为投资决策提供可靠的数据基础。