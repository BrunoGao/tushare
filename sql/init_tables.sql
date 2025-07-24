-- 创建数据库
CREATE DATABASE IF NOT EXISTS ljwx_stock DEFAULT CHARACTER SET = 'utf8mb4';
USE ljwx_stock;

-- 股票基本信息表
CREATE TABLE IF NOT EXISTS stock_basic (
    ts_code VARCHAR(20) PRIMARY KEY COMMENT '股票代码',
    symbol VARCHAR(10) NOT NULL COMMENT '股票简称代码',
    name VARCHAR(50) NOT NULL COMMENT '股票名称',
    area VARCHAR(20) COMMENT '地域',
    industry VARCHAR(50) COMMENT '所属行业',
    fullname VARCHAR(100) COMMENT '股票全称',
    enname VARCHAR(100) COMMENT '英文全称',
    market VARCHAR(20) COMMENT '市场类型',
    exchange VARCHAR(10) COMMENT '交易所代码',
    curr_type VARCHAR(10) COMMENT '交易货币',
    list_status VARCHAR(1) COMMENT '上市状态',
    list_date DATE COMMENT '上市日期',
    delist_date DATE COMMENT '退市日期',
    is_hs VARCHAR(1) COMMENT '是否沪深港通标的',
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    INDEX idx_industry (industry),
    INDEX idx_list_date (list_date),
    INDEX idx_exchange (exchange)
) COMMENT='股票基本信息表';

-- 创建按月分表的示例表(2023年1月)
CREATE TABLE IF NOT EXISTS stock_daily_202301 (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    ts_code VARCHAR(20) NOT NULL COMMENT '股票代码',
    trade_date DATE NOT NULL COMMENT '交易日期',
    open DECIMAL(10,3) COMMENT '开盘价',
    high DECIMAL(10,3) COMMENT '最高价',
    low DECIMAL(10,3) COMMENT '最低价',
    close DECIMAL(10,3) COMMENT '收盘价',
    pre_close DECIMAL(10,3) COMMENT '昨收价',
    `change` DECIMAL(10,3) COMMENT '涨跌额',
    pct_chg DECIMAL(8,4) COMMENT '涨跌幅',
    change_pct DECIMAL(8,4) COMMENT '涨跌幅(百分比)',
    vol BIGINT COMMENT '成交量(手)',
    amount DECIMAL(20,3) COMMENT '成交额(千元)',
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    UNIQUE KEY uk_code_date (ts_code, trade_date),
    INDEX idx_code (ts_code),
    INDEX idx_date (trade_date),
    INDEX idx_code_date (ts_code, trade_date)
) COMMENT='股票日线数据表-2023年1月';

-- 推荐结果表
CREATE TABLE IF NOT EXISTS recommend_result (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    ts_code VARCHAR(20) NOT NULL COMMENT '股票代码',
    name VARCHAR(50) COMMENT '股票名称',
    score DECIMAL(5,4) NOT NULL COMMENT '推荐分数(0-1)',
    recommend_date DATE NOT NULL COMMENT '推荐日期',
    strategy VARCHAR(50) NOT NULL COMMENT '推荐策略',
    reason TEXT COMMENT '推荐理由',
    rank_no INT COMMENT '排名',
    is_valid TINYINT DEFAULT 1 COMMENT '是否有效',
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    INDEX idx_date (recommend_date),
    INDEX idx_score (score DESC),
    INDEX idx_code_date (ts_code, recommend_date),
    INDEX idx_strategy_date (strategy, recommend_date)
) COMMENT='股票推荐结果表';

-- 技术指标表
CREATE TABLE IF NOT EXISTS technical_indicators (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    ts_code VARCHAR(20) NOT NULL COMMENT '股票代码',
    trade_date DATE NOT NULL COMMENT '交易日期',
    ma5 DECIMAL(10,3) COMMENT '5日均线',
    ma10 DECIMAL(10,3) COMMENT '10日均线',
    ma20 DECIMAL(10,3) COMMENT '20日均线',
    ma60 DECIMAL(10,3) COMMENT '60日均线',
    rsi DECIMAL(8,4) COMMENT 'RSI指标',
    macd DECIMAL(10,6) COMMENT 'MACD',
    kdj_k DECIMAL(8,4) COMMENT 'KDJ-K',
    kdj_d DECIMAL(8,4) COMMENT 'KDJ-D',
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    UNIQUE KEY uk_code_date (ts_code, trade_date),
    INDEX idx_code (ts_code),
    INDEX idx_date (trade_date)
) COMMENT='技术指标表';

-- 系统日志表
CREATE TABLE IF NOT EXISTS system_log (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    module VARCHAR(50) NOT NULL COMMENT '模块名称',
    operation VARCHAR(100) NOT NULL COMMENT '操作类型',
    status VARCHAR(20) NOT NULL COMMENT '状态',
    message TEXT COMMENT '日志信息',
    duration INT COMMENT '执行时长(秒)',
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    INDEX idx_module (module),
    INDEX idx_created_at (created_at),
    INDEX idx_status (status)
) COMMENT='系统操作日志表'; 