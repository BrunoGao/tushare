#!/usr/bin/env python3
"""
数据验证和清洗模块
确保TuShare数据的质量和准确性
"""

import pandas as pd
import numpy as np
import logging
from datetime import datetime, timedelta
from typing import Dict, List, Tuple, Optional, Any
from dataclasses import dataclass
from enum import Enum
import warnings

# 设置日志
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

class DataQualityLevel(Enum):
    """数据质量等级"""
    EXCELLENT = "excellent"  # 90-100%
    GOOD = "good"           # 80-89%
    FAIR = "fair"           # 70-79%
    POOR = "poor"           # 60-69%
    BAD = "bad"             # <60%

@dataclass
class ValidationResult:
    """验证结果"""
    is_valid: bool
    quality_score: float
    quality_level: DataQualityLevel
    issues: List[str]
    warnings: List[str]
    statistics: Dict[str, Any]
    cleaned_data: Optional[pd.DataFrame] = None

class DataValidator:
    """数据验证器"""
    
    def __init__(self):
        self.validation_rules = self._load_validation_rules()
        
    def _load_validation_rules(self) -> Dict[str, Any]:
        """加载验证规则"""
        return {
            'stock_basic': {
                'required_fields': ['ts_code', 'symbol', 'name'],
                'optional_fields': ['area', 'industry', 'market', 'list_date'],
                'format_rules': {
                    'ts_code': r'^\d{6}\.(SH|SZ)$',
                    'symbol': r'^\d{6}$'
                }
            },
            'daily': {
                'required_fields': ['ts_code', 'trade_date', 'open', 'high', 'low', 'close'],
                'optional_fields': ['pre_close', 'change', 'pct_chg', 'vol', 'amount'],
                'value_ranges': {
                    'open': (0.01, 10000),
                    'high': (0.01, 10000),
                    'low': (0.01, 10000),
                    'close': (0.01, 10000),
                    'pct_chg': (-20, 20),
                    'vol': (0, float('inf'))
                },
                'relationship_rules': [
                    {'name': 'high_low_relationship', 'rule': 'high >= low'},
                    {'name': 'ohlc_consistency', 'rule': 'low <= open <= high and low <= close <= high'}
                ]
            },
            'financial': {
                'required_fields': ['ts_code', 'end_date'],
                'optional_fields': ['ann_date', 'report_type', 'total_assets', 'revenue'],
                'logical_rules': [
                    {'name': 'balance_sheet_balance', 'rule': 'assets_balance_check'},
                    {'name': 'positive_revenue', 'rule': 'revenue >= 0'}
                ]
            }
        }
    
    def validate_stock_basic(self, df: pd.DataFrame) -> ValidationResult:
        """验证股票基础信息数据"""
        logger.info("开始验证股票基础信息数据...")
        
        issues = []
        warnings = []
        statistics = {}
        
        # 基础检查
        if df.empty:
            return ValidationResult(
                is_valid=False,
                quality_score=0.0,
                quality_level=DataQualityLevel.BAD,
                issues=["数据集为空"],
                warnings=[],
                statistics={}
            )
        
        rules = self.validation_rules['stock_basic']
        
        # 1. 必要字段检查
        missing_fields = self._check_required_fields(df, rules['required_fields'])
        if missing_fields:
            issues.append(f"缺少必要字段: {missing_fields}")
        
        # 2. 数据格式检查
        format_issues = self._check_format_rules(df, rules.get('format_rules', {}))
        issues.extend(format_issues)
        
        # 3. 重复数据检查
        duplicates = df.duplicated(subset=['ts_code']).sum()
        if duplicates > 0:
            issues.append(f"发现{duplicates}条重复数据")
            statistics['duplicate_count'] = duplicates
        
        # 4. 数据完整性检查
        completeness = self._calculate_completeness(df)
        statistics['completeness'] = completeness
        
        if completeness < 0.8:
            warnings.append(f"数据完整性较低: {completeness:.2%}")
        
        # 5. 特殊值检查
        if 'name' in df.columns:
            empty_names = df['name'].isna().sum()
            if empty_names > 0:
                warnings.append(f"{empty_names}只股票名称为空")
        
        # 计算质量分数
        quality_score = self._calculate_quality_score(len(issues), len(warnings), completeness)
        quality_level = self._get_quality_level(quality_score)
        
        # 生成统计信息
        statistics.update({
            'total_records': len(df),
            'valid_records': len(df) - duplicates,
            'issue_count': len(issues),
            'warning_count': len(warnings)
        })
        
        is_valid = len(issues) == 0
        
        logger.info(f"股票基础信息验证完成: 质量分数={quality_score:.2f}, 等级={quality_level.value}")
        
        return ValidationResult(
            is_valid=is_valid,
            quality_score=quality_score,
            quality_level=quality_level,
            issues=issues,
            warnings=warnings,
            statistics=statistics
        )
    
    def validate_daily_data(self, df: pd.DataFrame) -> ValidationResult:
        """验证日线行情数据"""
        logger.info("开始验证日线行情数据...")
        
        issues = []
        warnings = []
        statistics = {}
        
        if df.empty:
            return ValidationResult(
                is_valid=False,
                quality_score=0.0,
                quality_level=DataQualityLevel.BAD,
                issues=["数据集为空"],
                warnings=[],
                statistics={}
            )
        
        rules = self.validation_rules['daily']
        
        # 1. 必要字段检查
        missing_fields = self._check_required_fields(df, rules['required_fields'])
        if missing_fields:
            issues.append(f"缺少必要字段: {missing_fields}")
        
        # 2. 数值范围检查
        range_issues = self._check_value_ranges(df, rules.get('value_ranges', {}))
        issues.extend(range_issues)
        
        # 3. 价格关系检查
        relationship_issues = self._check_price_relationships(df)
        issues.extend(relationship_issues)
        
        # 4. 时间序列检查
        time_issues = self._check_time_series(df)
        issues.extend(time_issues)
        warnings.extend(self._check_time_series_warnings(df))
        
        # 5. 异常值检查
        outlier_warnings = self._detect_outliers(df)
        warnings.extend(outlier_warnings)
        
        # 6. 数据完整性检查
        completeness = self._calculate_completeness(df)
        statistics['completeness'] = completeness
        
        # 7. 重复数据检查
        duplicates = df.duplicated(subset=['ts_code', 'trade_date']).sum()
        if duplicates > 0:
            issues.append(f"发现{duplicates}条重复数据")
            statistics['duplicate_count'] = duplicates
        
        # 计算质量分数
        quality_score = self._calculate_quality_score(len(issues), len(warnings), completeness)
        quality_level = self._get_quality_level(quality_score)
        
        # 生成统计信息
        statistics.update({
            'total_records': len(df),
            'date_range': self._get_date_range(df),
            'stock_count': df['ts_code'].nunique() if 'ts_code' in df.columns else 0,
            'issue_count': len(issues),
            'warning_count': len(warnings)
        })
        
        is_valid = len(issues) == 0
        
        logger.info(f"日线数据验证完成: 质量分数={quality_score:.2f}, 等级={quality_level.value}")
        
        return ValidationResult(
            is_valid=is_valid,
            quality_score=quality_score,
            quality_level=quality_level,
            issues=issues,
            warnings=warnings,
            statistics=statistics
        )
    
    def validate_financial_data(self, df: pd.DataFrame) -> ValidationResult:
        """验证财务数据"""
        logger.info("开始验证财务数据...")
        
        issues = []
        warnings = []
        statistics = {}
        
        if df.empty:
            return ValidationResult(
                is_valid=False,
                quality_score=0.0,
                quality_level=DataQualityLevel.BAD,
                issues=["数据集为空"],
                warnings=[],
                statistics={}
            )
        
        rules = self.validation_rules['financial']
        
        # 1. 必要字段检查
        missing_fields = self._check_required_fields(df, rules['required_fields'])
        if missing_fields:
            issues.append(f"缺少必要字段: {missing_fields}")
        
        # 2. 财务逻辑检查
        logical_issues = self._check_financial_logic(df)
        issues.extend(logical_issues)
        
        # 3. 数据完整性检查
        completeness = self._calculate_completeness(df)
        statistics['completeness'] = completeness
        
        # 4. 重复数据检查
        duplicates = df.duplicated(subset=['ts_code', 'end_date']).sum()
        if duplicates > 0:
            issues.append(f"发现{duplicates}条重复数据")
        
        # 5. 财务指标合理性检查
        ratio_warnings = self._check_financial_ratios(df)
        warnings.extend(ratio_warnings)
        
        # 计算质量分数
        quality_score = self._calculate_quality_score(len(issues), len(warnings), completeness)
        quality_level = self._get_quality_level(quality_score)
        
        # 生成统计信息
        statistics.update({
            'total_records': len(df),
            'company_count': df['ts_code'].nunique() if 'ts_code' in df.columns else 0,
            'report_periods': df['end_date'].nunique() if 'end_date' in df.columns else 0,
            'issue_count': len(issues),
            'warning_count': len(warnings)
        })
        
        is_valid = len(issues) == 0
        
        logger.info(f"财务数据验证完成: 质量分数={quality_score:.2f}, 等级={quality_level.value}")
        
        return ValidationResult(
            is_valid=is_valid,
            quality_score=quality_score,
            quality_level=quality_level,
            issues=issues,
            warnings=warnings,
            statistics=statistics
        )
    
    def _check_required_fields(self, df: pd.DataFrame, required_fields: List[str]) -> List[str]:
        """检查必要字段"""
        missing_fields = []
        for field in required_fields:
            if field not in df.columns:
                missing_fields.append(field)
        return missing_fields
    
    def _check_format_rules(self, df: pd.DataFrame, format_rules: Dict[str, str]) -> List[str]:
        """检查格式规则"""
        issues = []
        for field, pattern in format_rules.items():
            if field in df.columns:
                import re
                invalid_count = ~df[field].astype(str).str.match(pattern, na=False).sum()
                if invalid_count > 0:
                    issues.append(f"{field}字段有{invalid_count}条格式不正确的数据")
        return issues
    
    def _check_value_ranges(self, df: pd.DataFrame, value_ranges: Dict[str, Tuple[float, float]]) -> List[str]:
        """检查数值范围"""
        issues = []
        for field, (min_val, max_val) in value_ranges.items():
            if field in df.columns:
                out_of_range = ((df[field] < min_val) | (df[field] > max_val)).sum()
                if out_of_range > 0:
                    issues.append(f"{field}字段有{out_of_range}条数据超出合理范围[{min_val}, {max_val}]")
        return issues
    
    def _check_price_relationships(self, df: pd.DataFrame) -> List[str]:
        """检查价格关系"""
        issues = []
        
        if all(field in df.columns for field in ['high', 'low', 'open', 'close']):
            # 检查最高价 >= 最低价
            high_low_violation = (df['high'] < df['low']).sum()
            if high_low_violation > 0:
                issues.append(f"{high_low_violation}条记录的最高价小于最低价")
            
            # 检查开盘价和收盘价是否在最高最低价范围内
            ohlc_violation = (
                (df['open'] < df['low']) | (df['open'] > df['high']) |
                (df['close'] < df['low']) | (df['close'] > df['high'])
            ).sum()
            if ohlc_violation > 0:
                issues.append(f"{ohlc_violation}条记录的开盘价或收盘价超出最高最低价范围")
        
        return issues
    
    def _check_time_series(self, df: pd.DataFrame) -> List[str]:
        """检查时间序列"""
        issues = []
        
        if 'trade_date' in df.columns:
            try:
                # 转换日期格式
                if df['trade_date'].dtype == 'object':
                    df['trade_date'] = pd.to_datetime(df['trade_date'], errors='coerce')
                
                # 检查无效日期
                invalid_dates = df['trade_date'].isna().sum()
                if invalid_dates > 0:
                    issues.append(f"{invalid_dates}条记录的交易日期无效")
                
                # 检查未来日期
                future_dates = (df['trade_date'] > datetime.now()).sum()
                if future_dates > 0:
                    issues.append(f"{future_dates}条记录的日期为未来日期")
                    
            except Exception as e:
                issues.append(f"日期格式检查失败: {e}")
        
        return issues
    
    def _check_time_series_warnings(self, df: pd.DataFrame) -> List[str]:
        """检查时间序列警告"""
        warnings = []
        
        if 'trade_date' in df.columns and 'ts_code' in df.columns:
            # 检查数据缺口
            for stock_code in df['ts_code'].unique()[:10]:  # 只检查前10只股票
                stock_data = df[df['ts_code'] == stock_code].sort_values('trade_date')
                if len(stock_data) > 1:
                    date_diff = stock_data['trade_date'].diff().dt.days
                    large_gaps = (date_diff > 7).sum()  # 超过7天的缺口
                    if large_gaps > 0:
                        warnings.append(f"股票{stock_code}存在{large_gaps}个较大的数据缺口")
                        break  # 只报告一个示例
        
        return warnings
    
    def _detect_outliers(self, df: pd.DataFrame) -> List[str]:
        """检测异常值"""
        warnings = []
        
        # 检查价格异常变动
        if 'pct_chg' in df.columns:
            extreme_changes = (abs(df['pct_chg']) > 15).sum()  # 超过15%的变动
            if extreme_changes > 0:
                warnings.append(f"{extreme_changes}条记录涨跌幅超过15%")
        
        # 检查成交量异常
        if 'vol' in df.columns:
            vol_mean = df['vol'].mean()
            vol_std = df['vol'].std()
            extreme_volume = (df['vol'] > vol_mean + 5 * vol_std).sum()
            if extreme_volume > 0:
                warnings.append(f"{extreme_volume}条记录成交量异常（超过均值+5倍标准差）")
        
        return warnings
    
    def _check_financial_logic(self, df: pd.DataFrame) -> List[str]:
        """检查财务逻辑"""
        issues = []
        
        # 检查资产负债表平衡
        if all(field in df.columns for field in ['total_assets', 'total_liab', 'total_hldr_eqy_exc_min_int']):
            balance_diff = abs(
                df['total_assets'] - df['total_liab'] - df['total_hldr_eqy_exc_min_int']
            )
            # 容忍1%的误差
            imbalance = (balance_diff > df['total_assets'] * 0.01).sum()
            if imbalance > 0:
                issues.append(f"{imbalance}条记录的资产负债表不平衡")
        
        # 检查收入为负
        if 'revenue' in df.columns:
            negative_revenue = (df['revenue'] < 0).sum()
            if negative_revenue > 0:
                issues.append(f"{negative_revenue}条记录的营业收入为负数")
        
        return issues
    
    def _check_financial_ratios(self, df: pd.DataFrame) -> List[str]:
        """检查财务比率"""
        warnings = []
        
        # 检查ROE异常值
        if 'roe' in df.columns:
            extreme_roe = (abs(df['roe']) > 100).sum()  # ROE超过100%
            if extreme_roe > 0:
                warnings.append(f"{extreme_roe}条记录的ROE超过100%")
        
        # 检查负债率异常
        if 'debt_to_assets' in df.columns:
            high_debt = (df['debt_to_assets'] > 1).sum()  # 资产负债率>100%
            if high_debt > 0:
                warnings.append(f"{high_debt}条记录的资产负债率超过100%")
        
        return warnings
    
    def _calculate_completeness(self, df: pd.DataFrame) -> float:
        """计算数据完整性"""
        if df.empty:
            return 0.0
        
        total_cells = df.shape[0] * df.shape[1]
        missing_cells = df.isnull().sum().sum()
        
        return (total_cells - missing_cells) / total_cells
    
    def _get_date_range(self, df: pd.DataFrame) -> Dict[str, str]:
        """获取日期范围"""
        if 'trade_date' not in df.columns or df.empty:
            return {'start': '', 'end': ''}
        
        try:
            if df['trade_date'].dtype == 'object':
                df['trade_date'] = pd.to_datetime(df['trade_date'], errors='coerce')
            
            valid_dates = df['trade_date'].dropna()
            if valid_dates.empty:
                return {'start': '', 'end': ''}
            
            return {
                'start': valid_dates.min().strftime('%Y-%m-%d'),
                'end': valid_dates.max().strftime('%Y-%m-%d')
            }
        except Exception:
            return {'start': '', 'end': ''}
    
    def _calculate_quality_score(self, issue_count: int, warning_count: int, completeness: float) -> float:
        """计算质量分数"""
        # 基础分数从完整性开始
        base_score = completeness * 100
        
        # 问题扣分
        issue_penalty = min(issue_count * 10, 50)  # 每个问题扣10分，最多扣50分
        warning_penalty = min(warning_count * 2, 20)  # 每个警告扣2分，最多扣20分
        
        # 计算最终分数
        final_score = max(base_score - issue_penalty - warning_penalty, 0)
        
        return min(final_score, 100.0)
    
    def _get_quality_level(self, score: float) -> DataQualityLevel:
        """获取质量等级"""
        if score >= 90:
            return DataQualityLevel.EXCELLENT
        elif score >= 80:
            return DataQualityLevel.GOOD
        elif score >= 70:
            return DataQualityLevel.FAIR
        elif score >= 60:
            return DataQualityLevel.POOR
        else:
            return DataQualityLevel.BAD

class DataCleaner:
    """数据清洗器"""
    
    def __init__(self):
        self.cleaning_strategies = {
            'stock_basic': self._clean_stock_basic,
            'daily': self._clean_daily_data,
            'financial': self._clean_financial_data
        }
    
    def clean_data(self, df: pd.DataFrame, data_type: str, validation_result: ValidationResult = None) -> pd.DataFrame:
        """
        清洗数据
        
        Args:
            df: 原始数据
            data_type: 数据类型 ('stock_basic', 'daily', 'financial')
            validation_result: 验证结果
            
        Returns:
            清洗后的数据
        """
        logger.info(f"开始清洗{data_type}数据...")
        
        if data_type not in self.cleaning_strategies:
            logger.warning(f"不支持的数据类型: {data_type}")
            return df
        
        # 复制数据避免修改原始数据
        cleaned_df = df.copy()
        
        # 应用特定的清洗策略
        cleaned_df = self.cleaning_strategies[data_type](cleaned_df, validation_result)
        
        # 通用清洗步骤
        cleaned_df = self._apply_common_cleaning(cleaned_df)
        
        logger.info(f"{data_type}数据清洗完成: {len(df)} -> {len(cleaned_df)} 条记录")
        
        return cleaned_df
    
    def _clean_stock_basic(self, df: pd.DataFrame, validation_result: ValidationResult = None) -> pd.DataFrame:
        """清洗股票基础信息"""
        # 1. 删除重复数据
        df = df.drop_duplicates(subset=['ts_code'], keep='last')
        
        # 2. 标准化股票代码格式
        if 'ts_code' in df.columns:
            df['ts_code'] = df['ts_code'].str.upper()
        
        # 3. 清理股票名称
        if 'name' in df.columns:
            df['name'] = df['name'].str.strip()
            # 删除名称为空的记录
            df = df.dropna(subset=['name'])
        
        # 4. 标准化行业分类
        if 'industry' in df.columns:
            df['industry'] = df['industry'].fillna('其他')
        
        # 5. 处理上市日期
        if 'list_date' in df.columns:
            df['list_date'] = pd.to_datetime(df['list_date'], errors='coerce')
            # 删除上市日期无效的记录
            df = df.dropna(subset=['list_date'])
        
        return df
    
    def _clean_daily_data(self, df: pd.DataFrame, validation_result: ValidationResult = None) -> pd.DataFrame:
        """清洗日线数据"""
        original_count = len(df)
        
        # 1. 删除重复数据
        df = df.drop_duplicates(subset=['ts_code', 'trade_date'], keep='last')
        
        # 2. 处理日期格式
        if 'trade_date' in df.columns:
            df['trade_date'] = pd.to_datetime(df['trade_date'], errors='coerce')
            # 删除日期无效的记录
            df = df.dropna(subset=['trade_date'])
            
            # 删除未来日期的记录
            df = df[df['trade_date'] <= datetime.now()]
        
        # 3. 价格数据清洗
        price_fields = ['open', 'high', 'low', 'close', 'pre_close']
        for field in price_fields:
            if field in df.columns:
                # 删除负价格和零价格
                df = df[df[field] > 0]
                
                # 处理极端异常值（价格变化超过50%的情况）
                if field in ['open', 'high', 'low', 'close'] and 'pre_close' in df.columns:
                    price_change = abs((df[field] - df['pre_close']) / df['pre_close'])
                    df = df[price_change <= 0.5]  # 过滤掉变化超过50%的异常数据
        
        # 4. 检查和修正OHLC关系
        if all(field in df.columns for field in ['open', 'high', 'low', 'close']):
            # 确保 high >= max(open, close) 和 low <= min(open, close)
            df.loc[df['high'] < df[['open', 'close']].max(axis=1), 'high'] = df[['open', 'close']].max(axis=1)
            df.loc[df['low'] > df[['open', 'close']].min(axis=1), 'low'] = df[['open', 'close']].min(axis=1)
        
        # 5. 成交量和成交额清洗
        if 'vol' in df.columns:
            # 成交量为负的设为0
            df.loc[df['vol'] < 0, 'vol'] = 0
            
            # 处理极端成交量（超过均值+5倍标准差）
            if len(df) > 0:
                vol_mean = df['vol'].mean()
                vol_std = df['vol'].std()
                if vol_std > 0:
                    vol_threshold = vol_mean + 5 * vol_std
                    df.loc[df['vol'] > vol_threshold, 'vol'] = vol_threshold
        
        if 'amount' in df.columns:
            # 成交额为负的设为0
            df.loc[df['amount'] < 0, 'amount'] = 0
        
        # 6. 涨跌幅清洗
        if 'pct_chg' in df.columns:
            # 限制涨跌幅在合理范围内（-20% ~ 20%）
            df.loc[df['pct_chg'] > 20, 'pct_chg'] = 20
            df.loc[df['pct_chg'] < -20, 'pct_chg'] = -20
        
        # 7. 按时间排序
        if 'trade_date' in df.columns:
            df = df.sort_values(['ts_code', 'trade_date'])
        
        logger.info(f"日线数据清洗: 删除了{original_count - len(df)}条异常记录")
        
        return df
    
    def _clean_financial_data(self, df: pd.DataFrame, validation_result: ValidationResult = None) -> pd.DataFrame:
        """清洗财务数据"""
        original_count = len(df)
        
        # 1. 删除重复数据
        df = df.drop_duplicates(subset=['ts_code', 'end_date'], keep='last')
        
        # 2. 处理日期格式
        if 'end_date' in df.columns:
            df['end_date'] = pd.to_datetime(df['end_date'], errors='coerce')
            df = df.dropna(subset=['end_date'])
        
        if 'ann_date' in df.columns:
            df['ann_date'] = pd.to_datetime(df['ann_date'], errors='coerce')
        
        # 3. 财务数据清洗
        financial_fields = ['total_assets', 'revenue', 'n_income', 'total_liab']
        
        for field in financial_fields:
            if field in df.columns:
                # 将极端负值设为0（除了净利润可以为负）
                if field != 'n_income':
                    df.loc[df[field] < 0, field] = 0
                
                # 处理极端异常值
                if len(df[df[field].notna()]) > 0:
                    q99 = df[field].quantile(0.99)
                    q01 = df[field].quantile(0.01)
                    
                    # 将极端值限制在99分位数内
                    df.loc[df[field] > q99, field] = q99
                    if field != 'n_income':  # 净利润允许负值
                        df.loc[df[field] < q01, field] = q01
        
        # 4. 财务比率清洗
        ratio_fields = ['roe', 'roa', 'gross_margin', 'net_margin', 'debt_to_assets']
        
        for field in ratio_fields:
            if field in df.columns:
                # 限制比率在合理范围内
                if field in ['roe', 'roa']:
                    # ROE和ROA限制在-100%到200%之间
                    df.loc[df[field] > 200, field] = 200
                    df.loc[df[field] < -100, field] = -100
                elif field in ['gross_margin', 'net_margin']:
                    # 毛利率和净利率限制在-100%到100%之间
                    df.loc[df[field] > 100, field] = 100
                    df.loc[df[field] < -100, field] = -100
                elif field == 'debt_to_assets':
                    # 资产负债率限制在0%到200%之间
                    df.loc[df[field] > 200, field] = 200
                    df.loc[df[field] < 0, field] = 0
        
        # 5. 修正资产负债表平衡
        if all(field in df.columns for field in ['total_assets', 'total_liab', 'total_hldr_eqy_exc_min_int']):
            # 如果不平衡，调整股东权益
            assets_liab_sum = df['total_liab'] + df['total_hldr_eqy_exc_min_int']
            imbalance_mask = abs(df['total_assets'] - assets_liab_sum) > df['total_assets'] * 0.01
            
            # 对于不平衡的记录，用资产减去负债来计算股东权益
            df.loc[imbalance_mask, 'total_hldr_eqy_exc_min_int'] = (
                df.loc[imbalance_mask, 'total_assets'] - df.loc[imbalance_mask, 'total_liab']
            )
        
        # 6. 按时间排序
        if 'end_date' in df.columns:
            df = df.sort_values(['ts_code', 'end_date'])
        
        logger.info(f"财务数据清洗: 删除了{original_count - len(df)}条异常记录")
        
        return df
    
    def _apply_common_cleaning(self, df: pd.DataFrame) -> pd.DataFrame:
        """应用通用清洗规则"""
        # 1. 删除完全空白的行
        df = df.dropna(how='all')
        
        # 2. 重置索引
        df = df.reset_index(drop=True)
        
        # 3. 优化数据类型
        df = self._optimize_dtypes(df)
        
        return df
    
    def _optimize_dtypes(self, df: pd.DataFrame) -> pd.DataFrame:
        """优化数据类型以节省内存"""
        for col in df.columns:
            col_type = df[col].dtype
            
            if col_type != 'object':
                # 数值类型优化
                if str(col_type)[:3] == 'int':
                    if df[col].min() >= 0:
                        # 无符号整数
                        if df[col].max() < 255:
                            df[col] = df[col].astype('uint8')
                        elif df[col].max() < 65535:
                            df[col] = df[col].astype('uint16')
                        elif df[col].max() < 4294967295:
                            df[col] = df[col].astype('uint32')
                    else:
                        # 有符号整数
                        if df[col].min() > -128 and df[col].max() < 127:
                            df[col] = df[col].astype('int8')
                        elif df[col].min() > -32768 and df[col].max() < 32767:
                            df[col] = df[col].astype('int16')
                        elif df[col].min() > -2147483648 and df[col].max() < 2147483647:
                            df[col] = df[col].astype('int32')
                
                elif str(col_type)[:5] == 'float':
                    # 浮点数优化
                    if abs(df[col]).max() < 3.4e38:
                        df[col] = df[col].astype('float32')
        
        return df

class DataQualityReporter:
    """数据质量报告生成器"""
    
    def __init__(self):
        pass
    
    def generate_report(self, validation_results: Dict[str, ValidationResult]) -> Dict[str, Any]:
        """
        生成数据质量报告
        
        Args:
            validation_results: 各类数据的验证结果
            
        Returns:
            完整的数据质量报告
        """
        logger.info("生成数据质量报告...")
        
        report = {
            'generated_at': datetime.now().isoformat(),
            'overall_summary': self._generate_overall_summary(validation_results),
            'detailed_results': {},
            'recommendations': self._generate_recommendations(validation_results)
        }
        
        # 详细结果
        for data_type, result in validation_results.items():
            report['detailed_results'][data_type] = {
                'quality_score': result.quality_score,
                'quality_level': result.quality_level.value,
                'is_valid': result.is_valid,
                'issues_count': len(result.issues),
                'warnings_count': len(result.warnings),
                'issues': result.issues,
                'warnings': result.warnings,
                'statistics': result.statistics
            }
        
        return report
    
    def _generate_overall_summary(self, validation_results: Dict[str, ValidationResult]) -> Dict[str, Any]:
        """生成总体摘要"""
        if not validation_results:
            return {'status': 'no_data', 'message': '无验证结果'}
        
        # 计算总体质量分数
        total_score = sum(result.quality_score for result in validation_results.values())
        avg_score = total_score / len(validation_results)
        
        # 统计问题和警告
        total_issues = sum(len(result.issues) for result in validation_results.values())
        total_warnings = sum(len(result.warnings) for result in validation_results.values())
        
        # 确定总体状态
        valid_datasets = sum(1 for result in validation_results.values() if result.is_valid)
        total_datasets = len(validation_results)
        
        if valid_datasets == total_datasets:
            overall_status = 'excellent' if avg_score >= 90 else 'good'
        elif valid_datasets >= total_datasets * 0.7:
            overall_status = 'fair'
        else:
            overall_status = 'poor'
        
        return {
            'overall_status': overall_status,
            'average_quality_score': round(avg_score, 2),
            'valid_datasets': valid_datasets,
            'total_datasets': total_datasets,
            'total_issues': total_issues,
            'total_warnings': total_warnings,
            'dataset_quality_scores': {
                data_type: result.quality_score 
                for data_type, result in validation_results.items()
            }
        }
    
    def _generate_recommendations(self, validation_results: Dict[str, ValidationResult]) -> List[str]:
        """生成改进建议"""
        recommendations = []
        
        for data_type, result in validation_results.items():
            if result.quality_score < 80:
                recommendations.append(f"建议重新获取和清洗{data_type}数据，当前质量分数较低({result.quality_score:.1f})")
            
            if len(result.issues) > 0:
                recommendations.append(f"{data_type}数据存在{len(result.issues)}个严重问题，需要立即处理")
            
            if len(result.warnings) > 5:
                recommendations.append(f"{data_type}数据存在较多警告({len(result.warnings)}个)，建议审查数据源")
        
        # 通用建议
        if not recommendations:
            recommendations.append("数据质量良好，建议保持当前的数据获取和处理流程")
        
        recommendations.append("建议定期运行数据质量检查，及时发现和解决数据问题")
        
        return recommendations

def main():
    """主函数 - 数据验证和清洗示例"""
    # 创建验证器和清洗器
    validator = DataValidator()
    cleaner = DataCleaner()
    reporter = DataQualityReporter()
    
    # 模拟数据进行测试
    print("=== 数据验证和清洗系统测试 ===")
    
    # 测试股票基础信息验证
    stock_basic_data = pd.DataFrame({
        'ts_code': ['000001.SZ', '000002.SZ', '600000.SH'],
        'symbol': ['000001', '000002', '600000'],
        'name': ['平安银行', '万科A', '浦发银行'],
        'area': ['深圳', '深圳', '上海'],
        'industry': ['银行', '房地产', '银行'],
        'market': ['主板', '主板', '主板'],
        'list_date': ['19910403', '19910129', '19991110']
    })
    
    print("\n1. 股票基础信息验证:")
    stock_result = validator.validate_stock_basic(stock_basic_data)
    print(f"验证结果: 质量分数={stock_result.quality_score:.1f}, 等级={stock_result.quality_level.value}")
    print(f"问题: {stock_result.issues}")
    print(f"警告: {stock_result.warnings}")
    
    # 清洗数据
    cleaned_stock_data = cleaner.clean_data(stock_basic_data, 'stock_basic', stock_result)
    print(f"清洗后记录数: {len(cleaned_stock_data)}")
    
    # 测试日线数据验证（模拟一些问题数据）
    daily_data = pd.DataFrame({
        'ts_code': ['000001.SZ'] * 5,
        'trade_date': ['20250101', '20250102', '20250103', '20250106', '20250107'],
        'open': [10.0, 10.5, 11.0, 10.8, 10.2],
        'high': [10.5, 11.2, 11.5, 11.0, 10.5],
        'low': [9.8, 10.2, 10.5, 10.0, 9.9],
        'close': [10.2, 11.0, 10.8, 10.1, 10.0],
        'pct_chg': [2.0, 7.84, -1.82, -6.48, -0.99],
        'vol': [1000000, 1500000, 1200000, 800000, 900000]
    })
    
    print("\n2. 日线数据验证:")
    daily_result = validator.validate_daily_data(daily_data)
    print(f"验证结果: 质量分数={daily_result.quality_score:.1f}, 等级={daily_result.quality_level.value}")
    print(f"问题: {daily_result.issues}")
    print(f"警告: {daily_result.warnings}")
    
    # 生成质量报告
    validation_results = {
        'stock_basic': stock_result,
        'daily_data': daily_result
    }
    
    print("\n3. 生成数据质量报告:")
    quality_report = reporter.generate_report(validation_results)
    
    print("总体摘要:")
    summary = quality_report['overall_summary']
    print(f"  - 总体状态: {summary['overall_status']}")
    print(f"  - 平均质量分数: {summary['average_quality_score']}")
    print(f"  - 有效数据集: {summary['valid_datasets']}/{summary['total_datasets']}")
    print(f"  - 总问题数: {summary['total_issues']}")
    print(f"  - 总警告数: {summary['total_warnings']}")
    
    print("\n改进建议:")
    for i, recommendation in enumerate(quality_report['recommendations'], 1):
        print(f"  {i}. {recommendation}")
    
    print("\n=== 测试完成 ===")

if __name__ == "__main__":
    main()