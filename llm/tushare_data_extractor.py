"""
TuShare历史数据提取器
用于生成大模型训练数据集
"""

import pandas as pd
import tushare as ts
import json
import logging
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Tuple
import numpy as np
import time
import os

class TuShareDataExtractor:
    """TuShare历史数据提取器"""
    
    def __init__(self, token: Optional[str] = None):
        """
        初始化TuShare数据提取器
        
        Args:
            token: TuShare Pro API token，如果为None将尝试从环境变量获取
        """
        self.token = token or os.getenv('TUSHARE_TOKEN')
        if self.token:
            ts.set_token(self.token)
            self.pro = ts.pro_api()
            logging.info("✅ TuShare Pro API已初始化")
        else:
            logging.warning("⚠️ 未设置TuShare Pro token，将使用免费接口")
            self.pro = None
        
        self.logger = logging.getLogger(__name__)
        
    def get_stock_list(self, exchange: str = None, limit: int = 100) -> pd.DataFrame:
        """
        获取股票列表
        
        Args:
            exchange: 交易所代码 (SSE, SZSE)
            limit: 获取数量限制
            
        Returns:
            股票基本信息DataFrame
        """
        try:
            if self.pro:
                # 使用Pro API
                if exchange:
                    stocks = self.pro.stock_basic(exchange=exchange, fields='ts_code,symbol,name,area,industry,list_date')
                else:
                    stocks = self.pro.stock_basic(fields='ts_code,symbol,name,area,industry,list_date')
            else:
                # 使用免费API
                stocks = ts.get_stock_basics()
                stocks.reset_index(inplace=True)
                stocks.rename(columns={'code': 'ts_code', 'name': 'name'}, inplace=True)
            
            if limit:
                stocks = stocks.head(limit)
                
            self.logger.info(f"✅ 获取股票列表成功: {len(stocks)}只股票")
            return stocks
            
        except Exception as e:
            self.logger.error(f"❌ 获取股票列表失败: {e}")
            return pd.DataFrame()
    
    def get_stock_daily_data(self, ts_code: str, start_date: str, end_date: str) -> pd.DataFrame:
        """
        获取股票日线数据
        
        Args:
            ts_code: 股票代码
            start_date: 开始日期 YYYYMMDD
            end_date: 结束日期 YYYYMMDD
            
        Returns:
            日线数据DataFrame
        """
        max_retries = 3
        retry_delay = 1.0
        
        for attempt in range(max_retries):
            try:
                if self.pro:
                    # 使用Pro API
                    df = self.pro.daily(ts_code=ts_code, start_date=start_date, end_date=end_date)
                else:
                    # 使用免费API - 格式化日期
                    formatted_start = f"{start_date[:4]}-{start_date[4:6]}-{start_date[6:]}"
                    formatted_end = f"{end_date[:4]}-{end_date[4:6]}-{end_date[6:]}"
                    code = ts_code.split('.')[0]  # 提取股票代码
                    df = ts.get_hist_data(code, start=formatted_start, end=formatted_end)
                    if not df.empty:
                        df.reset_index(inplace=True)
                        df['ts_code'] = ts_code
                        df.rename(columns={'date': 'trade_date'}, inplace=True)
                
                if not df.empty:
                    df['trade_date'] = pd.to_datetime(df['trade_date'])
                    df = df.sort_values('trade_date')
                    
                time.sleep(0.2)  # 增加延迟避免频率限制
                return df
                
            except Exception as e:
                if attempt < max_retries - 1:
                    self.logger.warning(f"获取{ts_code}数据失败 (尝试{attempt+1}/{max_retries}): {e}, 重试中...")
                    time.sleep(retry_delay * (attempt + 1))
                else:
                    self.logger.error(f"❌ 获取{ts_code}日线数据失败: {e}")
                    return pd.DataFrame()
    
    def get_stock_financial_data(self, ts_code: str, periods: List[str] = None) -> pd.DataFrame:
        """
        获取股票财务数据
        
        Args:
            ts_code: 股票代码
            periods: 报告期列表
            
        Returns:
            财务数据DataFrame
        """
        if not self.pro:
            return pd.DataFrame()
            
        try:
            # 获取利润表
            income = self.pro.income(ts_code=ts_code, fields='ts_code,ann_date,f_ann_date,end_date,total_revenue,total_cogs,n_income')
            # 获取资产负债表  
            balancesheet = self.pro.balancesheet(ts_code=ts_code, fields='ts_code,ann_date,f_ann_date,end_date,total_assets,total_liab,total_equity')
            # 获取现金流量表
            cashflow = self.pro.cashflow(ts_code=ts_code, fields='ts_code,ann_date,f_ann_date,end_date,n_cashflow_act,n_cashflow_inv,n_cashflow_fin')
            
            # 合并财务数据
            financial_data = income
            if not balancesheet.empty:
                financial_data = financial_data.merge(balancesheet, on=['ts_code', 'ann_date', 'end_date'], how='outer')
            if not cashflow.empty:
                financial_data = financial_data.merge(cashflow, on=['ts_code', 'ann_date', 'end_date'], how='outer')
                
            time.sleep(0.2)  # 避免频率限制
            return financial_data
            
        except Exception as e:
            self.logger.error(f"❌ 获取{ts_code}财务数据失败: {e}")
            return pd.DataFrame()
    
    def get_market_news(self, start_date: str, end_date: str, limit: int = 1000) -> pd.DataFrame:
        """
        获取市场新闻数据
        
        Args:
            start_date: 开始日期
            end_date: 结束日期
            limit: 获取数量限制
            
        Returns:
            新闻数据DataFrame
        """
        if not self.pro:
            return pd.DataFrame()
            
        try:
            news = self.pro.news(start_date=start_date, end_date=end_date, limit=limit,
                               fields='datetime,title,content,channels,score')
            if not news.empty:
                news['datetime'] = pd.to_datetime(news['datetime'])
                
            time.sleep(0.1)
            return news
            
        except Exception as e:
            self.logger.error(f"❌ 获取市场新闻失败: {e}")
            return pd.DataFrame()
    
    def extract_comprehensive_dataset(self, 
                                    stock_codes: List[str], 
                                    start_date: str, 
                                    end_date: str,
                                    include_financial: bool = True,
                                    include_news: bool = True) -> Dict[str, pd.DataFrame]:
        """
        提取综合数据集
        
        Args:
            stock_codes: 股票代码列表
            start_date: 开始日期 YYYYMMDD
            end_date: 结束日期 YYYYMMDD
            include_financial: 是否包含财务数据
            include_news: 是否包含新闻数据
            
        Returns:
            包含各类数据的字典
        """
        dataset = {
            'daily_data': [],
            'financial_data': [],
            'news_data': [],
            'metadata': {
                'extraction_date': datetime.now().isoformat(),
                'date_range': {'start': start_date, 'end': end_date},
                'stock_count': len(stock_codes),
                'data_types': []
            }
        }
        
        self.logger.info(f"🚀 开始提取{len(stock_codes)}只股票的综合数据集")
        
        # 1. 提取日线数据
        self.logger.info("📊 提取日线数据...")
        for i, ts_code in enumerate(stock_codes):
            self.logger.info(f"  处理 {ts_code} ({i+1}/{len(stock_codes)})")
            
            daily_data = self.get_stock_daily_data(ts_code, start_date, end_date)
            if not daily_data.empty:
                dataset['daily_data'].append(daily_data)
                
            # 财务数据
            if include_financial:
                financial_data = self.get_stock_financial_data(ts_code)
                if not financial_data.empty:
                    dataset['financial_data'].append(financial_data)
        
        # 2. 提取市场新闻
        if include_news:
            self.logger.info("📰 提取市场新闻...")
            news_data = self.get_market_news(start_date, end_date)
            if not news_data.empty:
                dataset['news_data'] = news_data
        
        # 3. 合并数据
        if dataset['daily_data']:
            dataset['daily_data'] = pd.concat(dataset['daily_data'], ignore_index=True)
            dataset['metadata']['data_types'].append('daily_price')
            
        if dataset['financial_data']:
            dataset['financial_data'] = pd.concat(dataset['financial_data'], ignore_index=True)
            dataset['metadata']['data_types'].append('financial')
            
        if isinstance(dataset['news_data'], pd.DataFrame) and not dataset['news_data'].empty:
            dataset['metadata']['data_types'].append('news')
        
        self.logger.info(f"✅ 数据提取完成！")
        self.logger.info(f"  日线数据: {len(dataset['daily_data']) if isinstance(dataset['daily_data'], pd.DataFrame) else 0}条")
        self.logger.info(f"  财务数据: {len(dataset['financial_data']) if isinstance(dataset['financial_data'], pd.DataFrame) else 0}条")
        self.logger.info(f"  新闻数据: {len(dataset['news_data']) if isinstance(dataset['news_data'], pd.DataFrame) else 0}条")
        
        return dataset
    
    def save_dataset(self, dataset: Dict, output_dir: str = "data/tushare_dataset"):
        """
        保存数据集到文件
        
        Args:
            dataset: 数据集字典
            output_dir: 输出目录
        """
        os.makedirs(output_dir, exist_ok=True)
        
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        
        # 保存各类数据
        if isinstance(dataset['daily_data'], pd.DataFrame) and not dataset['daily_data'].empty:
            daily_path = f"{output_dir}/daily_data_{timestamp}.csv"
            dataset['daily_data'].to_csv(daily_path, index=False, encoding='utf-8')
            self.logger.info(f"✅ 日线数据已保存: {daily_path}")
            
        if isinstance(dataset['financial_data'], pd.DataFrame) and not dataset['financial_data'].empty:
            financial_path = f"{output_dir}/financial_data_{timestamp}.csv"
            dataset['financial_data'].to_csv(financial_path, index=False, encoding='utf-8')
            self.logger.info(f"✅ 财务数据已保存: {financial_path}")
            
        if isinstance(dataset['news_data'], pd.DataFrame) and not dataset['news_data'].empty:
            news_path = f"{output_dir}/news_data_{timestamp}.csv"
            dataset['news_data'].to_csv(news_path, index=False, encoding='utf-8')
            self.logger.info(f"✅ 新闻数据已保存: {news_path}")
            
        # 保存元数据
        metadata_path = f"{output_dir}/metadata_{timestamp}.json"
        with open(metadata_path, 'w', encoding='utf-8') as f:
            json.dump(dataset['metadata'], f, ensure_ascii=False, indent=2)
        self.logger.info(f"✅ 元数据已保存: {metadata_path}")


def main():
    """测试数据提取器"""
    logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
    
    # 初始化提取器
    extractor = TuShareDataExtractor()
    
    # 获取股票列表
    stocks = extractor.get_stock_list(limit=10)  # 测试用，只取10只股票
    if stocks.empty:
        print("❌ 无法获取股票列表")
        return
    
    stock_codes = stocks['ts_code'].tolist() if 'ts_code' in stocks.columns else stocks.index.tolist()
    
    # 设置日期范围（最近6个月）
    end_date = datetime.now().strftime('%Y%m%d')
    start_date = (datetime.now() - timedelta(days=180)).strftime('%Y%m%d')
    
    print(f"📅 提取日期范围: {start_date} ~ {end_date}")
    print(f"📈 股票列表: {stock_codes[:5]}...")  # 显示前5只
    
    # 提取数据集
    dataset = extractor.extract_comprehensive_dataset(
        stock_codes=stock_codes,
        start_date=start_date,
        end_date=end_date,
        include_financial=False,  # 测试时不包含财务数据
        include_news=False       # 测试时不包含新闻数据
    )
    
    # 保存数据集
    extractor.save_dataset(dataset)
    
    print("✅ 数据提取测试完成！")


if __name__ == "__main__":
    main()