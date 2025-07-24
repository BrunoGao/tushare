import plotly.graph_objects as go
from plotly.subplots import make_subplots
import pandas as pd
import numpy as np
from typing import Dict, List, Any, Optional
from datetime import datetime, timedelta
import sys, os
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import config
from utils.db_helper import db
from analysis.technical_indicators import tech_indicator
from sqlalchemy import text
import base64
import io

class StockChartGenerator:
    """专业股票图表生成器"""
    
    def __init__(self):
        self.colors = {
            'up': '#00da3c',      # 上涨绿色
            'down': '#fd1050',    # 下跌红色
            'ma5': '#1f77b4',     # MA5蓝色
            'ma20': '#ff7f0e',    # MA20橙色
            'ma60': '#2ca02c',    # MA60绿色
            'volume': '#9467bd',  # 成交量紫色
            'macd': '#d62728',    # MACD红色
            'signal': '#ff69b4',  # 信号粉色
            'buy': '#00ff00',     # 买入信号
            'sell': '#ff0000',    # 卖出信号
            'bg': '#1e1e1e',      # 背景色
            'grid': '#3e3e3e'     # 网格色
        }
        
    def create_comprehensive_chart(self, ts_code: str, days: int = 120) -> Dict[str, Any]:
        """创建综合技术分析图表"""
        try:
            # 获取数据
            end_date = datetime.now().strftime('%Y%m%d')
            start_date = (datetime.now() - timedelta(days=days)).strftime('%Y%m%d')
            df = db.query_stock_data(ts_code, start_date, end_date)
            
            if df.empty:
                return {'success': False, 'error': f'{ts_code} 无数据'}
                
            # 计算技术指标
            df = tech_indicator.calculate_all_indicators(df)
            signals = tech_indicator.generate_signals(df)
            
            # 合并信号数据
            df = pd.concat([df, signals], axis=1)
            df = df.sort_values('trade_date')
            
            # 创建子图布局
            fig = make_subplots(
                rows=4, cols=1,
                shared_xaxes=True,
                vertical_spacing=0.03,
                subplot_titles=['股价走势 & 均线系统', '成交量', 'MACD', 'RSI & KDJ'],
                row_width=[0.5, 0.15, 0.15, 0.2]
            )
            
            # 第1行：K线图 + 均线 + 交易信号
            self._add_candlestick_chart(fig, df, row=1)
            self._add_moving_averages(fig, df, row=1)
            self._add_trading_signals(fig, df, row=1)
            self._add_support_resistance(fig, df, row=1)
            
            # 第2行：成交量
            self._add_volume_chart(fig, df, row=2)
            
            # 第3行：MACD
            self._add_macd_chart(fig, df, row=3)
            
            # 第4行：RSI + KDJ
            self._add_rsi_kdj_chart(fig, df, row=4)
            
            # 更新布局
            self._update_layout(fig, ts_code)
            
            # 转换为HTML (包含内联Plotly.js)
            html_content = fig.to_html(include_plotlyjs=True, div_id="chart-div", full_html=False)
            
            # 获取股票基本信息
            with db.engine.connect() as conn:
                sql = "SELECT name FROM stock_basic WHERE ts_code = :ts_code"
                result = conn.execute(text(sql), {'ts_code': ts_code})
                stock_name = result.scalar() or ts_code
                
            return {
                'success': True,
                'chart_html': html_content,
                'stock_code': ts_code,
                'stock_name': stock_name,
                'data_points': len(df),
                'date_range': f"{df['trade_date'].min()} ~ {df['trade_date'].max()}",
                'latest_price': float(df['close'].iloc[-1]),
                'latest_signals': {
                    'buy': int(df['buy_signal'].iloc[-1]) if 'buy_signal' in df.columns else 0,
                    'sell': int(df['sell_signal'].iloc[-1]) if 'sell_signal' in df.columns else 0,
                    'strength_score': float(df['strength_score'].iloc[-1]) if 'strength_score' in df.columns else 50
                }
            }
            
        except Exception as e:
            return {'success': False, 'error': str(e)}
            
    def _add_candlestick_chart(self, fig, df, row):
        """添加K线图"""
        # K线图
        fig.add_trace(
            go.Candlestick(
                x=df['trade_date'],
                open=df['open'],
                high=df['high'],
                low=df['low'],
                close=df['close'],
                name='K线',
                increasing_line_color=self.colors['up'],
                decreasing_line_color=self.colors['down'],
                increasing_fillcolor=self.colors['up'],
                decreasing_fillcolor=self.colors['down']
            ),
            row=row, col=1
        )
        
    def _add_moving_averages(self, fig, df, row):
        """添加移动平均线"""
        # MA5
        if 'ma5' in df.columns:
            fig.add_trace(
                go.Scatter(
                    x=df['trade_date'],
                    y=df['ma5'],
                    mode='lines',
                    name='MA5',
                    line=dict(color=self.colors['ma5'], width=1),
                    opacity=0.8
                ),
                row=row, col=1
            )
            
        # MA20
        if 'ma20' in df.columns:
            fig.add_trace(
                go.Scatter(
                    x=df['trade_date'],
                    y=df['ma20'],
                    mode='lines',
                    name='MA20',
                    line=dict(color=self.colors['ma20'], width=1),
                    opacity=0.8
                ),
                row=row, col=1
            )
            
        # MA60
        if 'ma60' in df.columns:
            fig.add_trace(
                go.Scatter(
                    x=df['trade_date'],
                    y=df['ma60'],
                    mode='lines',
                    name='MA60',
                    line=dict(color=self.colors['ma60'], width=1),
                    opacity=0.8
                ),
                row=row, col=1
            )
            
    def _add_trading_signals(self, fig, df, row):
        """添加交易信号"""
        # 买入信号
        if 'buy_signal' in df.columns:
            buy_points = df[df['buy_signal'] == 1]
            if not buy_points.empty:
                fig.add_trace(
                    go.Scatter(
                        x=buy_points['trade_date'],
                        y=buy_points['low'] * 0.99,  # 稍微低于最低价
                        mode='markers',
                        name='买入信号',
                        marker=dict(
                            symbol='triangle-up',
                            color=self.colors['buy'],
                            size=12,
                            line=dict(color='white', width=1)
                        ),
                        text='BUY',
                        textposition='bottom center'
                    ),
                    row=row, col=1
                )
                
        # 卖出信号
        if 'sell_signal' in df.columns:
            sell_points = df[df['sell_signal'] == 1]
            if not sell_points.empty:
                fig.add_trace(
                    go.Scatter(
                        x=sell_points['trade_date'],
                        y=sell_points['high'] * 1.01,  # 稍微高于最高价
                        mode='markers',
                        name='卖出信号',
                        marker=dict(
                            symbol='triangle-down',
                            color=self.colors['sell'],
                            size=12,
                            line=dict(color='white', width=1)
                        ),
                        text='SELL',
                        textposition='top center'
                    ),
                    row=row, col=1
                )
                
    def _add_support_resistance(self, fig, df, row):
        """添加支撑阻力位"""
        if 'support' in df.columns and 'resistance' in df.columns:
            # 支撑线
            fig.add_trace(
                go.Scatter(
                    x=df['trade_date'],
                    y=df['support'],
                    mode='lines',
                    name='支撑位',
                    line=dict(color='green', width=1, dash='dash'),
                    opacity=0.6
                ),
                row=row, col=1
            )
            
            # 阻力线
            fig.add_trace(
                go.Scatter(
                    x=df['trade_date'],
                    y=df['resistance'],
                    mode='lines',
                    name='阻力位',
                    line=dict(color='red', width=1, dash='dash'),
                    opacity=0.6
                ),
                row=row, col=1
            )
            
    def _add_volume_chart(self, fig, df, row):
        """添加成交量图"""
        # 成交量柱状图
        colors = [self.colors['up'] if close >= open else self.colors['down'] 
                 for close, open in zip(df['close'], df['open'])]
        
        fig.add_trace(
            go.Bar(
                x=df['trade_date'],
                y=df['vol'],
                name='成交量',
                marker_color=colors,
                opacity=0.7
            ),
            row=row, col=1
        )
        
        # 成交量均线
        if len(df) >= 20:
            vol_ma = df['vol'].rolling(20).mean()
            fig.add_trace(
                go.Scatter(
                    x=df['trade_date'],
                    y=vol_ma,
                    mode='lines',
                    name='成交量MA20',
                    line=dict(color=self.colors['volume'], width=1),
                    opacity=0.8
                ),
                row=row, col=1
            )
            
    def _add_macd_chart(self, fig, df, row):
        """添加MACD图"""
        if 'macd' in df.columns and 'macd_signal' in df.columns:
            # MACD线
            fig.add_trace(
                go.Scatter(
                    x=df['trade_date'],
                    y=df['macd'],
                    mode='lines',
                    name='MACD',
                    line=dict(color=self.colors['macd'], width=1)
                ),
                row=row, col=1
            )
            
            # 信号线
            fig.add_trace(
                go.Scatter(
                    x=df['trade_date'],
                    y=df['macd_signal'],
                    mode='lines',
                    name='Signal',
                    line=dict(color=self.colors['signal'], width=1)
                ),
                row=row, col=1
            )
            
            # MACD柱状图
            if 'macd_hist' in df.columns:
                colors = [self.colors['up'] if val >= 0 else self.colors['down'] 
                         for val in df['macd_hist']]
                
                fig.add_trace(
                    go.Bar(
                        x=df['trade_date'],
                        y=df['macd_hist'],
                        name='MACD Histogram',
                        marker_color=colors,
                        opacity=0.6
                    ),
                    row=row, col=1
                )
                
    def _add_rsi_kdj_chart(self, fig, df, row):
        """添加RSI和KDJ图"""
        # RSI
        if 'rsi14' in df.columns:
            fig.add_trace(
                go.Scatter(
                    x=df['trade_date'],
                    y=df['rsi14'],
                    mode='lines',
                    name='RSI(14)',
                    line=dict(color='purple', width=1),
                    yaxis='y4'
                ),
                row=row, col=1
            )
            
            # RSI超买超卖线
            fig.add_hline(y=70, line_dash="dash", line_color="red", opacity=0.5, row=row, col=1)
            fig.add_hline(y=30, line_dash="dash", line_color="green", opacity=0.5, row=row, col=1)
            
        # KDJ
        if all(col in df.columns for col in ['k', 'd', 'j']):
            fig.add_trace(
                go.Scatter(
                    x=df['trade_date'],
                    y=df['k'],
                    mode='lines',
                    name='K',
                    line=dict(color='blue', width=1),
                    yaxis='y4'
                ),
                row=row, col=1
            )
            
            fig.add_trace(
                go.Scatter(
                    x=df['trade_date'],
                    y=df['d'],
                    mode='lines',
                    name='D',
                    line=dict(color='orange', width=1),
                    yaxis='y4'
                ),
                row=row, col=1
            )
            
    def _update_layout(self, fig, ts_code):
        """更新图表布局"""
        fig.update_layout(
            title=f'{ts_code} 综合技术分析图表',
            xaxis_rangeslider_visible=False,
            template='plotly_dark',
            height=1000,
            showlegend=True,
            legend=dict(
                orientation="h",
                yanchor="bottom",
                y=1.02,
                xanchor="right",
                x=1
            )
        )
        
        # 更新X轴
        fig.update_xaxes(
            showgrid=True,
            gridwidth=1,
            gridcolor=self.colors['grid'],
            showspikes=True,
            spikecolor="white",
            spikesnap="cursor",
            spikemode="across"
        )
        
        # 更新Y轴
        fig.update_yaxes(
            showgrid=True,
            gridwidth=1,
            gridcolor=self.colors['grid'],
            showspikes=True,
            spikecolor="white",
            spikesnap="cursor",
            spikemode="across"
        )
        
    def create_realtime_signal_chart(self, ts_code: str, timeframe: str = '1d') -> Dict[str, Any]:
        """创建实时交易信号图表"""
        try:
            # 获取最近30天数据用于实时展示
            end_date = datetime.now().strftime('%Y%m%d')
            start_date = (datetime.now() - timedelta(days=30)).strftime('%Y%m%d')
            df = db.query_stock_data(ts_code, start_date, end_date)
            
            if df.empty:
                return {'success': False, 'error': f'{ts_code} 无实时数据'}
                
            # 计算实时指标
            df = tech_indicator.calculate_all_indicators(df)
            signals = tech_indicator.generate_signals(df)
            df = pd.concat([df, signals], axis=1)
            
            # 创建实时信号图
            fig = make_subplots(
                rows=2, cols=1,
                shared_xaxes=True,
                vertical_spacing=0.05,
                subplot_titles=['实时价格走势 & 交易信号', '强弱度指标'],
                row_width=[0.7, 0.3]
            )
            
            # 主图：K线 + 信号
            self._add_candlestick_chart(fig, df, row=1)
            self._add_moving_averages(fig, df, row=1)
            self._add_trading_signals(fig, df, row=1)
            
            # 副图：强弱度
            if 'strength_score' in df.columns:
                fig.add_trace(
                    go.Scatter(
                        x=df['trade_date'],
                        y=df['strength_score'],
                        mode='lines+markers',
                        name='强弱度',
                        line=dict(color='cyan', width=2),
                        fill='tonexty'
                    ),
                    row=2, col=1
                )
                
                # 强弱度分界线
                fig.add_hline(y=60, line_dash="dash", line_color="green", opacity=0.5, row=2, col=1)
                fig.add_hline(y=40, line_dash="dash", line_color="red", opacity=0.5, row=2, col=1)
                
            # 更新布局
            fig.update_layout(
                title=f'{ts_code} 实时交易信号监控',
                xaxis_rangeslider_visible=False,
                template='plotly_dark',
                height=600,
                showlegend=True
            )
            
            # 转换为HTML (包含内联Plotly.js)
            html_content = fig.to_html(include_plotlyjs=True, div_id="realtime-chart-div", full_html=False)
            
            # 获取最新信号
            latest = df.iloc[-1]
            current_signals = {
                'buy_signal': int(latest.get('buy_signal', 0)),
                'sell_signal': int(latest.get('sell_signal', 0)),
                'ma_golden_cross': int(latest.get('ma_golden_cross', 0)),
                'macd_golden_cross': int(latest.get('macd_golden_cross', 0)),
                'strength_score': float(latest.get('strength_score', 50)),
                'rsi': float(latest.get('rsi14', 50)),
                'current_price': float(latest['close'])
            }
            
            return {
                'success': True,
                'chart_html': html_content,
                'current_signals': current_signals,
                'signal_summary': self._generate_signal_summary(current_signals),
                'timestamp': datetime.now().isoformat()
            }
            
        except Exception as e:
            return {'success': False, 'error': str(e)}
            
    def _generate_signal_summary(self, signals: Dict) -> str:
        """生成信号摘要"""
        summary = []
        
        if signals['buy_signal']:
            summary.append("🟢 强烈买入信号")
        elif signals['sell_signal']:
            summary.append("🔴 强烈卖出信号")
        else:
            summary.append("🟡 持有观望")
            
        if signals['ma_golden_cross']:
            summary.append("📈 均线金叉")
        if signals['macd_golden_cross']:
            summary.append("📊 MACD金叉")
            
        strength = signals['strength_score']
        if strength > 70:
            summary.append("💪 强势股")
        elif strength < 30:
            summary.append("📉 弱势股")
        else:
            summary.append("⚖️ 中性股")
            
        rsi = signals['rsi']
        if rsi > 70:
            summary.append("⚠️ RSI超买")
        elif rsi < 30:
            summary.append("✅ RSI超卖")
            
        return " | ".join(summary)
        
    def create_portfolio_dashboard(self, stock_codes: List[str]) -> Dict[str, Any]:
        """创建投资组合仪表板"""
        try:
            portfolio_data = []
            
            for code in stock_codes[:10]:  # 最多显示10只股票
                # 获取最新数据
                end_date = datetime.now().strftime('%Y%m%d')
                start_date = (datetime.now() - timedelta(days=30)).strftime('%Y%m%d')
                df = db.query_stock_data(code, start_date, end_date)
                
                if not df.empty:
                    # 计算指标
                    df = tech_indicator.calculate_all_indicators(df)
                    signals = tech_indicator.generate_signals(df)
                    
                    latest = df.iloc[-1]
                    latest_signals = signals.iloc[-1] if not signals.empty else {}
                    
                    portfolio_data.append({
                        'ts_code': code,
                        'current_price': float(latest['close']),
                        'change_pct': float(latest.get('change_pct', 0)),
                        'strength_score': float(latest.get('strength_score', 50)),
                        'buy_signal': int(latest_signals.get('buy_signal', 0)),
                        'sell_signal': int(latest_signals.get('sell_signal', 0)),
                        'rsi': float(latest.get('rsi14', 50))
                    })
                    
            if not portfolio_data:
                return {'success': False, 'error': '投资组合无有效数据'}
                
            # 创建仪表板图表
            fig = make_subplots(
                rows=2, cols=2,
                subplot_titles=['价格表现', '强弱度分布', '信号统计', 'RSI分布'],
                specs=[[{"type": "bar"}, {"type": "scatter"}],
                       [{"type": "pie"}, {"type": "histogram"}]]
            )
            
            # 价格表现柱状图
            codes = [data['ts_code'] for data in portfolio_data]
            changes = [data['change_pct'] for data in portfolio_data]
            colors = [self.colors['up'] if x >= 0 else self.colors['down'] for x in changes]
            
            fig.add_trace(
                go.Bar(x=codes, y=changes, marker_color=colors, name='涨跌幅%'),
                row=1, col=1
            )
            
            # 强弱度散点图
            strengths = [data['strength_score'] for data in portfolio_data]
            fig.add_trace(
                go.Scatter(x=codes, y=strengths, mode='markers+lines', name='强弱度'),
                row=1, col=2
            )
            
            # 信号饼图
            buy_count = sum(1 for data in portfolio_data if data['buy_signal'])
            sell_count = sum(1 for data in portfolio_data if data['sell_signal'])
            hold_count = len(portfolio_data) - buy_count - sell_count
            
            fig.add_trace(
                go.Pie(labels=['买入', '卖出', '持有'], values=[buy_count, sell_count, hold_count]),
                row=2, col=1
            )
            
            # RSI直方图
            rsi_values = [data['rsi'] for data in portfolio_data]
            fig.add_trace(
                go.Histogram(x=rsi_values, name='RSI分布'),
                row=2, col=2
            )
            
            fig.update_layout(
                title='投资组合实时监控仪表板',
                template='plotly_dark',
                height=800
            )
            
            # 转换为HTML (包含内联Plotly.js)
            html_content = fig.to_html(include_plotlyjs=True, div_id="portfolio-chart-div", full_html=False)
            
            return {
                'success': True,
                'dashboard_html': html_content,
                'portfolio_summary': {
                    'total_stocks': len(portfolio_data),
                    'buy_signals': buy_count,
                    'sell_signals': sell_count,
                    'avg_strength': sum(strengths) / len(strengths),
                    'avg_change': sum(changes) / len(changes)
                },
                'timestamp': datetime.now().isoformat()
            }
            
        except Exception as e:
            return {'success': False, 'error': str(e)}

# 全局图表生成器实例
chart_generator = StockChartGenerator()

if __name__ == "__main__":
    # 测试图表生成
    test_code = '000001.SZ'
    
    # 测试综合图表
    result = chart_generator.create_comprehensive_chart(test_code)
    if result['success']:
        print(f"✅ {test_code} 综合图表生成成功")
        # 保存到文件
        with open(f'charts/{test_code}_comprehensive.html', 'w', encoding='utf-8') as f:
            f.write(result['chart_html'])
    else:
        print(f"❌ 图表生成失败: {result['error']}")
        
    # 测试实时信号图表
    result = chart_generator.create_realtime_signal_chart(test_code)
    if result['success']:
        print(f"✅ {test_code} 实时信号图表生成成功")
        print(f"📊 当前信号: {result['signal_summary']}")
    else:
        print(f"❌ 实时图表生成失败: {result['error']}") 