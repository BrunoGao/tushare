#!/usr/bin/env python3
"""
简化版图表生成器
不依赖复杂的技术指标库，提供基础图表功能
"""
import pandas as pd
import json
from datetime import datetime
from typing import Dict, List, Optional
import logging

logger = logging.getLogger(__name__)

class SimpleChartGenerator:
    """简化版图表生成器"""
    
    def create_basic_chart(self, ts_code: str, days: int = 120) -> Dict:
        """创建基础K线图"""
        try:
            from utils.db_helper import db
            from utils.technical_indicators_simple import tech_indicator
            
            # 获取股票数据
            end_date = datetime.now().strftime('%Y%m%d')
            start_date = (datetime.now() - pd.Timedelta(days=days)).strftime('%Y%m%d')
            df = db.query_stock_data(ts_code, start_date, end_date)
            
            if df.empty:
                return {
                    'success': False,
                    'error': f'未找到股票 {ts_code} 的数据'
                }
            
            # 计算技术指标
            df_with_indicators = tech_indicator.calculate_all_indicators(df)
            
            # 准备图表数据
            chart_data = self._prepare_chart_data(df_with_indicators)
            
            # 生成HTML图表
            chart_html = self._generate_chart_html(ts_code, chart_data)
            
            return {
                'success': True,
                'chart_html': chart_html,
                'stock_code': ts_code,
                'data_points': len(df),
                'latest_price': float(df['close'].iloc[-1]),
                'latest_date': df['trade_date'].iloc[-1].strftime('%Y-%m-%d')
            }
            
        except Exception as e:
            logger.error(f"创建图表失败: {e}")
            return {
                'success': False,
                'error': str(e)
            }
    
    def _prepare_chart_data(self, df: pd.DataFrame) -> Dict:
        """准备图表数据"""
        try:
            # 转换日期格式 - 修复日期类型问题
            if hasattr(df['trade_date'].iloc[0], 'strftime'):
                # 如果是date或datetime对象
                df['date_str'] = df['trade_date'].apply(lambda x: x.strftime('%Y-%m-%d'))
            else:
                # 如果是字符串
                df['date_str'] = df['trade_date'].astype(str)
            
            # K线数据
            kline_data = []
            for _, row in df.iterrows():
                kline_data.append([
                    row['date_str'],
                    float(row['open']),
                    float(row['close']),
                    float(row['low']),
                    float(row['high'])
                ])
            
            # 成交量数据
            volume_data = []
            for _, row in df.iterrows():
                volume_data.append([
                    row['date_str'],
                    float(row.get('vol', 0))
                ])
            
            # 均线数据
            ma_data = {}
            for ma_period in [5, 10, 20, 60]:
                ma_col = f'ma{ma_period}'
                if ma_col in df.columns:
                    ma_data[ma_period] = []
                    for _, row in df.iterrows():
                        if pd.notna(row[ma_col]):
                            ma_data[ma_period].append([
                                row['date_str'],
                                float(row[ma_col])
                            ])
            
            return {
                'kline': kline_data,
                'volume': volume_data,
                'ma_lines': ma_data,
                'dates': df['date_str'].tolist(),
                'latest_indicators': self._get_latest_indicators(df)
            }
            
        except Exception as e:
            logger.error(f"准备图表数据失败: {e}")
            return {}
    
    def _get_latest_indicators(self, df: pd.DataFrame) -> Dict:
        """获取最新技术指标"""
        try:
            latest = df.iloc[-1]
            return {
                'rsi': float(latest.get('rsi14', 0)) if pd.notna(latest.get('rsi14')) else 0,
                'macd': float(latest.get('macd', 0)) if pd.notna(latest.get('macd')) else 0,
                'ma5': float(latest.get('ma5', 0)) if pd.notna(latest.get('ma5')) else 0,
                'ma20': float(latest.get('ma20', 0)) if pd.notna(latest.get('ma20')) else 0,
                'strength_score': float(latest.get('strength_score', 50)) if pd.notna(latest.get('strength_score')) else 50
            }
        except:
            return {}
    
    def _generate_chart_html(self, ts_code: str, chart_data: Dict) -> str:
        """生成图表HTML"""
        try:
            # 使用ECharts生成图表
            html_template = f"""
            <div id="chart-container" style="width: 100%; height: 600px;"></div>
            <script src="https://cdn.jsdelivr.net/npm/echarts@5.4.3/dist/echarts.min.js"></script>
            <script>
                var chartDom = document.getElementById('chart-container');
                var myChart = echarts.init(chartDom);
                
                var klineData = {json.dumps(chart_data.get('kline', []))};
                var volumeData = {json.dumps(chart_data.get('volume', []))};
                var ma5Data = {json.dumps(chart_data.get('ma_lines', {}).get(5, []))};
                var ma20Data = {json.dumps(chart_data.get('ma_lines', {}).get(20, []))};
                
                var option = {{
                    title: {{
                        text: '{ts_code} 股票走势图',
                        left: 'center'
                    }},
                    tooltip: {{
                        trigger: 'axis',
                        axisPointer: {{
                            type: 'cross'
                        }}
                    }},
                    legend: {{
                        data: ['K线', 'MA5', 'MA20', '成交量'],
                        top: 30
                    }},
                    grid: [
                        {{
                            left: '10%',
                            right: '8%',
                            height: '50%'
                        }},
                        {{
                            left: '10%',
                            right: '8%',
                            top: '70%',
                            height: '16%'
                        }}
                    ],
                    xAxis: [
                        {{
                            type: 'category',
                            data: klineData.map(item => item[0]),
                            scale: true,
                            boundaryGap: false,
                            axisLine: {{ onZero: false }},
                            splitLine: {{ show: false }},
                            min: 'dataMin',
                            max: 'dataMax'
                        }},
                        {{
                            type: 'category',
                            gridIndex: 1,
                            data: volumeData.map(item => item[0]),
                            scale: true,
                            boundaryGap: false,
                            axisLine: {{ onZero: false }},
                            axisTick: {{ show: false }},
                            splitLine: {{ show: false }},
                            axisLabel: {{ show: false }},
                            min: 'dataMin',
                            max: 'dataMax'
                        }}
                    ],
                    yAxis: [
                        {{
                            scale: true,
                            splitArea: {{ show: true }}
                        }},
                        {{
                            scale: true,
                            gridIndex: 1,
                            splitNumber: 2,
                            axisLabel: {{ show: false }},
                            axisLine: {{ show: false }},
                            axisTick: {{ show: false }},
                            splitLine: {{ show: false }}
                        }}
                    ],
                    dataZoom: [
                        {{
                            type: 'inside',
                            xAxisIndex: [0, 1],
                            start: 80,
                            end: 100
                        }},
                        {{
                            show: true,
                            xAxisIndex: [0, 1],
                            type: 'slider',
                            top: '85%',
                            start: 80,
                            end: 100
                        }}
                    ],
                    series: [
                        {{
                            name: 'K线',
                            type: 'candlestick',
                            data: klineData.map(item => [item[1], item[2], item[3], item[4]]),
                            itemStyle: {{
                                color: '#ec0000',
                                color0: '#00da3c',
                                borderColor: '#8A0000',
                                borderColor0: '#008F28'
                            }}
                        }},
                        {{
                            name: 'MA5',
                            type: 'line',
                            data: ma5Data.map(item => item[1]),
                            smooth: true,
                            lineStyle: {{
                                opacity: 0.5,
                                color: '#FF6B6B'
                            }}
                        }},
                        {{
                            name: 'MA20',
                            type: 'line',
                            data: ma20Data.map(item => item[1]),
                            smooth: true,
                            lineStyle: {{
                                opacity: 0.5,
                                color: '#4ECDC4'
                            }}
                        }},
                        {{
                            name: '成交量',
                            type: 'bar',
                            xAxisIndex: 1,
                            yAxisIndex: 1,
                            data: volumeData.map(item => item[1])
                        }}
                    ]
                }};
                
                myChart.setOption(option);
                
                // 响应式调整
                window.addEventListener('resize', function() {{
                    myChart.resize();
                }});
            </script>
            """
            
            return html_template
            
        except Exception as e:
            logger.error(f"生成图表HTML失败: {e}")
            return f"<div>图表生成失败: {str(e)}</div>"

# 创建全局实例
simple_chart_generator = SimpleChartGenerator()