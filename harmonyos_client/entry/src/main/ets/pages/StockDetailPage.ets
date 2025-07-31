/**
 * 股票详情页面 - 现代化设计
 * 参考ExpressVPN的卡片式布局和视觉层次
 */

import { StockBasicInfo, KLineData, TechnicalAnalysis, AIRecommendation } from '../types/ApiTypes';
import { DataManager } from '../managers/DataManager';
import { ConfigManager } from '../config/ConfigManager';

@Component
export struct StockDetailPage {
  @Prop tsCode: string;
  @State stockInfo: StockBasicInfo | null = null;
  @State klineData: KLineData | null = null;
  @State technicalAnalysis: TechnicalAnalysis | null = null;
  @State aiPrediction: any = null;
  @State isLoading: boolean = true;
  @State selectedPeriod: string = '1D';
  @State isInWatchlist: boolean = false;

  private dataManager: DataManager = DataManager.getInstance();
  private configManager: ConfigManager = ConfigManager.getInstance();

  aboutToAppear() {
    this.loadStockData();
    this.isInWatchlist = this.dataManager.isInWatchlist(this.tsCode);
  }

  build() {
    Column() {
      // 自定义导航栏
      this.buildCustomNavBar()
      
      if (this.isLoading) {
        this.buildLoadingView()
      } else {
        Scroll() {
          Column() {
            // 股票头部信息
            this.buildStockHeader()
            
            // 价格图表区域
            this.buildPriceChart()
            
            // 时间周期选择器
            this.buildPeriodSelector()
            
            // 技术指标卡片
            this.buildTechnicalIndicators()
            
            // AI分析卡片
            this.buildAIAnalysis()
            
            // 操作按钮区域
            this.buildActionButtons()
          }
          .padding({ left: 20, right: 20, bottom: 20 })
        }
        .width('100%')
        .height('100%')
      }
    }
    .width('100%')
    .height('100%')
    .backgroundColor('#F8F9FA')
  }

  /**
   * 自定义导航栏
   */
  @Builder
  buildCustomNavBar() {
    Row() {
      Button() {
        Image($r('app.media.ic_back'))
          .width(24)
          .height(24)
          .fillColor('#1F2937')
      }
      .width(44)
      .height(44)
      .backgroundColor('#FFFFFF')
      .borderRadius(22)
      .shadow({
        radius: 8,
        color: 'rgba(0, 0, 0, 0.1)',
        offsetX: 0,
        offsetY: 2
      })
      .onClick(() => {
        // 返回上一页
      })

      Blank()

      Text(this.stockInfo?.name || this.tsCode)
        .fontSize(18)
        .fontWeight(FontWeight.Medium)
        .fontColor('#1F2937')

      Blank()

      Row() {
        Button() {
          Image(this.isInWatchlist ? $r('app.media.ic_favorite_filled') : $r('app.media.ic_favorite'))
            .width(24)
            .height(24)
            .fillColor(this.isInWatchlist ? '#EF4444' : '#6B7280')
        }
        .width(44)
        .height(44)
        .backgroundColor('#FFFFFF')
        .borderRadius(22)
        .shadow({
          radius: 8,
          color: 'rgba(0, 0, 0, 0.1)',
          offsetX: 0,
          offsetY: 2
        })
        .onClick(() => {
          this.toggleWatchlist();
        })

        Button() {
          Image($r('app.media.ic_share'))
            .width(24)
            .height(24)
            .fillColor('#6B7280')
        }
        .width(44)
        .height(44)
        .backgroundColor('#FFFFFF')
        .borderRadius(22)
        .margin({ left: 8 })
        .shadow({
          radius: 8,
          color: 'rgba(0, 0, 0, 0.1)',
          offsetX: 0,
          offsetY: 2
        })
        .onClick(() => {
          // 分享功能
        })
      }
    }
    .width('100%')
    .height(60)
    .padding({ left: 20, right: 20, top: 10 })
    .backgroundColor('#F8F9FA')
  }

  /**
   * 股票头部信息
   */
  @Builder
  buildStockHeader() {
    Column() {
      // 公司名称和代码
      Row() {
        Column() {
          Text(this.stockInfo?.name || '--')
            .fontSize(24)
            .fontWeight(FontWeight.Bold)
            .fontColor('#1F2937')
            .alignSelf(ItemAlign.Start)
          
          Text(this.stockInfo?.ts_code || '--')
            .fontSize(14)
            .fontColor('#6B7280')
            .margin({ top: 4 })
            .alignSelf(ItemAlign.Start)
        }
        .alignItems(HorizontalAlign.Start)
        .flexGrow(1)

        // 行业标签
        Text(this.stockInfo?.industry || '--')
          .fontSize(12)
          .fontColor('#3B82F6')
          .padding({ horizontal: 12, vertical: 6 })
          .backgroundColor('rgba(59, 130, 246, 0.1)')
          .borderRadius(16)
      }
      .width('100%')
      .margin({ bottom: 20 })

      // 当前价格和涨跌
      this.buildPriceDisplay()
      
      // 市场状态指示器
      this.buildMarketStatus()
    }
    .width('100%')
    .padding(24)
    .backgroundColor('#FFFFFF')
    .borderRadius(20)
    .shadow({
      radius: 12,
      color: 'rgba(0, 0, 0, 0.06)',
      offsetX: 0,
      offsetY: 4
    })
    .margin({ top: 10, bottom: 20 })
  }

  /**
   * 价格显示区域
   */
  @Builder
  buildPriceDisplay() {
    Row() {
      Column() {
        Text(`¥${this.stockInfo?.latest_price?.close?.toFixed(2) || '--'}`)
          .fontSize(32)
          .fontWeight(FontWeight.Bold)
          .fontColor('#1F2937')
          .alignSelf(ItemAlign.Start)
        
        Row() {
          Text(this.formatPriceChange())
            .fontSize(16)
            .fontColor(this.getPriceChangeColor())
          
          Image(this.getPriceChangeIcon())
            .width(16)
            .height(16)
            .fillColor(this.getPriceChangeColor())
            .margin({ left: 6 })
        }
        .margin({ top: 8 })
        .alignSelf(ItemAlign.Start)
      }
      .alignItems(HorizontalAlign.Start)

      Blank()

      // 实时状态指示器
      Row() {
        Circle({ width: 8, height: 8 })
          .fill('#10B981')
        
        Text('实时')
          .fontSize(12)
          .fontColor('#10B981')
          .margin({ left: 6 })
      }
      .padding({ horizontal: 12, vertical: 6 })
      .backgroundColor('rgba(16, 185, 129, 0.1)')
      .borderRadius(16)
    }
    .width('100%')
    .margin({ bottom: 16 })
  }

  /**
   * 市场状态
   */
  @Builder
  buildMarketStatus() {
    Row() {
      this.buildStatusItem('开盘', this.stockInfo?.latest_price?.open?.toFixed(2) || '--')
      this.buildStatusItem('最高', this.stockInfo?.latest_price?.high?.toFixed(2) || '--')
      this.buildStatusItem('最低', this.stockInfo?.latest_price?.low?.toFixed(2) || '--')
      this.buildStatusItem('成交量', this.formatVolume(this.stockInfo?.latest_price?.volume || 0))
    }
    .justifyContent(FlexAlign.SpaceBetween)
    .width('100%')
  }

  /**
   * 状态项
   */
  @Builder
  buildStatusItem(label: string, value: string) {
    Column() {
      Text(value)
        .fontSize(14)
        .fontWeight(FontWeight.Medium)
        .fontColor('#1F2937')
      
      Text(label)
        .fontSize(12)
        .fontColor('#6B7280')
        .margin({ top: 2 })
    }
    .alignItems(HorizontalAlign.Center)
  }

  /**
   * 价格图表区域
   */
  @Builder
  buildPriceChart() {
    Column() {
      Row() {
        Text('价格走势')
          .fontSize(18)
          .fontWeight(FontWeight.Medium)
          .fontColor('#1F2937')
        
        Blank()
        
        Button('全屏查看')
          .fontSize(12)
          .fontColor('#3B82F6')
          .backgroundColor('transparent')
          .padding(0)
      }
      .width('100%')
      .margin({ bottom: 16 })

      // 图表容器
      Container() {
        // 这里可以集成图表库或使用Canvas绘制
        Column() {
          Text('K线图表')
            .fontSize(16)
            .fontColor('#6B7280')
            .margin({ bottom: 8 })
          
          Text('图表功能开发中...')
            .fontSize(12)
            .fontColor('#9CA3AF')
        }
        .justifyContent(FlexAlign.Center)
        .alignItems(HorizontalAlign.Center)
        .width('100%')
        .height(200)
        .backgroundColor('#F9FAFB')
        .borderRadius(12)
        .border({ width: 1, color: '#E5E7EB' })
      }
      .width('100%')
    }
    .width('100%')
    .padding(20)
    .backgroundColor('#FFFFFF')
    .borderRadius(16)
    .shadow({
      radius: 8,
      color: 'rgba(0, 0, 0, 0.04)',
      offsetX: 0,
      offsetY: 2
    })
    .margin({ bottom: 20 })
  }

  /**
   * 时间周期选择器
   */
  @Builder
  buildPeriodSelector() {
    Row() {
      this.buildPeriodButton('1D')
      this.buildPeriodButton('5D')
      this.buildPeriodButton('1M')
      this.buildPeriodButton('3M')
      this.buildPeriodButton('1Y')
    }
    .justifyContent(FlexAlign.SpaceEvenly)
    .width('100%')
    .height(48)
    .backgroundColor('#F3F4F6')
    .borderRadius(24)
    .margin({ bottom: 20 })
  }

  /**
   * 周期按钮
   */
  @Builder
  buildPeriodButton(period: string) {
    Button(period)
      .fontSize(14)
      .fontColor(this.selectedPeriod === period ? '#FFFFFF' : '#6B7280')
      .backgroundColor(this.selectedPeriod === period ? '#3B82F6' : 'transparent')
      .borderRadius(20)
      .padding({ horizontal: 16, vertical: 8 })
      .onClick(() => {
        this.selectedPeriod = period;
        this.loadKLineData();
      })
  }

  /**
   * 技术指标卡片
   */
  @Builder
  buildTechnicalIndicators() {
    Column() {
      Row() {
        Text('技术指标')
          .fontSize(18)
          .fontWeight(FontWeight.Medium)
          .fontColor('#1F2937')
        
        Blank()
        
        Row() {
          Circle({ width: 6, height: 6 })
            .fill('#10B981')
          
          Text('多头')
            .fontSize(12)
            .fontColor('#10B981')
            .margin({ left: 4 })
        }
        .padding({ horizontal: 8, vertical: 4 })
        .backgroundColor('rgba(16, 185, 129, 0.1)')
        .borderRadius(12)
      }
      .width('100%')
      .margin({ bottom: 16 })

      // 指标网格
      Grid() {
        GridItem() {
          this.buildIndicatorItem('MA5', this.technicalAnalysis?.latest_indicators.ma5?.toFixed(2) || '--', '#3B82F6')
        }
        
        GridItem() {
          this.buildIndicatorItem('MA20', this.technicalAnalysis?.latest_indicators.ma20?.toFixed(2) || '--', '#8B5CF6')
        }
        
        GridItem() {
          this.buildIndicatorItem('RSI', this.technicalAnalysis?.latest_indicators.rsi6?.toFixed(1) || '--', '#F59E0B')
        }
        
        GridItem() {
          this.buildIndicatorItem('MACD', this.technicalAnalysis?.latest_indicators.macd_dif?.toFixed(4) || '--', '#EF4444')
        }
      }
      .columnsTemplate('1fr 1fr')
      .rowsTemplate('1fr 1fr')
      .columnsGap(12)
      .rowsGap(12)
      .width('100%')
      .height(120)
    }
    .width('100%')
    .padding(20)
    .backgroundColor('#FFFFFF')
    .borderRadius(16)
    .shadow({
      radius: 8,
      color: 'rgba(0, 0, 0, 0.04)',
      offsetX: 0,
      offsetY: 2
    })
    .margin({ bottom: 20 })
  }

  /**
   * 指标项
   */
  @Builder
  buildIndicatorItem(name: string, value: string, color: string) {
    Column() {
      Text(value)
        .fontSize(18)
        .fontWeight(FontWeight.Bold)
        .fontColor(color)
        .margin({ bottom: 4 })
      
      Text(name)
        .fontSize(12)
        .fontColor('#6B7280')
    }
    .alignItems(HorizontalAlign.Center)
    .justifyContent(FlexAlign.Center)
    .width('100%')
    .height('100%')
    .backgroundColor('#F9FAFB')
    .borderRadius(12)
  }

  /**
   * AI分析卡片
   */
  @Builder
  buildAIAnalysis() {
    Column() {
      Row() {
        Row() {
          Image($r('app.media.ic_ai'))
            .width(20)
            .height(20)
            .fillColor('#8B5CF6')
          
          Text('AI智能分析')
            .fontSize(18)
            .fontWeight(FontWeight.Medium)
            .fontColor('#1F2937')
            .margin({ left: 8 })
        }
        
        Blank()
        
        Text('85%')
          .fontSize(14)
          .fontColor('#10B981')
          .fontWeight(FontWeight.Medium)
      }
      .width('100%')
      .margin({ bottom: 16 })

      if (this.aiPrediction) {
        this.buildAIPredictionContent()
      } else {
        this.buildAILoadingState()
      }
    }
    .width('100%')
    .padding(20)
    .backgroundColor('#FFFFFF')
    .borderRadius(16)
    .shadow({
      radius: 8,
      color: 'rgba(0, 0, 0, 0.04)',
      offsetX: 0,
      offsetY: 2
    })
    .margin({ bottom: 20 })
  }

  /**
   * AI预测内容
   */
  @Builder
  buildAIPredictionContent() {
    Column() {
      // 预测结果
      Row() {
        Text(this.aiPrediction?.prediction_text || '持有')
          .fontSize(20)
          .fontWeight(FontWeight.Bold)
          .fontColor(this.getAIPredictionColor())
        
        Blank()
        
        Text(`置信度: ${(this.aiPrediction?.confidence * 100 || 0).toFixed(1)}%`)
          .fontSize(12)
          .fontColor('#6B7280')
      }
      .width('100%')
      .margin({ bottom: 12 })

      // 预测描述
      Text('基于技术指标和市场趋势分析，当前建议持有观望，注意关键支撑位。')
        .fontSize(14)
        .fontColor('#6B7280')
        .lineHeight(20)
    }
    .alignItems(HorizontalAlign.Start)
    .width('100%')
  }

  /**
   * AI加载状态
   */
  @Builder
  buildAILoadingState() {
    Row() {
      LoadingProgress()
        .width(20)
        .height(20)
        .color('#8B5CF6')
      
      Text('AI正在分析...')
        .fontSize(14)
        .fontColor('#6B7280')
        .margin({ left: 12 })
    }
    .width('100%')
    .justifyContent(FlexAlign.Center)
    .padding({ vertical: 20 })
  }

  /**
   * 操作按钮区域
   */
  @Builder
  buildActionButtons() {
    Row() {
      Button('加入自选')
        .fontSize(16)
        .fontColor(this.isInWatchlist ? '#6B7280' : '#FFFFFF')
        .backgroundColor(this.isInWatchlist ? '#F3F4F6' : '#3B82F6')
        .borderRadius(24)
        .flexGrow(1)
        .height(48)
        .onClick(() => {
          this.toggleWatchlist();
        })
      
      Button('实时监控')
        .fontSize(16)
        .fontColor('#FFFFFF')
        .backgroundColor('#10B981')
        .borderRadius(24)
        .flexGrow(1)
        .height(48)
        .margin({ left: 12 })
        .onClick(() => {
          this.startRealTimeMonitoring();
        })
    }
    .width('100%')
    .margin({ top: 20 })
  }

  /**
   * 加载视图
   */
  @Builder
  buildLoadingView() {
    Column() {
      LoadingProgress()
        .width(40)
        .height(40)
        .color('#3B82F6')
        .margin({ bottom: 16 })
      
      Text('加载股票信息...')
        .fontSize(16)
        .fontColor('#6B7280')
    }
    .justifyContent(FlexAlign.Center)
    .alignItems(HorizontalAlign.Center)
    .width('100%')
    .height('100%')
  }

  // ============ 数据加载方法 ============

  private async loadStockData() {
    this.isLoading = true;
    
    try {
      // 并行加载股票数据
      const [stockInfo, klineData, technicalAnalysis] = await Promise.all([
        this.dataManager.getStockBasicInfo(this.tsCode),
        this.dataManager.getKLineData(this.tsCode, 120),
        this.dataManager.getTechnicalAnalysis(this.tsCode)
      ]);

      this.stockInfo = stockInfo;
      this.klineData = klineData;
      this.technicalAnalysis = technicalAnalysis;

      // 加载AI预测
      this.loadAIPrediction();
      
    } catch (error) {
      console.error('加载股票数据失败:', error);
    } finally {
      this.isLoading = false;
    }
  }

  private async loadKLineData() {
    const days = this.getPeriodDays(this.selectedPeriod);
    this.klineData = await this.dataManager.getKLineData(this.tsCode, days);
  }

  private async loadAIPrediction() {
    try {
      // 这里调用AI预测API
      // this.aiPrediction = await this.dataManager.getAIPrediction(this.tsCode);
    } catch (error) {
      console.error('加载AI预测失败:', error);
    }
  }

  // ============ 工具方法 ============

  private formatPriceChange(): string {
    const change = this.stockInfo?.latest_price?.pct_chg || 0;
    const amount = this.stockInfo?.latest_price?.close || 0;
    return `${change >= 0 ? '+' : ''}${change.toFixed(2)}% (${(amount * change / 100).toFixed(2)})`;
  }

  private getPriceChangeColor(): string {
    const change = this.stockInfo?.latest_price?.pct_chg || 0;
    return change >= 0 ? '#10B981' : '#EF4444';
  }

  private getPriceChangeIcon(): Resource {
    const change = this.stockInfo?.latest_price?.pct_chg || 0;
    return change >= 0 ? $r('app.media.ic_arrow_up') : $r('app.media.ic_arrow_down');
  }

  private getAIPredictionColor(): string {
    const prediction = this.aiPrediction?.prediction_text;
    if (prediction?.includes('买入')) return '#10B981';
    if (prediction?.includes('卖出')) return '#EF4444';
    return '#F59E0B';
  }

  private formatVolume(volume: number): string {
    if (volume >= 100000000) {
      return (volume / 100000000).toFixed(1) + '亿';
    } else if (volume >= 10000) {
      return (volume / 10000).toFixed(1) + '万';
    }
    return volume.toString();
  }

  private getPeriodDays(period: string): number {
    switch (period) {
      case '1D': return 1;
      case '5D': return 5;
      case '1M': return 30;
      case '3M': return 90;
      case '1Y': return 365;
      default: return 30;
    }
  }

  private toggleWatchlist() {
    if (this.isInWatchlist) {
      this.dataManager.removeFromWatchlist(this.tsCode);
    } else {
      this.dataManager.addToWatchlist(this.tsCode);
    }
    this.isInWatchlist = !this.isInWatchlist;
  }

  private startRealTimeMonitoring() {
    // 启动实时监控
  }
}