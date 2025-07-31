/**
 * 现代化市场页面 - 参考ExpressVPN设计风格
 * 简洁、现代、用户友好的界面设计
 */

import { DataState, MarketOverview, StockBasicInfo } from '../types/ApiTypes';
import { DataManager } from '../managers/DataManager';
import { ConfigManager } from '../config/ConfigManager';

@Component
export struct MarketPage {
  @Prop dataState: DataState;
  @Prop onRefresh: () => void;
  
  private dataManager: DataManager = DataManager.getInstance();
  private configManager: ConfigManager = ConfigManager.getInstance();
  
  @State private isRefreshing: boolean = false;
  @State private selectedTimeframe: string = '1D';

  build() {
    Scroll() {
      Column() {
        // 顶部渐变容器
        this.buildGradientHeader()
        
        // 主要内容区域
        Column() {
          // 市场状态卡片
          this.buildMarketStatusCard()
          
          // 快速操作区域
          this.buildQuickActions()
          
          // 热门股票列表
          this.buildHotStocksList()
          
          // AI推荐预览
          this.buildAIRecommendationsPreview()
        }
        .padding({ left: 20, right: 20, bottom: 20 })
        .width('100%')
      }
    }
    .backgroundColor('#F8F9FA')
    .width('100%')
    .height('100%')
    .refreshing(this.isRefreshing, {
      onRefreshing: () => {
        this.handleRefresh();
      }
    })
  }

  /**
   * 渐变头部区域
   */
  @Builder
  buildGradientHeader() {
    Stack() {
      // 渐变背景
      Column()
        .width('100%')
        .height(280)
        .linearGradient({
          angle: 135,
          colors: [['#667eea', 0.0], ['#764ba2', 1.0]]
        })
        .borderRadius({ bottomLeft: 24, bottomRight: 24 })

      // 头部内容
      Column() {
        // 问候语和时间
        Row() {
          Column() {
            Text('早上好')
              .fontSize(16)
              .fontColor('#FFFFFF')
              .opacity(0.8)
              .margin({ bottom: 4 })
            
            Text('市场概览')
              .fontSize(28)
              .fontWeight(FontWeight.Bold)
              .fontColor('#FFFFFF')
          }
          .alignItems(HorizontalAlign.Start)
          
          Blank()
          
          // 个人头像或设置按钮
          Button() {
            Image($r('app.media.ic_person'))
              .width(24)
              .height(24)
              .fillColor('#FFFFFF')
          }
          .width(48)
          .height(48)
          .backgroundColor('rgba(255, 255, 255, 0.2)')
          .borderRadius(24)
          .border({ width: 1, color: 'rgba(255, 255, 255, 0.3)' })
        }
        .width('100%')
        .margin({ top: 40, bottom: 32 })

        // 主要市场指标 - 大圆形显示
        this.buildMainMarketIndicator()
      }
      .padding({ left: 20, right: 20 })
      .width('100%')
    }
    .width('100%')
    .height(280)
  }

  /**
   * 主要市场指标 - 类似ExpressVPN的大圆形按钮
   */
  @Builder
  buildMainMarketIndicator() {
    Stack() {
      // 主要圆形背景
      Circle({ width: 140, height: 140 })
        .fill('rgba(255, 255, 255, 0.15)')
        .stroke('rgba(255, 255, 255, 0.3)')
        .strokeWidth(2)
      
      // 内部圆形
      Circle({ width: 120, height: 120 })
        .fill('rgba(255, 255, 255, 0.1)')
      
      // 市场指数信息
      Column() {
        Text('上证指数')
          .fontSize(14)
          .fontColor('#FFFFFF')
          .opacity(0.8)
          .margin({ bottom: 4 })
        
        Text(this.dataState.marketOverview?.indices[0]?.close?.toFixed(2) || '3,234.56')
          .fontSize(24)
          .fontWeight(FontWeight.Bold)
          .fontColor('#FFFFFF')
          .margin({ bottom: 2 })
        
        Row() {
          Text(this.getMarketChangeText())
            .fontSize(12)
            .fontColor(this.getMarketChangeColor())
          
          Image(this.getMarketTrendIcon())
            .width(12)
            .height(12)
            .fillColor(this.getMarketChangeColor())
            .margin({ left: 4 })
        }
      }
    }
    .width(140)
    .height(140)
    .onClick(() => {
      // 点击查看详细市场信息
      this.showMarketDetails();
    })
  }

  /**
   * 市场状态卡片
   */
  @Builder
  buildMarketStatusCard() {
    Column() {
      Row() {
        Text('市场状态')
          .fontSize(18)
          .fontWeight(FontWeight.Medium)
          .fontColor('#1F2937')
        
        Blank()
        
        Text('实时')
          .fontSize(12)
          .fontColor('#10B981')
          .padding({ horizontal: 8, vertical: 4 })
          .backgroundColor('rgba(16, 185, 129, 0.1)')
          .borderRadius(12)
      }
      .width('100%')
      .margin({ bottom: 16 })

      // 市场统计网格
      Grid() {
        GridItem() {
          this.buildStatItem(
            '上涨',
            this.dataState.marketOverview?.market_stats.rising_count?.toString() || '1,234',
            '#10B981'
          )
        }
        
        GridItem() {
          this.buildStatItem(
            '下跌', 
            this.dataState.marketOverview?.market_stats.falling_count?.toString() || '856',
            '#EF4444'
          )
        }
        
        GridItem() {
          this.buildStatItem(
            '平盘',
            this.dataState.marketOverview?.advance_decline.unchanged?.toString() || '234',
            '#6B7280'
          )
        }
        
        GridItem() {
          this.buildStatItem(
            '总数',
            this.dataState.marketOverview?.market_stats.total_stocks?.toString() || '4,567', 
            '#3B82F6'
          )
        }
      }
      .columnsTemplate('1fr 1fr 1fr 1fr')
      .width('100%')
      .height(80)
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
    .margin({ top: -60, bottom: 20 })
  }

  /**
   * 统计项构建器
   */
  @Builder
  buildStatItem(title: string, value: string, color: string) {
    Column() {
      Text(value)
        .fontSize(20)
        .fontWeight(FontWeight.Bold)
        .fontColor(color)
        .margin({ bottom: 4 })
      
      Text(title)
        .fontSize(12)
        .fontColor('#6B7280')
    }
    .alignItems(HorizontalAlign.Center)
    .justifyContent(FlexAlign.Center)
    .width('100%')
    .height('100%')
  }

  /**
   * 快速操作区域
   */
  @Builder
  buildQuickActions() {
    Column() {
      Text('快速操作')
        .fontSize(18)
        .fontWeight(FontWeight.Medium)
        .fontColor('#1F2937')
        .alignSelf(ItemAlign.Start)
        .margin({ bottom: 16 })

      Row() {
        this.buildActionButton('搜索股票', $r('app.media.ic_search'), '#3B82F6', () => {
          this.navigateToSearch();
        })
        
        this.buildActionButton('自选股', $r('app.media.ic_favorite'), '#F59E0B', () => {
          this.navigateToWatchlist();
        })
        
        this.buildActionButton('AI推荐', $r('app.media.ic_ai'), '#8B5CF6', () => {
          this.navigateToAI();
        })
        
        this.buildActionButton('实时监控', $r('app.media.ic_monitor'), '#10B981', () => {
          this.navigateToRealtime();
        })
      }
      .justifyContent(FlexAlign.SpaceBetween)
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
   * 操作按钮构建器
   */
  @Builder
  buildActionButton(label: string, icon: Resource, color: string, onTap: () => void) {
    Column() {
      Button() {
        Image(icon)
          .width(24)
          .height(24)
          .fillColor('#FFFFFF')
      }
      .width(56)
      .height(56)
      .backgroundColor(color)
      .borderRadius(28)
      .shadow({
        radius: 8,
        color: color + '40',
        offsetX: 0,
        offsetY: 4
      })
      .onClick(onTap)
      
      Text(label)
        .fontSize(12)
        .fontColor('#6B7280')
        .margin({ top: 8 })
    }
    .alignItems(HorizontalAlign.Center)
  }

  /**
   * 热门股票列表
   */
  @Builder
  buildHotStocksList() {
    Column() {
      Row() {
        Text('热门股票')
          .fontSize(18)
          .fontWeight(FontWeight.Medium)
          .fontColor('#1F2937')
        
        Blank()
        
        Button('查看全部')
          .fontSize(14)
          .fontColor('#3B82F6')
          .backgroundColor('transparent')
          .padding(0)
      }
      .width('100%')
      .margin({ bottom: 16 })

      // 股票列表
      Column() {
        ForEach(this.dataState.hotStocks.slice(0, 5), (stock: StockBasicInfo) => {
          this.buildStockItem(stock)
        })
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
   * 股票项构建器
   */
  @Builder
  buildStockItem(stock: StockBasicInfo) {
    Row() {
      // 股票信息
      Column() {
        Text(stock.name)
          .fontSize(16)
          .fontWeight(FontWeight.Medium)
          .fontColor('#1F2937')
          .alignSelf(ItemAlign.Start)
        
        Text(stock.ts_code)
          .fontSize(12)
          .fontColor('#6B7280')
          .margin({ top: 2 })
          .alignSelf(ItemAlign.Start)
      }
      .alignItems(HorizontalAlign.Start)
      .flexGrow(1)

      // 价格信息
      Column() {
        Text(stock.latest_price?.close?.toFixed(2) || '--')
          .fontSize(16)
          .fontWeight(FontWeight.Medium)
          .fontColor('#1F2937')
          .alignSelf(ItemAlign.End)
        
        Text(this.formatPercentChange(stock.latest_price?.pct_chg || 0))
          .fontSize(12)
          .fontColor(this.getPercentChangeColor(stock.latest_price?.pct_chg || 0))
          .margin({ top: 2 })
          .alignSelf(ItemAlign.End)
      }
      .alignItems(HorizontalAlign.End)
    }
    .width('100%')
    .padding({ vertical: 12 })
    .border({ 
      width: { bottom: 0.5 }, 
      color: '#E5E7EB' 
    })
    .onClick(() => {
      this.navigateToStockDetail(stock.ts_code);
    })
  }

  /**
   * AI推荐预览
   */
  @Builder
  buildAIRecommendationsPreview() {
    Column() {
      Row() {
        Text('AI智能推荐')
          .fontSize(18)
          .fontWeight(FontWeight.Medium)
          .fontColor('#1F2937')
        
        Blank()
        
        Row() {
          Image($r('app.media.ic_ai'))
            .width(16)
            .height(16)
            .fillColor('#8B5CF6')
          
          Text('AI分析')
            .fontSize(12)
            .fontColor('#8B5CF6')
            .margin({ left: 4 })
        }
      }
      .width('100%')
      .margin({ bottom: 16 })

      if (this.dataState.aiRecommendations.length > 0) {
        Column() {
          ForEach(this.dataState.aiRecommendations.slice(0, 3), (rec: any) => {
            this.buildRecommendationItem(rec)
          })
        }
      } else {
        this.buildEmptyRecommendations()
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
  }

  /**
   * 推荐项构建器
   */
  @Builder
  buildRecommendationItem(recommendation: any) {
    Row() {
      Column() {
        Text(recommendation.name || recommendation.ts_code)
          .fontSize(16)
          .fontWeight(FontWeight.Medium)
          .fontColor('#1F2937')
          .alignSelf(ItemAlign.Start)
        
        Text(recommendation.reasoning?.substring(0, 30) + '...' || 'AI推荐')
          .fontSize(12)
          .fontColor('#6B7280')
          .margin({ top: 2 })
          .alignSelf(ItemAlign.Start)
      }
      .alignItems(HorizontalAlign.Start)
      .flexGrow(1)

      // 推荐标签
      Text(recommendation.recommendation === 'buy' ? '买入' : 
           recommendation.recommendation === 'sell' ? '卖出' : '持有')
        .fontSize(12)
        .fontColor('#FFFFFF')
        .padding({ horizontal: 12, vertical: 6 })
        .backgroundColor(this.getRecommendationColor(recommendation.recommendation))
        .borderRadius(16)
    }
    .width('100%')
    .padding({ vertical: 12 })
    .border({ 
      width: { bottom: 0.5 }, 
      color: '#E5E7EB' 
    })
  }

  /**
   * 空推荐状态
   */
  @Builder
  buildEmptyRecommendations() {
    Column() {
      Image($r('app.media.ic_ai_empty'))
        .width(48)
        .height(48)
        .opacity(0.4)
        .margin({ bottom: 12 })
      
      Text('AI正在分析市场')
        .fontSize(14)
        .fontColor('#6B7280')
        .margin({ bottom: 8 })
      
      Button('立即获取推荐')
        .fontSize(14)
        .fontColor('#FFFFFF')
        .backgroundColor('#8B5CF6')
        .borderRadius(20)
        .padding({ horizontal: 20, vertical: 8 })
        .onClick(() => {
          this.dataManager.getAIRecommendations(20, false);
        })
    }
    .alignItems(HorizontalAlign.Center)
    .padding({ vertical: 24 })
  }

  // ============ 工具方法 ============

  private async handleRefresh() {
    this.isRefreshing = true;
    await this.onRefresh();
    this.isRefreshing = false;
  }

  private getMarketChangeText(): string {
    const change = this.dataState.marketOverview?.indices[0]?.pct_chg || 0;
    return `${change >= 0 ? '+' : ''}${change.toFixed(2)}%`;
  }

  private getMarketChangeColor(): string {
    const change = this.dataState.marketOverview?.indices[0]?.pct_chg || 0;
    return change >= 0 ? '#10B981' : '#EF4444';
  }

  private getMarketTrendIcon(): Resource {
    const change = this.dataState.marketOverview?.indices[0]?.pct_chg || 0;
    return change >= 0 ? $r('app.media.ic_trend_up') : $r('app.media.ic_trend_down');
  }

  private formatPercentChange(change: number): string {
    return `${change >= 0 ? '+' : ''}${change.toFixed(2)}%`;
  }

  private getPercentChangeColor(change: number): string {
    return change >= 0 ? '#10B981' : '#EF4444';
  }

  private getRecommendationColor(recommendation: string): string {
    switch (recommendation) {
      case 'buy': return '#10B981';
      case 'sell': return '#EF4444';
      default: return '#F59E0B';
    }
  }

  // ============ 导航方法 ============

  private showMarketDetails() {
    // 显示市场详情
  }

  private navigateToSearch() {
    // 导航到搜索页面
  }

  private navigateToWatchlist() {
    // 导航到自选股页面
  }

  private navigateToAI() {
    // 导航到AI推荐页面
  }

  private navigateToRealtime() {
    // 导航到实时监控页面
  }

  private navigateToStockDetail(tsCode: string) {
    // 导航到股票详情页面
  }
}