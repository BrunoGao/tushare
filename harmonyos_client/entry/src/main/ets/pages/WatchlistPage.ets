/**
 * 自选股页面 - ExpressVPN风格设计
 * 个人股票关注列表和投资组合管理
 */

import { DataState, StockBasicInfo } from '../types/ApiTypes';
import { DataManager } from '../managers/DataManager';
import { ConfigManager } from '../config/ConfigManager';

@Component
export struct WatchlistPage {
  @Prop dataState: DataState;
  @Prop onRefresh: () => void;
  
  @State private isEditMode: boolean = false;
  @State private selectedItems: string[] = [];
  @State private sortBy: string = 'add_time'; // add_time, pct_chg, name
  @State private viewMode: 'list' | 'grid' = 'list';
  @State private totalValue: number = 0;
  @State private totalGain: number = 0;
  @State private totalGainPercent: number = 0;

  private dataManager: DataManager = DataManager.getInstance();
  private configManager: ConfigManager = ConfigManager.getInstance();

  aboutToAppear() {
    this.calculatePortfolioStats();
  }

  build() {
    Column() {
      // 自选股头部
      this.buildWatchlistHeader()
      
      // 投资组合概览
      this.buildPortfolioOverview()
      
      // 操作工具栏
      this.buildToolbar()
      
      // 自选股列表
      if (this.dataState.watchlist.length === 0) {
        this.buildEmptyWatchlist()
      } else {
        this.buildWatchlistContent()
      }
    }
    .width('100%')
    .height('100%')
    .backgroundColor('#F8F9FA')
  }

  /**
   * 自选股头部
   */
  @Builder
  buildWatchlistHeader() {
    Stack() {
      // 渐变背景
      Column()
        .width('100%')
        .height(160)
        .linearGradient({
          angle: 135,
          colors: [['#f59e0b', 0.0], ['#d97706', 1.0]]
        })
        .borderRadius({ bottomLeft: 24, bottomRight: 24 })

      // 头部内容
      Column() {
        Row() {
          Column() {
            Text('我的自选')
              .fontSize(28)
              .fontWeight(FontWeight.Bold)
              .fontColor('#FFFFFF')
              .margin({ bottom: 4 })
            
            Text(`${this.dataState.watchlist.length} 只股票`)
              .fontSize(14)
              .fontColor('#FFFFFF')
              .opacity(0.8)
          }
          .alignItems(HorizontalAlign.Start)
          .flexGrow(1)

          // 操作按钮
          Row() {
            Button() {
              Image($r('app.media.ic_add'))
                .width(20)
                .height(20)
                .fillColor('#FFFFFF')
            }
            .width(40)
            .height(40)
            .backgroundColor('rgba(255, 255, 255, 0.2)')
            .borderRadius(20)
            .onClick(() => {
              this.showAddStockDialog();
            })

            Button() {
              Image($r('app.media.ic_edit'))
                .width(20)
                .height(20)
                .fillColor('#FFFFFF')
            }
            .width(40)
            .height(40)
            .backgroundColor('rgba(255, 255, 255, 0.2)')
            .borderRadius(20)
            .margin({ left: 8 })
            .onClick(() => {
              this.toggleEditMode();
            })
          }
        }
        .width('100%')
        .margin({ bottom: 20 })

        // 快速操作
        Row() {
          this.buildQuickActionChip('全部上涨', this.getUpStocksCount(), '#10B981')
          this.buildQuickActionChip('全部下跌', this.getDownStocksCount(), '#EF4444')
          this.buildQuickActionChip('平盘', this.getFlatStocksCount(), '#6B7280')
        }
        .justifyContent(FlexAlign.SpaceEvenly)
        .width('100%')
      }
      .padding({ left: 20, right: 20, top: 40 })
      .width('100%')
    }
    .width('100%')
    .height(160)
  }

  /**
   * 快速操作芯片
   */
  @Builder
  buildQuickActionChip(label: string, count: number, color: string) {
    Column() {
      Text(count.toString())
        .fontSize(18)
        .fontWeight(FontWeight.Bold) 
        .fontColor('#FFFFFF')
      
      Text(label)
        .fontSize(10)
        .fontColor('#FFFFFF')
        .opacity(0.8)
        .margin({ top: 2 })
    }
    .alignItems(HorizontalAlign.Center)
    .padding({ horizontal: 16, vertical: 8 })
    .backgroundColor('rgba(255, 255, 255, 0.2)')
    .borderRadius(16)
  }

  /**
   * 投资组合概览
   */
  @Builder
  buildPortfolioOverview() {
    Column() {
      Text('投资组合概览')
        .fontSize(18)
        .fontWeight(FontWeight.Medium)
        .fontColor('#1F2937')
        .alignSelf(ItemAlign.Start)
        .margin({ bottom: 16 })

      // 主要指标
      Row() {
        this.buildPortfolioMetric('总市值', `¥${this.formatNumber(this.totalValue)}`, '#1F2937')
        this.buildPortfolioMetric('总盈亏', `¥${this.formatNumber(this.totalGain)}`, this.totalGain >= 0 ? '#10B981' : '#EF4444')
        this.buildPortfolioMetric('收益率', `${this.totalGainPercent >= 0 ? '+' : ''}${this.totalGainPercent.toFixed(2)}%`, this.totalGainPercent >= 0 ? '#10B981' : '#EF4444')
      }
      .justifyContent(FlexAlign.SpaceBetween)
      .width('100%')
      .margin({ bottom: 16 })

      // 今日盈亏统计
      Row() {
        Column() {
          Text('今日盈亏')
            .fontSize(12)
            .fontColor('#6B7280')
          
          Text(this.calculateTodayPnL())
            .fontSize(16)
            .fontWeight(FontWeight.Medium)
            .fontColor(this.getTodayPnLColor())
            .margin({ top: 4 })
        }
        .alignItems(HorizontalAlign.Start)

        Blank()

        // 盈亏分布饼图占位
        Stack() {
          Circle({ width: 60, height: 60 })
            .fill('#F3F4F6')
          
          Circle({ width: 50, height: 50 })
            .fill('#10B981')
            .opacity(0.3)
          
          Text('盈亏')
            .fontSize(10)
            .fontColor('#6B7280')
        }
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
    .margin({ top: -30, left: 20, right: 20, bottom: 20 })
  }

  /**
   * 投资组合指标
   */
  @Builder
  buildPortfolioMetric(label: string, value: string, color: string) {
    Column() {
      Text(value)
        .fontSize(20)
        .fontWeight(FontWeight.Bold)
        .fontColor(color)
      
      Text(label)
        .fontSize(12)
        .fontColor('#6B7280')
        .margin({ top: 4 })
    }
    .alignItems(HorizontalAlign.Center)
  }

  /**
   * 操作工具栏
   */
  @Builder
  buildToolbar() {
    Row() {
      // 视图切换
      Row() {
        Button() {
          Image($r('app.media.ic_view_list'))
            .width(16)
            .height(16)
            .fillColor(this.viewMode === 'list' ? '#F59E0B' : '#6B7280')
        }
        .width(32)
        .height(32)
        .backgroundColor(this.viewMode === 'list' ? 'rgba(245, 158, 11, 0.1)' : '#F3F4F6')
        .borderRadius(8)
        .onClick(() => {
          this.viewMode = 'list';
        })

        Button() {
          Image($r('app.media.ic_view_grid'))
            .width(16)
            .height(16)
            .fillColor(this.viewMode === 'grid' ? '#F59E0B' : '#6B7280')
        }
        .width(32)
        .height(32)
        .backgroundColor(this.viewMode === 'grid' ? 'rgba(245, 158, 11, 0.1)' : '#F3F4F6')
        .borderRadius(8)
        .margin({ left: 4 })
        .onClick(() => {
          this.viewMode = 'grid';
        })
      }

      Blank()

      // 排序选择
      Button() {
        Row() {
          Text(this.getSortLabel())
            .fontSize(12)
            .fontColor('#6B7280')
          
          Image($r('app.media.ic_sort'))
            .width(16)
            .height(16)
            .fillColor('#6B7280')
            .margin({ left: 4 })
        }
      }
      .backgroundColor('#F3F4F6')
      .borderRadius(16)
      .padding({ horizontal: 12, vertical: 6 })
      .onClick(() => {
        this.showSortOptions();
      })

      // 编辑模式下的批量操作
      if (this.isEditMode) {
        Button() {
          Text(`删除(${this.selectedItems.length})`)
            .fontSize(12)
            .fontColor('#EF4444')
        }
        .backgroundColor('rgba(239, 68, 68, 0.1)')
        .borderRadius(16)
        .padding({ horizontal: 12, vertical: 6 })
        .margin({ left: 8 })
        .onClick(() => {
          this.deleteSelectedStocks();
        })
      }
    }
    .width('100%')
    .padding({ left: 20, right: 20 })
    .margin({ bottom: 20 })
  }

  /**
   * 自选股内容
   */
  @Builder
  buildWatchlistContent() {
    if (this.viewMode === 'list') {
      this.buildWatchlistList()
    } else {
      this.buildWatchlistGrid()
    }
  }

  /**
   * 列表视图
   */
  @Builder
  buildWatchlistList() {
    List() {
      ForEach(this.dataState.watchlist, (stock: StockBasicInfo, index: number) => {
        ListItem() {
          this.buildStockListItem(stock, index)
        }
        .margin({ horizontal: 20, vertical: 4 })
      })
    }
    .width('100%')
    .layoutWeight(1)
    .padding({ bottom: 20 })
    .divider({ strokeWidth: 0 })
  }

  /**
   * 网格视图
   */
  @Builder
  buildWatchlistGrid() {
    Grid() {
      ForEach(this.dataState.watchlist, (stock: StockBasicInfo) => {
        GridItem() {
          this.buildStockGridItem(stock)
        }
      })
    }
    .columnsTemplate('1fr 1fr')
    .columnsGap(12)
    .rowsGap(12)
    .width('100%')
    .layoutWeight(1)
    .padding({ left: 20, right: 20, bottom: 20 })
  }

  /**
   * 股票列表项
   */
  @Builder
  buildStockListItem(stock: StockBasicInfo, index: number) {
    Row() {
      // 编辑模式选择框
      if (this.isEditMode) {
        Checkbox({ name: stock.ts_code, group: 'watchlist' })
          .select(this.selectedItems.includes(stock.ts_code))
          .onChange((value: boolean) => {
            this.onStockSelect(stock.ts_code, value);
          })
          .margin({ right: 12 })
      }

      // 股票信息
      Column() {
        Text(stock.name)
          .fontSize(16)
          .fontWeight(FontWeight.Medium)
          .fontColor('#1F2937')
          .alignSelf(ItemAlign.Start)
          .maxLines(1)
          .textOverflow({ overflow: TextOverflow.Ellipsis })

        Row() {
          Text(stock.ts_code)
            .fontSize(12)
            .fontColor('#6B7280')
          
          if (stock.industry) {
            Text(stock.industry)
              .fontSize(10)
              .fontColor('#3B82F6')
              .backgroundColor('rgba(59, 130, 246, 0.1)')
              .padding({ horizontal: 6, vertical: 2 })
              .borderRadius(8)
              .margin({ left: 8 })
          }
        }
        .margin({ top: 4 })
        .alignSelf(ItemAlign.Start)
      }
      .alignItems(HorizontalAlign.Start)
      .flexGrow(1)

      // 价格信息
      Column() {
        Text(`¥${stock.latest_price?.close?.toFixed(2) || '--'}`)
          .fontSize(16)
          .fontWeight(FontWeight.Bold)
          .fontColor('#1F2937')
          .alignSelf(ItemAlign.End)

        Row() {
          Text(this.formatPriceChange(stock.latest_price?.pct_chg || 0))
            .fontSize(12)
            .fontColor(this.getPriceChangeColor(stock.latest_price?.pct_chg || 0))
          
          Image(this.getPriceChangeIcon(stock.latest_price?.pct_chg || 0))
            .width(12)
            .height(12)
            .fillColor(this.getPriceChangeColor(stock.latest_price?.pct_chg || 0))
            .margin({ left: 4 })
        }
        .margin({ top: 4 })
        .alignSelf(ItemAlign.End)
      }
      .alignItems(HorizontalAlign.End)
    }
    .width('100%')
    .padding(16)
    .backgroundColor('#FFFFFF')
    .borderRadius(12)
    .shadow({
      radius: 6,
      color: 'rgba(0, 0, 0, 0.03)',
      offsetX: 0,
      offsetY: 1
    })
    .onClick(() => {
      if (!this.isEditMode) {
        this.navigateToStockDetail(stock.ts_code);
      }
    })
  }

  /**
   * 股票网格项
   */
  @Builder
  buildStockGridItem(stock: StockBasicInfo) {
    Column() {
      // 编辑模式选择框
      if (this.isEditMode) {
        Row() {
          Checkbox({ name: stock.ts_code, group: 'watchlist' })
            .select(this.selectedItems.includes(stock.ts_code))
            .onChange((value: boolean) => {
              this.onStockSelect(stock.ts_code, value);
            })
          
          Blank()
        }
        .width('100%')
        .margin({ bottom: 8 })
      }

      // 股票名称
      Text(stock.name)
        .fontSize(14)
        .fontWeight(FontWeight.Medium)
        .fontColor('#1F2937')
        .maxLines(1)
        .textOverflow({ overflow: TextOverflow.Ellipsis })
        .width('100%')
        .textAlign(TextAlign.Center)

      Text(stock.ts_code)
        .fontSize(10)
        .fontColor('#6B7280')
        .margin({ top: 2 })

      // 价格信息
      Text(`¥${stock.latest_price?.close?.toFixed(2) || '--'}`)
        .fontSize(18)
        .fontWeight(FontWeight.Bold)
        .fontColor('#1F2937')
        .margin({ top: 12 })

      Text(this.formatPriceChange(stock.latest_price?.pct_chg || 0))
        .fontSize(12)
        .fontColor(this.getPriceChangeColor(stock.latest_price?.pct_chg || 0))
        .margin({ top: 4 })
    }
    .width('100%')
    .padding(16)
    .backgroundColor('#FFFFFF')
    .borderRadius(12)
    .shadow({
      radius: 6,
      color: 'rgba(0, 0, 0, 0.03)',
      offsetX: 0,
      offsetY: 1
    })
    .alignItems(HorizontalAlign.Center)
    .justifyContent(FlexAlign.Center)
    .onClick(() => {
      if (!this.isEditMode) {
        this.navigateToStockDetail(stock.ts_code);
      }
    })
  }

  /**
   * 空自选股状态
   */
  @Builder
  buildEmptyWatchlist() {
    Column() {
      Image($r('app.media.ic_watchlist_empty'))
        .width(120)
        .height(120)
        .opacity(0.4)
        .margin({ bottom: 24 })
      
      Text('自选股为空')
        .fontSize(20)
        .fontWeight(FontWeight.Medium)
        .fontColor('#1F2937')
        .margin({ bottom: 8 })
      
      Text('添加感兴趣的股票到自选列表')
        .fontSize(14)
        .fontColor('#6B7280')
        .textAlign(TextAlign.Center)
        .margin({ bottom: 32 })

      Button('添加股票')
        .fontSize(16)
        .fontColor('#FFFFFF')
        .backgroundColor('#F59E0B')
        .borderRadius(24)
        .padding({ horizontal: 32, vertical: 12 })
        .shadow({
          radius: 8,
          color: 'rgba(245, 158, 11, 0.3)',
          offsetX: 0,
          offsetY: 4
        })
        .onClick(() => {
          this.showAddStockDialog();
        })
    }
    .justifyContent(FlexAlign.Center)
    .alignItems(HorizontalAlign.Center)
    .width('100%')
    .layoutWeight(1)
  }

  // ============ 业务逻辑方法 ============

  private calculatePortfolioStats() {
    // 计算投资组合统计数据
    this.totalValue = this.dataState.watchlist.reduce((sum, stock) => {
      return sum + (stock.latest_price?.close || 0) * 100; // 假设每只股票100股
    }, 0);
    
    this.totalGain = this.dataState.watchlist.reduce((sum, stock) => {
      const change = (stock.latest_price?.pct_chg || 0) / 100;
      return sum + (stock.latest_price?.close || 0) * 100 * change;
    }, 0);
    
    this.totalGainPercent = this.totalValue > 0 ? (this.totalGain / (this.totalValue - this.totalGain)) * 100 : 0;
  }

  private calculateTodayPnL(): string {
    const todayPnL = this.dataState.watchlist.reduce((sum, stock) => {
      const change = (stock.latest_price?.pct_chg || 0) / 100;
      return sum + (stock.latest_price?.close || 0) * 100 * change;
    }, 0);
    
    return `¥${this.formatNumber(todayPnL)}`;
  }

  private getTodayPnLColor(): string {
    const todayPnL = this.dataState.watchlist.reduce((sum, stock) => {
      const change = (stock.latest_price?.pct_chg || 0) / 100;
      return sum + (stock.latest_price?.close || 0) * 100 * change;
    }, 0);
    
    return todayPnL >= 0 ? '#10B981' : '#EF4444';
  }

  private getUpStocksCount(): number {
    return this.dataState.watchlist.filter(stock => (stock.latest_price?.pct_chg || 0) > 0).length;
  }

  private getDownStocksCount(): number {
    return this.dataState.watchlist.filter(stock => (stock.latest_price?.pct_chg || 0) < 0).length;
  }

  private getFlatStocksCount(): number {
    return this.dataState.watchlist.filter(stock => (stock.latest_price?.pct_chg || 0) === 0).length;
  }

  private toggleEditMode() {
    this.isEditMode = !this.isEditMode;
    if (!this.isEditMode) {
      this.selectedItems = [];
    }
  }

  private onStockSelect(tsCode: string, selected: boolean) {
    if (selected) {
      this.selectedItems.push(tsCode);
    } else {
      const index = this.selectedItems.indexOf(tsCode);
      if (index > -1) {
        this.selectedItems.splice(index, 1);
      }
    }
  }

  private deleteSelectedStocks() {
    // 删除选中的股票
    this.selectedItems.forEach(tsCode => {
      this.dataManager.removeFromWatchlist(tsCode);
    });
    this.selectedItems = [];
    this.isEditMode = false;
  }

  private showAddStockDialog() {
    // 显示添加股票对话框
    console.log('显示添加股票对话框');
  }

  private showSortOptions() {
    // 显示排序选项
    console.log('显示排序选项');
  }

  private getSortLabel(): string {
    switch (this.sortBy) {
      case 'add_time': return '添加时间';
      case 'pct_chg': return '涨跌幅';
      case 'name': return '股票名称';
      default: return '排序';
    }
  }

  private navigateToStockDetail(tsCode: string) {
    console.log('导航到股票详情:', tsCode);
  }

  // ============ 工具方法 ============

  private formatNumber(num: number): string {
    if (Math.abs(num) >= 100000000) {
      return (num / 100000000).toFixed(2) + '亿';
    } else if (Math.abs(num) >= 10000) {
      return (num / 10000).toFixed(1) + '万';
    }
    return num.toFixed(2);
  }

  private formatPriceChange(change: number): string {
    return `${change >= 0 ? '+' : ''}${change.toFixed(2)}%`;
  }

  private getPriceChangeColor(change: number): string {
    return change >= 0 ? '#10B981' : '#EF4444';
  }

  private getPriceChangeIcon(change: number): Resource {
    return change >= 0 ? $r('app.media.ic_arrow_up') : $r('app.media.ic_arrow_down');
  }
}