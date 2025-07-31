/**
 * 股票列表页面 - ExpressVPN风格设计
 * 现代化搜索和股票浏览界面
 */

import { DataState, StockBasicInfo } from '../types/ApiTypes';
import { DataManager } from '../managers/DataManager';
import { ConfigManager } from '../config/ConfigManager';

@Component
export struct StockListPage {
  @Prop dataState: DataState;
  @Prop onRefresh: () => void;
  
  @State private searchText: string = '';
  @State private isSearching: boolean = false;
  @State private selectedCategory: string = '全部';
  @State private sortBy: string = 'pct_chg'; // pct_chg, volume, market_cap
  @State private sortOrder: 'asc' | 'desc' = 'desc';
  @State private searchResults: StockBasicInfo[] = [];
  @State private categories: string[] = ['全部', '沪深主板', '科创板', '创业板', '北交所'];

  private dataManager: DataManager = DataManager.getInstance();
  private configManager: ConfigManager = ConfigManager.getInstance();

  aboutToAppear() {
    this.searchResults = this.dataState.hotStocks;
  }

  build() {
    Column() {
      // 搜索头部区域
      this.buildSearchHeader()
      
      // 筛选和排序
      this.buildFilterSection()
      
      // 股票列表
      if (this.isSearching) {
        this.buildLoadingState()
      } else if (this.searchResults.length === 0) {
        this.buildEmptyState()
      } else {
        this.buildStockList()
      }
    }
    .width('100%')
    .height('100%')
    .backgroundColor('#F8F9FA')
  }

  /**
   * 搜索头部区域
   */
  @Builder
  buildSearchHeader() {
    Column() {
      // 渐变背景
      Stack() {
        Column()
          .width('100%')
          .height(140)
          .linearGradient({
            angle: 135,
            colors: [['#4f46e5', 0.0], ['#7c3aed', 1.0]]
          })
          .borderRadius({ bottomLeft: 20, bottomRight: 20 })

        // 搜索内容
        Column() {
          Text('股票搜索')
            .fontSize(24)
            .fontWeight(FontWeight.Bold)
            .fontColor('#FFFFFF')
            .margin({ bottom: 20 })

          // 搜索框
          Row() {
            Image($r('app.media.ic_search'))
              .width(20)
              .height(20)
              .fillColor('#9CA3AF')
              .margin({ left: 16, right: 12 })

            TextInput({ 
              placeholder: '搜索股票代码或名称...',
              text: this.searchText
            })
              .placeholderColor('#9CA3AF')
              .placeholderFont({ size: 16 })
              .fontSize(16)
              .fontColor('#1F2937')
              .backgroundColor('transparent')
              .border({ width: 0 })
              .flexGrow(1)
              .onChange((value: string) => {
                this.searchText = value;
                this.handleSearch(value);
              })

            if (this.searchText) {
              Button() {
                Image($r('app.media.ic_close'))
                  .width(16)
                  .height(16)
                  .fillColor('#9CA3AF')
              }
              .width(32)
              .height(32)
              .backgroundColor('transparent')
              .margin({ right: 8 })
              .onClick(() => {
                this.searchText = '';
                this.handleSearch('');
              })
            }
          }
          .width('100%')
          .height(48)
          .backgroundColor('#FFFFFF')
          .borderRadius(24)
          .shadow({
            radius: 8,
            color: 'rgba(0, 0, 0, 0.1)',
            offsetX: 0,
            offsetY: 2
          })
        }
        .padding({ left: 20, right: 20, top: 30 })
        .width('100%')
      }
    }
    .width('100%')
    .height(140)
  }

  /**
   * 筛选和排序区域
   */
  @Builder
  buildFilterSection() {
    Column() {
      // 分类筛选
      Row() {
        Text('分类')
          .fontSize(14)
          .fontColor('#6B7280')
          .margin({ right: 12 })

        Scroll() {
          Row() {
            ForEach(this.categories, (category: string) => {
              this.buildCategoryChip(category)
            })
          }
          .width('100%')
        }
        .scrollable(ScrollDirection.Horizontal)
        .scrollBar(BarState.Off)
        .flexGrow(1)
      }
      .width('100%')
      .margin({ bottom: 16 })

      // 排序选项
      Row() {
        Text('排序')
          .fontSize(14)
          .fontColor('#6B7280')
          .margin({ right: 12 })

        this.buildSortChip('涨跌幅', 'pct_chg')
        this.buildSortChip('成交量', 'volume')
        this.buildSortChip('市值', 'market_cap')

        Blank()

        // 排序方向
        Button() {
          Row() {
            Image(this.sortOrder === 'desc' ? $r('app.media.ic_arrow_down') : $r('app.media.ic_arrow_up'))
              .width(16)
              .height(16)
              .fillColor('#6B7280')
            
            Text(this.sortOrder === 'desc' ? '降序' : '升序')
              .fontSize(12)
              .fontColor('#6B7280')
              .margin({ left: 4 })
          }
        }
        .backgroundColor('#F3F4F6')
        .borderRadius(16)
        .padding({ horizontal: 12, vertical: 6 })
        .onClick(() => {
          this.sortOrder = this.sortOrder === 'desc' ? 'asc' : 'desc';
          this.applySorting();
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
    .margin({ top: -20, left: 20, right: 20, bottom: 20 })
  }

  /**
   * 分类芯片
   */
  @Builder
  buildCategoryChip(category: string) {
    Button(category)
      .fontSize(12)
      .fontColor(this.selectedCategory === category ? '#FFFFFF' : '#6B7280')
      .backgroundColor(this.selectedCategory === category ? '#4F46E5' : '#F3F4F6')
      .borderRadius(16)
      .padding({ horizontal: 16, vertical: 8 })
      .margin({ right: 8 })
      .onClick(() => {
        this.selectedCategory = category;
        this.applyFilters();
      })
  }

  /**
   * 排序芯片
   */
  @Builder
  buildSortChip(label: string, value: string) {
    Button(label)
      .fontSize(12)
      .fontColor(this.sortBy === value ? '#FFFFFF' : '#6B7280')
      .backgroundColor(this.sortBy === value ? '#7C3AED' : '#F3F4F6')
      .borderRadius(16)
      .padding({ horizontal: 12, vertical: 6 })
      .margin({ right: 8 })
      .onClick(() => {
        this.sortBy = value;
        this.applySorting();
      })
  }

  /**
   * 股票列表
   */
  @Builder
  buildStockList() {
    List() {
      ForEach(this.searchResults, (stock: StockBasicInfo, index: number) => {
        ListItem() {
          this.buildStockListItem(stock, index)
        }
        .backgroundColor('#FFFFFF')
        .borderRadius(12)
        .margin({ horizontal: 20, vertical: 6 })
        .shadow({
          radius: 6,
          color: 'rgba(0, 0, 0, 0.03)',
          offsetX: 0,
          offsetY: 1
        })
      })
    }
    .width('100%')
    .layoutWeight(1)
    .padding({ bottom: 20 })
    .divider({ strokeWidth: 0 })
  }

  /**
   * 股票列表项
   */
  @Builder
  buildStockListItem(stock: StockBasicInfo, index: number) {
    Row() {
      // 左侧序号和股票信息
      Row() {
        // 排名指示器
        Column() {
          Text((index + 1).toString())
            .fontSize(14)
            .fontWeight(FontWeight.Bold)
            .fontColor(index < 3 ? '#F59E0B' : '#6B7280')
        }
        .width(32)
        .alignItems(HorizontalAlign.Center)

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
        .margin({ left: 12 })
      }
      .flexGrow(1)

      // 右侧价格信息
      Column() {
        Text(`¥${stock.latest_price?.close?.toFixed(2) || '--'})
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
    .onClick(() => {
      this.navigateToStockDetail(stock.ts_code);
    })
  }

  /**
   * 加载状态
   */
  @Builder
  buildLoadingState() {
    Column() {
      LoadingProgress()
        .width(40)
        .height(40)
        .color('#4F46E5')
        .margin({ bottom: 16 })
      
      Text('搜索中...')
        .fontSize(16)
        .fontColor('#6B7280')
    }
    .justifyContent(FlexAlign.Center)
    .alignItems(HorizontalAlign.Center)
    .width('100%')
    .layoutWeight(1)
  }

  /**
   * 空状态
   */
  @Builder
  buildEmptyState() {
    Column() {
      Image($r('app.media.ic_search_empty'))
        .width(80)
        .height(80)
        .opacity(0.4)
        .margin({ bottom: 16 })
      
      Text('未找到相关股票')
        .fontSize(18)
        .fontColor('#1F2937')
        .margin({ bottom: 8 })
      
      Text('尝试使用不同的关键词搜索')
        .fontSize(14)
        .fontColor('#6B7280')
        .margin({ bottom: 24 })

      Button('浏览热门股票')
        .fontSize(16)
        .fontColor('#FFFFFF')
        .backgroundColor('#4F46E5')
        .borderRadius(24)
        .padding({ horizontal: 24, vertical: 12 })
        .onClick(() => {
          this.searchText = '';
          this.searchResults = this.dataState.hotStocks;
        })
    }
    .justifyContent(FlexAlign.Center)
    .alignItems(HorizontalAlign.Center)
    .width('100%')
    .layoutWeight(1)
  }

  // ============ 业务逻辑方法 ============

  private async handleSearch(query: string) {
    if (!query.trim()) {
      this.searchResults = this.dataState.hotStocks;
      return;
    }

    this.isSearching = true;
    
    try {
      // 模拟搜索API调用
      await new Promise(resolve => setTimeout(resolve, 500));
      
      // 这里应该调用真实的搜索API
      const results = await this.dataManager.searchStocks(query);
      this.searchResults = results || [];
      
    } catch (error) {
      console.error('搜索失败:', error);
      this.searchResults = [];
    } finally {
      this.isSearching = false;
    }
  }

  private applyFilters() {
    // 应用分类筛选逻辑
    let filtered = this.dataState.hotStocks;
    
    if (this.selectedCategory !== '全部') {
      // 这里可以根据实际数据结构进行筛选
      filtered = filtered.filter(stock => {
        // 实现分类筛选逻辑
        return true;
      });
    }
    
    this.searchResults = filtered;
    this.applySorting();
  }

  private applySorting() {
    this.searchResults = this.searchResults.sort((a, b) => {
      let aValue: number = 0;
      let bValue: number = 0;
      
      switch (this.sortBy) {
        case 'pct_chg':
          aValue = a.latest_price?.pct_chg || 0;
          bValue = b.latest_price?.pct_chg || 0;
          break;
        case 'volume':
          aValue = a.latest_price?.volume || 0;
          bValue = b.latest_price?.volume || 0;
          break;
        case 'market_cap':
          aValue = a.market_cap || 0;
          bValue = b.market_cap || 0;
          break;
      }
      
      return this.sortOrder === 'desc' ? bValue - aValue : aValue - bValue;
    });
  }

  // ============ 工具方法 ============

  private formatPriceChange(change: number): string {
    return `${change >= 0 ? '+' : ''}${change.toFixed(2)}%`;
  }

  private getPriceChangeColor(change: number): string {
    return change >= 0 ? '#10B981' : '#EF4444';
  }

  private getPriceChangeIcon(change: number): Resource {
    return change >= 0 ? $r('app.media.ic_arrow_up') : $r('app.media.ic_arrow_down');
  }

  private navigateToStockDetail(tsCode: string) {
    // 导航到股票详情页面
    console.log('导航到:', tsCode);
  }
}