/**
 * 主页面
 * 应用的主界面，包含底部导航和各个功能模块
 */

import { DataManager } from '../managers/DataManager';
import { ConfigManager } from '../config/ConfigManager';
import { DataState } from '../types/ApiTypes';
import { MarketPage } from './MarketPage';
import { StockListPage } from './StockListPage';
import { RecommendationPage } from './RecommendationPage';
import { WatchlistPage } from './WatchlistPage';
import { SettingsPage } from './SettingsPage';

@Entry
@Component
struct MainPage {
  @State currentTabIndex: number = 0;
  @State dataState: DataState = {
    marketOverview: null,
    hotStocks: [],
    watchlist: [],
    recentSearches: [],
    selectedStock: null,
    klineData: {},
    technicalAnalysis: {},
    aiRecommendations: [],
    isLoading: false,
    error: null
  };

  private dataManager: DataManager = DataManager.getInstance();
  private configManager: ConfigManager = ConfigManager.getInstance();

  aboutToAppear() {
    // 注册数据状态变化监听
    this.dataManager.onStateChange((state: DataState) => {
      this.dataState = state;
    });

    // 初始化数据加载
    this.loadInitialData();
  }

  aboutToDisappear() {
    // 移除监听器
    this.dataManager.removeStateChangeCallback((state: DataState) => {
      this.dataState = state;
    });
  }

  /**
   * 加载初始数据
   */
  private async loadInitialData() {
    try {
      await Promise.all([
        this.dataManager.getMarketOverview(),
        this.dataManager.getHotStocks(20),
        this.dataManager.getAIRecommendations(20)
      ]);
    } catch (error) {
      console.error('加载初始数据失败:', error);
    }
  }

  /**
   * 处理下拉刷新
   */
  private async onRefresh() {
    await this.dataManager.refreshData();
  }

  build() {
    Column() {
      // 顶部状态栏
      this.buildStatusBar()

      // 主内容区域
      Tabs({ barPosition: BarPosition.End, index: this.currentTabIndex }) {
        // 市场页面
        TabContent() {
          MarketPage({
            dataState: this.dataState,
            onRefresh: () => this.onRefresh()
          })
        }
        .tabBar(this.buildTabBarItem(0, $r('app.media.ic_market'), '市场'))

        // 股票列表页面
        TabContent() {
          StockListPage({
            dataState: this.dataState,
            onRefresh: () => this.onRefresh()
          })
        }
        .tabBar(this.buildTabBarItem(1, $r('app.media.ic_stocks'), '股票'))

        // AI推荐页面
        TabContent() {
          RecommendationPage({
            dataState: this.dataState,
            onRefresh: () => this.onRefresh()
          })
        }
        .tabBar(this.buildTabBarItem(2, $r('app.media.ic_ai'), 'AI推荐'))

        // 自选股页面
        TabContent() {
          WatchlistPage({
            dataState: this.dataState,
            onRefresh: () => this.onRefresh()
          })
        }
        .tabBar(this.buildTabBarItem(3, $r('app.media.ic_watchlist'), '自选'))

        // 设置页面
        TabContent() {
          SettingsPage()
        }
        .tabBar(this.buildTabBarItem(4, $r('app.media.ic_settings'), '设置'))
      }
      .onChange((index: number) => {
        this.currentTabIndex = index;
      })
      .animationDuration(300)
      .scrollable(false)
      .barBackgroundColor(this.configManager.isDarkMode() ? '#1F1F1F' : '#F8F8F8')
    }
    .width('100%')
    .height('100%')
    .backgroundColor(this.configManager.isDarkMode() ? '#000000' : '#FFFFFF')
  }

  /**
   * 构建状态栏
   */
  @Builder
  buildStatusBar() {
    Row() {
      // 应用标题
      Text('灵境万象')
        .fontSize(20)
        .fontWeight(FontWeight.Bold)
        .fontColor(this.configManager.isDarkMode() ? '#FFFFFF' : '#000000')

      Blank()

      // 系统状态指示器
      Row() {
        if (this.dataState.isLoading) {
          LoadingProgress()
            .width(16)
            .height(16)
            .color(this.configManager.getPrimaryColor())
        }

        if (this.dataState.error) {
          Image($r('app.media.ic_error'))
            .width(16)
            .height(16)
            .fillColor('#FF0000')
        }
      }
      .justifyContent(FlexAlign.End)
    }
    .width('100%')
    .height(56)
    .padding({ left: 16, right: 16 })
    .backgroundColor(this.configManager.isDarkMode() ? '#1F1F1F' : '#FFFFFF')
    .border({ 
      width: { bottom: this.configManager.isDarkMode() ? 0 : 0.5 }, 
      color: '#E5E5E5' 
    })
  }

  /**
   * 构建底部导航栏项目
   */
  @Builder
  buildTabBarItem(index: number, icon: Resource, title: string) {
    Column() {
      Image(icon)
        .width(24)
        .height(24)
        .fillColor(this.currentTabIndex === index ? 
          this.configManager.getPrimaryColor() : 
          (this.configManager.isDarkMode() ? '#888888' : '#666666'))
        .animation({ duration: 200 })

      Text(title)
        .fontSize(12)
        .fontColor(this.currentTabIndex === index ? 
          this.configManager.getPrimaryColor() : 
          (this.configManager.isDarkMode() ? '#888888' : '#666666'))
        .margin({ top: 4 })
        .animation({ duration: 200 })
    }
    .justifyContent(FlexAlign.Center)
    .width('100%')
    .height(56)
    .onClick(() => {
      this.currentTabIndex = index;
      
      // 切换到对应页面时刷新数据
      if (index === 0) { // 市场页面
        this.dataManager.getMarketOverview();
      } else if (index === 1) { // 股票页面
        this.dataManager.getHotStocks();
      } else if (index === 2) { // AI推荐页面
        this.dataManager.getAIRecommendations();
      }
    })
  }
}