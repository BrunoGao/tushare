/**
 * 数据管理器
 * 负责数据的缓存、状态管理和数据同步
 */

import { hilog } from '@kit.PerformanceAnalysisKit';
import { ApiService } from '../services/ApiService';
import { ConfigManager } from '../config/ConfigManager';
import { 
  DataState, 
  CacheItem, 
  MarketOverview, 
  StockBasicInfo, 
  KLineData,
  TechnicalAnalysis,
  AIRecommendation
} from '../types/ApiTypes';

export class DataManager {
  private static instance: DataManager;
  private TAG = '[DataManager]';
  private apiService: ApiService;
  private configManager: ConfigManager;
  
  // 数据状态
  private dataState: DataState = {
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

  // 缓存存储
  private cache: Map<string, CacheItem<any>> = new Map();
  
  // 状态更新回调
  private stateChangeCallbacks: Array<(state: DataState) => void> = [];
  
  // 定时器
  private refreshTimer: number | null = null;

  private constructor() {
    this.apiService = ApiService.getInstance();
    this.configManager = ConfigManager.getInstance();
  }

  public static getInstance(): DataManager {
    if (!DataManager.instance) {
      DataManager.instance = new DataManager();
    }
    return DataManager.instance;
  }

  /**
   * 初始化数据管理器
   */
  public init(): void {
    hilog.info(0x0000, this.TAG, '数据管理器初始化开始');
    
    // 启动定时刷新
    this.startAutoRefresh();
    
    // 加载缓存数据
    this.loadCachedData();
    
    hilog.info(0x0000, this.TAG, '数据管理器初始化完成');
  }

  /**
   * 销毁数据管理器
   */
  public destroy(): void {
    this.stopAutoRefresh();
    this.clearCache();
    this.stateChangeCallbacks = [];
    hilog.info(0x0000, this.TAG, '数据管理器已销毁');
  }

  // ============ 状态管理 ============

  /**
   * 获取当前状态
   */
  public getState(): DataState {
    return { ...this.dataState };
  }

  /**
   * 更新状态
   */
  private updateState(updates: Partial<DataState>): void {
    this.dataState = { ...this.dataState, ...updates };
    this.notifyStateChange();
  }

  /**
   * 注册状态变化回调
   */
  public onStateChange(callback: (state: DataState) => void): void {
    this.stateChangeCallbacks.push(callback);
  }

  /**
   * 移除状态变化回调
   */
  public removeStateChangeCallback(callback: (state: DataState) => void): void {
    const index = this.stateChangeCallbacks.indexOf(callback);
    if (index > -1) {
      this.stateChangeCallbacks.splice(index, 1);
    }
  }

  /**
   * 通知状态变化
   */
  private notifyStateChange(): void {
    this.stateChangeCallbacks.forEach(callback => {
      try {
        callback(this.getState());
      } catch (error) {
        hilog.error(0x0000, this.TAG, '状态变化回调执行失败: %{public}s', error.message);
      }
    });
  }

  // ============ 缓存管理 ============

  /**
   * 设置缓存
   */
  private setCache<T>(key: string, data: T, ttl?: number): void {
    const defaultTtl = this.configManager.getAppConfig()?.cache.ttl || 300000; // 5分钟
    const cacheItem: CacheItem<T> = {
      data,
      timestamp: Date.now(),
      ttl: ttl || defaultTtl
    };
    this.cache.set(key, cacheItem);
  }

  /**
   * 获取缓存
   */
  private getCache<T>(key: string): T | null {
    const item = this.cache.get(key);
    if (!item) {
      return null;
    }

    // 检查是否过期
    if (Date.now() - item.timestamp > item.ttl) {
      this.cache.delete(key);
      return null;
    }

    return item.data as T;
  }

  /**
   * 清理过期缓存
   */
  private cleanExpiredCache(): void {
    const now = Date.now();
    for (const [key, item] of this.cache.entries()) {
      if (now - item.timestamp > item.ttl) {
        this.cache.delete(key);
      }
    }
  }

  /**
   * 清空所有缓存
   */
  private clearCache(): void {
    this.cache.clear();
  }

  // ============ 数据获取方法 ============

  /**
   * 获取市场概览
   */
  public async getMarketOverview(useCache: boolean = true): Promise<MarketOverview | null> {
    const cacheKey = 'market_overview';
    
    // 尝试从缓存获取
    if (useCache) {
      const cached = this.getCache<MarketOverview>(cacheKey);
      if (cached) {
        this.updateState({ marketOverview: cached });
        return cached;
      }
    }

    try {
      this.updateState({ isLoading: true, error: null });
      
      const response = await this.apiService.getMarketOverview();
      if (response.success) {
        this.setCache(cacheKey, response.data);
        this.updateState({ marketOverview: response.data, isLoading: false });
        return response.data;
      } else {
        throw new Error(response.message);
      }
    } catch (error) {
      hilog.error(0x0000, this.TAG, '获取市场概览失败: %{public}s', error.message);
      this.updateState({ 
        isLoading: false, 
        error: { code: 500, message: error.message } 
      });
      return null;
    }
  }

  /**
   * 获取热门股票
   */
  public async getHotStocks(limit: number = 20, useCache: boolean = true): Promise<StockBasicInfo[]> {
    const cacheKey = `hot_stocks_${limit}`;
    
    if (useCache) {
      const cached = this.getCache<StockBasicInfo[]>(cacheKey);
      if (cached) {
        this.updateState({ hotStocks: cached });
        return cached;
      }
    }

    try {
      this.updateState({ isLoading: true, error: null });
      
      const response = await this.apiService.getHotStocks(limit);
      if (response.success) {
        this.setCache(cacheKey, response.data);
        this.updateState({ hotStocks: response.data, isLoading: false });
        return response.data;
      } else {
        throw new Error(response.message);
      }
    } catch (error) {
      hilog.error(0x0000, this.TAG, '获取热门股票失败: %{public}s', error.message);
      this.updateState({ 
        isLoading: false, 
        error: { code: 500, message: error.message } 
      });
      return [];
    }
  }

  /**
   * 获取股票基本信息
   */
  public async getStockBasicInfo(tsCode: string, useCache: boolean = true): Promise<StockBasicInfo | null> {
    const cacheKey = `stock_basic_${tsCode}`;
    
    if (useCache) {
      const cached = this.getCache<StockBasicInfo>(cacheKey);
      if (cached) {
        return cached;
      }
    }

    try {
      const response = await this.apiService.getStockBasicInfo(tsCode);
      if (response.success) {
        this.setCache(cacheKey, response.data);
        return response.data;
      } else {
        throw new Error(response.message);
      }
    } catch (error) {
      hilog.error(0x0000, this.TAG, '获取股票基本信息失败: %{public}s', error.message);
      return null;
    }
  }

  /**
   * 获取K线数据
   */
  public async getKLineData(tsCode: string, days: number = 120, useCache: boolean = true): Promise<KLineData | null> {
    const cacheKey = `kline_${tsCode}_${days}`;
    
    if (useCache) {
      const cached = this.getCache<KLineData>(cacheKey);
      if (cached) {
        const currentData = { ...this.dataState.klineData };
        currentData[tsCode] = cached;
        this.updateState({ klineData: currentData });
        return cached;
      }
    }

    try {
      const response = await this.apiService.getKLineData(tsCode, days);
      if (response.success) {
        this.setCache(cacheKey, response.data);
        const currentData = { ...this.dataState.klineData };
        currentData[tsCode] = response.data;
        this.updateState({ klineData: currentData });
        return response.data;
      } else {
        throw new Error(response.message);
      }
    } catch (error) {
      hilog.error(0x0000, this.TAG, '获取K线数据失败: %{public}s', error.message);
      return null;
    }
  }

  /**
   * 获取技术分析
   */
  public async getTechnicalAnalysis(tsCode: string, useCache: boolean = true): Promise<TechnicalAnalysis | null> {
    const cacheKey = `technical_${tsCode}`;
    
    if (useCache) {
      const cached = this.getCache<TechnicalAnalysis>(cacheKey);
      if (cached) {
        const currentData = { ...this.dataState.technicalAnalysis };
        currentData[tsCode] = cached;
        this.updateState({ technicalAnalysis: currentData });
        return cached;
      }
    }

    try {
      const response = await this.apiService.getTechnicalAnalysis(tsCode);
      if (response.success) {
        this.setCache(cacheKey, response.data);
        const currentData = { ...this.dataState.technicalAnalysis };
        currentData[tsCode] = response.data;
        this.updateState({ technicalAnalysis: currentData });
        return response.data;
      } else {
        throw new Error(response.message);
      }
    } catch (error) {
      hilog.error(0x0000, this.TAG, '获取技术分析失败: %{public}s', error.message);
      return null;
    }
  }

  /**
   * 获取AI推荐
   */
  public async getAIRecommendations(limit: number = 20, useCache: boolean = true): Promise<AIRecommendation[]> {
    const cacheKey = `ai_recommendations_${limit}`;
    
    if (useCache) {
      const cached = this.getCache<AIRecommendation[]>(cacheKey);
      if (cached) {
        this.updateState({ aiRecommendations: cached });
        return cached;
      }
    }

    try {
      this.updateState({ isLoading: true, error: null });
      
      const response = await this.apiService.getAIRecommendations(limit);
      if (response.success && response.data.recommendations) {
        const recommendations = response.data.recommendations;
        this.setCache(cacheKey, recommendations);
        this.updateState({ aiRecommendations: recommendations, isLoading: false });
        return recommendations;
      } else {
        throw new Error(response.message);
      }
    } catch (error) {
      hilog.error(0x0000, this.TAG, '获取AI推荐失败: %{public}s', error.message);
      this.updateState({ 
        isLoading: false, 
        error: { code: 500, message: error.message } 
      });
      return [];
    }
  }

  // ============ 自选股管理 ============

  /**
   * 添加到自选股
   */
  public addToWatchlist(tsCode: string): void {
    if (!this.dataState.watchlist.includes(tsCode)) {
      const newWatchlist = [...this.dataState.watchlist, tsCode];
      this.updateState({ watchlist: newWatchlist });
      this.saveWatchlist();
    }
  }

  /**
   * 从自选股移除
   */
  public removeFromWatchlist(tsCode: string): void {
    const newWatchlist = this.dataState.watchlist.filter(code => code !== tsCode);
    this.updateState({ watchlist: newWatchlist });
    this.saveWatchlist();
  }

  /**
   * 检查是否在自选股中
   */
  public isInWatchlist(tsCode: string): boolean {
    return this.dataState.watchlist.includes(tsCode);
  }

  // ============ 搜索历史管理 ============

  /**
   * 添加搜索历史
   */
  public addToRecentSearches(query: string): void {
    const searches = this.dataState.recentSearches.filter(s => s !== query);
    searches.unshift(query);
    
    // 最多保留10条搜索历史
    const newSearches = searches.slice(0, 10);
    this.updateState({ recentSearches: newSearches });
    this.saveRecentSearches();
  }

  /**
   * 清空搜索历史
   */
  public clearRecentSearches(): void {
    this.updateState({ recentSearches: [] });
    this.saveRecentSearches();
  }

  // ============ 数据刷新和同步 ============

  /**
   * 刷新所有数据
   */
  public async refreshData(): Promise<void> {
    hilog.info(0x0000, this.TAG, '开始刷新数据');
    
    try {
      // 并行刷新主要数据
      await Promise.all([
        this.getMarketOverview(false),
        this.getHotStocks(20, false),
        this.getAIRecommendations(20, false)
      ]);
      
      hilog.info(0x0000, this.TAG, '数据刷新完成');
    } catch (error) {
      hilog.error(0x0000, this.TAG, '数据刷新失败: %{public}s', error.message);
    }
  }

  /**
   * 启动自动刷新
   */
  private startAutoRefresh(): void {
    const interval = this.configManager.getRefreshInterval();
    
    this.refreshTimer = setInterval(() => {
      this.refreshData();
      this.cleanExpiredCache();
    }, interval);
    
    hilog.info(0x0000, this.TAG, '自动刷新已启动，间隔: %{public}d ms', interval);
  }

  /**
   * 停止自动刷新
   */
  private stopAutoRefresh(): void {
    if (this.refreshTimer) {
      clearInterval(this.refreshTimer);
      this.refreshTimer = null;
      hilog.info(0x0000, this.TAG, '自动刷新已停止');
    }
  }

  // ============ 数据持久化 ============

  /**
   * 保存数据
   */
  public saveData(): void {
    this.saveWatchlist();
    this.saveRecentSearches();
  }

  /**
   * 加载缓存数据
   */
  private loadCachedData(): void {
    // 这里可以从持久化存储加载数据
    // 暂时使用空实现
  }

  /**
   * 保存自选股
   */
  private saveWatchlist(): void {
    // 实现保存自选股到本地存储
  }

  /**
   * 保存搜索历史
   */
  private saveRecentSearches(): void {
    // 实现保存搜索历史到本地存储
  }
}