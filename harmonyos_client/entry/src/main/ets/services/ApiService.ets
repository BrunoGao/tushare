/**
 * API服务类
 * 负责与后端API进行通信
 */

import { http } from '@kit.NetworkKit';
import { hilog } from '@kit.PerformanceAnalysisKit';
import { 
  ApiConfig, 
  ApiResponse, 
  StockBasicInfo, 
  KLineData, 
  TechnicalAnalysis,
  AIRecommendation,
  MarketOverview,
  SearchResult 
} from '../types/ApiTypes';

export class ApiService {
  private static instance: ApiService;
  private config: ApiConfig | null = null;
  private TAG = '[ApiService]';

  private constructor() {}

  public static getInstance(): ApiService {
    if (!ApiService.instance) {
      ApiService.instance = new ApiService();
    }
    return ApiService.instance;
  }

  /**
   * 初始化API服务
   */
  public init(config: ApiConfig): void {
    this.config = config;
    hilog.info(0x0000, this.TAG, 'API服务初始化完成, baseUrl: %{public}s', config.baseUrl);
  }

  /**
   * 销毁API服务
   */
  public destroy(): void {
    this.config = null;
    hilog.info(0x0000, this.TAG, 'API服务已销毁');
  }

  /**
   * 通用HTTP请求方法
   */
  private async request<T>(
    url: string, 
    method: http.RequestMethod = http.RequestMethod.GET,
    data?: any,
    headers?: Record<string, string>
  ): Promise<ApiResponse<T>> {
    if (!this.config) {
      throw new Error('API服务未初始化');
    }

    const fullUrl = `${this.config.baseUrl}${url}`;
    
    const requestHeaders: Record<string, string> = {
      'Content-Type': 'application/json',
      'X-API-Key': this.config.apiKey,
      ...headers
    };

    try {
      hilog.info(0x0000, this.TAG, '发起请求: %{public}s %{public}s', method, fullUrl);

      const httpRequest = http.createHttp();
      
      const response = await httpRequest.request(fullUrl, {
        method: method,
        header: requestHeaders,
        extraData: data ? JSON.stringify(data) : undefined,
        expectDataType: http.HttpDataType.STRING,
        connectTimeout: this.config.timeout,
        readTimeout: this.config.timeout
      });

      httpRequest.destroy();

      if (response.responseCode === 200) {
        const result = JSON.parse(response.result as string) as ApiResponse<T>;
        hilog.info(0x0000, this.TAG, '请求成功: %{public}s', result.message);
        return result;
      } else {
        throw new Error(`HTTP错误: ${response.responseCode}`);
      }
    } catch (error) {
      hilog.error(0x0000, this.TAG, '请求失败: %{public}s', error.message);
      throw error;
    }
  }

  // ============ 市场数据API ============

  /**
   * 获取市场概览
   */
  public async getMarketOverview(): Promise<ApiResponse<MarketOverview>> {
    return this.request<MarketOverview>('/api/v1/market/overview');
  }

  /**
   * 获取热门股票
   */
  public async getHotStocks(limit: number = 20, sortBy: string = 'pct_chg'): Promise<ApiResponse<StockBasicInfo[]>> {
    return this.request<StockBasicInfo[]>(`/api/v1/market/stocks/hot?limit=${limit}&sort_by=${sortBy}`);
  }

  // ============ 股票数据API ============

  /**
   * 获取股票基本信息
   */
  public async getStockBasicInfo(tsCode: string): Promise<ApiResponse<StockBasicInfo>> {
    return this.request<StockBasicInfo>(`/api/v1/stocks/${tsCode}/basic`);
  }

  /**
   * 获取K线数据
   */
  public async getKLineData(tsCode: string, days: number = 120, period: string = 'daily'): Promise<ApiResponse<KLineData>> {
    return this.request<KLineData>(`/api/v1/stocks/${tsCode}/kline?days=${days}&period=${period}`);
  }

  /**
   * 获取技术分析
   */
  public async getTechnicalAnalysis(tsCode: string, days: number = 60): Promise<ApiResponse<TechnicalAnalysis>> {
    return this.request<TechnicalAnalysis>(`/api/v1/analysis/technical/${tsCode}?days=${days}`);
  }

  // ============ AI分析API ============

  /**
   * 获取AI预测
   */
  public async getAIPrediction(tsCode: string, model: string = 'random_forest'): Promise<ApiResponse<any>> {
    return this.request<any>(`/api/v1/ai/predict/${tsCode}?model=${model}`);
  }

  /**
   * 获取AI推荐
   */
  public async getAIRecommendations(
    limit: number = 20, 
    strategy: string = 'multi_strategy', 
    minScore: number = 60
  ): Promise<ApiResponse<AIRecommendation[]>> {
    return this.request<AIRecommendation[]>(`/api/v1/ai/recommendations?limit=${limit}&strategy=${strategy}&min_score=${minScore}`);
  }

  // ============ 搜索API ============

  /**
   * 搜索股票
   */
  public async searchStocks(query: string, limit: number = 10): Promise<ApiResponse<SearchResult>> {
    return this.request<SearchResult>(`/api/v1/search/stocks?q=${encodeURIComponent(query)}&limit=${limit}`);
  }

  // ============ 系统API ============

  /**
   * 系统健康检查
   */
  public async getSystemHealth(): Promise<ApiResponse<any>> {
    return this.request<any>('/api/v1/system/health');
  }

  // ============ 实时数据WebSocket ============

  /**
   * 创建WebSocket连接（用于实时数据）
   */
  public createWebSocketConnection(): WebSocket | null {
    if (!this.config) {
      hilog.error(0x0000, this.TAG, 'API服务未初始化，无法创建WebSocket连接');
      return null;
    }

    try {
      const wsUrl = this.config.baseUrl.replace('http', 'ws') + '/ws/realtime';
      const webSocket = new WebSocket(wsUrl);
      
      webSocket.onopen = () => {
        hilog.info(0x0000, this.TAG, 'WebSocket连接已建立');
      };
      
      webSocket.onclose = () => {
        hilog.info(0x0000, this.TAG, 'WebSocket连接已关闭');
      };
      
      webSocket.onerror = (error) => {
        hilog.error(0x0000, this.TAG, 'WebSocket连接错误: %{public}s', JSON.stringify(error));
      };

      return webSocket;
    } catch (error) {
      hilog.error(0x0000, this.TAG, '创建WebSocket连接失败: %{public}s', error.message);
      return null;
    }
  }
}