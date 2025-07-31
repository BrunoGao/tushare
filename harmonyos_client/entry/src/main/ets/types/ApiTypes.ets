/**
 * API相关类型定义
 */

// ============ 基础类型 ============

export interface ApiConfig {
  baseUrl: string;
  apiKey: string;
  timeout: number;
}

export interface ApiResponse<T> {
  success: boolean;
  code: number;
  message: string;
  data: T;
  timestamp: string;
  server_time: string;
}

export interface PaginatedResponse<T> extends ApiResponse<T[]> {
  pagination: {
    total: number;
    page: number;
    per_page: number;
    total_pages: number;
    has_next: boolean;
    has_prev: boolean;
  };
}

// ============ 市场数据类型 ============

export interface MarketOverview {
  indices: MarketIndex[];
  market_stats: MarketStatistics;
  hot_sectors: HotSector[];
  advance_decline: AdvanceDeclineData;
  update_time: string;
}

export interface MarketIndex {
  code: string;
  name: string;
  close: number;
  pct_chg: number;
}

export interface MarketStatistics {
  total_stocks: number;
  rising_count: number;
  falling_count: number;
  avg_change: number;
}

export interface HotSector {
  name: string;
  change: number;
  stock_count: number;
}

export interface AdvanceDeclineData {
  advance: number;
  decline: number;
  unchanged: number;
}

// ============ 股票数据类型 ============

export interface StockBasicInfo {
  ts_code: string;
  name: string;
  industry: string;
  market: string;
  list_date: string;
  exchange: string;
  curr_type: string;
  list_status: string;
  latest_price?: StockPrice;
}

export interface StockPrice {
  close: number;
  pct_chg: number;
  volume: number;
  amount: number;
  open: number;
  high: number;
  low: number;
  pre_close: number;
  trade_date: string;
}

export interface KLineData {
  ts_code: string;
  period: string;
  klines: KLineItem[];
  count: number;
}

export interface KLineItem {
  date: string;
  open: number;
  close: number;
  high: number;
  low: number;
  volume: number;
  amount: number;
  pct_chg: number;
}

// ============ 技术分析类型 ============

export interface TechnicalAnalysis {
  ts_code: string;
  latest_indicators: TechnicalIndicators;
  signals: TechnicalSignals;
  analysis_time: string;
}

export interface TechnicalIndicators {
  ma5: number;
  ma10: number;
  ma20: number;
  ma60: number;
  rsi6: number;
  rsi12: number;
  macd_dif: number;
  macd_dea: number;
  kdj_k: number;
  kdj_d: number;
  vol_ratio: number;
  turn_rate: number;
}

export interface TechnicalSignals {
  ma_signal?: string;
  rsi_signal?: string;
  macd_signal?: string;
  overall_signal?: 'buy' | 'sell' | 'hold';
}

// ============ AI分析类型 ============

export interface AIRecommendation {
  ts_code: string;
  name: string;
  recommendation: 'buy' | 'sell' | 'hold';
  confidence: number;
  score: number;
  technical_score: number;
  ai_score: number;
  target_price?: number;
  stop_loss?: number;
  reasoning: string;
  risk_level: string;
  key_factors: string[];
  technical_signals: Record<string, any>;
  market_position: string;
  sector: string;
  holding_period: string;
  recommendation_date: string;
}

export interface AIPrediction {
  ts_code: string;
  model_name: string;
  prediction: number;
  prediction_text: string;
  probability: number[];
  confidence: number;
  timestamp: string;
}

// ============ 搜索类型 ============

export interface SearchResult {
  query: string;
  results: StockSearchItem[];
  count: number;
}

export interface StockSearchItem {
  ts_code: string;
  name: string;
  industry: string;
  market: string;
  display_name: string;
}

// ============ 实时数据类型 ============

export interface RealtimeData {
  type: string;
  ts_code?: string;
  data: any;
  timestamp: string;
}

export interface RealtimeStockData {
  ts_code: string;
  name: string;
  price: StockPrice;
  indicators: TechnicalIndicators;
  signals: TechnicalSignals;
}

// ============ 错误类型 ============

export interface ApiError {
  code: number;
  message: string;
  error_code?: string;
  details?: Record<string, any>;
}

// ============ 配置类型 ============

export interface AppConfig {
  api: {
    baseUrl: string;
    apiKey: string;
    timeout: number;
  };
  theme: {
    isDarkMode: boolean;
    primaryColor: string;
  };
  features: {
    enableRealtime: boolean;
    enableNotifications: boolean;
    enableAI: boolean;
  };
  cache: {
    maxSize: number;
    ttl: number;
  };
}

// ============ 数据管理类型 ============

export interface CacheItem<T> {
  data: T;
  timestamp: number;
  ttl: number;
}

export interface DataState {
  marketOverview: MarketOverview | null;
  hotStocks: StockBasicInfo[];
  watchlist: string[];
  recentSearches: string[];
  selectedStock: StockBasicInfo | null;
  klineData: Record<string, KLineData>;
  technicalAnalysis: Record<string, TechnicalAnalysis>;
  aiRecommendations: AIRecommendation[];
  isLoading: boolean;
  error: ApiError | null;
}

// ============ UI组件类型 ============

export interface TabItem {
  id: string;
  title: string;
  icon: Resource;
  component: any;
}

export interface ListItem {
  id: string;
  title: string;
  subtitle?: string;
  value?: string;
  color?: string;
  icon?: Resource;
  onTap?: () => void;
}

export interface ChartConfig {
  type: 'line' | 'candlestick' | 'bar';
  data: any[];
  options: Record<string, any>;
}

// ============ 用户偏好类型 ============

export interface UserPreferences {
  theme: 'light' | 'dark' | 'auto';
  language: 'zh-CN' | 'en-US';
  defaultChartPeriod: string;
  refreshInterval: number;
  notificationSettings: NotificationSettings;
  displaySettings: DisplaySettings;
}

export interface NotificationSettings {
  priceAlerts: boolean;
  aiRecommendations: boolean;
  marketNews: boolean;
  systemUpdates: boolean;
}

export interface DisplaySettings {
  showPercentage: boolean;
  showVolume: boolean;
  compactMode: boolean;
  animationsEnabled: boolean;
}