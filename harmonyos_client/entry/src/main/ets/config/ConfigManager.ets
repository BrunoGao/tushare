/**
 * 配置管理器
 * 管理应用的配置信息和用户偏好设置
 */

import { preferences } from '@kit.ArkData';
import { hilog } from '@kit.PerformanceAnalysisKit';
import { AppConfig, UserPreferences } from '../types/ApiTypes';

export class ConfigManager {
  private static instance: ConfigManager;
  private TAG = '[ConfigManager]';
  private preferencesStore: preferences.Preferences | null = null;
  private config: AppConfig | null = null;
  private userPrefs: UserPreferences | null = null;

  private constructor() {}

  public static getInstance(): ConfigManager {
    if (!ConfigManager.instance) {
      ConfigManager.instance = new ConfigManager();
    }
    return ConfigManager.instance;
  }

  /**
   * 初始化配置管理器
   */
  public async init(): Promise<void> {
    try {
      // 初始化偏好设置存储
      this.preferencesStore = await preferences.getPreferences(getContext(), 'app_config');
      
      // 加载应用配置
      await this.loadAppConfig();
      
      // 加载用户偏好
      await this.loadUserPreferences();
      
      hilog.info(0x0000, this.TAG, '配置管理器初始化完成');
    } catch (error) {
      hilog.error(0x0000, this.TAG, '配置管理器初始化失败: %{public}s', error.message);
      
      // 使用默认配置
      this.loadDefaultConfig();
    }
  }

  /**
   * 加载应用配置
   */
  private async loadAppConfig(): Promise<void> {
    const savedConfig = await this.preferencesStore?.get('app_config', '');
    
    if (savedConfig && typeof savedConfig === 'string' && savedConfig.length > 0) {
      try {
        this.config = JSON.parse(savedConfig) as AppConfig;
        hilog.info(0x0000, this.TAG, '已加载保存的应用配置');
      } catch (error) {
        hilog.error(0x0000, this.TAG, '解析应用配置失败: %{public}s', error.message);
        this.loadDefaultConfig();
      }
    } else {
      this.loadDefaultConfig();
    }
  }

  /**
   * 加载默认配置
   */
  private loadDefaultConfig(): void {
    this.config = {
      api: {
        baseUrl: 'http://14.127.218.229:5005', // 默认API地址
        apiKey: 'ljwx_harmonyos_client',
        timeout: 10000
      },
      theme: {
        isDarkMode: false,
        primaryColor: '#007DFF'
      },
      features: {
        enableRealtime: true,
        enableNotifications: true,
        enableAI: true
      },
      cache: {
        maxSize: 100,
        ttl: 300000 // 5分钟
      }
    };
    
    hilog.info(0x0000, this.TAG, '已加载默认应用配置');
  }

  /**
   * 加载用户偏好设置
   */
  private async loadUserPreferences(): Promise<void> {
    const savedPrefs = await this.preferencesStore?.get('user_preferences', '');
    
    if (savedPrefs && typeof savedPrefs === 'string' && savedPrefs.length > 0) {
      try {
        this.userPrefs = JSON.parse(savedPrefs) as UserPreferences;
        hilog.info(0x0000, this.TAG, '已加载用户偏好设置');
      } catch (error) {
        hilog.error(0x0000, this.TAG, '解析用户偏好设置失败: %{public}s', error.message);
        this.loadDefaultUserPreferences();
      }
    } else {
      this.loadDefaultUserPreferences();
    }
  }

  /**
   * 加载默认用户偏好设置
   */
  private loadDefaultUserPreferences(): void {
    this.userPrefs = {
      theme: 'auto',
      language: 'zh-CN',
      defaultChartPeriod: 'daily',
      refreshInterval: 30000, // 30秒
      notificationSettings: {
        priceAlerts: true,
        aiRecommendations: true,
        marketNews: false,
        systemUpdates: true
      },
      displaySettings: {
        showPercentage: true,
        showVolume: true,
        compactMode: false,
        animationsEnabled: true
      }
    };
    
    hilog.info(0x0000, this.TAG, '已加载默认用户偏好设置');
  }

  /**
   * 保存应用配置
   */
  public async saveAppConfig(): Promise<void> {
    if (!this.preferencesStore || !this.config) {
      return;
    }

    try {
      await this.preferencesStore.put('app_config', JSON.stringify(this.config));
      await this.preferencesStore.flush();
      hilog.info(0x0000, this.TAG, '应用配置已保存');
    } catch (error) {
      hilog.error(0x0000, this.TAG, '保存应用配置失败: %{public}s', error.message);
    }
  }

  /**
   * 保存用户偏好设置
   */
  public async saveUserPreferences(): Promise<void> {
    if (!this.preferencesStore || !this.userPrefs) {
      return;
    }

    try {
      await this.preferencesStore.put('user_preferences', JSON.stringify(this.userPrefs));
      await this.preferencesStore.flush();
      hilog.info(0x0000, this.TAG, '用户偏好设置已保存');
    } catch (error) {
      hilog.error(0x0000, this.TAG, '保存用户偏好设置失败: %{public}s', error.message);
    }
  }

  // ============ 获取配置方法 ============

  public getApiBaseUrl(): string {
    return this.config?.api.baseUrl || 'http://14.127.218.229:5005';
  }

  public getApiKey(): string {
    return this.config?.api.apiKey || 'ljwx_harmonyos_client';
  }

  public getApiTimeout(): number {
    return this.config?.api.timeout || 10000;
  }

  public isDarkMode(): boolean {
    if (this.userPrefs?.theme === 'dark') {
      return true;
    } else if (this.userPrefs?.theme === 'light') {
      return false;
    } else {
      // auto模式，根据系统设置判断
      // 这里简化处理，返回配置中的默认值
      return this.config?.theme.isDarkMode || false;
    }
  }

  public getPrimaryColor(): string {
    return this.config?.theme.primaryColor || '#007DFF';
  }

  public isFeatureEnabled(feature: 'realtime' | 'notifications' | 'ai'): boolean {
    switch (feature) {
      case 'realtime':
        return this.config?.features.enableRealtime || true;
      case 'notifications':
        return this.config?.features.enableNotifications || true;
      case 'ai':
        return this.config?.features.enableAI || true;
      default:
        return false;
    }
  }

  public getRefreshInterval(): number {
    return this.userPrefs?.refreshInterval || 30000;
  }

  public getDefaultChartPeriod(): string {
    return this.userPrefs?.defaultChartPeriod || 'daily';
  }

  public getUserPreferences(): UserPreferences | null {
    return this.userPrefs;
  }

  public getAppConfig(): AppConfig | null {
    return this.config;
  }

  // ============ 更新配置方法 ============

  public updateApiConfig(baseUrl?: string, apiKey?: string, timeout?: number): void {
    if (!this.config) return;

    if (baseUrl) this.config.api.baseUrl = baseUrl;
    if (apiKey) this.config.api.apiKey = apiKey;
    if (timeout) this.config.api.timeout = timeout;

    this.saveAppConfig();
  }

  public updateThemeConfig(isDarkMode?: boolean, primaryColor?: string): void {
    if (!this.config) return;

    if (isDarkMode !== undefined) this.config.theme.isDarkMode = isDarkMode;
    if (primaryColor) this.config.theme.primaryColor = primaryColor;

    this.saveAppConfig();
  }

  public updateUserPreferences(prefs: Partial<UserPreferences>): void {
    if (!this.userPrefs) return;

    Object.assign(this.userPrefs, prefs);
    this.saveUserPreferences();
  }

  /**
   * 重置为默认配置
   */
  public async resetToDefaults(): Promise<void> {
    this.loadDefaultConfig();
    this.loadDefaultUserPreferences();
    
    await this.saveAppConfig();
    await this.saveUserPreferences();
    
    hilog.info(0x0000, this.TAG, '已重置为默认配置');
  }
}