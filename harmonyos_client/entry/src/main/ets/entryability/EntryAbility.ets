/**
 * 灵境万象股票分析系统 - 鸿蒙ArkTS客户端
 * 主入口文件
 */

import { AbilityConstant, UIAbility, Want } from '@kit.AbilityKit';
import { hilog } from '@kit.PerformanceAnalysisKit';
import { window } from '@kit.ArkUI';
import { ApiService } from './services/ApiService';
import { ConfigManager } from './config/ConfigManager';
import { DataManager } from './managers/DataManager';

export default class EntryAbility extends UIAbility {
  private TAG = '[EntryAbility]';

  onCreate(want: Want, launchParam: AbilityConstant.LaunchParam): void {
    hilog.info(0x0000, this.TAG, '%{public}s', 'Ability onCreate');
    
    // 初始化配置管理器
    ConfigManager.getInstance().init();
    
    // 初始化API服务
    ApiService.getInstance().init({
      baseUrl: ConfigManager.getInstance().getApiBaseUrl(),
      apiKey: ConfigManager.getInstance().getApiKey(),
      timeout: 10000
    });
    
    // 初始化数据管理器
    DataManager.getInstance().init();
  }

  onDestroy(): void {
    hilog.info(0x0000, this.TAG, '%{public}s', 'Ability onDestroy');
    
    // 清理资源
    DataManager.getInstance().destroy();
    ApiService.getInstance().destroy();
  }

  onWindowStageCreate(windowStage: window.WindowStage): void {
    hilog.info(0x0000, this.TAG, '%{public}s', 'Ability onWindowStageCreate');

    windowStage.loadContent('pages/MainPage', (err) => {
      if (err.code) {
        hilog.error(0x0000, this.TAG, 'Failed to load the content. Cause: %{public}s', JSON.stringify(err) ?? '');
        return;
      }
      hilog.info(0x0000, this.TAG, 'Succeeded in loading the content.');
    });
  }

  onWindowStageDestroy(): void {
    hilog.info(0x0000, this.TAG, '%{public}s', 'Ability onWindowStageDestroy');
  }

  onForeground(): void {
    hilog.info(0x0000, this.TAG, '%{public}s', 'Ability onForeground');
    
    // 应用进入前台时刷新数据
    DataManager.getInstance().refreshData();
  }

  onBackground(): void {
    hilog.info(0x0000, this.TAG, '%{public}s', 'Ability onBackground');
    
    // 应用进入后台时保存数据
    DataManager.getInstance().saveData();
  }
}