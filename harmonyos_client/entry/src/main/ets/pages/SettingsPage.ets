/**
 * 设置页面 - ExpressVPN风格设计
 * 应用配置和个人偏好设置
 */

import { ConfigManager } from '../config/ConfigManager';
import { DataManager } from '../managers/DataManager';

@Component
export struct SettingsPage {
  @State private userInfo: any = {
    name: '用户',
    avatar: '',
    vipLevel: 'basic'
  };
  @State private isDarkMode: boolean = false;
  @State private primaryColor: string = '#4F46E5';
  @State private pushEnabled: boolean = true;
  @State private soundEnabled: boolean = true;
  @State private vibrationEnabled: boolean = true;
  @State private dataSync: boolean = true;
  @State private cacheSize: string = '0 MB';
  @State private appVersion: string = '2.1.0';

  private configManager: ConfigManager = ConfigManager.getInstance();
  private dataManager: DataManager = DataManager.getInstance();

  aboutToAppear() {
    this.loadSettings();
  }

  build() {
    Scroll() {
      Column() {
        // 用户信息头部
        this.buildUserProfile()
        
        // 设置分组
        Column() {
          // 外观设置
          this.buildSettingsGroup('外观设置', this.buildAppearanceSettings())
          
          // 通知设置
          this.buildSettingsGroup('通知设置', this.buildNotificationSettings())
          
          // 数据和同步
          this.buildSettingsGroup('数据和同步', this.buildDataSettings())
          
          // 应用信息
          this.buildSettingsGroup('应用信息', this.buildAppInfo())
          
          // 危险操作
          this.buildSettingsGroup('危险操作', this.buildDangerousActions())
        }
        .padding({ left: 20, right: 20, bottom: 40 })
      }
    }
    .width('100%')
    .height('100%')
    .backgroundColor('#F8F9FA')
  }

  /**
   * 用户资料头部
   */
  @Builder
  buildUserProfile() {
    Stack() {
      // 渐变背景
      Column()
        .width('100%')
        .height(200)
        .linearGradient({
          angle: 135,
          colors: [['#6366f1', 0.0], ['#8b5cf6', 1.0]]
        })
        .borderRadius({ bottomLeft: 24, bottomRight: 24 })

      // 用户信息
      Column() {
        // 头像
        Stack() {
          if (this.userInfo.avatar) {
            Image(this.userInfo.avatar)
              .width(80)
              .height(80)
          } else {
            Circle({ width: 80, height: 80 })
              .fill('rgba(255, 255, 255, 0.2)')
            
            Text(this.userInfo.name.charAt(0))
              .fontSize(32)
              .fontWeight(FontWeight.Bold)
              .fontColor('#FFFFFF')
          }
          
          // VIP标识
          if (this.userInfo.vipLevel !== 'basic') {
            Row() {
              Image($r('app.media.ic_vip'))
                .width(12)
                .height(12)
                .fillColor('#FFD700')
              
              Text('VIP')
                .fontSize(8)
                .fontColor('#FFD700')
                .margin({ left: 2 })
            }
            .padding({ horizontal: 6, vertical: 2 })
            .backgroundColor('rgba(0, 0, 0, 0.3)')
            .borderRadius(8)
            .position({ x: 60, y: 0 })
          }
        }
        .margin({ bottom: 16 })

        // 用户名和级别
        Text(this.userInfo.name)
          .fontSize(24)
          .fontWeight(FontWeight.Bold)
          .fontColor('#FFFFFF')
          .margin({ bottom: 4 })
        
        Text(this.getUserLevelText())
          .fontSize(14)
          .fontColor('#FFFFFF')
          .opacity(0.8)
          .margin({ bottom: 20 })

        // 快速操作
        Row() {
          this.buildQuickAction('个人资料', $r('app.media.ic_profile'), () => {
            this.editProfile();
          })
          
          this.buildQuickAction('使用统计', $r('app.media.ic_stats'), () => {
            this.showUsageStats();
          })
          
          this.buildQuickAction('帮助中心', $r('app.media.ic_help'), () => {
            this.showHelp();
          })
        }
        .justifyContent(FlexAlign.SpaceEvenly)
        .width('100%')
      }
      .padding({ left: 20, right: 20, top: 50 })
      .width('100%')
    }
    .width('100%')
    .height(200)
    .margin({ bottom: 30 })
  }

  /**
   * 快速操作按钮
   */
  @Builder
  buildQuickAction(label: string, icon: Resource, onTap: () => void) {
    Column() {
      Button() {
        Image(icon)
          .width(20)
          .height(20)
          .fillColor('#FFFFFF')
      }
      .width(44)
      .height(44)
      .backgroundColor('rgba(255, 255, 255, 0.2)')
      .borderRadius(22)
      .onClick(onTap)
      
      Text(label)
        .fontSize(12)
        .fontColor('#FFFFFF')
        .opacity(0.8)
        .margin({ top: 8 })
    }
    .alignItems(HorizontalAlign.Center)
  }

  /**
   * 设置分组
   */
  @Builder
  buildSettingsGroup(title: string, content: () => void) {
    Column() {
      Text(title)
        .fontSize(18)
        .fontWeight(FontWeight.Medium)
        .fontColor('#1F2937')
        .alignSelf(ItemAlign.Start)
        .margin({ bottom: 16 })

      Column() {
        content()
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
    .width('100%')
    .margin({ bottom: 24 })
  }

  /**
   * 外观设置
   */
  @Builder
  buildAppearanceSettings() {
    Column() {
      // 深色模式
      this.buildSettingItem(
        '深色模式', 
        '更护眼的界面配色',
        $r('app.media.ic_dark_mode'),
        () => this.buildToggle(this.isDarkMode, (value: boolean) => {
          this.isDarkMode = value;
          this.configManager.setDarkMode(value);
        })
      )
      
      Divider().margin({ vertical: 16 })
      
      // 主题颜色
      this.buildSettingItem(
        '主题颜色',
        '自定义应用主色调',
        $r('app.media.ic_palette'),
        () => this.buildColorPicker()
      )
      
      Divider().margin({ vertical: 16 })
      
      // 字体大小
      this.buildSettingItem(
        '字体大小',
        '调整界面文字大小',
        $r('app.media.ic_font_size'),
        () => Text('标准')
          .fontSize(14)
          .fontColor('#6B7280')
      )
    }
  }

  /**
   * 通知设置
   */
  @Builder
  buildNotificationSettings() {
    Column() {
      // 推送通知
      this.buildSettingItem(
        '推送通知',
        '接收价格提醒和重要消息',
        $r('app.media.ic_notification'),
        () => this.buildToggle(this.pushEnabled, (value: boolean) => {
          this.pushEnabled = value;
          this.configManager.setPushEnabled(value);
        })
      )
      
      Divider().margin({ vertical: 16 })
      
      // 声音提醒
      this.buildSettingItem(
        '声音提醒',
        '通知时播放提示音',
        $r('app.media.ic_sound'),
        () => this.buildToggle(this.soundEnabled, (value: boolean) => {
          this.soundEnabled = value;
        })
      )
      
      Divider().margin({ vertical: 16 })
      
      // 震动反馈
      this.buildSettingItem(
        '震动反馈',
        '操作时的触感反馈',
        $r('app.media.ic_vibration'),
        () => this.buildToggle(this.vibrationEnabled, (value: boolean) => {
          this.vibrationEnabled = value;
        })
      )
    }
  }

  /**
   * 数据和同步设置
   */
  @Builder
  buildDataSettings() {
    Column() {
      // 数据同步
      this.buildSettingItem(
        '数据同步',
        '跨设备同步自选股和设置',
        $r('app.media.ic_sync'),
        () => this.buildToggle(this.dataSync, (value: boolean) => {
          this.dataSync = value;
        })
      )
      
      Divider().margin({ vertical: 16 })
      
      // 缓存管理
      this.buildSettingItem(
        '缓存大小',
        `当前占用 ${this.cacheSize}`,
        $r('app.media.ic_storage'),
        () => Button('清除')
          .fontSize(14)
          .fontColor('#EF4444')
          .backgroundColor('rgba(239, 68, 68, 0.1)')
          .borderRadius(16)
          .padding({ horizontal: 12, vertical: 6 })
          .onClick(() => {
            this.clearCache();
          })
      )
      
      Divider().margin({ vertical: 16 })
      
      // 数据导出
      this.buildSettingItem(
        '数据导出',
        '导出自选股和历史记录',
        $r('app.media.ic_export'),
        () => Button('导出')
          .fontSize(14)
          .fontColor('#3B82F6')
          .backgroundColor('rgba(59, 130, 246, 0.1)')
          .borderRadius(16)
          .padding({ horizontal: 12, vertical: 6 })
          .onClick(() => {
            this.exportData();
          })
      )
    }
  }

  /**
   * 应用信息
   */
  @Builder
  buildAppInfo() {
    Column() {
      // 应用版本
      this.buildSettingItem(
        '应用版本',
        `当前版本 ${this.appVersion}`,
        $r('app.media.ic_version'),
        () => Button('检查更新')
          .fontSize(14)
          .fontColor('#10B981')
          .backgroundColor('rgba(16, 185, 129, 0.1)')
          .borderRadius(16)
          .padding({ horizontal: 12, vertical: 6 })
          .onClick(() => {
            this.checkUpdate();
          })
      )
      
      Divider().margin({ vertical: 16 })
      
      // 关于我们
      this.buildSettingItem(
        '关于我们',
        '了解灵境万象',
        $r('app.media.ic_info'),
        () => Image($r('app.media.ic_arrow_right'))
          .width(16)
          .height(16)
          .fillColor('#6B7280')
      )
      
      Divider().margin({ vertical: 16 })
      
      // 隐私政策
      this.buildSettingItem(
        '隐私政策',
        '用户隐私保护声明',
        $r('app.media.ic_privacy'),
        () => Image($r('app.media.ic_arrow_right'))
          .width(16)
          .height(16)
          .fillColor('#6B7280')
      )
      
      Divider().margin({ vertical: 16 })
      
      // 服务条款
      this.buildSettingItem(
        '服务条款',
        '用户服务协议',
        $r('app.media.ic_terms'),
        () => Image($r('app.media.ic_arrow_right'))
          .width(16)
          .height(16)
          .fillColor('#6B7280')
      )
    }
  }

  /**
   * 危险操作
   */
  @Builder
  buildDangerousActions() {
    Column() {
      // 清除所有数据
      this.buildSettingItem(
        '清除所有数据',
        '删除所有本地数据和设置',
        $r('app.media.ic_delete'),
        () => Button('清除')
          .fontSize(14)
          .fontColor('#EF4444')
          .backgroundColor('rgba(239, 68, 68, 0.1)')
          .borderRadius(16)
          .padding({ horizontal: 12, vertical: 6 })
          .onClick(() => {
            this.showClearDataDialog();
          })
      )
      
      Divider().margin({ vertical: 16 })
      
      // 账户注销
      this.buildSettingItem(
        '注销账户',
        '永久删除账户和相关数据',
        $r('app.media.ic_logout'),
        () => Button('注销')
          .fontSize(14)
          .fontColor('#EF4444')
          .backgroundColor('rgba(239, 68, 68, 0.1)')
          .borderRadius(16)
          .padding({ horizontal: 12, vertical: 6 })
          .onClick(() => {
            this.showLogoutDialog();
          })
      )
    }
  }

  /**
   * 设置项
   */
  @Builder
  buildSettingItem(title: string, subtitle: string, icon: Resource, trailing: () => void) {
    Row() {
      // 图标
      Image(icon)
        .width(24)
        .height(24)
        .fillColor('#6B7280')
        .margin({ right: 16 })

      // 文本信息
      Column() {
        Text(title)
          .fontSize(16)
          .fontWeight(FontWeight.Medium)
          .fontColor('#1F2937')
          .alignSelf(ItemAlign.Start)
        
        Text(subtitle)
          .fontSize(12)
          .fontColor('#6B7280')
          .margin({ top: 2 })
          .alignSelf(ItemAlign.Start)
      }
      .alignItems(HorizontalAlign.Start)
      .flexGrow(1)

      // 尾部控件
      trailing()
    }
    .width('100%')
    .alignItems(VerticalAlign.Center)
  }

  /**
   * 开关控件
   */
  @Builder
  buildToggle(checked: boolean, onChange: (value: boolean) => void) {
    Toggle({ type: ToggleType.Switch, isOn: checked })
      .selectedColor('#6366F1')
      .switchPointColor('#FFFFFF')
      .onChange(onChange)
  }

  /**
   * 颜色选择器
   */
  @Builder
  buildColorPicker() {
    Row() {
      ForEach(['#4F46E5', '#EF4444', '#10B981', '#F59E0B', '#8B5CF6'], (color: string) => {
        Button()
          .width(24)
          .height(24)
          .backgroundColor(color)
          .borderRadius(12)
          .border({ 
            width: this.primaryColor === color ? 2 : 0, 
            color: '#FFFFFF' 
          })
          .margin({ right: 8 })
          .onClick(() => {
            this.primaryColor = color;
            this.configManager.setPrimaryColor(color);
          })
      })
    }
  }

  // ============ 业务逻辑方法 ============

  private loadSettings() {
    this.isDarkMode = this.configManager.isDarkMode();
    this.primaryColor = this.configManager.getPrimaryColor();
    this.pushEnabled = this.configManager.isPushEnabled();
    this.calculateCacheSize();
  }

  private async calculateCacheSize() {
    // 计算缓存大小
    try {
      // 这里应该调用实际的缓存大小计算方法
      this.cacheSize = '12.5 MB';
    } catch (error) {
      this.cacheSize = '未知';
    }
  }

  private getUserLevelText(): string {
    switch (this.userInfo.vipLevel) {
      case 'premium': return '高级会员';
      case 'vip': return 'VIP会员';
      default: return '普通用户';
    }
  }

  private editProfile() {
    console.log('编辑个人资料');
  }

  private showUsageStats() {
    console.log('显示使用统计');
  }

  private showHelp() {
    console.log('显示帮助中心');
  }

  private clearCache() {
    // 清除缓存
    console.log('清除缓存');
    // 更新缓存大小显示
    this.cacheSize = '0 MB';
  }

  private exportData() {
    console.log('导出数据');
  }

  private checkUpdate() {
    console.log('检查更新');
  }

  private showClearDataDialog() {
    // 显示确认对话框
    AlertDialog.show({
      title: '清除所有数据',
      message: '此操作将删除所有本地数据和设置，且无法恢复。确定要继续吗？',
      primaryButton: {
        value: '取消',
        action: () => {}
      },
      secondaryButton: {
        value: '确定清除',
        fontColor: '#EF4444',
        action: () => {
          this.clearAllData();
        }
      }
    });
  }

  private showLogoutDialog() {
    // 显示注销确认对话框
    AlertDialog.show({
      title: '注销账户',
      message: '此操作将永久删除您的账户和所有相关数据，且无法恢复。确定要继续吗？',
      primaryButton: {
        value: '取消',
        action: () => {}
      },
      secondaryButton: {
        value: '确定注销',
        fontColor: '#EF4444',
        action: () => {
          this.logoutAccount();
        }
      }
    });
  }

  private clearAllData() {
    console.log('清除所有数据');
    // 执行清除操作
  }

  private logoutAccount() {
    console.log('注销账户');
    // 执行注销操作
  }
}