# 灵境万象 - HarmonyOS客户端

## 项目概述

灵境万象股票分析系统的HarmonyOS原生客户端，基于ArkTS开发，提供现代化的股票分析和投资决策支持。

## 功能特性

### 📊 市场概览
- 实时市场指数显示
- 市场涨跌统计
- 热门股票列表
- ExpressVPN风格的现代化UI设计

### 🔍 股票搜索
- 智能股票搜索
- 分类筛选（沪深主板/科创板/创业板/北交所）
- 多种排序方式
- 列表/网格双视图模式

### 🤖 AI智能推荐
- 基于机器学习的股票推荐
- 多策略投资建议
- 风险偏好匹配
- 推荐置信度显示

### ⭐ 自选股管理
- 个性化股票关注列表
- 投资组合概览
- 实时盈亏统计
- 批量操作支持

### ⚙️ 个性化设置
- 深色/浅色主题切换
- 自定义主题颜色
- 通知设置管理
- 数据同步配置

## 技术架构

### 开发技术栈
- **开发语言**: ArkTS (TypeScript)
- **UI框架**: ArkUI 声明式开发
- **网络请求**: @kit.NetworkKit
- **数据存储**: @kit.ArkData Preferences
- **状态管理**: 响应式数据绑定

### 项目结构
```
harmonyos_client/
├── AppScope/                    # 应用级配置
│   ├── app.json5               # 应用配置
│   └── resources/              # 应用级资源
├── entry/                      # 主入口模块
│   ├── src/main/
│   │   ├── ets/               # ArkTS源码
│   │   │   ├── entryability/  # 入口Ability
│   │   │   ├── pages/         # 页面组件
│   │   │   ├── managers/      # 数据管理器
│   │   │   ├── services/      # 网络服务
│   │   │   ├── types/         # 类型定义
│   │   │   └── config/        # 配置管理
│   │   ├── resources/         # 模块资源
│   │   └── module.json5       # 模块配置
│   └── build-profile.json5    # 构建配置
├── build-profile.json5         # 项目构建配置
└── package.json               # 项目依赖
```

### 核心组件

#### 🎯 DataManager
- 统一数据状态管理
- API请求封装
- 本地缓存机制
- 数据同步支持

#### 🌐 ApiService  
- HTTP客户端封装
- WebSocket实时通信
- 请求重试机制
- 错误处理

#### ⚙️ ConfigManager
- 用户偏好设置
- 主题配置管理
- 功能开关控制
- 持久化存储

## 开发环境

### 系统要求
- DevEco Studio 4.0+
- HarmonyOS SDK API 10+
- Node.js 16+

### 安装步骤

1. **克隆项目**
   ```bash
   git clone <repository-url>
   cd tushare/harmonyos_client
   ```

2. **安装依赖**
   ```bash
   npm install
   ```

3. **配置开发环境**
   - 安装DevEco Studio
   - 下载HarmonyOS SDK
   - 配置签名证书

4. **打开项目**
   - 使用DevEco Studio打开`harmonyos_client`目录
   - 等待项目索引完成
   - 连接HarmonyOS设备或启动模拟器

5. **运行项目**
   - 点击运行按钮或使用快捷键
   - 选择目标设备
   - 等待编译和安装完成

## API配置

### 后端服务配置
修改 `entry/src/main/ets/config/ConfigManager.ets`:

```typescript
private readonly API_BASE_URL = 'http://your-server:5005';
private readonly WEBSOCKET_URL = 'ws://your-server:8765';
```

### 网络权限
项目已配置必要的网络权限：
- `ohos.permission.INTERNET` - 网络访问
- `ohos.permission.GET_NETWORK_INFO` - 网络状态检测

## 构建发布

### Debug构建
```bash
hvigorw assembleHap --mode debug
```

### Release构建
```bash  
hvigorw assembleHap --mode release
```

构建产物位于：`entry/build/outputs/hap/`

## 设计理念

### UI设计原则
- **现代化**: 采用ExpressVPN的设计语言
- **一致性**: 统一的视觉风格和交互模式
- **可访问性**: 支持不同屏幕尺寸和用户需求
- **性能优化**: 流畅的动画和响应体验

### 用户体验
- **直观导航**: 底部标签导航，符合移动端使用习惯
- **智能搜索**: 实时搜索建议和历史记录
- **个性化**: 用户可自定义主题和功能偏好
- **实时更新**: WebSocket实时数据推送

## 版本历史

### v1.0.0 (当前版本)
- ✨ 完整的五大核心功能模块
- 🎨 ExpressVPN风格的现代化UI设计
- 🔄 跨平台数据同步支持
- 📱 响应式布局适配
- 🔐 完善的权限管理

## 开发计划

### 短期计划
- [ ] 图表库集成（ECharts或自绘）
- [ ] 实时WebSocket数据展示
- [ ] 离线数据缓存优化
- [ ] 推送通知功能

### 长期计划
- [ ] 平板电脑适配
- [ ] 可穿戴设备支持
- [ ] AI语音交互
- [ ] 社区分享功能

## 贡献指南

1. Fork项目到个人账户
2. 创建功能分支: `git checkout -b feature/your-feature`
3. 提交更改: `git commit -am 'Add some feature'`
4. 推送分支: `git push origin feature/your-feature`
5. 提交Pull Request

## 许可证

本项目采用 MIT 许可证 - 查看 [LICENSE](../LICENSE) 文件了解详情。

## 联系方式

- 项目主页: https://github.com/yourusername/tushare-analysis-system
- 问题反馈: https://github.com/yourusername/tushare-analysis-system/issues
- 邮箱: your.email@example.com

---

**灵境万象** - 让数据洞察未来，让智能驱动投资 🚀