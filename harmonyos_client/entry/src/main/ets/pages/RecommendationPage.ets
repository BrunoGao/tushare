/**
 * AI推荐页面 - ExpressVPN风格设计
 * 智能投资建议和市场分析展示
 */

import { DataState, AIRecommendation } from '../types/ApiTypes';
import { DataManager } from '../managers/DataManager';
import { ConfigManager } from '../config/ConfigManager';

@Component
export struct RecommendationPage {
  @Prop dataState: DataState;
  @Prop onRefresh: () => void;
  
  @State private selectedStrategy: string = '全部';
  @State private selectedTimeframe: string = '短期';
  @State private isGenerating: boolean = false;
  @State private strategies: string[] = ['全部', '价值投资', '成长投资', '技术分析', '动量策略'];
  @State private timeframes: string[] = ['短期', '中期', '长期'];
  @State private riskLevels: string[] = ['保守', '稳健', '积极'];
  @State private selectedRiskLevel: string = '稳健';

  private dataManager: DataManager = DataManager.getInstance();
  private configManager: ConfigManager = ConfigManager.getInstance();

  build() {
    Column() {
      // AI推荐头部
      this.buildAIHeader()
      
      // 策略筛选区域
      this.buildStrategyFilters()
      
      // AI推荐内容
      if (this.isGenerating) {
        this.buildGeneratingState()
      } else if (this.dataState.aiRecommendations.length === 0) {
        this.buildEmptyRecommendations()
      } else {
        this.buildRecommendationsList()
      }
    }
    .width('100%')
    .height('100%')
    .backgroundColor('#F8F9FA')
  }

  /**
   * AI推荐头部
   */
  @Builder
  buildAIHeader() {
    Stack() {
      // 渐变背景
      Column()
        .width('100%')
        .height(180)
        .linearGradient({
          angle: 135,
          colors: [['#8b5cf6', 0.0], ['#a855f7', 1.0]]
        })
        .borderRadius({ bottomLeft: 24, bottomRight: 24 })

      // 头部内容
      Column() {
        Row() {
          Column() {
            Text('AI智能推荐')
              .fontSize(28)
              .fontWeight(FontWeight.Bold)
              .fontColor('#FFFFFF')
              .margin({ bottom: 8 })
            
            Text('基于大数据和机器学习的投资建议')
              .fontSize(14)
              .fontColor('#FFFFFF')
              .opacity(0.8)
          }
          .alignItems(HorizontalAlign.Start)
          .flexGrow(1)

          // AI助手图标
          Stack() {
            Circle({ width: 60, height: 60 })
              .fill('rgba(255, 255, 255, 0.15)')
            
            Circle({ width: 48, height: 48 })
              .fill('rgba(255, 255, 255, 0.1)')
            
            Image($r('app.media.ic_ai_brain'))
              .width(24)
              .height(24)
              .fillColor('#FFFFFF')
          }
        }
        .width('100%')
        .margin({ bottom: 20 })

        // AI状态指示器
        Row() {
          Row() {
            Circle({ width: 8, height: 8 })
              .fill('#10B981')
              .margin({ right: 8 })
            
            Text('AI分析引擎在线')
              .fontSize(12)
              .fontColor('#FFFFFF')
              .opacity(0.9)
          }
          .padding({ horizontal: 12, vertical: 6 })
          .backgroundColor('rgba(255, 255, 255, 0.2)')
          .borderRadius(16)

          Blank()

          Text(`${this.dataState.aiRecommendations.length} 条推荐`)
            .fontSize(12)
            .fontColor('#FFFFFF')
            .opacity(0.8)
        }
        .width('100%')
      }
      .padding({ left: 20, right: 20, top: 40 })
      .width('100%')
    }
    .width('100%')
    .height(180)
  }

  /**
   * 策略筛选区域
   */
  @Builder
  buildStrategyFilters() {
    Column() {
      // 投资策略
      Row() {
        Text('投资策略')
          .fontSize(14)
          .fontWeight(FontWeight.Medium)
          .fontColor('#1F2937')
          .margin({ right: 12 })

        Scroll() {
          Row() {
            ForEach(this.strategies, (strategy: string) => {
              this.buildFilterChip(strategy, this.selectedStrategy, (value: string) => {
                this.selectedStrategy = value;
                this.applyFilters();
              })
            })
          }
        }
        .scrollable(ScrollDirection.Horizontal)
        .scrollBar(BarState.Off)
        .flexGrow(1)
      }
      .width('100%')
      .margin({ bottom: 16 })

      // 时间周期和风险等级
      Row() {
        // 时间周期
        Column() {
          Text('时间周期')
            .fontSize(12)
            .fontColor('#6B7280')
            .alignSelf(ItemAlign.Start)
            .margin({ bottom: 8 })
          
          Row() {
            ForEach(this.timeframes, (timeframe: string) => {
              this.buildSmallFilterChip(timeframe, this.selectedTimeframe, (value: string) => {
                this.selectedTimeframe = value;
                this.applyFilters();
              })
            })
          }
        }
        .alignItems(HorizontalAlign.Start)
        .flexGrow(1)

        // 风险等级
        Column() {
          Text('风险偏好')
            .fontSize(12)
            .fontColor('#6B7280')
            .alignSelf(ItemAlign.Start)
            .margin({ bottom: 8 })
          
          Row() {
            ForEach(this.riskLevels, (risk: string) => {
              this.buildSmallFilterChip(risk, this.selectedRiskLevel, (value: string) => {
                this.selectedRiskLevel = value;
                this.applyFilters();
              })
            })
          }
        }
        .alignItems(HorizontalAlign.Start)
        .flexGrow(1)
      }
      .width('100%')
      .margin({ bottom: 16 })

      // 操作按钮
      Row() {
        Button('重新生成推荐')
          .fontSize(14)
          .fontColor('#FFFFFF')
          .backgroundColor('#8B5CF6')
          .borderRadius(20)
          .flexGrow(1)
          .height(40)
          .onClick(() => {
            this.generateRecommendations();
          })

        Button('导出报告')
          .fontSize(14)
          .fontColor('#8B5CF6')
          .backgroundColor('rgba(139, 92, 246, 0.1)')
          .borderRadius(20)
          .margin({ left: 12 })
          .padding({ horizontal: 20 })
          .height(40)
          .onClick(() => {
            this.exportReport();
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
    .margin({ top: -30, left: 20, right: 20, bottom: 20 })
  }

  /**
   * 筛选芯片
   */
  @Builder
  buildFilterChip(text: string, selectedValue: string, onSelect: (value: string) => void) {
    Button(text)
      .fontSize(12)
      .fontColor(selectedValue === text ? '#FFFFFF' : '#6B7280')
      .backgroundColor(selectedValue === text ? '#8B5CF6' : '#F3F4F6')
      .borderRadius(16)
      .padding({ horizontal: 16, vertical: 8 })
      .margin({ right: 8 })
      .onClick(() => onSelect(text))
  }

  /**
   * 小型筛选芯片
   */
  @Builder
  buildSmallFilterChip(text: string, selectedValue: string, onSelect: (value: string) => void) {
    Button(text)
      .fontSize(10)
      .fontColor(selectedValue === text ? '#FFFFFF' : '#6B7280')
      .backgroundColor(selectedValue === text ? '#8B5CF6' : '#F3F4F6')
      .borderRadius(12)
      .padding({ horizontal: 12, vertical: 4 })
      .margin({ right: 6 })
      .onClick(() => onSelect(text))
  }

  /**
   * 推荐列表
   */
  @Builder
  buildRecommendationsList() {
    List() {
      ForEach(this.dataState.aiRecommendations, (recommendation: any, index: number) => {
        ListItem() {
          this.buildRecommendationCard(recommendation, index)
        }
        .margin({ horizontal: 20, vertical: 8 })
      })
    }
    .width('100%')
    .layoutWeight(1)
    .padding({ bottom: 20 })
    .divider({ strokeWidth: 0 })
  }

  /**
   * 推荐卡片
   */
  @Builder
  buildRecommendationCard(recommendation: any, index: number) {
    Column() {
      // 卡片头部
      Row() {
        // 股票信息
        Column() {
          Text(recommendation.name || recommendation.ts_code)
            .fontSize(18)
            .fontWeight(FontWeight.Bold)
            .fontColor('#1F2937')
            .alignSelf(ItemAlign.Start)
          
          Text(recommendation.ts_code)
            .fontSize(12)
            .fontColor('#6B7280')
            .margin({ top: 2 })
            .alignSelf(ItemAlign.Start)
        }
        .alignItems(HorizontalAlign.Start)
        .flexGrow(1)

        // 推荐等级
        this.buildRecommendationBadge(recommendation.recommendation, recommendation.confidence)
      }
      .width('100%')
      .margin({ bottom: 16 })

      // 价格信息
      if (recommendation.current_price) {
        Row() {
          Column() {
            Text('当前价格')
              .fontSize(12)
              .fontColor('#6B7280')
            
            Text(`¥${recommendation.current_price.toFixed(2)}`)
              .fontSize(16)
              .fontWeight(FontWeight.Medium)
              .fontColor('#1F2937')
              .margin({ top: 2 })
          }
          .alignItems(HorizontalAlign.Start)

          Column() {
            Text('目标价格')
              .fontSize(12)
              .fontColor('#6B7280')
            
            Text(`¥${recommendation.target_price?.toFixed(2) || '--'}`)
              .fontSize(16)
              .fontWeight(FontWeight.Medium)
              .fontColor('#10B981')
              .margin({ top: 2 })
          }
          .alignItems(HorizontalAlign.Start)
          .margin({ left: 40 })

          Column() {
            Text('预期收益')
              .fontSize(12)
              .fontColor('#6B7280')
            
            Text(this.calculateExpectedReturn(recommendation))
              .fontSize(16)
              .fontWeight(FontWeight.Medium)
              .fontColor(this.getReturnColor(recommendation))
              .margin({ top: 2 })
          }
          .alignItems(HorizontalAlign.Start)
          .margin({ left: 40 })

          Blank()
        }
        .width('100%')
        .margin({ bottom: 16 })
      }

      // AI分析原因
      Column() {
        Text('AI分析')
          .fontSize(14)
          .fontWeight(FontWeight.Medium)
          .fontColor('#1F2937')
          .alignSelf(ItemAlign.Start)
          .margin({ bottom: 8 })
        
        Text(recommendation.reasoning || '基于技术分析和基本面分析的综合推荐')
          .fontSize(14)
          .fontColor('#6B7280')
          .lineHeight(20)
          .alignSelf(ItemAlign.Start)
      }
      .width('100%')
      .alignItems(HorizontalAlign.Start)
      .margin({ bottom: 16 })

      // 关键指标
      if (recommendation.key_metrics) {
        this.buildKeyMetrics(recommendation.key_metrics)
      }

      // 操作按钮
      Row() {
        Button('查看详情')
          .fontSize(14)
          .fontColor('#8B5CF6')
          .backgroundColor('rgba(139, 92, 246, 0.1)')
          .borderRadius(20)
          .flexGrow(1)
          .height(36)
          .onClick(() => {
            this.viewRecommendationDetail(recommendation);
          })

        Button('加入自选')
          .fontSize(14)
          .fontColor('#FFFFFF')
          .backgroundColor('#10B981')
          .borderRadius(20)
          .margin({ left: 12 })
          .padding({ horizontal: 20 })
          .height(36)
          .onClick(() => {
            this.addToWatchlist(recommendation);
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
      color: 'rgba(0, 0, 0, 0.06)',
      offsetX: 0,
      offsetY: 2
    })
  }

  /**
   * 推荐等级标识
   */
  @Builder
  buildRecommendationBadge(recommendation: string, confidence: number) {
    Column() {
      Text(this.getRecommendationText(recommendation))
        .fontSize(14)
        .fontWeight(FontWeight.Bold)
        .fontColor('#FFFFFF')
        .padding({ horizontal: 12, vertical: 6 })
        .backgroundColor(this.getRecommendationColor(recommendation))
        .borderRadius(16)
      
      Text(`置信度 ${(confidence * 100 || 85).toFixed(0)}%`)
        .fontSize(10)
        .fontColor('#6B7280')
        .margin({ top: 4 })
    }
    .alignItems(HorizontalAlign.Center)
  }

  /**
   * 关键指标
   */
  @Builder
  buildKeyMetrics(metrics: any) {
    Column() {
      Text('关键指标')
        .fontSize(12)
        .fontColor('#6B7280')
        .alignSelf(ItemAlign.Start)
        .margin({ bottom: 8 })
      
      Grid() {
        GridItem() {
          this.buildMetricItem('PE', metrics.pe?.toFixed(1) || '--')
        }
        GridItem() {
          this.buildMetricItem('PB', metrics.pb?.toFixed(1) || '--')
        }
        GridItem() {
          this.buildMetricItem('ROE', `${(metrics.roe * 100 || 0).toFixed(1)}%`)
        }
        GridItem() {
          this.buildMetricItem('收益率', `${(metrics.expected_return * 100 || 0).toFixed(1)}%`)
        }
      }
      .columnsTemplate('1fr 1fr 1fr 1fr')
      .width('100%')
      .height(60)
    }
    .width('100%')
    .margin({ bottom: 16 })
  }

  /**
   * 指标项
   */
  @Builder
  buildMetricItem(label: string, value: string) {
    Column() {
      Text(value)
        .fontSize(14)
        .fontWeight(FontWeight.Medium)
        .fontColor('#1F2937')
      
      Text(label)
        .fontSize(10)
        .fontColor('#6B7280')
        .margin({ top: 2 })
    }
    .alignItems(HorizontalAlign.Center)
    .justifyContent(FlexAlign.Center)
    .width('100%')
    .height('100%')
    .backgroundColor('#F9FAFB')
    .borderRadius(8)
  }

  /**
   * 生成中状态
   */
  @Builder
  buildGeneratingState() {
    Column() {
      // AI分析动画
      Stack() {
        Circle({ width: 80, height: 80 })
          .fill('rgba(139, 92, 246, 0.1)')
        
        Circle({ width: 60, height: 60 })
          .fill('rgba(139, 92, 246, 0.2)')
        
        LoadingProgress()
          .width(40)
          .height(40)
          .color('#8B5CF6')
      }
      .margin({ bottom: 20 })
      
      Text('AI正在分析市场数据')
        .fontSize(18)
        .fontWeight(FontWeight.Medium)
        .fontColor('#1F2937')
        .margin({ bottom: 8 })
      
      Text('请稍候，正在为您生成个性化投资建议...')
        .fontSize(14)
        .fontColor('#6B7280')
        .textAlign(TextAlign.Center)
        .lineHeight(20)
        .margin({ bottom: 24 })

      // 进度指示器
      Column() {
        Row() {
          Text('数据收集')
            .fontSize(12)
            .fontColor('#10B981')
          Blank()
          Text('✓')
            .fontSize(12)
            .fontColor('#10B981')
        }
        .width('100%')
        .margin({ bottom: 8 })

        Row() {
          Text('技术分析')
            .fontSize(12)
            .fontColor('#8B5CF6')
          Blank()
          LoadingProgress()
            .width(12)
            .height(12)
            .color('#8B5CF6')
        }
        .width('100%')
        .margin({ bottom: 8 })

        Row() {
          Text('风险评估')
            .fontSize(12)
            .fontColor('#6B7280')
          Blank()
          Text('⏳')
            .fontSize(12)
            .fontColor('#6B7280')
        }
        .width('100%')
      }
      .width(200)
      .padding(16)
      .backgroundColor('#F9FAFB')
      .borderRadius(12)
    }
    .justifyContent(FlexAlign.Center)
    .alignItems(HorizontalAlign.Center)
    .width('100%')
    .layoutWeight(1)
  }

  /**
   * 空推荐状态
   */
  @Builder
  buildEmptyRecommendations() {
    Column() {
      Image($r('app.media.ic_ai_empty'))
        .width(120)
        .height(120)
        .opacity(0.4)
        .margin({ bottom: 24 })
      
      Text('暂无AI推荐')
        .fontSize(20)
        .fontWeight(FontWeight.Medium)
        .fontColor('#1F2937')
        .margin({ bottom: 8 })
      
      Text('点击下方按钮生成个性化投资建议')
        .fontSize(14)
        .fontColor('#6B7280')
        .textAlign(TextAlign.Center)
        .margin({ bottom: 32 })

      Button('生成AI推荐')
        .fontSize(16)
        .fontColor('#FFFFFF')
        .backgroundColor('#8B5CF6')
        .borderRadius(24)
        .padding({ horizontal: 32, vertical: 12 })
        .shadow({
          radius: 8,
          color: 'rgba(139, 92, 246, 0.3)',
          offsetX: 0,
          offsetY: 4
        })
        .onClick(() => {
          this.generateRecommendations();
        })
    }
    .justifyContent(FlexAlign.Center)
    .alignItems(HorizontalAlign.Center)
    .width('100%')
    .layoutWeight(1)
  }

  // ============ 业务逻辑方法 ============

  private async generateRecommendations() {
    this.isGenerating = true;
    
    try {
      await this.dataManager.getAIRecommendations(20, true);
    } catch (error) {
      console.error('生成推荐失败:', error);
    } finally {
      this.isGenerating = false;
    }
  }

  private applyFilters() {
    // 应用筛选逻辑
    console.log('应用筛选:', this.selectedStrategy, this.selectedTimeframe, this.selectedRiskLevel);
  }

  private exportReport() {
    // 导出推荐报告
    console.log('导出报告');
  }

  private viewRecommendationDetail(recommendation: any) {
    // 查看推荐详情
    console.log('查看详情:', recommendation);
  }

  private addToWatchlist(recommendation: any) {
    this.dataManager.addToWatchlist(recommendation.ts_code);
  }

  // ============ 工具方法 ============

  private getRecommendationText(recommendation: string): string {
    switch (recommendation) {
      case 'buy': return '买入';
      case 'sell': return '卖出';
      case 'hold': return '持有';
      default: return '观望';
    }
  }

  private getRecommendationColor(recommendation: string): string {
    switch (recommendation) {
      case 'buy': return '#10B981';
      case 'sell': return '#EF4444';
      case 'hold': return '#F59E0B';
      default: return '#6B7280';
    }
  }

  private calculateExpectedReturn(recommendation: any): string {
    if (recommendation.current_price && recommendation.target_price) {
      const returnPct = ((recommendation.target_price - recommendation.current_price) / recommendation.current_price) * 100;
      return `${returnPct >= 0 ? '+' : ''}${returnPct.toFixed(1)}%`;
    }
    return '--';
  }

  private getReturnColor(recommendation: any): string {
    const returnPct = recommendation.target_price && recommendation.current_price ?
      ((recommendation.target_price - recommendation.current_price) / recommendation.current_price) * 100 : 0;
    return returnPct >= 0 ? '#10B981' : '#EF4444';
  }
}