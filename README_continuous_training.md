# ljwx-stock 持续训练系统

## 概述
ljwx-stock持续训练系统允许自动增加训练数据集并持续改进模型性能。系统支持增量训练、自动调度和性能监控。

## 核心功能

### 1. 持续训练 (`continuous_training.py`)
- **增量数据生成**: 自动获取新的股票数据并生成训练样本
- **智能去重**: 避免重复训练相同的数据
- **模型增量更新**: 基于最新模型进行增量训练
- **性能评估**: 自动测试新模型的性能

### 2. 自动调度 (`scheduler.py`)
- **定时训练**: 按配置的时间自动执行训练
- **性能监控**: 监控模型性能变化趋势
- **模型管理**: 自动清理旧模型，保持系统整洁
- **报告生成**: 定期生成训练报告

### 3. 配置管理 (`training_config.json`)
- **灵活配置**: 支持各种训练参数的自定义
- **调度设置**: 可配置训练时间和频率
- **性能阈值**: 设置性能监控标准

## 使用方法

### 快速开始

1. **安装依赖**
```bash
pip install schedule pandas requests tushare
```

2. **设置环境变量**
```bash
export TUSHARE_TOKEN="your_tushare_token"  # 可选，用于TuShare Pro API
```

3. **立即执行一次训练**
```bash
python continuous_training.py
```

### 自动调度

1. **启动调度器**
```bash
python scheduler.py
```

2. **立即执行训练任务**
```bash
python scheduler.py run
```

3. **查看训练报告**
```bash
python scheduler.py report
```

4. **清理旧模型**
```bash
python scheduler.py cleanup
```

5. **监控性能**
```bash
python scheduler.py monitor
```

## 系统架构

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   TuShare API   │    │  训练数据生成器   │    │   Ollama引擎    │
│                 │────│                 │────│                 │
│ • 股票数据      │    │ • 数据处理      │    │ • 模型训练      │
│ • 技术指标      │    │ • 样本生成      │    │ • 模型部署      │
└─────────────────┘    └─────────────────┘    └─────────────────┘
         │                       │                       │
         └───────────────────────┼───────────────────────┘
                                 │
                    ┌─────────────────┐
                    │   调度管理器     │
                    │                 │
                    │ • 定时训练      │
                    │ • 性能监控      │
                    │ • 模型管理      │
                    │ • 报告生成      │
                    └─────────────────┘
```

## 训练流程

### 增量训练流程
1. **获取最新模型**: 自动识别当前最新的ljwx-stock模型
2. **数据分析**: 分析现有训练数据，识别已训练的股票
3. **新数据生成**: 获取新股票或新时间段的数据
4. **数据合并**: 合并新旧训练数据并去重
5. **模型训练**: 基于最新模型创建增量训练版本
6. **性能测试**: 自动测试新模型的性能
7. **结果保存**: 保存训练结果和性能报告

### 调度流程
1. **定时触发**: 按设定时间自动触发训练任务
2. **执行训练**: 运行增量训练流程
3. **性能监控**: 分析模型性能变化
4. **模型清理**: 删除过期的旧模型
5. **报告生成**: 生成并记录训练报告

## 配置说明

### 训练配置
```json
{
  "continuous_training": {
    "base_model": "ljwx-stock",              // 基础模型名称
    "max_training_examples_per_batch": 200, // 每批训练样本数量
    "stock_batch_size": 30,                 // 每批处理股票数量
    "days_back": 90,                        // 历史数据天数
    "incremental_days": 30,                 // 增量数据天数
    "max_models_to_keep": 5                 // 保留模型数量
  }
}
```

### 调度配置
```json
{
  "scheduler": {
    "daily_training_time": "02:00",         // 每日训练时间
    "weekly_training_days": ["monday", "friday"], // 每周训练日
    "weekly_training_times": ["10:00", "16:00"]   // 每周训练时间
  }
}
```

## 性能监控

### 监控指标
- **成功率**: 测试用例通过比例
- **响应时间**: 模型推理速度
- **数据量**: 训练数据总量
- **性能趋势**: 历史性能变化

### 性能阈值
- **最低成功率**: 80%
- **最大响应时间**: 30秒
- **质量关键词**: 技术指标、投资建议、风险评估等

## 文件结构

```
tushare/
├── continuous_training.py          # 持续训练主脚本
├── scheduler.py                    # 自动调度器
├── training_config.json            # 配置文件
├── data/
│   ├── llm_training/               # 训练数据目录
│   │   ├── stock_training_data_*.jsonl
│   │   └── *_test_results_*.json
│   └── models/                     # 模型结果目录
│       └── continuous_training_results_*.json
├── logs/                           # 日志目录
│   └── scheduler_*.log
└── llm/                           # 核心组件
    ├── tushare_data_extractor.py
    ├── llm_training_data_generator.py
    └── ollama_trainer.py
```

## 使用示例

### 示例1: 立即执行训练
```bash
# 执行一次完整的增量训练
python continuous_training.py

# 查看训练结果
ls data/models/continuous_training_results_*.json
```

### 示例2: 设置定时训练
```bash
# 启动调度器（后台运行）
nohup python scheduler.py > scheduler.log 2>&1 &

# 查看调度器状态
tail -f scheduler.log
```

### 示例3: 自定义训练参数
```python
# 修改 training_config.json
{
  "continuous_training": {
    "stock_batch_size": 50,        # 增加股票数量
    "days_back": 120,              # 增加历史数据天数
    "max_training_examples_per_batch": 300  # 增加训练样本
  }
}
```

## 故障排除

### 常见问题

1. **TuShare API限制**
   - 使用免费API时有访问频率限制
   - 建议设置适当的延迟或使用Pro API

2. **Ollama服务未启动**
   - 确保Ollama服务正在运行: `ollama serve`
   - 检查模型是否存在: `ollama list`

3. **磁盘空间不足**
   - 定期清理旧模型: `python scheduler.py cleanup`
   - 监控磁盘使用情况

4. **训练时间过长**
   - 减少训练样本数量
   - 优化Modelfile格式

### 日志分析
```bash
# 查看训练日志
tail -f logs/scheduler_$(date +%Y%m%d).log

# 搜索错误信息
grep -i error logs/scheduler_*.log

# 分析性能趋势
grep "性能测试" logs/scheduler_*.log
```

## 最佳实践

1. **数据质量**: 确保TuShare数据源稳定可靠
2. **增量训练**: 避免重复训练相同数据
3. **性能监控**: 定期检查模型性能变化
4. **资源管理**: 及时清理旧模型释放空间
5. **备份策略**: 定期备份重要的训练数据和配置

## 扩展功能

### 可扩展的组件
1. **数据源扩展**: 支持其他金融数据API
2. **训练策略**: 支持不同的训练算法
3. **性能评估**: 添加更多评估指标
4. **通知系统**: 集成邮件或消息通知

### 自定义开发
```python
# 自定义数据处理器
class CustomDataProcessor:
    def process_data(self, raw_data):
        # 自定义数据处理逻辑
        pass

# 自定义性能评估器
class CustomPerformanceEvaluator:
    def evaluate(self, model_name):
        # 自定义性能评估逻辑
        pass
```

---

通过这个持续训练系统，ljwx-stock模型可以不断学习新的市场数据，持续改进分析能力，为用户提供更准确的股票投资建议。