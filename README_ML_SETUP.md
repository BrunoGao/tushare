# LJWX Stock 专业模型开发组件安装指南

## 🎯 概述
为了使用完整的量化金融专业工作流程（策略→数据→训练→评估），需要安装以下机器学习组件：

## 📋 必需组件
- **MLflow**: 实验管理和模型版本控制
- **Optuna**: 超参数优化
- **LightGBM**: 高性能梯度提升框架
- **XGBoost**: 极端梯度提升
- **CatBoost**: 分类提升算法
- **SciPy**: 科学计算库
- **statsmodels**: 统计建模

## 🚀 快速安装

### 方法1: 使用pip3（推荐）
```bash
# 升级pip
pip3 install --upgrade pip

# 安装所有专业模型开发组件
pip3 install mlflow>=2.8.0 optuna>=3.4.0 lightgbm>=4.1.0 xgboost>=2.0.0 catboost>=1.2.0 scipy>=1.11.0 statsmodels>=0.14.0
```

### 方法2: 分步安装
```bash
pip3 install mlflow>=2.8.0      # MLflow实验管理
pip3 install optuna>=3.4.0      # Optuna超参数优化  
pip3 install lightgbm>=4.1.0    # LightGBM
pip3 install xgboost>=2.0.0     # XGBoost
pip3 install catboost>=1.2.0    # CatBoost
pip3 install scipy>=1.11.0      # SciPy科学计算
pip3 install statsmodels>=0.14.0 # 统计模型
```

### 方法3: 使用requirements.txt
```bash
pip3 install -r requirements.txt
```

## 🔍 验证安装

运行以下Python代码验证安装：

```python
# 测试导入所有组件
try:
    import mlflow
    print("✅ MLflow 可用")
except ImportError:
    print("❌ MLflow 不可用")

try:
    import optuna
    print("✅ Optuna 可用")
except ImportError:
    print("❌ Optuna 不可用")

try:
    import lightgbm
    print("✅ LightGBM 可用")
except ImportError:
    print("❌ LightGBM 不可用")

try:
    import xgboost
    print("✅ XGBoost 可用")
except ImportError:
    print("❌ XGBoost 不可用")

try:
    import catboost
    print("✅ CatBoost 可用")
except ImportError:
    print("❌ CatBoost 不可用")

try:
    import scipy
    print("✅ SciPy 可用")
except ImportError:
    print("❌ SciPy 不可用")

try:
    import statsmodels
    print("✅ statsmodels 可用")
except ImportError:
    print("❌ statsmodels 不可用")

print("🎯 验证完成！")
```

## 🎉 安装完成后

1. **重启LJWX Stock服务器**:
   ```bash
   python3 unified_app.py
   ```

2. **检查启动日志**，应该看到：
   ```
   ✅ 模型开发和评估组件已可用
   ```

3. **访问Admin界面**，验证以下功能可用：
   - 第一步：策略设计
   - 第二步：数据准备（基于策略生成训练数据集）
   - 第三步：模型训练（MLflow实验管理、Optuna超参数优化）
   - 第四步：模型评估（Walk-Forward验证、风险调整指标）

## 🛠️ 故障排除

### 问题1: pip/pip3 命令未找到
**解决方案**: 
- macOS: `python3 -m pip install <package>`
- 或安装pip: `python3 -m ensurepip --upgrade`

### 问题2: 某些包安装失败
**解决方案**:
```bash
# 尝试单独安装失败的包
pip3 install --upgrade setuptools wheel
pip3 install <package_name> --no-cache-dir
```

### 问题3: 权限问题
**解决方案**:
```bash
# 使用用户安装
pip3 install <package_name> --user
```

### 问题4: 依赖冲突
**解决方案**:
```bash
# 创建虚拟环境
python3 -m venv tushare_venv
source tushare_venv/bin/activate  # macOS/Linux
# 或
tushare_venv\Scripts\activate     # Windows

pip install -r requirements.txt
```

## 📊 功能对照表

| 组件 | 功能 | Admin界面位置 |
|------|------|---------------|
| MLflow | 实验跟踪、模型版本控制 | 第三步 → 实验管理 |
| Optuna | 超参数自动优化 | 第三步 → 超参数优化 |
| LightGBM/XGBoost/CatBoost | 机器学习算法 | 第三步 → 模型训练 |
| SciPy/statsmodels | 统计分析、风险指标 | 第四步 → 风险调整指标 |

## 🎯 下一步

安装完成后，您可以：

1. **创建投资策略** - 定义量化投资逻辑
2. **生成训练数据集** - 基于策略需求自动生成特征和标签
3. **训练机器学习模型** - 使用专业算法和超参数优化
4. **全面评估模型** - 包括Walk-Forward验证和风险调整指标
5. **部署到生产环境** - 完整的MLOps流程

享受专业的量化金融模型开发体验！🚀