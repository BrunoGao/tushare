#!/bin/bash

# LJWX Stock 专业模型开发组件快速安装脚本

echo "🚀 LJWX Stock 专业模型开发组件安装器"
echo "========================================="

# 升级pip
echo "🔄 升级pip..."
pip install --upgrade pip

# 安装核心机器学习组件
echo "🔄 安装MLflow实验管理..."
pip install mlflow>=2.8.0

echo "🔄 安装Optuna超参数优化..."
pip install optuna>=3.4.0

echo "🔄 安装LightGBM..."
pip install lightgbm>=4.1.0

echo "🔄 安装XGBoost..."
pip install xgboost>=2.0.0

echo "🔄 安装CatBoost..."
pip install catboost>=1.2.0

echo "🔄 安装SciPy科学计算..."
pip install scipy>=1.11.0

echo "🔄 安装统计模型..."
pip install statsmodels>=0.14.0

echo ""
echo "✅ 专业模型开发组件安装完成！"
echo ""
echo "下一步:"
echo "1. 重启LJWX Stock服务器: python unified_app.py"
echo "2. 访问admin界面验证模型开发功能"
echo "3. 开始使用专业的量化金融模型训练流程"
echo ""
echo "🎯 现在可以使用完整的策略→数据→训练→评估工作流程了！"