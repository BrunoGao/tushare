#!/bin/bash
# 使用ARM64原生Python设置M2 Ultra环境

echo "🚀 使用ARM64原生Python设置M2 Ultra环境"
echo "=========================================="

# 使用Homebrew的ARM64 Python
PYTHON_CMD="/opt/homebrew/bin/python3"

if [ ! -f "$PYTHON_CMD" ]; then
    echo "❌ 未找到Homebrew Python，请先安装："
    echo "brew install python"
    exit 1
fi

echo "✅ 使用Python: $PYTHON_CMD"
echo "Python版本: $($PYTHON_CMD --version)"
echo "Python架构: $($PYTHON_CMD -c 'import platform; print(platform.machine())')"

# 创建虚拟环境
VENV_PATH="/Users/brunogao/IdeaProjects/tushare/venv_m2_native"

echo ""
echo "📦 创建ARM64原生虚拟环境..."
$PYTHON_CMD -m venv $VENV_PATH

# 激活虚拟环境
source $VENV_PATH/bin/activate

echo "✅ 虚拟环境已激活"
echo "当前Python: $(which python)"
echo "当前架构: $(python -c 'import platform; print(platform.machine())')"

# 升级pip
echo ""
echo "⬆️ 升级pip..."
python -m pip install --upgrade pip

# 安装基础科学计算库
echo ""
echo "📊 安装科学计算库..."
pip install numpy scipy pandas matplotlib seaborn scikit-learn

# 安装PyTorch (ARM64版本，支持MPS)
echo ""
echo "🔥 安装PyTorch (ARM64 + MPS支持)..."
pip install torch torchvision torchaudio

# 安装XGBoost和LightGBM
echo ""
echo "🌳 安装机器学习库..."
pip install xgboost lightgbm

# 安装其他必要库
echo ""
echo "📋 安装其他依赖..."
pip install tushare akshare yfinance psutil joblib

# 测试关键库
echo ""
echo "🧪 测试关键库加载..."
python << 'EOF'
import platform
print(f"🔧 Python架构: {platform.machine()}")
print()

try:
    import numpy as np
    print(f"✅ NumPy {np.__version__}")
    # 测试BLAS性能
    import time
    a = np.random.rand(1000, 1000)
    start = time.time()
    np.dot(a, a)
    print(f"   矩阵乘法性能: {time.time() - start:.4f}秒")
except Exception as e:
    print(f"❌ NumPy错误: {e}")

try:
    import torch
    print(f"✅ PyTorch {torch.__version__}")
    if torch.backends.mps.is_available():
        print("✅ MPS (Metal Performance Shaders) 可用")
        device = torch.device('mps')
        x = torch.randn(100, 100).to(device)
        y = torch.randn(100, 100).to(device)
        start = time.time()
        z = torch.mm(x, y)
        print(f"   MPS矩阵乘法性能: {time.time() - start:.4f}秒")
    else:
        print("⚠️ MPS不可用")
except Exception as e:
    print(f"❌ PyTorch错误: {e}")

try:
    import xgboost as xgb
    print(f"✅ XGBoost {xgb.__version__}")
except Exception as e:
    print(f"❌ XGBoost错误: {e}")

try:
    import lightgbm as lgb
    print(f"✅ LightGBM {lgb.__version__}")
except Exception as e:
    print(f"❌ LightGBM错误: {e}")

try:
    import sklearn
    print(f"✅ scikit-learn {sklearn.__version__}")
except Exception as e:
    print(f"❌ scikit-learn错误: {e}")
EOF

echo ""
echo "🎯 M2 Ultra原生环境设置完成！"
echo ""
echo "📋 使用方法："
echo "1. source $VENV_PATH/bin/activate"
echo "2. cd /Users/brunogao/IdeaProjects/tushare"
echo "3. python test_m2_ultra_performance.py"
echo ""
echo "🚀 现在您拥有真正的ARM64原生M2 Ultra性能！"