#!/bin/bash
# M2 Ultra环境修复脚本 - 安装ARM64原生Python环境

echo "🔧 M2 Ultra环境修复脚本"
echo "=========================================="

# 检查当前架构
echo "当前系统架构: $(uname -m)"
echo "当前Python架构: $(python -c 'import platform; print(platform.machine())')"

# 检查是否有ARM64版本的conda
ARM64_CONDA_PATH="/opt/homebrew/Caskroom/miniconda/base/bin/conda"
MINIFORGE_PATH="/opt/homebrew/bin/conda"

if [ -f "$ARM64_CONDA_PATH" ]; then
    echo "✅ 发现ARM64版本的miniconda"
    CONDA_CMD="$ARM64_CONDA_PATH"
elif [ -f "$MINIFORGE_PATH" ]; then
    echo "✅ 发现miniforge (ARM64原生)"
    CONDA_CMD="$MINIFORGE_PATH"
else
    echo "❌ 未发现ARM64版本的conda"
    echo ""
    echo "📋 解决方案1: 安装ARM64原生miniforge"
    echo "curl -L -O 'https://github.com/conda-forge/miniforge/releases/latest/download/Miniforge3-MacOSX-arm64.sh'"
    echo "bash Miniforge3-MacOSX-arm64.sh"
    echo ""
    echo "📋 解决方案2: 使用homebrew安装"
    echo "brew install --cask miniconda"
    echo ""
    echo "安装后请重启终端并重新运行此脚本"
    exit 1
fi

echo "使用conda: $CONDA_CMD"

# 检查当前conda环境的架构
CONDA_ARCH=$($CONDA_CMD info | grep "platform" | awk '{print $3}' || echo "unknown")
echo "Conda平台: $CONDA_ARCH"

if [[ "$CONDA_ARCH" == *"osx-arm64"* ]]; then
    echo "✅ Conda环境已经是ARM64原生"
else
    echo "⚠️ Conda环境不是ARM64原生，建议重新安装"
fi

echo ""
echo "🔄 创建ARM64优化的Python环境..."

# 创建新的ARM64环境
$CONDA_CMD create -n tushare_m2 python=3.12 -y

# 激活环境并安装依赖
echo "📦 安装ARM64优化的机器学习库..."

# 激活环境的函数
activate_env() {
    source $($CONDA_CMD info --base)/etc/profile.d/conda.sh
    conda activate tushare_m2
}

# 在子shell中执行安装
(
    activate_env
    
    # 验证Python架构
    echo "新环境Python架构: $(python -c 'import platform; print(platform.machine())')"
    
    # 安装基础科学计算库 (ARM64优化版本)
    conda install -c conda-forge numpy scipy pandas scikit-learn matplotlib seaborn -y
    
    # 安装PyTorch with MPS支持
    pip install torch torchvision torchaudio
    
    # 安装XGBoost和LightGBM的ARM64版本
    conda install -c conda-forge xgboost lightgbm -y
    
    # 安装其他依赖
    pip install tushare akshare yfinance psutil
    
    # 验证关键库的加载
    echo ""
    echo "🧪 验证关键库..."
    python -c "
import platform
print(f'Python架构: {platform.machine()}')

try:
    import torch
    print(f'✅ PyTorch: {torch.__version__}')
    if torch.backends.mps.is_available():
        print('✅ MPS可用')
    else:
        print('⚠️ MPS不可用')
except Exception as e:
    print(f'❌ PyTorch错误: {e}')

try:
    import xgboost as xgb
    print(f'✅ XGBoost: {xgb.__version__}')
except Exception as e:
    print(f'❌ XGBoost错误: {e}')

try:
    import lightgbm as lgb
    print(f'✅ LightGBM: {lgb.__version__}')
except Exception as e:
    print(f'❌ LightGBM错误: {e}')

try:
    import numpy as np
    print(f'✅ NumPy: {np.__version__} (BLAS: {np.__config__.show()})')
except Exception as e:
    print(f'❌ NumPy错误: {e}')
"
)

echo ""
echo "✅ M2 Ultra环境修复完成！"
echo ""
echo "📋 使用新环境的步骤："
echo "1. conda activate tushare_m2"
echo "2. cd /Users/brunogao/IdeaProjects/tushare"
echo "3. python test_m2_ultra_performance.py"
echo ""
echo "🚀 现在您将获得真正的M2 Ultra性能！"