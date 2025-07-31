"""
AI模块健康检查工具
检查AI相关依赖和模块状态
"""
import sys
import os
import importlib
from typing import Dict, Any

def check_ai_dependencies() -> Dict[str, Any]:
    """检查AI相关依赖"""
    dependencies = {
        'pandas': False,
        'numpy': False,
        'sklearn': False,
        'xgboost': False,
        'lightgbm': False,
        'joblib': False,
        'ollama': False
    }
    
    # 检查每个依赖
    for dep in dependencies.keys():
        try:
            if dep == 'sklearn':
                importlib.import_module('sklearn.ensemble')
            elif dep in ['xgboost', 'lightgbm']:
                # 这些库可能有架构问题，需要特殊处理
                try:
                    module = importlib.import_module(dep)
                    # 尝试创建一个简单实例来测试是否真的可用
                    if dep == 'xgboost':
                        module.XGBClassifier(n_estimators=1)
                    elif dep == 'lightgbm':
                        module.LGBMClassifier(n_estimators=1, verbose=-1)
                except Exception:
                    # 导入成功但实例化失败，标记为不可用
                    raise ImportError(f"{dep} 架构不兼容")
            else:
                importlib.import_module(dep)
            dependencies[dep] = True
        except (ImportError, Exception) as e:
            print(f"依赖 {dep} 不可用: {e}")
            dependencies[dep] = False
    
    # 计算可用性
    available_count = sum(dependencies.values())
    total_count = len(dependencies)
    availability = available_count / total_count
    
    return {
        'dependencies': dependencies,
        'availability': availability,
        'available_count': available_count,
        'total_count': total_count,
        'status': 'healthy' if availability >= 0.7 else 'warning' if availability >= 0.5 else 'error'
    }

def check_ai_modules() -> Dict[str, Any]:
    """检查AI模块状态"""
    modules = {
        'ml_trainer': False,
        'feature_engineering': False,
        'intelligent_recommender': False,
        'ollama_analyzer': False
    }
    
    # 添加项目路径
    project_root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    if project_root not in sys.path:
        sys.path.insert(0, project_root)
    
    # 检查每个模块
    for module_name in modules.keys():
        try:
            if module_name == 'ml_trainer':
                from ai.ml_trainer import ml_trainer
                modules[module_name] = True
            elif module_name == 'feature_engineering':
                from llm.feature_engineering import feature_engineer
                modules[module_name] = True
            elif module_name == 'intelligent_recommender':
                from llm.intelligent_recommender import intelligent_recommender
                modules[module_name] = True
            elif module_name == 'ollama_analyzer':
                from llm.ollama_analyzer import ollama_analyzer
                modules[module_name] = True
        except Exception as e:
            print(f"模块 {module_name} 加载失败: {e}")
            modules[module_name] = False
    
    # 计算可用性
    available_count = sum(modules.values())
    total_count = len(modules)
    availability = available_count / total_count
    
    return {
        'modules': modules,
        'availability': availability,
        'available_count': available_count,
        'total_count': total_count,
        'status': 'healthy' if availability >= 0.75 else 'warning' if availability >= 0.5 else 'error'
    }

def get_ai_system_health() -> Dict[str, Any]:
    """获取AI系统整体健康状态"""
    deps_health = check_ai_dependencies()
    modules_health = check_ai_modules()
    
    # 计算整体状态
    overall_availability = (deps_health['availability'] + modules_health['availability']) / 2
    
    if overall_availability >= 0.8:
        overall_status = 'healthy'
    elif overall_availability >= 0.5:
        overall_status = 'warning'
    else:
        overall_status = 'error'
    
    return {
        'overall_status': overall_status,
        'overall_availability': overall_availability,
        'dependencies': deps_health,
        'modules': modules_health,
        'recommendations': _get_recommendations(deps_health, modules_health)
    }

def _get_recommendations(deps_health: Dict, modules_health: Dict) -> list:
    """获取修复建议"""
    recommendations = []
    
    # 检查缺失的依赖
    missing_deps = [dep for dep, available in deps_health['dependencies'].items() if not available]
    if missing_deps:
        recommendations.append(f"安装缺失的依赖包: pip install {' '.join(missing_deps)}")
    
    # 检查缺失的模块
    missing_modules = [mod for mod, available in modules_health['modules'].items() if not available]
    if missing_modules:
        recommendations.append(f"修复模块导入错误: {', '.join(missing_modules)}")
    
    if not recommendations:
        recommendations.append("系统运行正常，无需修复")
    
    return recommendations

if __name__ == "__main__":
    # 运行健康检查
    health = get_ai_system_health()
    
    print("🔍 AI系统健康检查报告")
    print("=" * 50)
    print(f"整体状态: {health['overall_status']}")
    print(f"可用性: {health['overall_availability']:.1%}")
    
    print(f"\n📦 依赖包状态:")
    for dep, status in health['dependencies']['dependencies'].items():
        print(f"  {dep}: {'✅' if status else '❌'}")
    
    print(f"\n🧩 模块状态:")
    for mod, status in health['modules']['modules'].items():
        print(f"  {mod}: {'✅' if status else '❌'}")
    
    print(f"\n💡 修复建议:")
    for rec in health['recommendations']:
        print(f"  - {rec}")