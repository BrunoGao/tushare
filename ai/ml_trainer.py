"""
AI股票分析模型训练器
使用机器学习算法训练股票预测模型
"""
import pandas as pd
import numpy as np
import logging
import joblib
import json
from typing import Dict, List, Tuple, Optional, Any
from datetime import datetime, timedelta
from sklearn.ensemble import RandomForestClassifier, GradientBoostingClassifier
from sklearn.linear_model import LogisticRegression
from sklearn.svm import SVC
from sklearn.neural_network import MLPClassifier
from sklearn.preprocessing import StandardScaler, LabelEncoder
from sklearn.model_selection import train_test_split, GridSearchCV, cross_val_score
from sklearn.metrics import classification_report, accuracy_score, precision_score, recall_score, f1_score
import sys, os

# 可选依赖 - 动态导入以避免架构问题
XGBOOST_AVAILABLE = False
LIGHTGBM_AVAILABLE = False

def check_xgboost():
    global XGBOOST_AVAILABLE
    try:
        import xgboost as xgb
        # 尝试创建一个简单实例来测试是否真的可用
        test = xgb.XGBClassifier(n_estimators=1)
        XGBOOST_AVAILABLE = True
        return xgb
    except Exception as e:
        print(f"⚠️ XGBoost不可用: {e}")
        return None

def check_lightgbm():
    global LIGHTGBM_AVAILABLE
    try:
        import lightgbm as lgb
        # 尝试创建一个简单实例来测试是否真的可用
        test = lgb.LGBMClassifier(n_estimators=1, verbose=-1)
        LIGHTGBM_AVAILABLE = True
        return lgb
    except Exception as e:
        print(f"⚠️ LightGBM不可用: {e}")
        return None

# 移除 TA-Lib 依赖检查，已在 utils/technical_indicators.py 中处理
print("✅ 使用内置技术指标计算器")

# 添加项目路径
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from database.db_manager import DatabaseManager
from llm.feature_engineering import feature_engineer

# 日志配置
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class StockMLTrainer:
    """股票机器学习模型训练器"""
    
    def __init__(self):
        self.db_manager = DatabaseManager()
        self.feature_engineer = feature_engineer
        self.models = {}
        self.scalers = {}
        self.label_encoders = {}
        self.feature_names = []
        self.model_performance = {}
        
        # 支持的模型 - 根据可用依赖动态构建
        self.available_models = {
            'random_forest': RandomForestClassifier(n_estimators=100, random_state=42),
            'gradient_boosting': GradientBoostingClassifier(n_estimators=100, random_state=42),
            'logistic_regression': LogisticRegression(random_state=42, max_iter=1000),
            'svm': SVC(random_state=42, probability=True),
            'neural_network': MLPClassifier(hidden_layer_sizes=(100, 50), random_state=42, max_iter=500)
        }
        
        # 只有在XGBoost可用时才添加
        xgb = check_xgboost()
        if xgb is not None:
            self.available_models['xgboost'] = xgb.XGBClassifier(n_estimators=100, random_state=42)
        
        # 只有在LightGBM可用时才添加  
        lgb = check_lightgbm()
        if lgb is not None:
            self.available_models['lightgbm'] = lgb.LGBMClassifier(n_estimators=100, random_state=42, verbose=-1)
        
        logger.info(f"✅ ML训练器初始化完成，可用模型: {list(self.available_models.keys())}")
    
    def prepare_training_data(self, days_back: int = 180, stock_limit: int = 500) -> Tuple[pd.DataFrame, np.ndarray, np.ndarray]:
        """准备训练数据"""
        logger.info(f"🔄 开始准备训练数据: {days_back}天, {stock_limit}只股票")
        
        # 获取训练股票列表
        stock_list = self.feature_engineer.get_stock_list_for_training(stock_limit)
        
        if not stock_list:
            raise ValueError("无法获取训练股票列表")
        
        # 设置日期范围
        end_date = datetime.now().strftime('%Y%m%d')
        start_date = (datetime.now() - timedelta(days=days_back)).strftime('%Y%m%d')
        
        # 预处理数据
        training_data, selected_features = self.feature_engineer.preprocess_training_data(
            stock_list, start_date, end_date
        )
        
        if training_data.empty:
            raise ValueError("训练数据为空")
        
        self.feature_names = selected_features
        
        # 准备特征和目标变量
        X = training_data[selected_features].fillna(0)
        y = training_data['price_direction_5d'].fillna(0)
        
        # 移除缺失目标变量的行
        valid_mask = ~y.isnull()
        X = X[valid_mask]
        y = y[valid_mask]
        
        # 标准化特征
        self.scalers['feature_scaler'] = StandardScaler()
        X_scaled = self.scalers['feature_scaler'].fit_transform(X)
        
        # 编码目标变量 (-1, 0, 1) -> (0, 1, 2)
        self.label_encoders['target'] = LabelEncoder()
        y_encoded = self.label_encoders['target'].fit_transform(y)
        
        logger.info(f"✅ 训练数据准备完成: {X_scaled.shape[0]}条记录, {X_scaled.shape[1]}个特征")
        logger.info(f"目标变量分布: {np.bincount(y_encoded)}")
        
        return training_data, X_scaled, y_encoded
    
    def train_single_model(self, model_name: str, X_train: np.ndarray, y_train: np.ndarray, 
                          X_test: np.ndarray, y_test: np.ndarray) -> Dict[str, Any]:
        """训练单个模型"""
        if model_name not in self.available_models:
            raise ValueError(f"不支持的模型: {model_name}")
        
        logger.info(f"🔧 开始训练模型: {model_name}")
        
        model = self.available_models[model_name]
        
        # 训练模型
        start_time = datetime.now()
        model.fit(X_train, y_train)
        training_time = (datetime.now() - start_time).total_seconds()
        
        # 预测
        y_pred = model.predict(X_test)
        y_prob = model.predict_proba(X_test) if hasattr(model, 'predict_proba') else None
        
        # 评估性能
        accuracy = accuracy_score(y_test, y_pred)
        precision = precision_score(y_test, y_pred, average='weighted')
        recall = recall_score(y_test, y_pred, average='weighted')
        f1 = f1_score(y_test, y_pred, average='weighted')
        
        # 交叉验证
        cv_scores = cross_val_score(model, X_train, y_train, cv=5, scoring='accuracy')
        
        performance = {
            'model_name': model_name,
            'accuracy': accuracy,
            'precision': precision,
            'recall': recall,
            'f1_score': f1,
            'cv_mean': cv_scores.mean(),
            'cv_std': cv_scores.std(),
            'training_time': training_time,
            'training_samples': len(X_train),
            'test_samples': len(X_test),
            'feature_count': X_train.shape[1]
        }
        
        # 保存模型
        self.models[model_name] = model
        self.model_performance[model_name] = performance
        
        logger.info(f"✅ 模型训练完成: {model_name}, 准确率: {accuracy:.4f}")
        
        return performance
    
    def train_all_models(self, X: np.ndarray, y: np.ndarray, test_size: float = 0.2) -> Dict[str, Dict[str, Any]]:
        """训练所有模型"""
        logger.info("🚀 开始训练所有模型...")
        
        # 分割数据
        X_train, X_test, y_train, y_test = train_test_split(
            X, y, test_size=test_size, random_state=42, stratify=y
        )
        
        all_performance = {}
        
        for model_name in self.available_models.keys():
            try:
                performance = self.train_single_model(
                    model_name, X_train, y_train, X_test, y_test
                )
                all_performance[model_name] = performance
            except Exception as e:
                logger.error(f"训练模型{model_name}失败: {e}")
                continue
        
        # 找到最佳模型
        if all_performance:
            best_model = max(all_performance.keys(), key=lambda x: all_performance[x]['accuracy'])
            logger.info(f"🏆 最佳模型: {best_model}, 准确率: {all_performance[best_model]['accuracy']:.4f}")
        
        return all_performance
    
    def hyperparameter_tuning(self, model_name: str, X: np.ndarray, y: np.ndarray) -> Dict[str, Any]:
        """超参数调优"""
        if model_name not in self.available_models:
            raise ValueError(f"不支持的模型: {model_name}")
        
        logger.info(f"🔍 开始超参数调优: {model_name}")
        
        # 定义参数网格 - 根据可用模型动态构建
        param_grids = {
            'random_forest': {
                'n_estimators': [50, 100, 200],
                'max_depth': [10, 20, None],
                'min_samples_split': [2, 5, 10]
            },
            'gradient_boosting': {
                'n_estimators': [50, 100, 200],
                'learning_rate': [0.01, 0.1, 0.2],
                'max_depth': [3, 5, 7]
            },
            'logistic_regression': {
                'C': [0.1, 1, 10],
                'penalty': ['l1', 'l2'],
                'solver': ['liblinear', 'saga']
            }
        }
        
        # 只有在XGBoost可用时才添加参数网格
        if XGBOOST_AVAILABLE:
            param_grids['xgboost'] = {
                'n_estimators': [50, 100, 200],
                'learning_rate': [0.01, 0.1, 0.2],
                'max_depth': [3, 5, 7]
            }
        
        # 只有在LightGBM可用时才添加参数网格
        if LIGHTGBM_AVAILABLE:
            param_grids['lightgbm'] = {
                'n_estimators': [50, 100, 200],
                'learning_rate': [0.01, 0.1, 0.2], 
                'max_depth': [3, 5, 7],
                'num_leaves': [31, 50, 100]
            }
        
        if model_name not in param_grids:
            logger.warning(f"模型{model_name}没有定义参数网格，跳过调优")
            return {}
        
        # 网格搜索
        model = self.available_models[model_name]
        grid_search = GridSearchCV(
            model, param_grids[model_name], 
            cv=5, scoring='accuracy', n_jobs=-1
        )
        
        grid_search.fit(X, y)
        
        # 更新模型
        self.models[model_name] = grid_search.best_estimator_
        
        result = {
            'best_params': grid_search.best_params_,
            'best_score': grid_search.best_score_,
            'cv_results': grid_search.cv_results_
        }
        
        logger.info(f"✅ 超参数调优完成: {model_name}, 最佳得分: {result['best_score']:.4f}")
        
        return result
    
    def get_feature_importance(self, model_name: str) -> List[Dict[str, Any]]:
        """获取特征重要性"""
        if model_name not in self.models:
            return []
        
        model = self.models[model_name]
        
        if hasattr(model, 'feature_importances_'):
            importances = model.feature_importances_
        elif hasattr(model, 'coef_'):
            importances = np.abs(model.coef_[0])
        else:
            return []
        
        feature_importance = []
        for i, importance in enumerate(importances):
            if i < len(self.feature_names):
                feature_importance.append({
                    'feature': self.feature_names[i],
                    'importance': float(importance)
                })
        
        # 按重要性排序
        feature_importance.sort(key=lambda x: x['importance'], reverse=True)
        
        return feature_importance
    
    def predict_single_stock(self, ts_code: str, model_name: str = None) -> Dict[str, Any]:
        """预测单只股票"""
        if not self.models:
            raise ValueError("没有训练好的模型")
        
        # 选择模型
        if model_name is None:
            model_name = max(self.model_performance.keys(), 
                           key=lambda x: self.model_performance[x]['accuracy'])
        
        if model_name not in self.models:
            raise ValueError(f"模型{model_name}不存在")
        
        # 获取股票数据
        end_date = datetime.now().strftime('%Y%m%d')
        start_date = (datetime.now() - timedelta(days=90)).strftime('%Y%m%d')
        
        df = self.feature_engineer.fetch_raw_stock_data(ts_code, start_date, end_date)
        if df.empty:
            raise ValueError(f"无法获取股票{ts_code}的数据")
        
        # 特征工程
        df = self.feature_engineer.calculate_technical_features(df)
        df = self.feature_engineer.calculate_fundamental_features(df)
        df = self.feature_engineer.calculate_market_features(df)
        
        # 获取最新数据
        latest_data = df.iloc[-1:][self.feature_names].fillna(0)
        
        # 标准化
        if 'feature_scaler' in self.scalers:
            X_scaled = self.scalers['feature_scaler'].transform(latest_data)
        else:
            X_scaled = latest_data.values
        
        # 预测
        model = self.models[model_name]
        prediction = model.predict(X_scaled)[0]
        probability = model.predict_proba(X_scaled)[0] if hasattr(model, 'predict_proba') else None
        
        # 解码预测结果
        if 'target' in self.label_encoders:
            prediction_label = self.label_encoders['target'].inverse_transform([prediction])[0]
        else:
            prediction_label = prediction
        
        result = {
            'ts_code': ts_code,
            'model_name': model_name,
            'prediction': int(prediction_label),
            'prediction_text': self._prediction_to_text(prediction_label),
            'probability': probability.tolist() if probability is not None else None,
            'confidence': float(np.max(probability)) if probability is not None else 0.5,
            'timestamp': datetime.now().isoformat()
        }
        
        return result
    
    def _prediction_to_text(self, prediction: int) -> str:
        """将预测结果转换为文本"""
        mapping = {
            -1: "卖出",
            0: "持有", 
            1: "买入"
        }
        return mapping.get(prediction, "未知")
    
    def save_models(self, model_dir: str = "models") -> bool:
        """保存模型"""
        try:
            os.makedirs(model_dir, exist_ok=True)
            
            # 保存模型
            for model_name, model in self.models.items():
                model_path = os.path.join(model_dir, f"{model_name}.joblib")
                joblib.dump(model, model_path)
            
            # 保存预处理器
            preprocessor_path = os.path.join(model_dir, "preprocessors.joblib")
            preprocessors = {
                'scalers': self.scalers,
                'label_encoders': self.label_encoders,
                'feature_names': self.feature_names
            }
            joblib.dump(preprocessors, preprocessor_path)
            
            # 保存性能指标
            performance_path = os.path.join(model_dir, "performance.json")
            with open(performance_path, 'w', encoding='utf-8') as f:
                json.dump(self.model_performance, f, ensure_ascii=False, indent=2)
            
            logger.info(f"✅ 模型已保存到: {model_dir}")
            return True
            
        except Exception as e:
            logger.error(f"保存模型失败: {e}")
            return False
    
    def load_models(self, model_dir: str = "models") -> bool:
        """加载模型"""
        try:
            if not os.path.exists(model_dir):
                logger.warning(f"模型目录不存在: {model_dir}")
                return False
            
            # 加载预处理器
            preprocessor_path = os.path.join(model_dir, "preprocessors.joblib")
            if os.path.exists(preprocessor_path):
                preprocessors = joblib.load(preprocessor_path)
                self.scalers = preprocessors.get('scalers', {})
                self.label_encoders = preprocessors.get('label_encoders', {})
                self.feature_names = preprocessors.get('feature_names', [])
            
            # 加载模型
            for model_name in self.available_models.keys():
                model_path = os.path.join(model_dir, f"{model_name}.joblib")
                if os.path.exists(model_path):
                    self.models[model_name] = joblib.load(model_path)
            
            # 加载性能指标
            performance_path = os.path.join(model_dir, "performance.json")
            if os.path.exists(performance_path):
                with open(performance_path, 'r', encoding='utf-8') as f:
                    self.model_performance = json.load(f)
            
            logger.info(f"✅ 从{model_dir}加载了{len(self.models)}个模型")
            return True
            
        except Exception as e:
            logger.error(f"加载模型失败: {e}")
            return False

# 全局实例
ml_trainer = StockMLTrainer()

def main():
    """主函数 - 用于训练和测试"""
    try:
        logger.info("🚀 开始AI股票分析模型训练...")
        
        # 准备训练数据
        training_data, X, y = ml_trainer.prepare_training_data(days_back=180, stock_limit=100)
        
        # 训练所有模型
        performance = ml_trainer.train_all_models(X, y)
        
        # 显示结果
        print("\n📊 模型性能对比:")
        print("=" * 80)
        for model_name, perf in performance.items():
            print(f"{model_name:20} | 准确率: {perf['accuracy']:.4f} | F1: {perf['f1_score']:.4f} | CV: {perf['cv_mean']:.4f}±{perf['cv_std']:.4f}")
        
        # 保存模型
        success = ml_trainer.save_models()
        if success:
            print("\n✅ 模型训练完成并已保存!")
        
        # 测试预测
        test_stock = "000001.SZ"
        try:
            prediction = ml_trainer.predict_single_stock(test_stock)
            print(f"\n🎯 测试预测 {test_stock}:")
            print(f"预测结果: {prediction['prediction_text']}")
            print(f"置信度: {prediction['confidence']:.4f}")
        except Exception as e:
            print(f"测试预测失败: {e}")
        
    except Exception as e:
        logger.error(f"主程序运行失败: {e}")

if __name__ == "__main__":
    main()