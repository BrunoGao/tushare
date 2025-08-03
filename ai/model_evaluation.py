"""
模型评估和反馈系统
用于跟踪模型性能、收集用户反馈并持续改进模型
"""
import pandas as pd
import numpy as np
import logging
import json
import os
from typing import Dict, List, Tuple, Optional, Any, Union
from datetime import datetime, timedelta
from sklearn.metrics import accuracy_score, precision_score, recall_score, f1_score, confusion_matrix
import sqlite3
import sys

sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from database.db_manager import DatabaseManager

# 日志配置
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class ModelEvaluationSystem:
    """模型评估系统"""
    
    def __init__(self):
        self.db_manager = DatabaseManager()
        self.feedback_db_path = "models/feedback.db"
        self.performance_history = {}
        self._init_feedback_database()
        
    def _init_feedback_database(self):
        """初始化反馈数据库"""
        try:
            os.makedirs(os.path.dirname(self.feedback_db_path), exist_ok=True)
            
            with sqlite3.connect(self.feedback_db_path) as conn:
                cursor = conn.cursor()
                
                # 预测记录表
                cursor.execute("""
                    CREATE TABLE IF NOT EXISTS prediction_records (
                        id INTEGER PRIMARY KEY AUTOINCREMENT,
                        timestamp DATETIME DEFAULT CURRENT_TIMESTAMP,
                        ts_code VARCHAR(20) NOT NULL,
                        model_name VARCHAR(50) NOT NULL,
                        prediction INTEGER NOT NULL,
                        confidence REAL NOT NULL,
                        actual_result INTEGER DEFAULT NULL,
                        actual_price_change REAL DEFAULT NULL,
                        evaluation_date DATETIME DEFAULT NULL,
                        is_correct BOOLEAN DEFAULT NULL,
                        metadata TEXT DEFAULT NULL
                    )
                """)
                
                # 用户反馈表
                cursor.execute("""
                    CREATE TABLE IF NOT EXISTS user_feedback (
                        id INTEGER PRIMARY KEY AUTOINCREMENT,
                        timestamp DATETIME DEFAULT CURRENT_TIMESTAMP,
                        prediction_id INTEGER,
                        user_id VARCHAR(50) DEFAULT NULL,
                        feedback_type VARCHAR(20) NOT NULL,
                        feedback_value TEXT NOT NULL,
                        comments TEXT DEFAULT NULL,
                        FOREIGN KEY (prediction_id) REFERENCES prediction_records (id)
                    )
                """)
                
                # 模型性能历史表
                cursor.execute("""
                    CREATE TABLE IF NOT EXISTS model_performance_history (
                        id INTEGER PRIMARY KEY AUTOINCREMENT,
                        timestamp DATETIME DEFAULT CURRENT_TIMESTAMP,
                        model_name VARCHAR(50) NOT NULL,
                        evaluation_period_start DATETIME NOT NULL,
                        evaluation_period_end DATETIME NOT NULL,
                        total_predictions INTEGER NOT NULL,
                        correct_predictions INTEGER NOT NULL,
                        accuracy REAL NOT NULL,
                        precision_score REAL DEFAULT NULL,
                        recall_score REAL DEFAULT NULL,
                        f1_score REAL DEFAULT NULL,
                        avg_confidence REAL DEFAULT NULL,
                        metadata TEXT DEFAULT NULL
                    )
                """)
                
                # 创建索引
                cursor.execute("CREATE INDEX IF NOT EXISTS idx_prediction_ts_code ON prediction_records(ts_code)")
                cursor.execute("CREATE INDEX IF NOT EXISTS idx_prediction_model ON prediction_records(model_name)")
                cursor.execute("CREATE INDEX IF NOT EXISTS idx_prediction_timestamp ON prediction_records(timestamp)")
                
                conn.commit()
                logger.info("✅ 反馈数据库初始化完成")
                
        except Exception as e:
            logger.error(f"初始化反馈数据库失败: {e}")
    
    def record_prediction(self, ts_code: str, model_name: str, prediction: int, 
                         confidence: float, metadata: Dict[str, Any] = None) -> int:
        """记录预测结果"""
        try:
            with sqlite3.connect(self.feedback_db_path) as conn:
                cursor = conn.cursor()
                
                cursor.execute("""
                    INSERT INTO prediction_records 
                    (ts_code, model_name, prediction, confidence, metadata)
                    VALUES (?, ?, ?, ?, ?)
                """, (
                    ts_code, model_name, prediction, confidence, 
                    json.dumps(metadata) if metadata else None
                ))
                
                prediction_id = cursor.lastrowid
                conn.commit()
                
                logger.debug(f"记录预测: {ts_code} - {model_name} - {prediction} (ID: {prediction_id})")
                return prediction_id
                
        except Exception as e:
            logger.error(f"记录预测失败: {e}")
            return -1
    
    def evaluate_predictions(self, evaluation_days: int = 5) -> Dict[str, Any]:
        """评估预测结果"""
        try:
            # 获取需要评估的预测（evaluation_date为空且预测时间超过evaluation_days天）
            cutoff_date = datetime.now() - timedelta(days=evaluation_days)
            
            with sqlite3.connect(self.feedback_db_path) as conn:
                cursor = conn.cursor()
                
                cursor.execute("""
                    SELECT id, ts_code, model_name, prediction, confidence, timestamp
                    FROM prediction_records
                    WHERE evaluation_date IS NULL 
                    AND timestamp < ?
                    ORDER BY timestamp DESC
                    LIMIT 1000
                """, (cutoff_date,))
                
                predictions_to_evaluate = cursor.fetchall()
            
            if not predictions_to_evaluate:
                logger.info("没有需要评估的预测记录")
                return {"message": "没有需要评估的预测记录"}
            
            logger.info(f"开始评估{len(predictions_to_evaluate)}条预测记录")
            
            evaluated_count = 0
            results_by_model = {}
            
            for pred_id, ts_code, model_name, prediction, confidence, pred_timestamp in predictions_to_evaluate:
                try:
                    # 获取实际结果
                    actual_result = self._get_actual_result(ts_code, pred_timestamp, evaluation_days)
                    
                    if actual_result is not None:
                        actual_direction, price_change = actual_result
                        is_correct = (prediction == actual_direction)
                        
                        # 更新数据库
                        with sqlite3.connect(self.feedback_db_path) as conn:
                            cursor = conn.cursor()
                            cursor.execute("""
                                UPDATE prediction_records
                                SET actual_result = ?, actual_price_change = ?, 
                                    is_correct = ?, evaluation_date = ?
                                WHERE id = ?
                            """, (actual_direction, price_change, is_correct, datetime.now(), pred_id))
                            conn.commit()
                        
                        # 统计
                        if model_name not in results_by_model:
                            results_by_model[model_name] = {
                                'total': 0,
                                'correct': 0,
                                'predictions': [],
                                'confidences': []
                            }
                        
                        results_by_model[model_name]['total'] += 1
                        if is_correct:
                            results_by_model[model_name]['correct'] += 1
                        
                        results_by_model[model_name]['predictions'].append({
                            'prediction': prediction,
                            'actual': actual_direction,
                            'confidence': confidence,
                            'is_correct': is_correct
                        })
                        results_by_model[model_name]['confidences'].append(confidence)
                        
                        evaluated_count += 1
                        
                except Exception as e:
                    logger.warning(f"评估预测{pred_id}失败: {e}")
                    continue
            
            # 计算各模型性能指标
            performance_summary = {}
            for model_name, results in results_by_model.items():
                if results['total'] > 0:
                    accuracy = results['correct'] / results['total']
                    avg_confidence = np.mean(results['confidences'])
                    
                    # 计算详细指标
                    y_true = [p['actual'] for p in results['predictions']]
                    y_pred = [p['prediction'] for p in results['predictions']]
                    
                    try:
                        precision = precision_score(y_true, y_pred, average='weighted', zero_division=0)
                        recall = recall_score(y_true, y_pred, average='weighted', zero_division=0)
                        f1 = f1_score(y_true, y_pred, average='weighted', zero_division=0)
                    except:
                        precision = recall = f1 = 0.0
                    
                    performance_summary[model_name] = {
                        'total_predictions': results['total'],
                        'correct_predictions': results['correct'],
                        'accuracy': accuracy,
                        'precision': precision,
                        'recall': recall,
                        'f1_score': f1,
                        'avg_confidence': avg_confidence
                    }
                    
                    # 保存性能历史
                    self._save_performance_history(
                        model_name, cutoff_date, datetime.now(),
                        results['total'], results['correct'], accuracy,
                        precision, recall, f1, avg_confidence
                    )
            
            logger.info(f"评估完成: {evaluated_count}条记录，{len(performance_summary)}个模型")
            
            return {
                'evaluated_count': evaluated_count,
                'evaluation_period': evaluation_days,
                'cutoff_date': cutoff_date.isoformat(),
                'performance_by_model': performance_summary
            }
            
        except Exception as e:
            logger.error(f"评估预测失败: {e}")
            return {'error': str(e)}
    
    def _get_actual_result(self, ts_code: str, pred_timestamp: str, days: int) -> Optional[Tuple[int, float]]:
        """获取实际结果"""
        try:
            # 解析预测时间
            if isinstance(pred_timestamp, str):
                pred_dt = datetime.fromisoformat(pred_timestamp.replace('Z', '+00:00').replace('+00:00', ''))
            else:
                pred_dt = pred_timestamp
            
            # 计算评估日期范围
            start_date = pred_dt.strftime('%Y%m%d')
            end_date = (pred_dt + timedelta(days=days + 5)).strftime('%Y%m%d')
            
            # 获取股票数据
            query = f"""
                SELECT trade_date, close, pct_chg
                FROM stock_daily_202507
                WHERE ts_code = '{ts_code}'
                  AND trade_date >= '{start_date}'
                  AND trade_date <= '{end_date}'
                ORDER BY trade_date
            """
            
            df = self.db_manager.fetch_data(query)
            
            if df.empty or len(df) < days:
                return None
            
            # 计算实际结果
            start_price = df.iloc[0]['close']
            end_price = df.iloc[min(days, len(df) - 1)]['close']
            price_change = (end_price - start_price) / start_price
            
            # 分类结果 (-1: 下跌, 0: 横盘, 1: 上涨)
            if price_change > 0.02:  # 上涨超过2%
                direction = 1
            elif price_change < -0.02:  # 下跌超过2%
                direction = -1
            else:  # 横盘
                direction = 0
            
            return direction, price_change
            
        except Exception as e:
            logger.warning(f"获取实际结果失败 {ts_code}: {e}")
            return None
    
    def _save_performance_history(self, model_name: str, period_start: datetime, 
                                 period_end: datetime, total: int, correct: int,
                                 accuracy: float, precision: float, recall: float,
                                 f1: float, avg_confidence: float):
        """保存性能历史"""
        try:
            with sqlite3.connect(self.feedback_db_path) as conn:
                cursor = conn.cursor()
                
                cursor.execute("""
                    INSERT INTO model_performance_history
                    (model_name, evaluation_period_start, evaluation_period_end,
                     total_predictions, correct_predictions, accuracy,
                     precision_score, recall_score, f1_score, avg_confidence)
                    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                """, (
                    model_name, period_start, period_end, total, correct,
                    accuracy, precision, recall, f1, avg_confidence
                ))
                
                conn.commit()
                
        except Exception as e:
            logger.error(f"保存性能历史失败: {e}")
    
    def add_user_feedback(self, prediction_id: int, feedback_type: str, 
                         feedback_value: str, user_id: str = None, 
                         comments: str = None) -> bool:
        """添加用户反馈"""
        try:
            with sqlite3.connect(self.feedback_db_path) as conn:
                cursor = conn.cursor()
                
                cursor.execute("""
                    INSERT INTO user_feedback
                    (prediction_id, user_id, feedback_type, feedback_value, comments)
                    VALUES (?, ?, ?, ?, ?)
                """, (prediction_id, user_id, feedback_type, feedback_value, comments))
                
                conn.commit()
                logger.info(f"用户反馈已记录: 预测ID {prediction_id}")
                return True
                
        except Exception as e:
            logger.error(f"添加用户反馈失败: {e}")
            return False
    
    def get_model_performance_report(self, model_name: str = None, 
                                   days_back: int = 30) -> Dict[str, Any]:
        """获取模型性能报告"""
        try:
            start_date = datetime.now() - timedelta(days=days_back)
            
            with sqlite3.connect(self.feedback_db_path) as conn:
                # 查询预测记录
                if model_name:
                    predictions_query = """
                        SELECT * FROM prediction_records
                        WHERE model_name = ? AND timestamp >= ?
                        ORDER BY timestamp DESC
                    """
                    params = (model_name, start_date)
                else:
                    predictions_query = """
                        SELECT * FROM prediction_records
                        WHERE timestamp >= ?
                        ORDER BY timestamp DESC
                    """
                    params = (start_date,)
                
                predictions_df = pd.read_sql_query(predictions_query, conn, params=params)
                
                # 查询性能历史
                if model_name:
                    performance_query = """
                        SELECT * FROM model_performance_history
                        WHERE model_name = ? AND timestamp >= ?
                        ORDER BY timestamp DESC
                    """
                    params = (model_name, start_date)
                else:
                    performance_query = """
                        SELECT * FROM model_performance_history
                        WHERE timestamp >= ?
                        ORDER BY timestamp DESC
                    """
                    params = (start_date,)
                
                performance_df = pd.read_sql_query(performance_query, conn, params=params)
                
                # 查询用户反馈
                feedback_query = """
                    SELECT uf.*, pr.model_name, pr.ts_code
                    FROM user_feedback uf
                    JOIN prediction_records pr ON uf.prediction_id = pr.id
                    WHERE uf.timestamp >= ?
                """
                if model_name:
                    feedback_query += " AND pr.model_name = ?"
                    params = (start_date, model_name)
                else:
                    params = (start_date,)
                
                feedback_df = pd.read_sql_query(feedback_query, conn, params=params)
            
            # 分析结果
            report = {
                'period': f"{start_date.strftime('%Y-%m-%d')} - {datetime.now().strftime('%Y-%m-%d')}",
                'model_name': model_name or 'ALL',
                'predictions_summary': {},
                'performance_trend': {},
                'user_feedback_summary': {},
                'recommendations': []
            }
            
            # 预测摘要
            if not predictions_df.empty:
                total_predictions = len(predictions_df)
                evaluated_predictions = predictions_df['is_correct'].notna().sum()
                
                if evaluated_predictions > 0:
                    accuracy = predictions_df['is_correct'].sum() / evaluated_predictions
                    avg_confidence = predictions_df['confidence'].mean()
                    
                    report['predictions_summary'] = {
                        'total_predictions': total_predictions,
                        'evaluated_predictions': evaluated_predictions,
                        'overall_accuracy': accuracy,
                        'avg_confidence': avg_confidence,
                        'by_model': {}
                    }
                    
                    # 按模型分组统计
                    for model in predictions_df['model_name'].unique():
                        model_data = predictions_df[predictions_df['model_name'] == model]
                        model_evaluated = model_data['is_correct'].notna().sum()
                        
                        if model_evaluated > 0:
                            model_accuracy = model_data['is_correct'].sum() / model_evaluated
                            model_confidence = model_data['confidence'].mean()
                            
                            report['predictions_summary']['by_model'][model] = {
                                'total': len(model_data),
                                'evaluated': model_evaluated,
                                'accuracy': model_accuracy,
                                'avg_confidence': model_confidence
                            }
            
            # 性能趋势
            if not performance_df.empty:
                performance_trend = []
                for _, row in performance_df.iterrows():
                    performance_trend.append({
                        'timestamp': row['timestamp'],
                        'model_name': row['model_name'],
                        'accuracy': row['accuracy'],
                        'total_predictions': row['total_predictions']
                    })
                
                report['performance_trend'] = performance_trend
            
            # 用户反馈摘要
            if not feedback_df.empty:
                feedback_summary = {
                    'total_feedback': len(feedback_df),
                    'by_type': feedback_df['feedback_type'].value_counts().to_dict(),
                    'by_model': feedback_df.groupby('model_name').size().to_dict()
                }
                
                report['user_feedback_summary'] = feedback_summary
            
            # 生成建议
            report['recommendations'] = self._generate_recommendations(report)
            
            return report
            
        except Exception as e:
            logger.error(f"获取性能报告失败: {e}")
            return {'error': str(e)}
    
    def _generate_recommendations(self, report: Dict[str, Any]) -> List[str]:
        """基于报告生成改进建议"""
        recommendations = []
        
        try:
            predictions_summary = report.get('predictions_summary', {})
            
            # 检查整体准确率
            overall_accuracy = predictions_summary.get('overall_accuracy', 0)
            if overall_accuracy < 0.6:
                recommendations.append("整体预测准确率偏低，建议重新训练模型或调整特征工程")
            
            # 检查各模型表现
            by_model = predictions_summary.get('by_model', {})
            best_model = None
            best_accuracy = 0
            
            for model_name, stats in by_model.items():
                accuracy = stats.get('accuracy', 0)
                if accuracy > best_accuracy:
                    best_accuracy = accuracy
                    best_model = model_name
                
                if accuracy < 0.5:
                    recommendations.append(f"模型 {model_name} 准确率过低 ({accuracy:.2%})，建议停用或重新训练")
                elif stats.get('avg_confidence', 0) < 0.6:
                    recommendations.append(f"模型 {model_name} 置信度偏低，建议检查模型训练质量")
            
            if best_model:
                recommendations.append(f"建议优先使用模型 {best_model} (准确率: {best_accuracy:.2%})")
            
            # 检查预测数量
            evaluated_predictions = predictions_summary.get('evaluated_predictions', 0)
            if evaluated_predictions < 50:
                recommendations.append("评估样本数量较少，建议积累更多预测数据以提高评估可靠性")
            
            # 检查用户反馈
            feedback_summary = report.get('user_feedback_summary', {})
            if feedback_summary.get('total_feedback', 0) < 10:
                recommendations.append("用户反馈数量较少，建议增加反馈收集机制")
            
        except Exception as e:
            logger.warning(f"生成建议失败: {e}")
            recommendations.append("建议定期检查模型性能并根据实际结果调整")
        
        return recommendations
    
    def get_feedback_statistics(self, days_back: int = 30) -> Dict[str, Any]:
        """获取反馈统计"""
        try:
            start_date = datetime.now() - timedelta(days=days_back)
            
            with sqlite3.connect(self.feedback_db_path) as conn:
                # 预测统计
                predictions_stats = pd.read_sql_query("""
                    SELECT 
                        model_name,
                        COUNT(*) as total_predictions,
                        SUM(CASE WHEN is_correct = 1 THEN 1 ELSE 0 END) as correct_predictions,
                        AVG(confidence) as avg_confidence,
                        COUNT(CASE WHEN is_correct IS NOT NULL THEN 1 END) as evaluated_predictions
                    FROM prediction_records
                    WHERE timestamp >= ?
                    GROUP BY model_name
                """, conn, params=(start_date,))
                
                # 用户反馈统计
                feedback_stats = pd.read_sql_query("""
                    SELECT 
                        feedback_type,
                        COUNT(*) as count
                    FROM user_feedback
                    WHERE timestamp >= ?
                    GROUP BY feedback_type
                """, conn, params=(start_date,))
                
                # 性能趋势
                performance_trend = pd.read_sql_query("""
                    SELECT 
                        model_name,
                        timestamp,
                        accuracy,
                        total_predictions
                    FROM model_performance_history
                    WHERE timestamp >= ?
                    ORDER BY timestamp DESC
                """, conn, params=(start_date,))
                
            return {
                'predictions_by_model': predictions_stats.to_dict('records') if not predictions_stats.empty else [],
                'feedback_by_type': feedback_stats.to_dict('records') if not feedback_stats.empty else [],
                'performance_trend': performance_trend.to_dict('records') if not performance_trend.empty else [],
                'period': f"{start_date.strftime('%Y-%m-%d')} - {datetime.now().strftime('%Y-%m-%d')}"
            }
            
        except Exception as e:
            logger.error(f"获取反馈统计失败: {e}")
            return {'error': str(e)}

# 全局实例
evaluation_system = ModelEvaluationSystem()

def main():
    """测试评估系统"""
    try:
        logger.info("开始测试模型评估系统...")
        
        # 测试记录预测
        pred_id = evaluation_system.record_prediction(
            ts_code="000001.SZ",
            model_name="test_model",
            prediction=1,
            confidence=0.75,
            metadata={"test": True}
        )
        
        print(f"测试预测记录ID: {pred_id}")
        
        # 测试用户反馈
        if pred_id > 0:
            feedback_success = evaluation_system.add_user_feedback(
                prediction_id=pred_id,
                feedback_type="accuracy",
                feedback_value="correct",
                user_id="test_user",
                comments="测试反馈"
            )
            print(f"用户反馈记录: {'成功' if feedback_success else '失败'}")
        
        # 获取反馈统计
        stats = evaluation_system.get_feedback_statistics(days_back=7)
        print(f"反馈统计: {json.dumps(stats, indent=2, ensure_ascii=False)}")
        
        # 测试评估（这个可能需要真实的历史数据）
        evaluation_result = evaluation_system.evaluate_predictions(evaluation_days=5)
        print(f"评估结果: {json.dumps(evaluation_result, indent=2, ensure_ascii=False)}")
        
        print("✅ 模型评估系统测试完成")
        
    except Exception as e:
        logger.error(f"测试失败: {e}")

if __name__ == "__main__":
    main()