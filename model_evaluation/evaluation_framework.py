#!/usr/bin/env python3
"""
模型评估框架
用于验证ljwx-stock模型的训练效果和性能
"""

import json
import sqlite3
import subprocess
import time
import logging
import os
import pandas as pd
import numpy as np
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Tuple, Any
from dataclasses import dataclass, asdict
import re
import threading
import queue

@dataclass
class TestCase:
    """测试用例"""
    id: str
    category: str  # 测试类别
    input_text: str  # 输入文本
    expected_keywords: List[str]  # 期望包含的关键词
    expected_sentiment: str  # 期望情感：positive/negative/neutral
    difficulty: str  # 难度：easy/medium/hard
    description: str  # 用例描述
    tags: List[str] = None

    def __post_init__(self):
        if self.tags is None:
            self.tags = []

@dataclass
class EvaluationResult:
    """评估结果"""
    test_case_id: str
    model_name: str
    input_text: str
    output_text: str
    response_time: float
    timestamp: str
    
    # 评估指标
    keyword_match_score: float  # 关键词匹配分数
    sentiment_match: bool  # 情感匹配
    relevance_score: float  # 相关性分数
    coherence_score: float  # 连贯性分数
    accuracy_score: float  # 准确性分数
    overall_score: float  # 总体分数
    
    # 元数据
    error_message: str = ""
    evaluation_notes: str = ""

@dataclass
class ModelPerformanceMetrics:
    """模型性能指标"""
    model_name: str
    evaluation_date: str
    
    # 基础指标
    total_tests: int
    passed_tests: int
    failed_tests: int
    success_rate: float
    
    # 性能指标
    avg_response_time: float
    min_response_time: float
    max_response_time: float
    
    # 质量指标
    avg_accuracy_score: float
    avg_relevance_score: float
    avg_coherence_score: float
    avg_overall_score: float
    
    # 分类指标
    category_scores: Dict[str, float]
    difficulty_scores: Dict[str, float]
    
    # 详细结果
    test_results: List[EvaluationResult]

class ModelEvaluator:
    """模型评估器"""
    
    def __init__(self, db_path: str = "data/model_evaluation.db"):
        self.db_path = db_path
        self.logger = logging.getLogger(__name__)
        os.makedirs(os.path.dirname(db_path), exist_ok=True)
        self.init_database()
        self.load_test_cases()
    
    def init_database(self):
        """初始化数据库"""
        with sqlite3.connect(self.db_path) as conn:
            # 测试用例表
            conn.execute('''
                CREATE TABLE IF NOT EXISTS test_cases (
                    id TEXT PRIMARY KEY,
                    category TEXT,
                    input_text TEXT,
                    expected_keywords TEXT,  -- JSON array
                    expected_sentiment TEXT,
                    difficulty TEXT,
                    description TEXT,
                    tags TEXT,  -- JSON array
                    created_at TEXT
                )
            ''')
            
            # 评估结果表
            conn.execute('''
                CREATE TABLE IF NOT EXISTS evaluation_results (
                    id TEXT PRIMARY KEY,
                    test_case_id TEXT,
                    model_name TEXT,
                    input_text TEXT,
                    output_text TEXT,
                    response_time REAL,
                    keyword_match_score REAL,
                    sentiment_match BOOLEAN,
                    relevance_score REAL,
                    coherence_score REAL,
                    accuracy_score REAL,
                    overall_score REAL,
                    error_message TEXT,
                    evaluation_notes TEXT,
                    timestamp TEXT,
                    FOREIGN KEY (test_case_id) REFERENCES test_cases (id)
                )
            ''')
            
            # 模型性能表
            conn.execute('''
                CREATE TABLE IF NOT EXISTS model_performance (
                    id TEXT PRIMARY KEY,
                    model_name TEXT,
                    evaluation_date TEXT,
                    performance_data TEXT,  -- JSON
                    created_at TEXT
                )
            ''')
            
            conn.commit()
    
    def load_test_cases(self):
        """加载默认测试用例"""
        default_test_cases = [
            # 技术分析类
            TestCase(
                id="tech_001",
                category="技术分析",
                input_text="平安银行(000001.SZ)当前价格12.50元，5日均线12.30，20日均线12.10，RSI为65，MACD为0.08，成交量放大，请分析投资机会",
                expected_keywords=["均线", "RSI", "MACD", "成交量", "投资", "分析"],
                expected_sentiment="positive",
                difficulty="medium",
                description="综合技术指标分析",
                tags=["技术分析", "多指标", "投资建议"]
            ),
            TestCase(
                id="tech_002",
                category="技术分析",
                input_text="贵州茅台(600519.SH)跌破20日均线，RSI为30，量能萎缩，如何操作？",
                expected_keywords=["跌破", "均线", "RSI", "量能", "操作建议"],
                expected_sentiment="negative",
                difficulty="medium",
                description="弱势股票分析",
                tags=["技术分析", "弱势", "操作指导"]
            ),
            
            # 风险评估类
            TestCase(
                id="risk_001",
                category="风险评估",
                input_text="某股票一天内涨停又跌停，波动率极大，投资风险如何？",
                expected_keywords=["涨停", "跌停", "波动率", "风险", "高风险"],
                expected_sentiment="negative",
                difficulty="easy",
                description="高波动风险识别",
                tags=["风险评估", "高波动", "风险警示"]
            ),
            TestCase(
                id="risk_002",
                category="风险评估",
                input_text="新股上市首日，没有历史数据，如何评估投资风险？",
                expected_keywords=["新股", "上市", "历史数据", "投资风险", "不确定性"],
                expected_sentiment="neutral",
                difficulty="hard",
                description="新股风险评估",
                tags=["风险评估", "新股", "数据缺失"]
            ),
            
            # 市场情绪类
            TestCase(
                id="sentiment_001",
                category="市场情绪",
                input_text="A股市场连续下跌，成交量创新低，投资者信心如何？",
                expected_keywords=["连续下跌", "成交量", "新低", "投资者信心", "悲观"],
                expected_sentiment="negative",
                difficulty="easy",
                description="熊市情绪分析",
                tags=["市场情绪", "熊市", "投资者信心"]
            ),
            TestCase(
                id="sentiment_003",
                category="市场情绪",
                input_text="科技股集体大涨，成交量放大，市场热度如何？",
                expected_keywords=["科技股", "大涨", "成交量", "放大", "市场热度", "乐观"],
                expected_sentiment="positive",
                difficulty="easy",
                description="牛市情绪分析",
                tags=["市场情绪", "牛市", "科技股"]
            ),
            
            # 投资策略类
            TestCase(
                id="strategy_001",
                category="投资策略",
                input_text="手中有10万资金，想投资银行股，如何配置？",
                expected_keywords=["10万", "资金", "银行股", "配置", "投资建议"],
                expected_sentiment="neutral",
                difficulty="medium",
                description="行业配置建议",
                tags=["投资策略", "行业配置", "银行股"]
            ),
            TestCase(
                id="strategy_002",
                category="投资策略",
                input_text="定投策略 vs 一次性投资，在震荡市场中哪种更好？",
                expected_keywords=["定投", "一次性投资", "震荡市场", "策略比较"],
                expected_sentiment="neutral",
                difficulty="hard",
                description="投资策略对比",
                tags=["投资策略", "定投", "策略对比"]
            ),
            
            # 基本面分析类
            TestCase(
                id="fundamental_001",
                category="基本面分析",
                input_text="某公司PE为50倍，ROE为15%，净利润增长30%，估值是否合理？",
                expected_keywords=["PE", "ROE", "净利润", "增长", "估值", "合理性"],
                expected_sentiment="neutral",
                difficulty="hard",
                description="估值分析",
                tags=["基本面分析", "估值", "财务指标"]
            ),
            
            # 异常情况类
            TestCase(
                id="edge_001",
                category="边界测试",
                input_text="股票代码不存在，请分析000000.XX",
                expected_keywords=["错误", "不存在", "无效", "代码"],
                expected_sentiment="neutral",
                difficulty="easy",
                description="无效股票代码处理",
                tags=["边界测试", "错误处理"]
            ),
            TestCase(
                id="edge_002",
                category="边界测试",
                input_text="",  # 空输入
                expected_keywords=["帮助", "问题", "投资", "股票"],
                expected_sentiment="neutral",
                difficulty="easy",
                description="空输入处理",
                tags=["边界测试", "空输入"]
            ),
        ]
        
        # 保存测试用例到数据库
        for test_case in default_test_cases:
            self.save_test_case(test_case)
    
    def save_test_case(self, test_case: TestCase):
        """保存测试用例"""
        with sqlite3.connect(self.db_path) as conn:
            conn.execute('''
                INSERT OR REPLACE INTO test_cases 
                (id, category, input_text, expected_keywords, expected_sentiment, 
                 difficulty, description, tags, created_at)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
            ''', (
                test_case.id,
                test_case.category,
                test_case.input_text,
                json.dumps(test_case.expected_keywords, ensure_ascii=False),
                test_case.expected_sentiment,
                test_case.difficulty,
                test_case.description,
                json.dumps(test_case.tags, ensure_ascii=False),
                datetime.now().isoformat()
            ))
            conn.commit()
    
    def get_test_cases(self, category: str = None, difficulty: str = None) -> List[TestCase]:
        """获取测试用例"""
        conditions = []
        params = []
        
        if category:
            conditions.append('category = ?')
            params.append(category)
        
        if difficulty:
            conditions.append('difficulty = ?')
            params.append(difficulty)
        
        where_clause = ' AND '.join(conditions) if conditions else '1=1'
        
        with sqlite3.connect(self.db_path) as conn:
            conn.row_factory = sqlite3.Row
            cursor = conn.execute(f'''
                SELECT * FROM test_cases WHERE {where_clause} ORDER BY id
            ''', params)
            
            test_cases = []
            for row in cursor.fetchall():
                test_case = TestCase(
                    id=row['id'],
                    category=row['category'],
                    input_text=row['input_text'],
                    expected_keywords=json.loads(row['expected_keywords']),
                    expected_sentiment=row['expected_sentiment'],
                    difficulty=row['difficulty'],
                    description=row['description'],
                    tags=json.loads(row['tags'])
                )
                test_cases.append(test_case)
            
            return test_cases
    
    def evaluate_model(self, model_name: str, test_cases: List[TestCase] = None, 
                      timeout: int = 30) -> ModelPerformanceMetrics:
        """评估模型性能"""
        if test_cases is None:
            test_cases = self.get_test_cases()
        
        self.logger.info(f"开始评估模型: {model_name}, 测试用例数量: {len(test_cases)}")
        
        results = []
        start_time = time.time()
        
        for i, test_case in enumerate(test_cases):
            self.logger.info(f"执行测试用例 {i+1}/{len(test_cases)}: {test_case.id}")
            
            try:
                result = self.run_single_test(model_name, test_case, timeout)
                results.append(result)
                
                # 保存单个结果
                self.save_evaluation_result(result)
                
            except Exception as e:
                self.logger.error(f"测试用例 {test_case.id} 执行失败: {e}")
                
                # 创建失败结果
                error_result = EvaluationResult(
                    test_case_id=test_case.id,
                    model_name=model_name,
                    input_text=test_case.input_text,
                    output_text="",
                    response_time=0.0,
                    timestamp=datetime.now().isoformat(),
                    keyword_match_score=0.0,
                    sentiment_match=False,
                    relevance_score=0.0,
                    coherence_score=0.0,
                    accuracy_score=0.0,
                    overall_score=0.0,
                    error_message=str(e)
                )
                results.append(error_result)
                self.save_evaluation_result(error_result)
        
        # 计算整体性能指标
        metrics = self.calculate_performance_metrics(model_name, results)
        
        # 保存性能指标
        self.save_performance_metrics(metrics)
        
        total_time = time.time() - start_time
        self.logger.info(f"模型评估完成，用时: {total_time:.2f}秒")
        
        return metrics
    
    def run_single_test(self, model_name: str, test_case: TestCase, timeout: int) -> EvaluationResult:
        """运行单个测试用例"""
        start_time = time.time()
        
        try:
            # 运行ollama模型
            result = subprocess.run(
                ['ollama', 'run', model_name],
                input=test_case.input_text,
                capture_output=True,
                text=True,
                timeout=timeout,
                encoding='utf-8'
            )
            
            response_time = time.time() - start_time
            
            if result.returncode != 0:
                raise Exception(f"模型执行失败: {result.stderr}")
            
            output_text = result.stdout.strip()
            
            # 评估输出质量
            scores = self.evaluate_output_quality(test_case, output_text)
            
            return EvaluationResult(
                test_case_id=test_case.id,
                model_name=model_name,
                input_text=test_case.input_text,
                output_text=output_text,
                response_time=response_time,
                timestamp=datetime.now().isoformat(),
                **scores
            )
            
        except subprocess.TimeoutExpired:
            response_time = time.time() - start_time
            raise Exception(f"模型响应超时 ({timeout}秒)")
        
        except Exception as e:
            response_time = time.time() - start_time
            raise Exception(f"测试执行异常: {str(e)}")
    
    def evaluate_output_quality(self, test_case: TestCase, output_text: str) -> Dict[str, Any]:
        """评估输出质量"""
        scores = {}
        
        # 1. 关键词匹配分数
        matched_keywords = 0
        for keyword in test_case.expected_keywords:
            if keyword.lower() in output_text.lower():
                matched_keywords += 1
        
        scores['keyword_match_score'] = matched_keywords / len(test_case.expected_keywords) if test_case.expected_keywords else 0.0
        
        # 2. 情感匹配
        scores['sentiment_match'] = self.check_sentiment_match(test_case.expected_sentiment, output_text)
        
        # 3. 相关性分数 (基于关键词和长度)
        scores['relevance_score'] = self.calculate_relevance_score(test_case, output_text)
        
        # 4. 连贯性分数 (基于文本结构)
        scores['coherence_score'] = self.calculate_coherence_score(output_text)
        
        # 5. 准确性分数 (基于回答质量)
        scores['accuracy_score'] = self.calculate_accuracy_score(test_case, output_text)
        
        # 6. 总体分数 (加权平均)
        scores['overall_score'] = (
            scores['keyword_match_score'] * 0.25 +
            (1.0 if scores['sentiment_match'] else 0.0) * 0.20 +
            scores['relevance_score'] * 0.20 +
            scores['coherence_score'] * 0.15 +
            scores['accuracy_score'] * 0.20
        )
        
        scores['error_message'] = ""
        scores['evaluation_notes'] = f"关键词匹配: {matched_keywords}/{len(test_case.expected_keywords)}"
        
        return scores
    
    def check_sentiment_match(self, expected_sentiment: str, output_text: str) -> bool:
        """检查情感匹配"""
        output_lower = output_text.lower()
        
        if expected_sentiment == "positive":
            positive_words = ["机会", "上涨", "买入", "乐观", "推荐", "良好", "增长", "强势"]
            return any(word in output_lower for word in positive_words)
        
        elif expected_sentiment == "negative":
            negative_words = ["风险", "下跌", "卖出", "谨慎", "避免", "悲观", "下降", "弱势"]
            return any(word in output_lower for word in negative_words)
        
        else:  # neutral
            return True  # 中性情感较难判断，暂时返回True
    
    def calculate_relevance_score(self, test_case: TestCase, output_text: str) -> float:
        """计算相关性分数"""
        # 基于关键词密度和回答长度
        if not output_text.strip():
            return 0.0
        
        # 检查是否包含股票投资相关词汇
        finance_keywords = ["股票", "投资", "市场", "分析", "风险", "收益", "价格", "交易"]
        finance_count = sum(1 for word in finance_keywords if word in output_text)
        
        # 长度合理性 (50-500字符比较合理)
        length_score = 1.0
        if len(output_text) < 50:
            length_score = len(output_text) / 50
        elif len(output_text) > 500:
            length_score = 500 / len(output_text)
        
        # 综合评分
        relevance_score = min(1.0, (finance_count / len(finance_keywords)) * 0.7 + length_score * 0.3)
        
        return relevance_score
    
    def calculate_coherence_score(self, output_text: str) -> float:
        """计算连贯性分数"""
        if not output_text.strip():
            return 0.0
        
        # 简单的连贯性检查
        sentences = re.split(r'[。！？.!?]', output_text)
        sentences = [s.strip() for s in sentences if s.strip()]
        
        if len(sentences) == 0:
            return 0.0
        
        # 检查是否有完整句子
        complete_sentences = len(sentences)
        
        # 检查是否有逻辑词汇
        logic_words = ["因为", "所以", "但是", "然而", "此外", "同时", "因此", "综合"]
        logic_count = sum(1 for word in logic_words if word in output_text)
        
        # 连贯性分数
        coherence_score = min(1.0, (complete_sentences / 5) * 0.6 + (logic_count / 3) * 0.4)
        
        return coherence_score
    
    def calculate_accuracy_score(self, test_case: TestCase, output_text: str) -> float:
        """计算准确性分数"""
        if not output_text.strip():
            return 0.0
        
        # 基于测试用例难度和回答完整性
        difficulty_weight = {"easy": 1.0, "medium": 0.8, "hard": 0.6}
        base_score = difficulty_weight.get(test_case.difficulty, 0.5)
        
        # 检查是否包含明显错误信息
        error_phrases = ["不知道", "无法", "无效", "错误", "失败"]
        has_errors = any(phrase in output_text for phrase in error_phrases)
        
        if has_errors:
            base_score *= 0.5
        
        # 检查回答的专业性
        professional_terms = ["技术指标", "基本面", "财务", "分析", "建议", "策略"]
        professional_count = sum(1 for term in professional_terms if term in output_text)
        professional_score = min(1.0, professional_count / 3)
        
        accuracy_score = base_score * 0.7 + professional_score * 0.3
        
        return min(1.0, accuracy_score)
    
    def calculate_performance_metrics(self, model_name: str, results: List[EvaluationResult]) -> ModelPerformanceMetrics:
        """计算性能指标"""
        if not results:
            return ModelPerformanceMetrics(
                model_name=model_name,
                evaluation_date=datetime.now().isoformat(),
                total_tests=0,
                passed_tests=0,
                failed_tests=0,
                success_rate=0.0,
                avg_response_time=0.0,
                min_response_time=0.0,
                max_response_time=0.0,
                avg_accuracy_score=0.0,
                avg_relevance_score=0.0,
                avg_coherence_score=0.0,
                avg_overall_score=0.0,
                category_scores={},
                difficulty_scores={},
                test_results=[]
            )
        
        # 基础统计
        total_tests = len(results)
        passed_tests = len([r for r in results if r.overall_score >= 0.6])  # 60%分数线
        failed_tests = total_tests - passed_tests
        success_rate = passed_tests / total_tests
        
        # 响应时间统计
        response_times = [r.response_time for r in results if r.response_time > 0]
        avg_response_time = np.mean(response_times) if response_times else 0.0
        min_response_time = np.min(response_times) if response_times else 0.0
        max_response_time = np.max(response_times) if response_times else 0.0
        
        # 质量指标统计
        avg_accuracy_score = np.mean([r.accuracy_score for r in results])
        avg_relevance_score = np.mean([r.relevance_score for r in results])
        avg_coherence_score = np.mean([r.coherence_score for r in results])
        avg_overall_score = np.mean([r.overall_score for r in results])
        
        # 分类统计
        category_scores = {}
        difficulty_scores = {}
        
        # 获取测试用例信息进行分类统计
        test_cases = {tc.id: tc for tc in self.get_test_cases()}
        
        for result in results:
            if result.test_case_id in test_cases:
                test_case = test_cases[result.test_case_id]
                
                # 按类别统计
                if test_case.category not in category_scores:
                    category_scores[test_case.category] = []
                category_scores[test_case.category].append(result.overall_score)
                
                # 按难度统计
                if test_case.difficulty not in difficulty_scores:
                    difficulty_scores[test_case.difficulty] = []
                difficulty_scores[test_case.difficulty].append(result.overall_score)
        
        # 计算平均分
        category_scores = {k: np.mean(v) for k, v in category_scores.items()}
        difficulty_scores = {k: np.mean(v) for k, v in difficulty_scores.items()}
        
        return ModelPerformanceMetrics(
            model_name=model_name,
            evaluation_date=datetime.now().isoformat(),
            total_tests=total_tests,
            passed_tests=passed_tests,
            failed_tests=failed_tests,
            success_rate=success_rate,
            avg_response_time=avg_response_time,
            min_response_time=min_response_time,
            max_response_time=max_response_time,
            avg_accuracy_score=avg_accuracy_score,
            avg_relevance_score=avg_relevance_score,
            avg_coherence_score=avg_coherence_score,
            avg_overall_score=avg_overall_score,
            category_scores=category_scores,
            difficulty_scores=difficulty_scores,
            test_results=results
        )
    
    def save_evaluation_result(self, result: EvaluationResult):
        """保存评估结果"""
        import uuid
        result_id = str(uuid.uuid4())
        
        with sqlite3.connect(self.db_path) as conn:
            conn.execute('''
                INSERT INTO evaluation_results 
                (id, test_case_id, model_name, input_text, output_text, response_time,
                 keyword_match_score, sentiment_match, relevance_score, coherence_score,
                 accuracy_score, overall_score, error_message, evaluation_notes, timestamp)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            ''', (
                result_id, result.test_case_id, result.model_name, result.input_text,
                result.output_text, result.response_time, result.keyword_match_score,
                result.sentiment_match, result.relevance_score, result.coherence_score,
                result.accuracy_score, result.overall_score, result.error_message,
                result.evaluation_notes, result.timestamp
            ))
            conn.commit()
    
    def save_performance_metrics(self, metrics: ModelPerformanceMetrics):
        """保存性能指标"""
        import uuid
        metrics_id = str(uuid.uuid4())
        
        with sqlite3.connect(self.db_path) as conn:
            conn.execute('''
                INSERT INTO model_performance 
                (id, model_name, evaluation_date, performance_data, created_at)
                VALUES (?, ?, ?, ?, ?)
            ''', (
                metrics_id,
                metrics.model_name,
                metrics.evaluation_date,
                json.dumps(asdict(metrics), ensure_ascii=False),
                datetime.now().isoformat()
            ))
            conn.commit()
    
    def get_model_performance_history(self, model_name: str) -> List[ModelPerformanceMetrics]:
        """获取模型性能历史"""
        with sqlite3.connect(self.db_path) as conn:
            conn.row_factory = sqlite3.Row
            cursor = conn.execute('''
                SELECT * FROM model_performance 
                WHERE model_name = ? 
                ORDER BY evaluation_date DESC
            ''', (model_name,))
            
            metrics_list = []
            for row in cursor.fetchall():
                performance_data = json.loads(row['performance_data'])
                
                # 重建 ModelPerformanceMetrics 对象
                metrics = ModelPerformanceMetrics(**performance_data)
                metrics_list.append(metrics)
            
            return metrics_list
    
    def compare_models(self, model_names: List[str]) -> Dict[str, Any]:
        """比较多个模型性能"""
        comparison_data = {}
        
        for model_name in model_names:
            history = self.get_model_performance_history(model_name)
            if history:
                latest_metrics = history[0]  # 最新的评估结果
                comparison_data[model_name] = {
                    'success_rate': latest_metrics.success_rate,
                    'avg_response_time': latest_metrics.avg_response_time,
                    'avg_overall_score': latest_metrics.avg_overall_score,
                    'evaluation_date': latest_metrics.evaluation_date
                }
            else:
                comparison_data[model_name] = {
                    'success_rate': 0.0,
                    'avg_response_time': 0.0,
                    'avg_overall_score': 0.0,
                    'evaluation_date': 'N/A'
                }
        
        return comparison_data