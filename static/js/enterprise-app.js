/**
 * ljwx-stock 企业级前端应用
 * 现代化的量化投资决策平台
 */

class EnterpriseStockApp {
    constructor() {
        this.socket = null;
        this.systemStatus = {
            is_running: false,
            progress: 0,
            current_task: '',
            logs: []
        };
        this.refreshInterval = null;
        this.init();
    }

    // 初始化应用
    init() {
        console.log('🚀 ljwx-stock 企业级应用初始化');
        this.initWebSocket();
        this.setupEventListeners();
        this.loadDashboardData();
        this.startAutoRefresh();
    }

    // 初始化WebSocket连接
    initWebSocket() {
        try {
            this.socket = io();
            
            this.socket.on('connected', (data) => {
                this.addLog('WebSocket连接成功', 'success');
                this.updateSystemStatus('success', '连接正常');
            });

            this.socket.on('log_update', (data) => {
                this.addLog(data.message, data.level);
            });

            this.socket.on('progress_update', (data) => {
                this.updateProgress(data.progress, data.task);
            });

            this.socket.on('task_completed', (data) => {
                this.handleTaskCompleted(data);
            });

            this.socket.on('task_failed', (data) => {
                this.handleTaskFailed(data);
            });

            this.socket.on('recommendation_generated', (data) => {
                this.handleRecommendationGenerated(data);
            });

            this.socket.on('validation_completed', (data) => {
                this.handleValidationCompleted(data);
            });

        } catch (error) {
            console.error('WebSocket初始化失败:', error);
            this.updateSystemStatus('danger', '连接失败');
        }
    }

    // 设置事件监听器
    setupEventListeners() {
        // 表单提交事件
        const forms = [
            'recommendationForm',
            'quickAnalysisForm', 
            'modelTrainingForm'
        ];

        forms.forEach(formId => {
            const form = document.getElementById(formId);
            if (form) {
                form.addEventListener('submit', (e) => {
                    e.preventDefault();
                    this.handleFormSubmit(formId);
                });
            }
        });

        // 页面可见性变化时刷新数据
        document.addEventListener('visibilitychange', () => {
            if (!document.hidden) {
                this.refreshDashboard();
            }
        });
    }

    // 处理表单提交
    handleFormSubmit(formId) {
        switch (formId) {
            case 'recommendationForm':
                this.generateRecommendations();
                break;
            case 'quickAnalysisForm':
                this.runQuickAnalysis();
                break;
            case 'modelTrainingForm':
                this.startModelTraining();
                break;
        }
    }

    // 加载仪表板数据
    async loadDashboardData() {
        try {
            // 显示加载骨架
            this.showLoadingSkeleton();
            
            // 分步加载，防止某个API阻塞整个页面
            await this.loadSystemStatus();
            await this.loadDashboardMetrics();
            
            // 隐藏加载骨架 - 主要内容已加载
            this.hideLoadingSkeleton();
            
            // 后台加载其他数据
            this.loadModelsInfo();
            
        } catch (error) {
            console.error('加载仪表板数据失败:', error);
            this.addLog('数据加载失败: ' + error.message, 'error');
            this.hideLoadingSkeleton();
            // 即使出错也要继续显示页面
            this.showFallbackContent();
        }
    }
    
    // 分步加载系统状态
    async loadSystemStatus() {
        try {
            const systemStatus = await this.fetchAPI('/api/status');
            this.updateSystemStatusFromAPI(systemStatus);
        } catch (error) {
            console.warn('系统状态加载失败:', error);
            this.updateSystemStatus('warning', '连接异常');
        }
    }
    
    // 分步加载仪表板指标
    async loadDashboardMetrics() {
        try {
            const dashboardData = await this.fetchAPI('/api/mobile/dashboard');
            this.updateDashboardMetrics(dashboardData);
        } catch (error) {
            console.warn('仪表板指标加载失败:', error);
            this.showDefaultMetrics();
        }
    }
    
    // 后台加载模型信息
    async loadModelsInfo() {
        try {
            const [modelsInfo, trainingDataInfo] = await Promise.allSettled([
                this.fetchAPI('/api/models-info'),
                this.fetchAPI('/api/training-data-info')
            ]);
            
            if (modelsInfo.status === 'fulfilled') {
                this.updateModelSelects(modelsInfo.value.models || []);
            }
            
            if (trainingDataInfo.status === 'fulfilled') {
                this.updateTrainingFileSelect(trainingDataInfo.value.files || []);
            }
            
            // 后台加载推荐数据
            this.refreshRecommendations();
            
        } catch (error) {
            console.warn('模型信息加载失败:', error);
        }
    }
    
    // 显示默认指标
    showDefaultMetrics() {
        document.getElementById('totalModels').textContent = '0';
        document.getElementById('totalRecommendations').textContent = '0';
        document.getElementById('hitRate').textContent = '0%';
        document.getElementById('totalStrategies').textContent = '0';
    }
    
    // 显示降级内容
    showFallbackContent() {
        this.showDefaultMetrics();
        this.addLog('使用离线模式显示页面', 'warning');
    }

    // API请求封装
    async fetchAPI(url, options = {}) {
        try {
            // 添加超时控制
            const controller = new AbortController();
            const timeout = setTimeout(() => controller.abort(), 10000); // 10秒超时
            
            const response = await fetch(url, {
                headers: {
                    'Content-Type': 'application/json',
                    ...options.headers
                },
                signal: controller.signal,
                ...options
            });
            
            clearTimeout(timeout);

            if (!response.ok) {
                throw new Error(`HTTP ${response.status}: ${response.statusText}`);
            }

            return await response.json();
        } catch (error) {
            if (error.name === 'AbortError') {
                console.warn(`API请求超时 ${url}`);
                throw new Error('请求超时，请稍后重试');
            } else {
                console.error(`API请求失败 ${url}:`, error);
                throw error;
            }
        }
    }

    // 更新仪表板指标
    updateDashboardMetrics(data) {
        const metrics = {
            totalModels: data.training_summary?.total_models || 0,
            totalRecommendations: data.recommendation_summary?.total_recommendations || 0,
            hitRate: ((data.recommendation_summary?.hit_rate || 0) * 100).toFixed(1) + '%',
            totalStrategies: data.strategy_summary?.total_strategies || 0
        };

        // 更新指标值
        Object.entries(metrics).forEach(([key, value]) => {
            const element = document.getElementById(key);
            if (element) {
                this.animateCounter(element, element.textContent, value);
            }
        });

        // 更新变化指示器
        this.updateMetricChanges(data);
    }

    // 动画更新计数器
    animateCounter(element, start, end) {
        const startVal = parseInt(start) || 0;
        const endVal = parseInt(end) || 0;
        const duration = 1000;
        const stepTime = 50;
        const steps = duration / stepTime;
        const increment = (endVal - startVal) / steps;
        
        let current = startVal;
        const timer = setInterval(() => {
            current += increment;
            if ((increment > 0 && current >= endVal) || (increment < 0 && current <= endVal)) {
                element.textContent = end;
                clearInterval(timer);
            } else {
                element.textContent = Math.floor(current);
            }
        }, stepTime);
    }

    // 更新指标变化
    updateMetricChanges(data) {
        // 这里可以实现指标变化的计算和显示
        // 暂时显示静态状态
        const changes = {
            modelsChange: '系统就绪',
            recommendationsChange: '数据同步',
            hitRateChange: '实时更新',
            strategiesChange: '策略优化'
        };

        Object.entries(changes).forEach(([key, value]) => {
            const element = document.getElementById(key);
            if (element) {
                element.innerHTML = `<i class="bi bi-check-circle me-1"></i>${value}`;
                element.className = 'metric-change positive';
            }
        });
    }

    // 更新模型选择框
    updateModelSelects(models) {
        const selects = ['recModelSelect'];
        selects.forEach(selectId => {
            const select = document.getElementById(selectId);
            if (select) {
                select.innerHTML = '<option value="">选择AI模型</option>' +
                    models.map(model => 
                        `<option value="${model.name}">${model.name}</option>`
                    ).join('');
            }
        });
    }

    // 更新训练文件选择框
    updateTrainingFileSelect(files) {
        const select = document.getElementById('trainingFile');
        if (select) {
            select.innerHTML = '<option value="">选择训练数据文件</option>' +
                files.map(file => 
                    `<option value="${file.filename}">${file.filename} (${file.sample_count}样本)</option>`
                ).join('');
        }
    }

    // 刷新推荐数据
    async refreshRecommendations() {
        try {
            const data = await this.fetchAPI('/api/mobile/recommendations/latest?limit=5');
            this.displayRecommendations(data.recommendations || []);
        } catch (error) {
            console.error('加载推荐数据失败:', error);
            this.displayRecommendations([]);
        }
    }

    // 显示推荐数据
    displayRecommendations(recommendations) {
        const container = document.getElementById('latestRecommendations');
        if (!container) return;

        if (recommendations.length === 0) {
            container.innerHTML = `
                <div class="text-center text-muted py-4">
                    <i class="bi bi-star fs-1 d-block mb-2 text-muted"></i>
                    <p class="mb-0">暂无推荐数据</p>
                    <small>点击生成推荐开始使用</small>
                </div>
            `;
            return;
        }

        container.innerHTML = recommendations.map(rec => `
            <div class="enterprise-table mb-2">
                <div class="p-3">
                    <div class="d-flex justify-content-between align-items-start">
                        <div>
                            <h6 class="mb-1 fw-bold">${rec.stock_name}</h6>
                            <small class="text-muted">${rec.stock_code}</small>
                        </div>
                        <div class="text-end">
                            <span class="status-badge-table ${this.getRecommendationTypeClass(rec.recommendation_type)}">
                                ${this.getRecommendationTypeLabel(rec.recommendation_type)}
                            </span>
                            <div class="mt-1">
                                <small class="text-muted">置信度: ${(rec.confidence_score * 100).toFixed(0)}%</small>
                            </div>
                        </div>
                    </div>
                    <div class="row mt-2">
                        <div class="col-6">
                            <small class="text-muted">收益率</small>
                            <div class="fw-bold ${rec.actual_return >= 0 ? 'text-success' : 'text-danger'}">
                                ${rec.actual_return ? (rec.actual_return * 100).toFixed(2) + '%' : '待验证'}
                            </div>
                        </div>
                        <div class="col-6 text-end">
                            <small class="text-muted">${this.formatTime(rec.created_time)}</small>
                        </div>
                    </div>
                </div>
            </div>
        `).join('');
    }

    // 生成股票推荐
    async generateRecommendations() {
        if (this.systemStatus.is_running) {
            this.showAlert('当前有任务正在运行，请等待完成', 'warning');
            return;
        }

        const config = {
            model_name: document.getElementById('recModelSelect').value,
            num_stocks: parseInt(document.getElementById('numStocks').value),
            analysis_type: document.getElementById('analysisType').value,
            use_daily_mode: document.getElementById('useDailyMode').checked,
            tushare_token: document.getElementById('recTushareToken').value
        };

        if (!config.model_name) {
            this.showAlert('请选择推荐模型', 'warning');
            return;
        }

        try {
            const data = await this.fetchAPI('/api/recommendations/generate', {
                method: 'POST',
                body: JSON.stringify(config)
            });

            if (data.status === 'started') {
                this.hideModal('stockRecommendationModal');
                this.addLog(`🚀 开始生成推荐: ${config.model_name}`, 'info');
                this.systemStatus.is_running = true;
                this.updateSystemStatus('warning', '生成中');
                this.showProgress();
            } else {
                this.showAlert('推荐生成失败: ' + (data.error || '未知错误'), 'danger');
            }
        } catch (error) {
            console.error('生成推荐失败:', error);
            this.showAlert('生成失败: ' + error.message, 'danger');
        }
    }

    // 运行快速分析
    async runQuickAnalysis() {
        const stockCode = document.getElementById('quickStockCode').value;
        const analysisType = document.getElementById('quickAnalysisType').value;
        
        if (!stockCode.trim()) {
            this.showAlert('请输入股票代码', 'warning');
            return;
        }

        const resultDiv = document.getElementById('quickAnalysisResult');
        const outputDiv = document.getElementById('quickAnalysisOutput');
        
        resultDiv.style.display = 'block';
        outputDiv.innerHTML = this.createLoadingHTML('分析中...');

        try {
            const data = await this.fetchAPI('/api/mobile/quick-analysis', {
                method: 'POST',
                body: JSON.stringify({
                    stock_code: stockCode,
                    analysis_type: analysisType
                })
            });

            if (data.error) {
                outputDiv.innerHTML = this.createAlertHTML(data.error, 'danger');
            } else {
                this.displayQuickAnalysisResult(data.analysis);
            }
        } catch (error) {
            outputDiv.innerHTML = this.createAlertHTML('分析失败: ' + error.message, 'danger');
        }
    }

    // 显示快速分析结果
    displayQuickAnalysisResult(analysis) {
        const outputDiv = document.getElementById('quickAnalysisOutput');
        
        if (analysis.error) {
            outputDiv.innerHTML = this.createAlertHTML(analysis.error, 'danger');
            return;
        }

        outputDiv.innerHTML = `
            <div class="enterprise-table">
                <div class="p-3">
                    <div class="row">
                        <div class="col-md-6">
                            <h6 class="fw-bold mb-3">价格信息</h6>
                            <div class="mb-2">
                                <small class="text-muted">当前价格</small>
                                <div class="fw-bold">¥${analysis.current_price}</div>
                            </div>
                            <div class="mb-2">
                                <small class="text-muted">涨跌幅</small>
                                <div class="fw-bold ${analysis.price_change >= 0 ? 'text-success' : 'text-danger'}">
                                    ${analysis.price_change >= 0 ? '+' : ''}${analysis.price_change.toFixed(2)}%
                                </div>
                            </div>
                            <div class="mb-2">
                                <small class="text-muted">成交量</small>
                                <div class="fw-bold">${(analysis.volume / 10000).toFixed(2)}万手</div>
                            </div>
                        </div>
                        <div class="col-md-6">
                            <h6 class="fw-bold mb-3">技术指标</h6>
                            <div class="mb-2">
                                <small class="text-muted">5日均线</small>
                                <div class="fw-bold">¥${analysis.ma5.toFixed(2)}</div>
                            </div>
                            <div class="mb-2">
                                <small class="text-muted">20日均线</small>
                                <div class="fw-bold">¥${analysis.ma20.toFixed(2)}</div>
                            </div>
                            <div class="mb-2">
                                <small class="text-muted">趋势判断</small>
                                <div>
                                    <span class="status-badge-table ${analysis.trend === '上涨' ? 'success' : 'danger'}">
                                        ${analysis.trend}
                                    </span>
                                </div>
                            </div>
                        </div>
                    </div>
                    <hr>
                    <small class="text-muted">
                        <i class="bi bi-clock me-1"></i>
                        分析时间: ${analysis.analysis_time}
                    </small>
                </div>
            </div>
        `;
    }

    // 开始模型训练
    async startModelTraining() {
        if (this.systemStatus.is_running) {
            this.showAlert('当前有任务正在运行，请等待完成', 'warning');
            return;
        }

        const config = {
            base_model: document.getElementById('baseModel').value,
            training_file: document.getElementById('trainingFile').value,
            model_name: document.getElementById('modelName').value || 
                       `ljwx-stock-enterprise-${new Date().toISOString().slice(0,16).replace(/[-:]/g, '')}`,
            sample_count: parseInt(document.getElementById('sampleCount').value)
        };

        if (!config.training_file) {
            this.showAlert('请选择训练数据文件', 'warning');
            return;
        }

        try {
            const data = await this.fetchAPI('/api/train-model', {
                method: 'POST',
                body: JSON.stringify(config)
            });

            if (data.status === 'started') {
                this.hideModal('modelTrainingModal');
                this.addLog('🚀 开始训练模型: ' + config.model_name, 'info');
                this.systemStatus.is_running = true;
                this.updateSystemStatus('warning', '训练中');
                this.showProgress();
            } else {
                this.showAlert('训练启动失败: ' + (data.error || '未知错误'), 'danger');
            }
        } catch (error) {
            console.error('启动训练失败:', error);
            this.showAlert('启动失败: ' + error.message, 'danger');
        }
    }

    // 系统状态更新
    updateSystemStatus(type, text) {
        const statusBadge = document.getElementById('systemStatus');
        if (statusBadge) {
            statusBadge.className = `status-badge ${type}`;
            statusBadge.innerHTML = `<i class="bi bi-circle-fill me-1"></i>${text}`;
        }
    }

    updateSystemStatusFromAPI(status) {
        this.systemStatus = status;
        if (status.is_running) {
            this.updateSystemStatus('warning', '处理中');
        } else {
            this.updateSystemStatus('success', '系统正常');
        }
    }

    // 进度管理
    updateProgress(progress, task) {
        this.systemStatus.progress = progress;
        this.systemStatus.current_task = task;
        
        const progressBar = document.getElementById('globalProgress');
        const taskText = document.getElementById('currentTask');
        const trainingProgress = document.getElementById('trainingProgress');
        const trainingStatus = document.getElementById('trainingStatus');
        const trainingTask = document.getElementById('currentTrainingTask');
        
        if (progressBar) progressBar.style.width = progress + '%';
        if (taskText) taskText.textContent = task || '';
        if (trainingProgress) trainingProgress.style.width = progress + '%';
        if (trainingStatus) trainingStatus.textContent = progress === 100 ? '训练完成' : '训练中';
        if (trainingTask) trainingTask.textContent = task || '正在处理...';

        if (progress > 0 && progress < 100) {
            this.showProgress();
        } else if (progress >= 100) {
            setTimeout(() => this.hideProgress(), 3000);
        }
    }

    showProgress() {
        const section = document.getElementById('progressSection');
        if (section) {
            section.style.display = 'block';
            section.classList.add('fade-in-up');
        }
    }

    hideProgress() {
        const section = document.getElementById('progressSection');
        if (section) {
            section.style.display = 'none';
        }
    }

    // 日志管理
    addLog(message, level = 'info') {
        const logContainer = document.getElementById('logContainer');
        if (!logContainer) return;

        const timestamp = new Date().toLocaleTimeString();
        const logEntry = document.createElement('div');
        logEntry.className = `log-entry ${level}`;
        logEntry.innerHTML = `<span class="log-timestamp">[${timestamp}]</span> ${message}`;

        logContainer.appendChild(logEntry);
        logContainer.scrollTop = logContainer.scrollHeight;

        // 限制日志条数
        while (logContainer.children.length > 100) {
            logContainer.removeChild(logContainer.firstChild);
        }

        // 更新最后更新时间
        const lastUpdate = document.getElementById('lastUpdate');
        if (lastUpdate) {
            lastUpdate.textContent = '刚刚';
        }
    }

    clearLogs() {
        fetch('/api/clear-logs', { method: 'POST' })
            .then(() => {
                const logContainer = document.getElementById('logContainer');
                if (logContainer) {
                    logContainer.innerHTML = '';
                }
                this.addLog('日志已清除');
            })
            .catch(error => {
                console.error('清除日志失败:', error);
            });
    }

    // 事件处理
    handleTaskCompleted(data) {
        this.systemStatus.is_running = false;
        this.updateSystemStatus('success', '系统正常');
        this.updateProgress(100, '任务完成');
        
        if (data.type === 'data_generation') {
            this.addLog(`✅ 数据生成完成：${data.sample_count} 个样本`, 'success');
        } else if (data.type === 'model_training') {
            this.addLog(`✅ 模型训练完成：${data.model_name}`, 'success');
        }
        
        this.refreshDashboard();
    }

    handleTaskFailed(data) {
        this.systemStatus.is_running = false;
        this.updateSystemStatus('danger', '执行失败');
        this.addLog(`❌ 任务失败：${data.error}`, 'error');
    }

    handleRecommendationGenerated(data) {
        this.addLog(`✅ 推荐生成完成: ${data.total_recommendations} 个`, 'success');
        this.refreshRecommendations();
        this.refreshDashboard();
    }

    handleValidationCompleted(data) {
        this.addLog(`✅ 推荐验证完成: ${data.validated_count} 条`, 'success');
        this.refreshRecommendations();
        this.refreshDashboard();
    }

    // UI辅助方法
    showModal(modalId) {
        const modal = new bootstrap.Modal(document.getElementById(modalId));
        modal.show();
    }

    hideModal(modalId) {
        const modalElement = document.getElementById(modalId);
        const modal = bootstrap.Modal.getInstance(modalElement);
        if (modal) modal.hide();
    }

    showAlert(message, type = 'info') {
        // 创建临时提示框
        const alert = document.createElement('div');
        alert.className = `alert alert-${type} alert-dismissible fade show position-fixed`;
        alert.style.cssText = 'top: 20px; right: 20px; z-index: 2000; min-width: 300px;';
        alert.innerHTML = `
            ${message}
            <button type="button" class="btn-close" data-bs-dismiss="alert"></button>
        `;
        
        document.body.appendChild(alert);
        
        // 3秒后自动移除
        setTimeout(() => {
            if (alert.parentNode) {
                alert.parentNode.removeChild(alert);
            }
        }, 3000);
    }

    createLoadingHTML(text = '加载中...') {
        return `
            <div class="text-center py-3">
                <div class="spinner-border spinner-border-sm text-primary me-2"></div>
                <span>${text}</span>
            </div>
        `;
    }

    createAlertHTML(message, type = 'info') {
        return `<div class="alert alert-${type} mb-0">${message}</div>`;
    }

    showLoadingSkeleton() {
        // 显示加载骨架屏
        const elements = ['totalModels', 'totalRecommendations', 'hitRate', 'totalStrategies'];
        elements.forEach(id => {
            const element = document.getElementById(id);
            if (element) {
                element.classList.add('loading-skeleton');
                element.textContent = '';
            }
        });
    }

    hideLoadingSkeleton() {
        // 隐藏加载骨架屏
        const elements = ['totalModels', 'totalRecommendations', 'hitRate', 'totalStrategies'];
        elements.forEach(id => {
            const element = document.getElementById(id);
            if (element) {
                element.classList.remove('loading-skeleton');
            }
        });
    }

    // 自动刷新 - 优化刷新频率
    startAutoRefresh() {
        // 每60秒自动刷新仪表板数据，减少服务器负载
        this.refreshInterval = setInterval(() => {
            if (!document.hidden && !this.systemStatus.is_running) {
                this.refreshDashboard();
            }
        }, 60000); // 改为60秒
    }

    stopAutoRefresh() {
        if (this.refreshInterval) {
            clearInterval(this.refreshInterval);
            this.refreshInterval = null;
        }
    }

    refreshDashboard() {
        this.loadDashboardData();
    }

    // 工具方法
    getRecommendationTypeClass(type) {
        const classes = {
            'buy': 'success',
            'sell': 'danger',
            'hold': 'warning',
            'neutral': 'info'
        };
        return classes[type] || 'info';
    }

    getRecommendationTypeLabel(type) {
        const labels = {
            'buy': '买入',
            'sell': '卖出',
            'hold': '持有',
            'neutral': '中性'
        };
        return labels[type] || type;
    }

    formatTime(timeStr) {
        try {
            const date = new Date(timeStr);
            return date.toLocaleString('zh-CN');
        } catch {
            return timeStr;
        }
    }
}

// 全局函数（供HTML调用）
let app;

document.addEventListener('DOMContentLoaded', function() {
    app = new EnterpriseStockApp();
});

// 导出全局函数供HTML模板调用
function showStockRecommendation() {
    app.showModal('stockRecommendationModal');
}

function showQuickAnalysis() {
    app.showModal('quickAnalysisModal');
}

function showModelTraining() {
    app.showModal('modelTrainingModal');
}

function generateRecommendations() {
    app.generateRecommendations();
}

function runQuickAnalysis() {
    app.runQuickAnalysis();
}

function startModelTraining() {
    app.startModelTraining();
}

function refreshDashboard() {
    app.refreshDashboard();
}

function refreshRecommendations() {
    app.refreshRecommendations();
}

function clearLogs() {
    app.clearLogs();
}

function showSettings() {
    app.addLog('设置功能开发中...', 'info');
}

function showDataManagement() {
    app.addLog('数据管理功能开发中...', 'info');
}

function showSystemInfo() {
    app.addLog('系统信息功能开发中...', 'info');
}