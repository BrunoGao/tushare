# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

# LJWX-Stock Intelligent Stock Analysis System

## System Overview

ljwx-stock is an AI-powered intelligent stock analysis and recommendation system that provides real-time data streaming, professional technical analysis, and AI-powered Q&A functionality. The system combines traditional technical analysis with large language models for comprehensive stock market insights.

## Core Architecture

The system follows a multi-layered architecture:

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│  WebSocket服务  │    │   技术分析引擎   │    │   AI分析引擎    │
│  实时数据推送    │◀──▶│   30+指标计算   │◀──▶│   LLM智能分析   │
│  订阅管理       │    │   信号生成      │    │   自然语言理解   │
└─────────────────┘    └─────────────────┘    └─────────────────┘
         │                       │                       │
         ▼                       ▼                       ▼
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   数据库层      │    │   Flask API层   │    │   前端展示层     │
│ ljwx_stock DB   │◀──▶│   RESTful API   │◀──▶│   专业Web界面   │
│ MySQL + Redis   │    │   CORS支持      │    │   实时图表      │
└─────────────────┘    └─────────────────┘    └─────────────────┘
```

### Key Components:

1. **Data Layer**: TuShare Pro API integration with MySQL storage and Redis caching
2. **Analysis Engine**: Technical indicators, AI-powered analysis via Ollama LLM integration
3. **Real-time System**: WebSocket server for live data streaming and client subscriptions
4. **API Layer**: Flask REST APIs with unified endpoints and CORS support
5. **Frontend**: Professional web interface with ECharts for stock visualizations
6. **HarmonyOS Client**: Native mobile application for cross-platform access

## Common Development Commands

### Environment Setup
```bash
# Create and activate virtual environment
python3 -m venv tushare_venv
source tushare_venv/bin/activate

# Install dependencies
pip install -r requirements.txt

# For AI/ML development
pip install -r requirements_ai.txt

# For Ollama LLM integration
pip install -r requirements_ollama.txt
```

### System Startup
```bash
# Start complete real-time system (recommended)
python3 start_realtime_system.py

# Initialize system dependencies and database
python3 start_realtime_system.py init

# Test system connections
python3 start_realtime_system.py test

# Start individual components
python3 api/app.py                    # Flask API only
python3 api/websocket_server.py       # WebSocket server only
python3 start_api_only.py            # API with minimal dependencies
```

### Data Management
```bash
# Comprehensive data fetching
python3 run.py fetch-comprehensive --mode full --category all

# Historical data (10 years)
python3 run.py fetch-10years

# Basic stock information
python3 scripts/fetch_stock_basic.py

# Daily data updates
python3 run.py scheduler --scheduler-cmd start

# Specific data categories
python3 comprehensive_data_fetcher.py  # Financial, sector, fund data
python3 fetch_moneyflow_data.py       # Money flow analysis
python3 fetch_holder_data.py          # Shareholder data
```

### AI Model Training
```bash
# Create base Ollama model
python3 create_ljwx_stock_model.py

# Train with TuShare data
python3 train_ljwx_stock.py

# Comprehensive training (50k samples, 5k stocks)
python3 comprehensive_training.py

# Continuous learning
python3 continuous_training.py

# Offline training (no API calls)
python3 comprehensive_training_offline.py
```

### Testing and Validation
```bash
# Integration tests
python3 test_integration.py

# AI system testing
python3 test_ai_system.py

# WebSocket connection test
python3 test_websocket_connection.py

# Performance benchmarking (Apple M2 optimized)
python3 test_m2_ultra_performance.py

# System health check
python3 run.py health-check
```

### Database Operations
```bash
# Initialize database schema
python3 run.py init-schema

# Database health and validation
python3 utils/data_scheduler.py health

# Full data synchronization
./start_full_sync.sh

# Technical indicators calculation
python3 database/technical_indicator_calculator.py
```

## Key Configuration

The system uses `config.py` for centralized configuration:

- **Database**: MySQL with ljwx_stock database, configurable via environment variables
- **TuShare**: Requires valid TS_TOKEN for data access
- **AI/LLM**: Ollama integration with configurable model (default: lingjingwanxiang:70b)
- **WebSocket**: Configurable host/port for real-time data streaming
- **API**: Flask with CORS support on configurable ports

Environment variables should be set in `.env` file (copy from `env.example`).

## Data Flow Architecture

### Real-time Data Pipeline:
1. **TuShare Pro API** → Raw market data
2. **Data Processing** → Technical indicator calculation
3. **Database Storage** → MySQL with intelligent caching
4. **WebSocket Broadcasting** → Real-time client updates
5. **AI Analysis** → LLM-powered insights and recommendations

### Training Pipeline:
1. **Historical Data Extraction** → Multi-year stock data collection
2. **Feature Engineering** → Technical indicators, market patterns
3. **Training Data Generation** → LLM-compatible format conversion
4. **Model Training** → Ollama fine-tuning with stock domain knowledge
5. **Model Deployment** → Integration with real-time analysis system

## AI/LLM Integration

The system integrates with Ollama for local LLM inference:

- **Base Model**: lingjingwanxiang:70b (Chinese financial domain)
- **Training Data**: 50,000+ samples covering 5,000+ stocks
- **Analysis Types**: Technical analysis, fundamental analysis, market sentiment
- **Response Formats**: Natural language recommendations with confidence scores

## Performance Optimizations

### Apple M2 Ultra Optimizations:
- Native ARM64 Python environment (`tushare_venv`)
- Optimized TA-Lib compilation for Apple Silicon
- Parallel data processing with threading
- Intelligent caching strategies for frequent data access

### Database Optimizations:
- Indexed queries for real-time performance
- Connection pooling for high concurrency
- Redis caching for frequently accessed data
- Batch processing for large data operations

## Development Workflow

1. **Development Environment**: Use `source tushare_venv/bin/activate` for consistent Python environment
2. **Testing Strategy**: Run integration tests before deploying changes
3. **Data Validation**: Use built-in health checks and data validators
4. **Performance Monitoring**: Built-in system monitoring and logging
5. **Error Handling**: Comprehensive logging system in `logs/` directory

## API Endpoints

### REST API (Port 5005):
- `/api/health` - System health check
- `/api/stocks/search` - Stock search functionality
- `/api/technical/indicators/{ts_code}` - Technical analysis data
- `/api/signals/realtime` - Real-time trading signals
- `/api/llm/intelligent` - AI-powered analysis

### WebSocket API (Port 8765):
- Real-time stock data subscriptions
- Live technical indicator updates
- AI query processing
- Multi-client subscription management

## Deployment Considerations

- **Production**: Use `run_production.py` for production deployment
- **Docker**: Docker Compose configuration available (`docker-compose.yml`)
- **Monitoring**: Built-in performance monitoring and health checks
- **Scaling**: WebSocket server supports multiple client connections
- **Security**: CORS configuration and API rate limiting implemented

## Strategy-Based AI Training System

### Architecture Overview
The system implements a sophisticated strategy-based training framework that converts investment strategies into training datasets for unified LLM training, enabling both general and personalized stock recommendations.

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   策略定义层     │    │   训练数据生成   │    │   统一模型训练   │
│                │    │                │    │                │
│ ┌─────────────┐ │    │ ┌─────────────┐ │    │ ┌─────────────┐ │
│ │ 技术分析策略 │ │───▶│ │技术指标训练集│ │───▶│ │  Ollama LLM  │ │
│ └─────────────┘ │    │ └─────────────┘ │    │ │   统一模型   │ │
│ ┌─────────────┐ │    │ ┌─────────────┐ │    │ │             │ │
│ │ 基本面策略  │ │───▶│ │基本面训练集 │ │───▶│ └─────────────┘ │
│ └─────────────┘ │    │ └─────────────┘ │    └─────────────────┘
│ ┌─────────────┐ │    │ ┌─────────────┐ │             │
│ │ 用户定制策略 │ │───▶│ │定制化训练集 │ │             │
│ └─────────────┘ │    │ └─────────────┘ │             │
└─────────────────┘    └─────────────────┘             │
                                                      │
┌─────────────────┐    ┌─────────────────┐             │
│   推荐生成层     │    │   异步调度系统   │             │
│                │    │                │◀────────────┘
│ ┌─────────────┐ │    │ ┌─────────────┐ │
│ │ 通用推荐引擎 │ │    │ │ 定时任务调度 │ │
│ └─────────────┘ │    │ └─────────────┘ │
│ ┌─────────────┐ │    │ ┌─────────────┐ │
│ │ 个性化推荐  │ │    │ │ 异步队列管理 │ │
│ └─────────────┘ │    │ └─────────────┘ │
└─────────────────┘    └─────────────────┘
```

### Key Features:
1. **Strategy-to-Training Pipeline**: Converts investment strategies into high-quality training datasets
2. **Unified LLM Training**: Single model trained on multiple strategy knowledge bases
3. **Dual Recommendation System**: Both general market recommendations and personalized user recommendations
4. **Asynchronous Updates**: Real-time recommendation panel updates via WebSocket
5. **Multi-Strategy Fusion**: Combines technical, fundamental, quantitative, and custom strategies

### Training Data Generation:
```python
# Example: Technical strategy to training data conversion
def generate_training_data(strategy, historical_data):
    for stock_code in stock_list:
        stock_data = get_historical_data(stock_code, period="2Y")
        indicators = calculate_indicators(stock_data)
        signals = apply_strategy(strategy, indicators)
        
        for date, signal in signals.items():
            sample = {
                "input": format_market_context(stock_data, indicators, date),
                "output": format_recommendation(signal, stock_code, date),
                "strategy_type": strategy["name"]
            }
            yield sample
```

### Recommendation Scheduling:
- **General Recommendations**: Daily at 8:00 AM using mainstream strategies
- **Personalized Recommendations**: Every 2 hours based on user strategies
- **Real-time Updates**: Every 15 minutes during trading hours
- **WebSocket Broadcasting**: Instant panel updates for all connected clients

### Implementation Commands:
```bash
# Generate training datasets from strategies
python3 strategy_training/generate_datasets.py --strategy-type all

# Train unified model with multi-strategy data
python3 train_unified_model.py --datasets strategy_datasets/ --epochs 10

# Start async recommendation scheduler  
python3 recommendation_scheduler.py --mode production

# Generate recommendations manually
curl -X POST http://localhost:5005/api/generate-recommendations \
  -H "Content-Type: application/json" \
  -d '{"num_stocks": 5, "strategy_types": ["technical", "fundamental"]}'
```

## Troubleshooting

### Common Issues:
1. **Database Connection**: Check MySQL service and credentials in config
2. **TuShare API Limits**: Monitor API usage and implement rate limiting
3. **Ollama Integration**: Ensure Ollama service is running and model is loaded
4. **Memory Usage**: Monitor system resources during large data operations
5. **WebSocket Connections**: Check firewall settings and port availability
6. **Recommendation Generation**: Verify TuShare token is valid and strategy models are loaded

### Log Files:
- `logs/api_server.log` - API service logs
- `logs/ai_training.log` - AI training logs
- `logs/system_startup.log` - System initialization logs
- `logs/comprehensive_training_*.log` - Training session logs
- `logs/recommendation_scheduler.log` - Async recommendation generation logs

## Important Notes

- Always activate the virtual environment before running commands
- The system requires a valid TuShare Pro token for data access
- AI model training requires significant computational resources
- WebSocket connections should be properly closed to avoid memory leaks
- Database schema updates should be tested in development first
- Strategy-based training requires historical data preprocessing
- Recommendation scheduling uses Celery for distributed task processing