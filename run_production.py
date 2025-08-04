#!/usr/bin/env python3
"""
生产环境启动器 - 使用Gunicorn提供更好的性能
"""

import os
import sys
from pathlib import Path

def main():
    """启动生产环境服务器"""
    print("🚀 启动ljwx-stock生产环境服务器")
    print("=" * 50)
    
    # 设置环境变量
    os.environ['FLASK_ENV'] = 'production'
    
    try:
        # 检查Gunicorn是否可用
        import gunicorn
        print("✅ 检测到Gunicorn")
        
        # 使用Gunicorn启动
        from gunicorn.app.wsgiapp import WSGIApplication
        
        print(f"🌐 服务器地址: http://localhost:5005")
        print(f"👥 工作进程: 4")
        print(f"🔧 工作模式: sync + threads")
        print("🛑 按 Ctrl+C 停止服务")
        print("=" * 50)
        
        # 使用标准sync worker避免异步兼容性问题
        cmd = [
            'gunicorn',
            '--bind=0.0.0.0:5005',
            '--workers=2',  # 使用2个同步worker
            '--threads=4',  # 每个worker使用4个线程
            '--timeout=120',
            '--keep-alive=5',
            '--log-level=warning',
            '--access-logfile=-',
            'wsgi_app:application'
        ]
        
        os.system(' '.join(cmd))
        
    except ImportError:
        print("⚠️  未安装Gunicorn，使用开发服务器")
        print("💡 建议运行: pip install gunicorn gevent")
        print("=" * 50)
        
        # 回退到开发服务器
        from unified_app import UnifiedStockApp
        app = UnifiedStockApp()
        app.run(host='0.0.0.0', port=5005, debug=False)
        
    except KeyboardInterrupt:
        print("\n👋 服务已停止")
    except Exception as e:
        print(f"❌ 启动失败: {e}")
        sys.exit(1)

if __name__ == '__main__':
    main()