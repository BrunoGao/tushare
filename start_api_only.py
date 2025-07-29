#!/usr/bin/env python3
"""
仅启动API服务的简化脚本
用于测试和调试
"""
import sys
import os
import logging

# 添加项目路径
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

# 配置日志
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)

def main():
    """启动API服务"""
    try:
        logger.info("🚀 启动API服务...")
        
        # 检查基本依赖
        try:
            import config
            logger.info("✅ config模块加载成功")
        except Exception as e:
            logger.error(f"❌ config模块加载失败: {e}")
            return
        
        # 启动Flask应用
        try:
            from api.app import app
            logger.info(f"🌐 启动API服务器: http://{config.API_HOST}:{config.API_PORT}")
            
            app.run(
                host=config.API_HOST,
                port=config.API_PORT,
                debug=True,  # 开启调试模式以便查看错误
                use_reloader=False
            )
            
        except Exception as e:
            logger.error(f"❌ API服务启动失败: {e}")
            import traceback
            traceback.print_exc()
            
    except KeyboardInterrupt:
        logger.info("🛑 API服务被用户中断")
    except Exception as e:
        logger.error(f"❌ 启动异常: {e}")
        import traceback
        traceback.print_exc()

if __name__ == "__main__":
    main()