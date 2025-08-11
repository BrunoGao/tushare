#!/usr/bin/env python3
"""
测试MySQL数据库配置
"""
import os

# 设置MySQL环境变量（如果需要的话）
# os.environ['MYSQL_HOST'] = 'localhost'
# os.environ['MYSQL_PORT'] = '3306' 
# os.environ['MYSQL_USER'] = 'root'
# os.environ['MYSQL_PASSWORD'] = 'your_password'

def test_mysql_connection():
    """测试MySQL连接"""
    try:
        import pymysql
        
        # MySQL连接配置
        mysql_config = {
            'host': os.getenv('MYSQL_HOST', 'localhost'),
            'port': int(os.getenv('MYSQL_PORT', 3306)),
            'user': os.getenv('MYSQL_USER', 'root'),
            'password': os.getenv('MYSQL_PASSWORD', ''),
            'charset': 'utf8mb4'
        }
        
        print(f"🔍 尝试连接MySQL服务器: {mysql_config['user']}@{mysql_config['host']}:{mysql_config['port']}")
        
        # 测试连接
        conn = pymysql.connect(**mysql_config)
        print("✅ MySQL连接成功")
        
        # 创建数据库
        try:
            with conn.cursor() as cursor:
                cursor.execute("CREATE DATABASE IF NOT EXISTS `ljwx-stock` CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci")
                cursor.execute("SHOW DATABASES LIKE 'ljwx-stock'")
                result = cursor.fetchone()
                if result:
                    print("✅ ljwx-stock数据库已创建")
                else:
                    print("❌ ljwx-stock数据库创建失败")
            conn.commit()
        finally:
            conn.close()
        
        # 测试连接到ljwx-stock数据库
        mysql_config['database'] = 'ljwx-stock'
        conn = pymysql.connect(**mysql_config)
        
        try:
            with conn.cursor() as cursor:
                cursor.execute("SELECT VERSION()")
                version = cursor.fetchone()[0]
                print(f"✅ MySQL版本: {version}")
                
                # 测试创建表
                cursor.execute("""
                    CREATE TABLE IF NOT EXISTS test_table (
                        id VARCHAR(255) PRIMARY KEY,
                        name VARCHAR(255) NOT NULL,
                        data JSON,
                        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                    ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
                """)
                print("✅ 测试表创建成功")
                
                # 清理测试表
                cursor.execute("DROP TABLE IF EXISTS test_table")
                print("✅ 测试表已清理")
                
            conn.commit()
        finally:
            conn.close()
            
        return True
        
    except ImportError:
        print("❌ pymysql模块未安装，请安装: pip install pymysql")
        return False
    except Exception as e:
        print(f"❌ MySQL连接失败: {e}")
        print("💡 建议:")
        print("   1. 检查MySQL服务是否运行")
        print("   2. 检查用户名密码是否正确") 
        print("   3. 检查用户是否有创建数据库权限")
        print("   4. 设置正确的环境变量:")
        print("      export MYSQL_HOST=localhost")
        print("      export MYSQL_USER=root")
        print("      export MYSQL_PASSWORD=your_password")
        return False

def test_sqlite_fallback():
    """测试SQLite回退"""
    print("\n🔄 测试SQLite回退...")
    try:
        import sqlite3
        import os
        
        os.makedirs('data', exist_ok=True)
        db_path = 'data/test_sqlite.db'
        
        with sqlite3.connect(db_path) as conn:
            conn.execute('''
                CREATE TABLE IF NOT EXISTS test_table (
                    id TEXT PRIMARY KEY,
                    name TEXT NOT NULL,
                    data TEXT,
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                )
            ''')
            print("✅ SQLite测试表创建成功")
            
            # 清理
            conn.execute("DROP TABLE IF EXISTS test_table")
            print("✅ SQLite测试表已清理")
        
        # 删除测试数据库
        if os.path.exists(db_path):
            os.remove(db_path)
        
        print("✅ SQLite回退功能正常")
        return True
        
    except Exception as e:
        print(f"❌ SQLite回退测试失败: {e}")
        return False

if __name__ == '__main__':
    print("🚀 ljwx-stock 数据库配置测试")
    print("=" * 50)
    
    # 测试MySQL
    mysql_ok = test_mysql_connection()
    
    # 测试SQLite回退
    sqlite_ok = test_sqlite_fallback()
    
    print("\n" + "=" * 50)
    print("📊 测试结果:")
    print(f"   MySQL连接: {'✅ 可用' if mysql_ok else '❌ 不可用，将使用SQLite'}")
    print(f"   SQLite回退: {'✅ 可用' if sqlite_ok else '❌ 不可用'}")
    
    if mysql_ok:
        print("\n🎉 MySQL配置正确，系统将使用MySQL数据库")
    elif sqlite_ok:
        print("\n⚠️  MySQL不可用，系统将使用SQLite数据库")
    else:
        print("\n❌ 数据库配置失败，请检查配置")