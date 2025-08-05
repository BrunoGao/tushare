#!/usr/bin/env python3
"""
数据获取进度监控脚本
实时显示数据获取进度和统计信息
"""
import json
import os
import time
from datetime import datetime
import argparse

def load_progress():
    """加载进度文件"""
    progress_file = 'logs/fetch_progress.json'
    if os.path.exists(progress_file):
        try:
            with open(progress_file, 'r') as f:
                return json.load(f)
        except:
            return None
    return None

def format_duration(seconds):
    """格式化时长"""
    if seconds < 60:
        return f"{seconds:.1f}秒"
    elif seconds < 3600:
        return f"{seconds/60:.1f}分钟"
    else:
        return f"{seconds/3600:.1f}小时"

def display_progress(progress):
    """显示进度信息"""
    if not progress:
        print("❌ 未找到进度文件")
        return
        
    completed = len(progress.get('completed', []))
    failed = len(progress.get('failed', []))
    stats = progress.get('stats', {})
    
    total_records = stats.get('total_records', 0)
    success_stocks = stats.get('success_stocks', 0)
    failed_stocks = stats.get('failed_stocks', 0)
    tables_created = len(stats.get('tables_created', []))
    
    # 计算预估总股票数（基于数据库中的股票数量）
    estimated_total = 5150  # 基于之前获取的股票总数
    
    completion_rate = (completed / estimated_total * 100) if estimated_total > 0 else 0
    
    # 计算运行时长
    start_time_str = stats.get('start_time')
    if start_time_str:
        start_time = datetime.fromisoformat(start_time_str)
        duration = (datetime.now() - start_time).total_seconds()
        duration_str = format_duration(duration)
        
        # 预估剩余时间
        if completed > 0:
            avg_time_per_stock = duration / completed
            remaining_stocks = estimated_total - completed
            estimated_remaining = avg_time_per_stock * remaining_stocks
            remaining_str = format_duration(estimated_remaining)
        else:
            remaining_str = "未知"
    else:
        duration_str = "未知"
        remaining_str = "未知"
    
    # 清屏并显示进度
    os.system('clear' if os.name == 'posix' else 'cls')
    
    print("=" * 70)
    print("📊 股票数据获取进度监控")
    print("=" * 70)
    
    # 进度条
    bar_length = 50
    filled_length = int(bar_length * completion_rate / 100)
    bar = "█" * filled_length + "░" * (bar_length - filled_length)
    
    print(f"进度: [{bar}] {completion_rate:.1f}%")
    print()
    
    # 详细统计
    print(f"📈 股票处理统计:")
    print(f"  总股票数(估算): {estimated_total:,}")
    print(f"  已完成股票:     {completed:,}")
    print(f"  成功获取:       {success_stocks:,}")
    print(f"  获取失败:       {failed_stocks:,}")
    print()
    
    print(f"📊 数据统计:")
    print(f"  总记录数:       {total_records:,}")
    print(f"  已创建表数:     {tables_created}")
    print(f"  平均每股记录:   {total_records//max(success_stocks,1):,}")
    print()
    
    print(f"⏱️  时间统计:")
    print(f"  运行时长:       {duration_str}")
    print(f"  预估剩余:       {remaining_str}")
    if completed > 0:
        avg_per_stock = duration / completed
        print(f"  平均每股耗时:   {avg_per_stock:.2f}秒")
    print()
    
    # 最后更新时间
    last_update = progress.get('last_update', '')
    if last_update:
        update_time = datetime.fromisoformat(last_update)
        print(f"🕒 最后更新: {update_time.strftime('%Y-%m-%d %H:%M:%S')}")
    
    print("=" * 70)
    print("💡 按 Ctrl+C 退出监控")

def monitor_progress(interval=5):
    """持续监控进度"""
    try:
        while True:
            progress = load_progress()
            display_progress(progress)
            time.sleep(interval)
    except KeyboardInterrupt:
        print("\n👋 监控已停止")

def show_summary():
    """显示最终汇总"""
    progress = load_progress()
    if not progress:
        print("❌ 未找到进度文件")
        return
        
    completed = len(progress.get('completed', []))
    failed = len(progress.get('failed', []))
    stats = progress.get('stats', {})
    
    print("=" * 70)
    print("📋 数据获取汇总报告")
    print("=" * 70)
    
    print(f"✅ 成功股票: {completed:,}")
    print(f"❌ 失败股票: {failed:,}")
    print(f"📈 总记录数: {stats.get('total_records', 0):,}")
    print(f"📊 创建表数: {len(stats.get('tables_created', []))}")
    
    # 显示失败的股票（如果不多的话）
    failed_list = progress.get('failed', [])
    if failed_list and len(failed_list) <= 20:
        print(f"\n❌ 失败股票列表:")
        for stock in failed_list:
            print(f"  - {stock}")
    elif len(failed_list) > 20:
        print(f"\n❌ 失败股票过多({len(failed_list)}只)，请查看日志文件")
    
    print("=" * 70)

def main():
    parser = argparse.ArgumentParser(description='监控股票数据获取进度')
    parser.add_argument('--monitor', action='store_true', help='持续监控模式')
    parser.add_argument('--summary', action='store_true', help='显示汇总报告')
    parser.add_argument('--interval', type=int, default=5, help='监控刷新间隔(秒)')
    
    args = parser.parse_args()
    
    if args.summary:
        show_summary()
    elif args.monitor:
        monitor_progress(args.interval)
    else:
        # 默认显示一次当前进度
        progress = load_progress()
        display_progress(progress)

if __name__ == "__main__":
    main()