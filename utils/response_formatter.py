"""
统一响应格式化工具
为所有API接口提供标准化的响应格式
"""
from flask import jsonify
from datetime import datetime
from typing import Any, Dict, Optional

class ResponseFormatter:
    """统一响应格式化器"""
    
    @staticmethod
    def success(data: Any = None, message: str = "操作成功", code: int = 200) -> tuple:
        """成功响应"""
        response = {
            "success": True,
            "code": code,
            "message": message,
            "data": data,
            "timestamp": datetime.now().isoformat(),
            "server_time": datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        }
        return jsonify(response), code
    
    @staticmethod
    def error(message: str = "操作失败", code: int = 400, error_code: str = None, details: Dict = None) -> tuple:
        """错误响应"""
        response = {
            "success": False,
            "code": code,
            "message": message,
            "error_code": error_code,
            "details": details,
            "timestamp": datetime.now().isoformat(),
            "server_time": datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        }
        return jsonify(response), code
    
    @staticmethod
    def paginated(data: list, total: int, page: int, per_page: int, message: str = "查询成功") -> tuple:
        """分页响应"""
        total_pages = (total + per_page - 1) // per_page
        
        response = {
            "success": True,
            "code": 200,
            "message": message,
            "data": data,
            "pagination": {
                "total": total,
                "page": page,
                "per_page": per_page,
                "total_pages": total_pages,
                "has_next": page < total_pages,
                "has_prev": page > 1
            },
            "timestamp": datetime.now().isoformat(),
            "server_time": datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        }
        return jsonify(response), 200
    
    @staticmethod
    def validate_error(errors: Dict) -> tuple:
        """参数验证错误响应"""
        return ResponseFormatter.error(
            message="参数验证失败",
            code=422,
            error_code="VALIDATION_ERROR",
            details={"validation_errors": errors}
        )