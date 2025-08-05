
// 在浏览器控制台运行此脚本来测试功能

console.log("🧪 开始快速功能测试...");

// 测试1: 检查全局函数是否存在
const functions = ['showQuickAnalysis', 'showRecommendations', 'showPortfolio', 'toggleRole'];
functions.forEach(func => {
    if (typeof window[func] === 'function') {
        console.log(`✅ ${func} 函数存在`);
    } else {
        console.log(`❌ ${func} 函数不存在`);
    }
});

// 测试2: 检查依赖
console.log('Bootstrap 可用:', typeof bootstrap !== 'undefined');
console.log('Socket.IO 可用:', typeof io !== 'undefined');

// 测试3: 测试通知功能
try {
    showNotification('info', '这是一个测试通知');
    console.log('✅ 通知功能正常');
} catch (error) {
    console.log('❌ 通知功能异常:', error);
}

// 测试4: 测试API调用
fetch('/api/status')
    .then(response => response.json())
    .then(data => {
        console.log('✅ API调用成功:', data);
    })
    .catch(error => {
        console.log('❌ API调用失败:', error);
    });

console.log("🏁 快速测试完成，请检查上述结果");
