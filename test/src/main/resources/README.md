# 1.测试smartdoc
相关的测试接口类在com.haoliang.controller.smartdoc包下,使用教程参考[https://dominick-li.blog.csdn.net/article/details/120824308]

通过执行mvn命令生成接口文档
```
 mvn smart-doc:markdown -Dfile.encoding=UTF-8  -pl :test -am 可生成最新的接口文档到 static目录下，
```
直接打开static/api.html查看文档或启动项目接口访问: http://127.0.0.1:9999/api.html

# 2.测试接口限流
需要启动Redis,然后配置文件把app.enableRateLimit设置为true开启限流功能
启动TestApplication类型后然后运行com.haoliang.controller.TestRateLimitController类的的main函数进行测试
如果需要使用分布式限流则需要把app.rateLimitModel设置为all

# 3.测试分布式锁
需要启动Redis,然后配置文件把app.enableRedisLock设置为true开启分布式锁功能
启动TestApplication类型后然后运行com.haoliang.controller.TestRedisLockController类的的main函数进行测试

# 4.测试防止表单重复提交
需要启动Redis,然后配置文件把app.enableRepeatSubmit设置为true开启防止表单重复提交功能
启动TestApplication类型后然后运行com.haoliang.controller.TestRepeatSubmitController类的的main函数进行测试

# 5.测试表单验证功能
启动TestApplication类型后然后运行com.haoliang.controller.TestValidController类的的main函数进行测试

# 6.测试excel的导入导出
6.1.Web测试
启动TestApplication类型后然后
访问http://127.0.0.1:9999/export测试导出excel接口
访问http://127.0.0.1:9999/import测试导入excel接口
6.2.Main函数测试
使用com.haoliang.test.TestExcel类中代码进行测试