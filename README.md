# 1.项目模块描述

模块 | 描述      | 使用端口 
--- |---------| --- 
test | 测试代码和样例 | 9999 
business | 业务代码模块  |  
common | 工具代码模块  |   
platform | 应用入口    | 9090


resources目录下文件描述

资源目录 | 描述
--- | --- 
docker | dockerfile目录 
release | 应用发布目录 
sql | 初始化脚本目录 

# 2.启动后台项目
-  把platform模块下resources/init.sql中数据插入到数据库中
 - 修改platform模块下resource/applicaition.yml中的数据库连接信息
```$xslt
    url: jdbc:mysql://localhost:3306/tiktok
    username: root
    password: 123456
```

# 3.部署相关描述

## 3.1.依赖软件

名称 | 版本 | 描述
--- | --- | ---
nginx | 1.14 | admin管理端web部署使用
mysql | 5.7.41 | 服务使用
redis | 4.0.9 | 缓存凭证
jdk |  jdk8 | 服务使用的jdk版本

## 3.2.部署目录文件描述
目录名称 | 描述 
--- | --- 
bin | 启动脚本目录
conf | 配置文件存放目录
conf.logback-spring.xml | &nbsp; &nbsp;日志输出配置文件
conf.application.yml | &nbsp; &nbsp; 应用配置文件
conf.nginx.conf | &nbsp; &nbsp; 正式环境nginx配置
lib |  服务依赖的maven依赖
logs | 服务产生的日志
ssl | 证书文件存放目录
web | 管理端页面部署地址
data | 业务相关文件存放目录
data.taskImage | &nbsp; &nbsp; 做单截图文件存储路径
data.banner | &nbsp; &nbsp; app首页banner图
data.html | &nbsp; &nbsp; app首页打开的网页链接存放地址
data.pdf  | &nbsp; &nbsp; 用户协议和隐私政策存放路径
data.video | &nbsp; &nbsp; 请客吃饭视频存储路径
data.userImage | &nbsp; &nbsp; 用户头像存储路径

# 4.应用配置文件描述
ssl证书配置
```yml
server:
  #配置ssl证书
  ssl:
    key-store: /home/tiktok/ssl/api/api.p12
    key-store-type: PKCS12
    key-store-password: tiktokapi
```
redis配置
```yml
spring:
  redis:
    #redis服务器的ip
    host: 127.0.0.1
    port: 6379
    password: 
```
数据库配置
```yml
spring:
  datasource:
    driver-class-name: com.mysql.cj.jdbc.Driver
    url: jdbc:mysql://localhost:3306/tiktok?createDatabaseIfNotExist=true&useSSL=false&serverTimezone=GMT%2b8&characterEncoding=utf8&connectTimeout=1000&socketTimeout=15000&autoReconnect=true&cachePrepStmts=true&useServerPrepStmts=true&zeroDateTimeBehavior=CONVERT_TO_NULL
    username: root
    password: 123456
```
邮箱账号授权码配置
```yml
mail:
  username: tiktokguildvip@gmail.com
  password: kjcvlkscuchizjwy
```

logback配置文件目录指定
```yml
logging:
  config: conf/logback-spring.xml
```

业务文件存储根路径配置
```yml
app:
  rootPath: /home/tiktok/data/
```

服务器访问地址配置
```yml
app:
  #服务器Ip
  ip: 192.168.2.199
  #服务器地址 如果使用了证书 http需要改成https
  serverIp: http://${app.ip}:${server.port}
```

# 5.启动服务
启动服务(正式环境 不输出nohup日志)

    /home/tiktok/bin/server.sh start

启动服务(测试环境 输出nohup日志)

    /home/tiktok/bin/server.sh dev

停止服务

    /home/tiktok/bin/server.sh stop

# 6.查看日志

查看info日志

    tail -f /home/tiktok/logs/server-info.log

查看error日志

    tail -f /home/tiktok/logs/server-error.log

查看nohup命令产生的日志(包含Info和error信息)

    tail -f /home/tiktok/logs/console-out.log  



