# 1.编辑文件
- 1.1.在项目的根路径下执行 mvn clean package 编译jar文件
- 1.2.把项目根目录下release/lib,release/conf目录复制到docker/server目录下
- 1.3.编译前端页面
```$xslt
cd vue-ui
npm i
npm build
```
把编译后dist目录下的文件复制到docker/nginx/html/目录下

# 2.使用server/platform-Dockerfile 构建后台服务镜像
docker build -t haoliang-platform:1.0.0 -f ./platform-Dockerfile .

# 3.使用server/monitor-Dockerfile 构建监控服务镜像
docker build -t haoliang-monitor:1.0.0 -f ./monitor-Dockerfile .

# 4.通过docker-compose.yml编排服务

安装docker-compose服务编排工具
```
sudo curl -L https://github.com/docker/compose/releases/download/1.16.1/docker-compose-`uname -s`-`uname -m` -o /usr/local/bin/docker-compose
#授权
sudo chmod +x /usr/local/bin/docker-compose
#查看版本
docker-compose version
```
编排服务需要的所有容器
```
cd docker
#添加 -d 在后台运行
docker-compose up  -d
```

命令 | 描述
--- | ---
docker-compose up | 根据docker-compose.yml构建容器 
docker-compose down | 移除所有容器

