package com.haoliang.listen;

import com.haoliang.config.WebSocketConfig;
import com.haoliang.netty.NettyServer;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.CommandLineRunner;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;

/**
 * @author Dominick Li
 * @description 容器加载后的初始化操作
 **/
@Component
@Slf4j
public class InitListen implements CommandLineRunner {

    @Resource
    private WebSocketConfig webSocketConfig;

    @Override
    public void run(String... args) throws Exception {
        new NettyServer().start(webSocketConfig);
    }

}
