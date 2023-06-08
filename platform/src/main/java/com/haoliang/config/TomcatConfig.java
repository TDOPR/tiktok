package com.haoliang.config;

import lombok.extern.slf4j.Slf4j;
import org.apache.catalina.connector.Connector;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.web.embedded.tomcat.TomcatServletWebServerFactory;
import org.springframework.boot.web.servlet.server.ServletWebServerFactory;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/4/24 17:45
 **/
@Slf4j
@ConditionalOnProperty(name = "websocket.ssl", havingValue = "true")
@Configuration
public class TomcatConfig {

    @Value("${app.callBackPort}")
    private Integer callBackPort;

    @Bean
    public ServletWebServerFactory servletContainer() {
        log.info("使用证书后开启http端口:{}",callBackPort);
        TomcatServletWebServerFactory tomcat = new TomcatServletWebServerFactory();
        tomcat.addAdditionalTomcatConnectors(createHTTPConnector());
        return tomcat;
    }

    private Connector createHTTPConnector() {
        Connector connector = new Connector("org.apache.coyote.http11.Http11NioProtocol");
        connector.setScheme("http");
        connector.setSecure(false);
        connector.setPort(callBackPort);
        return connector;
    }
}
