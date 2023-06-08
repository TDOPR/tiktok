package com.haoliang.config;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

@Data
@Configuration
@ConfigurationProperties(prefix = "websocket")
public class WebSocketConfig {

    private Integer port;

    private String adminwsPath;

    private boolean ssl;

    private String KeyStore;

    private String keyPassword;

}
