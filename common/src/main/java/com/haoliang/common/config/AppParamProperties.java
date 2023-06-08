package com.haoliang.common.config;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

@Data
@Configuration
@ConfigurationProperties(prefix = "app")
public class AppParamProperties {

    private String serverIp;

    private String virtualPathPrefix;

    private Integer fileMaxSize;

    private String version;

    private Integer delayTime;

    private String registerUrl;

    private Integer delayPayDay;

    private boolean enableQueryOrdersStatus;

}
