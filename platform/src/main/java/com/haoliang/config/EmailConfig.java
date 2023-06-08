package com.haoliang.config;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/10/26 11:52
 **/
@Configuration
@ConfigurationProperties(prefix = "mail")
@Data
public class EmailConfig {

    /**
     * 邮箱服务的地址
     * qq邮箱=smtp.qq.com   腾讯企业邮箱=smtp.exmail.qq.com
     */
    private String host;

    /**
     * 邮箱服务使用的端口
     * qq邮箱=25  腾讯企业邮箱=465
     */
    private Integer port;

    /**
     * 设置ssl访问
     * qq邮箱=false  腾讯企业邮箱=true
     */
    private boolean enableSSL;

    /**
     * 发件人
     */
    private String username;

    /**
     * 客户端授权码
     */
    private String password;

    /**
     * 发件人的昵称
     */
    private String formName;

}

