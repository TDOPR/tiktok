package com.haoliang.common.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.support.ResourceBundleMessageSource;

import java.util.Locale;

/**
 * @author Dominick Li
 * @Description 国际化配置
 * @CreateTime 2022/12/8 16:19
 **/

@Configuration
public class LocaleConfig {

    @Bean
    public ResourceBundleMessageSource messageSource() {
        Locale.setDefault(Locale.CHINA);
        ResourceBundleMessageSource source = new ResourceBundleMessageSource();
        source.setBasenames("i18n/messages");
        source.setUseCodeAsDefaultMessage(true);
        source.setDefaultEncoding("UTF-8");
        return source;
    }

//    @Bean
//    public ReloadableResourceBundleMessageSource messageSource() {
//        Locale.setDefault(Locale.CHINA);
//        ReloadableResourceBundleMessageSource source = new ReloadableResourceBundleMessageSource();
//        source.setBasename("classpath:i18n/messages");
//        source.setUseCodeAsDefaultMessage(true);
//        source.setDefaultEncoding("UTF-8");
//        return source;
//    }

}
