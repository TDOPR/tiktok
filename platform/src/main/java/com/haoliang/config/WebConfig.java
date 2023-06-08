package com.haoliang.config;


import com.haoliang.common.config.AppParamProperties;
import com.haoliang.common.config.GlobalProperties;
import com.haoliang.common.filter.HttpInterceptor;
import com.haoliang.common.util.OsUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.web.servlet.MultipartConfigFactory;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.util.unit.DataSize;
import org.springframework.util.unit.DataUnit;
import org.springframework.web.context.request.async.TimeoutCallableProcessingInterceptor;
import org.springframework.web.servlet.config.annotation.*;

import javax.servlet.MultipartConfigElement;

@Configuration
@Slf4j
public class WebConfig implements WebMvcConfigurer {

    @Autowired
    private AppParamProperties appParamProperties;

    /**
     * 文件上传配置
     */
    @Bean
    public MultipartConfigElement multipartConfigElement() {
        MultipartConfigFactory factory = new MultipartConfigFactory();
        //单个文件大小限制
        factory.setMaxFileSize(DataSize.of(appParamProperties.getFileMaxSize(), DataUnit.MEGABYTES));
        //最大请求大小限制
        factory.setMaxRequestSize(DataSize.of(appParamProperties.getFileMaxSize(), DataUnit.MEGABYTES));
        // 设置文件上传的临时目录
        // factory.setLocation(picSavePath+ File.separator+"tmp");
        return factory.createMultipartConfig();
    }

    @Override
    public void addResourceHandlers(ResourceHandlerRegistry registry) {
        String filePath = GlobalProperties.getRootPath();
        if (OsUtil.isWindowOs()) {
            filePath = filePath.replaceAll("/", "\\\\");
        }
        String pathPatterns = appParamProperties.getVirtualPathPrefix() + "/**";
        registry.addResourceHandler(pathPatterns).addResourceLocations("file:" + filePath);
        log.info("资源映射加载 --- {} --> {} ", pathPatterns, filePath);
    }


    @Bean
    public TimeoutCallableProcessingInterceptor timeoutInterceptor() {
        return new TimeoutCallableProcessingInterceptor();
    }

    @Override
    public void configureAsyncSupport(AsyncSupportConfigurer configurer) {
        configurer.setDefaultTimeout(1000 * 60 * 5);
        configurer.registerCallableInterceptors(timeoutInterceptor());
    }

    @Override
    public void addCorsMappings(CorsRegistry registry) {
        registry.addMapping("/**")
                .allowedOriginPatterns("*")
                .allowedMethods("POST", "GET", "PUT", "OPTIONS", "DELETE")
                .maxAge(3600)
                .allowCredentials(true);
    }

    @Override
    public void addInterceptors(InterceptorRegistry interceptor) {
        interceptor.addInterceptor(new HttpInterceptor()).addPathPatterns("/**");
    }

}