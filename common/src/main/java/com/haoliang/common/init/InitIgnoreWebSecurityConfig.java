package com.haoliang.common.init;

import cn.hutool.core.date.TimeInterval;
import com.haoliang.common.annotation.IgnoreWebSecurity;
import com.haoliang.common.config.IgnoreSecurityPropetties;
import com.haoliang.common.util.StringUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.annotation.AnnotatedElementUtils;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.lang.reflect.Method;
import java.util.Map;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/12/20 17:44
 **/
@Slf4j
@Configuration
public class InitIgnoreWebSecurityConfig implements ApplicationContextAware {

    @Autowired
    private IgnoreSecurityPropetties ignoreSecurityPropetties;

    @Override
    public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
        TimeInterval timeInterval = new TimeInterval();
        timeInterval.start();

        Map<String, Object> beanMap = applicationContext.getBeansWithAnnotation(RestController.class);
        beanMap.forEach((k, v) -> {

            //Controllers配置的访问路径
            String baseUrl = "";
            Class<?> controllerClass = v.getClass();
            RequestMapping annotation = AnnotatedElementUtils.findMergedAnnotation(controllerClass, RequestMapping.class);

            //如果RequestMapping注解存在,使用RequestMapping里配置的路径名称
            if (annotation != null) {
                baseUrl = annotation.value().length > 0 ? annotation.value()[0] : "";
            }
            if (!baseUrl.startsWith("/")) {
                baseUrl = "/" + baseUrl;
            }

            //获取所有声明的方法
            Method[] allMethods = controllerClass.getMethods();
            IgnoreWebSecurity ignoreWebSecurity;
            PostMapping postMapping;
            GetMapping getMapping;
            String methodType;

            for (Method method : allMethods) {
                //判断方法是否使用忽略权限认证注解
                ignoreWebSecurity = AnnotatedElementUtils.findMergedAnnotation(method, IgnoreWebSecurity.class);
                if (ignoreWebSecurity != null) {
                    methodType = "";
                    String url = "";
                    if (StringUtil.isEmpty(ignoreWebSecurity.value())) {
                        //目前只适配了PostMapping和GetMapping注解,其它类型请自行扩展
                        postMapping = AnnotatedElementUtils.findMergedAnnotation(method, PostMapping.class);
                        if (postMapping != null) {
                            url = postMapping.value().length > 0 ? postMapping.value()[0] : "";
                            methodType = "post";
                        } else {
                            getMapping = AnnotatedElementUtils.findMergedAnnotation(method, GetMapping.class);
                            if (getMapping != null) {
                                url = getMapping.value().length > 0 ? getMapping.value()[0] : "";
                                methodType = "get";
                            }
                        }
                    } else {
                        url = ignoreWebSecurity.value();
                    }

                    if (url.trim().length() > 0) {
                        //拼接Controller的访问路径并去除多余的斜杆
                        url = (baseUrl + "/" + url).replaceAll("/+", "/");
                    } else {
                        url = baseUrl;
                    }
                    if ("post".equals(methodType)) {
                        ignoreSecurityPropetties.getPost().add(url);
                    } else if ("get".equals(methodType)) {
                        ignoreSecurityPropetties.getGet().add(url);
                    }
                }
            }
        });
        log.info("根据注解获取需要忽略认证的耗时：{}ms", timeInterval.interval());
    }
}
