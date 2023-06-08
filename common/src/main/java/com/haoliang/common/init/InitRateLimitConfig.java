package com.haoliang.common.init;

import com.haoliang.common.annotation.RateLimit;
import com.haoliang.common.aspect.RateLimitAspect;
import com.haoliang.common.constant.CacheKeyPrefixConstants;
import org.redisson.api.RRateLimiter;
import org.redisson.api.RateIntervalUnit;
import org.redisson.api.RateType;
import org.redisson.api.RedissonClient;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.bind.annotation.RestController;

import java.lang.reflect.Method;
import java.util.Map;

/**
 * @author Dominick Li
 * @CreateTime 2020/5/2 12:35
 * @description 初始化需要RateLimit限流的许可证数量
 **/
@Configuration
@ConditionalOnProperty(name = "app.enableRateLimit", havingValue = "true")
public class InitRateLimitConfig implements ApplicationContextAware {

    @Autowired
    private RedissonClient client;

    @Value("${app.rateLimitModel}")
    private String rateLimitModel;

    @Override
    public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
        Map<String, Object> beanMap = applicationContext.getBeansWithAnnotation(RestController.class);
        RateType rateType="all".equals(rateLimitModel) ? RateType.OVERALL : RateType.PER_CLIENT;
        beanMap.forEach((k, v) -> {
            Class<?> controllerClass = v.getClass();
            //获取所有声明的方法
            Method[] allMethods = controllerClass.getSuperclass().getDeclaredMethods();
            RateLimit rateLimit;
            RRateLimiter rRateLimiter;
            for (Method method : allMethods) {
                //判断方法是否使用了限流注解
                if (method.isAnnotationPresent(RateLimit.class)) {
                    //获取配置的限流量,实际值可以动态获取,配置key,根据key从配置文件获取
                    rateLimit = method.getAnnotation(RateLimit.class);
                    String key = rateLimit.limitKey();
                    if (key.equals("")) {
                        key = method.getName();
                    }
                    System.out.println("ratelimitKey:" + key + ",许可证数是" + rateLimit.value());
                    String limitKey= CacheKeyPrefixConstants.RATE_LIMIT+key;
                    //key作为key.value为具体限流量,传递到切面的map中
                    rRateLimiter = client.getRateLimiter(limitKey);
                    //访问模式    访问数 访问速率  访问时间
                    //访问模式 RateType.PER_CLIENT=单实例共享     RateType.OVERALL=所有实例共享(分布式限流)
                    rRateLimiter.trySetRate(rateType, rateLimit.value(), rateLimit.time(), RateIntervalUnit.SECONDS);
                    RateLimitAspect.rateLimitMap.put(key, rRateLimiter);
                }
            }
        });
    }
}
