package com.haoliang.common.aspect;

import com.haoliang.common.annotation.RedisLock;
import com.haoliang.common.constant.CacheKeyPrefixConstants;
import com.haoliang.common.util.StringUtil;
import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.core.StringRedisTemplate;

import java.lang.reflect.Method;
import java.util.concurrent.TimeUnit;

/**
 * @author Dominick Li
 * @CreateTime 2020/3/12 23:24
 * @description 获取分布式锁切入点
 **/
@Aspect
@Slf4j
@Configuration
@ConditionalOnProperty(name = "app.enableRedisLock", havingValue = "true")
public class RedisLockAspect {

    @Autowired
    private StringRedisTemplate stringRedisTemplate;

    @Pointcut("@annotation(com.haoliang.common.annotation.RedisLock)")
    public void redisLockPointcut() {
    }

    @Around("redisLockPointcut()")
    public Object doAround(ProceedingJoinPoint proceedingJoinPoint) throws Throwable {
        Method method = currentMethod(proceedingJoinPoint);
        RedisLock redisLock = method.getAnnotation(RedisLock.class);
        String methodName = redisLock.name();
        if (StringUtil.isBlank(methodName)) {
            //如果注解里没有设置锁的名称,默认使用方法的名称
            methodName = method.getName();
        }
        String cacheKey = CacheKeyPrefixConstants.DISTRIBUTED_LOCK + methodName;
        boolean flag = true;
        int retryCount = redisLock.retry();
        do {
            if (!flag && retryCount > 0) {
                Thread.sleep(1000L);
                retryCount--;
            }
            flag = stringRedisTemplate.opsForValue().setIfAbsent(cacheKey, "1", redisLock.expired(), TimeUnit.SECONDS);
            if (flag) {
                //获取到锁结束循环
                break;
            }
            //根据配置的重试次数,执行n次获取锁的方法,默认不重试
        } while (retryCount > 0);
        //result为连接点的放回结果
        Object result = null;
        if (flag) {
            try {
                result = proceedingJoinPoint.proceed();
            } catch (Throwable e) {
                /*异常通知方法*/
                log.error("异常通知方法>目标方法名{},异常为：{}", method.getName(), e);
            } finally {
                if (redisLock.clear()) {
                    stringRedisTemplate.delete(cacheKey);
                }
            }
            return result;
        }
        log.error("执行:{} 未获取锁,重试次数:{}", method.getName(), redisLock.retry());
        return null;
    }

    private Method currentMethod(JoinPoint joinPoint) {
        String methodName = joinPoint.getSignature().getName();
        //获取目标类的所有方法，找到当前要执行的方法
        Method[] methods = joinPoint.getTarget().getClass().getMethods();
        Method resultMethod = null;
        for (Method method : methods) {
            if (method.getName().equals(methodName)) {
                resultMethod = method;
                break;
            }
        }
        return resultMethod;
    }
}
