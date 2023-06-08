package com.haoliang.common.aspect;

import com.haoliang.common.config.GlobalProperties;
import com.haoliang.common.model.JsonResult;
import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.stereotype.Component;


/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/4/12 11:20
 **/
@Component
@Aspect
@Slf4j
public class DynamicApiAspect {

    @Pointcut("@annotation(com.haoliang.common.annotation.DynamicApi)")
    public void redisLockPointcut() {
    }

    @Around("redisLockPointcut()")
    public Object doAround(ProceedingJoinPoint proceedingJoinPoint) throws Throwable {
        if (GlobalProperties.isProdEnv()) {
            return JsonResult.failureResult("正式环境测试接口暂未开放!");
        }
        return proceedingJoinPoint.proceed();
    }

}
