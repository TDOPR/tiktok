package com.haoliang.aspect;

import com.haoliang.common.annotation.OperationLog;
import com.haoliang.common.service.SysOperationLogService;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.AfterReturning;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.lang.reflect.Method;

/**
 * @author Dominick Li
 * @description 操作日志切入点
 **/

@Aspect
@Component
public class OperationLogAspect {

    @Resource
    private SysOperationLogService saveOperationLog;

    /**
     * 日志切入点
     */
    @Pointcut("@annotation(com.haoliang.common.annotation.OperationLog)")
    public void operationLogPointCut() {
    }

    @AfterReturning(pointcut = "operationLogPointCut()")
    public void doAfter(JoinPoint joinPoint) {
        String methodName = joinPoint.getSignature().getName();
        Method method = currentMethod(joinPoint, methodName);
        OperationLog log = method.getAnnotation(OperationLog.class);
        String module = log.module();
        String description = log.description();
        saveOperationLog.saveOperationLog(joinPoint, methodName, module, description);
    }

    private Method currentMethod(JoinPoint joinPoint, String methodName) {
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
