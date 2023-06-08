package com.haoliang.common.annotation;

import java.lang.annotation.*;

/**
 * @author Dominick Li
 * @CreateTime 2021/6/22 10:33
 * @description Redis独享锁信息
 **/
@Target({ElementType.PARAMETER, ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface RedisLock {

    /**
     * 锁的名称
     */
    String name() default "";

    /**
     * 重试重获取锁的次数,默认0 不重试
     */
    int retry() default 0;

    /**
     * 占有锁的时间,避免程序宕机导致锁无法释放
     */
    int expired() default 60;

    /**
     * 使用完立即清除锁
     */
    boolean clear() default true;
}
