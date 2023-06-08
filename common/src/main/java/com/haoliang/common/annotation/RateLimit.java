package com.haoliang.common.annotation;

import java.lang.annotation.*;

/**
 * @Description RateLimit限流注解
 * @Author Dominick Li
 * @Date 2019/11/1 19:00
 **/
@Target({ElementType.PARAMETER, ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface RateLimit {

    /**
     * 限流的方法名
     */
    String limitKey() default "";

    /**
     * 默认设置为1秒
     */
    int time() default  1;

    /**
     * 发放的许可证数量
     */
    int value()  default 5;
}
