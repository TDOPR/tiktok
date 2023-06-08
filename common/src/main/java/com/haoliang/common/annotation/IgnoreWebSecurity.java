package com.haoliang.common.annotation;

import java.lang.annotation.*;

/**
 * 忽略Security认证
 */
@Target({ElementType.TYPE,ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface IgnoreWebSecurity {
    String value() default "";

}
