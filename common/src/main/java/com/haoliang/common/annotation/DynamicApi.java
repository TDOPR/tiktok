package com.haoliang.common.annotation;

import java.lang.annotation.*;

/**
 * 控制接口是否开放
 */
@Target({ElementType.TYPE,ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface DynamicApi {

}
