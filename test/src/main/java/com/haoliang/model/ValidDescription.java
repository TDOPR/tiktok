package com.haoliang.model;

import org.hibernate.validator.constraints.Length;

import javax.validation.Valid;
import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

/**
 * @Description
 * @Author Dominick Li
 * @CreateTime 2022/10/21 15:00
 **/
public class ValidDescription {

//    @NotNull	不能是Null
//    @Null 必须是Null
//    @NotBlank	字符串不能为空、空字符串、全空格
//    @NotEmpty	带注解的元素不能是空，String类型不能为null，Array、Map不能为空，切size/length大于0
//    @Max 数值必须小于等于指定的最大值
//    @Min 数值必须大于等于指定的最小值
//    @Length(min=, max=)	字符串的长度是否在给定的范围之内
//    @Size(min=, max=)	验证对象（Array,Collection,Map,String）长度是否在给定的范围之内
//    @Email	带注解的元素必须是电子邮箱地址
//    @URL 字符串必须是一个URL
//    @AssertFalse boolean值必须为false
//    @AssertTrue	boolean值必须为true
//    @Pattern	带注解的元素必须符合指定的正则表达式


    @NotBlank
    private String userName;

    @NotBlank
    @Length(min = 6, max = 12)
    private String password;

    @Valid
    @NotNull(message = "不能为空")
    private UserDetail userDetail;

    public class UserDetail {

        @NotBlank
        private String address;

        @Max(60)
        @Min(18)
        @NotNull
        private Integer age;
    }


}
