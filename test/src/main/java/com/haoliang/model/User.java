package com.haoliang.model;

import lombok.Data;


@Data
public class User  {

    /**
     * 用户名称
     * @required
     */
    private String username;

    /**
     * 密码
     * @required
     */
    private String password;

    /**
     * 年龄
     */
    private Integer age;

}
