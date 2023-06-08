package com.haoliang.model;

import lombok.Data;
import org.hibernate.validator.constraints.Length;

import javax.validation.constraints.Min;
import javax.validation.constraints.NotEmpty;

/**
 * @Description
 * @Author Dominick Li
 **/
@Data
public class ValidSave {

    @NotEmpty(message = "不能为空")
    private String username;

    @NotEmpty(message = "不能为空")
    @Length(min = 6, max = 12,message = "长度需要在6~12范围内")
    private String password;

    @Min(value = 18, message = "最低不能低于18")
    private Integer age;
}
