package com.haoliang.model.vo;

import lombok.Data;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/3/24 14:29
 **/
@Data
public class UserValidVO {

    /**
     * 用户Id
     */
    private Integer id;

    /**
     * 是否为有效用户
     */
    private Integer valid;

}
