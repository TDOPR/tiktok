package com.haoliang.common.model.dto;

import lombok.Data;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/12/8 17:50
 **/
@Data
public class HeaderInfo {

    /**
     * 用户的token信息
     */
    private String token;

    /**
     * 国际化语言包名称
     */
    private String language;
}
