package com.haoliang.model.dto;

import lombok.Data;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/11/14 11:15
 **/
@Data
public class CustomerWalletLogsDTO {

    /**
     * 用户ID
     */
    private Integer userId;

    /**
     * 流水类型
     */
    private Integer type;

}
