package com.haoliang.model.condition;

import lombok.Data;

/**
 * @author Dominick Li
 * @Description 客户查询条件
 * @CreateTime 2022/11/11 18:52
 **/
@Data
public class EvmTokenRechargeCondition {

    /**
     * 币种Id
     */
    private Integer coinId;

    /**
     * 用户邮箱号
     */
    private String email;

    /**
     * 钱包地址
     */
    private String address;

}
