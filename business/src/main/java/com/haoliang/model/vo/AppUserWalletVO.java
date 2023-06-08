package com.haoliang.model.vo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;

/**
 * @Description
 * @Author Dominick Li
 * @CreateTime 2023/3/26 14:48
 **/
@Data
@Builder
@AllArgsConstructor
public class AppUserWalletVO {

    /**
     * 总充值
     */
    private String recharge;
    /**
     * 总提现
     */
    private String withdraw;
    /**
     * 总消费
     */
    private String consume;
    /**
     * usd余额
     */
    private String usd;
    /**
     * ttt余额
     */
    private String ttt;

}
