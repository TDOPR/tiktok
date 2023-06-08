package com.haoliang.model.dto;

import lombok.Data;

import javax.validation.constraints.NotNull;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/4/16 19:22
 **/
@Data
public class ProxyPayAmountDTO extends CheckVerificationDTO {

    /**
     * 法币类型 1=印尼 2=越南 3=泰国
     */
    @NotNull
    private Integer type;

    /**
     * 提现的USD
     *
     * @required
     */
    @NotNull
    private Integer amount;

    /**
     * 持卡人姓名
     */
    @NotNull
    private String name;

    /**
     * 银行编码
     */
    @NotNull
    private String bankCode;

    /**
     * 银行卡号
     */
    @NotNull
    private String bankNumber;

}
