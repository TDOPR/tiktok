package com.haoliang.model.dto;

import lombok.Data;

import javax.validation.constraints.NotNull;
import java.math.BigDecimal;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/12/7 14:59
 **/
@Data
public class UsdtWithdrawalDTO extends CheckVerificationDTO {

    /**
     * 金额
     */
    @NotNull
    private BigDecimal amount;

    /**
     * 区块链地址  USDT提现的时候需要
     */
    @NotNull
    private String address;

    /**
     * 提现网络类型
     */
    @NotNull
    private String networdName;
}
