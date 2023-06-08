package com.haoliang.model.dto;

import lombok.Data;

import javax.validation.constraints.NotNull;
import java.math.BigDecimal;

@Data
public class WalletDTO extends CheckVerificationDTO {

    /**
     * 金额
     * @required
     */
    @NotNull
    private BigDecimal amount;

}
