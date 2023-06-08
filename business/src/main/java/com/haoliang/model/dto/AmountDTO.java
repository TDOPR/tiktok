package com.haoliang.model.dto;

import lombok.Data;

import javax.validation.constraints.NotNull;
import java.math.BigDecimal;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/11/9 18:09
 **/
@Data
public class AmountDTO {

    /**
     * 金额
     * @required
     */
    @NotNull
    private BigDecimal amount;

}
