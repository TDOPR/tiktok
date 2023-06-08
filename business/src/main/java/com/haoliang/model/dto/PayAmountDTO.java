package com.haoliang.model.dto;

import lombok.Data;

import javax.validation.constraints.NotNull;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/11/9 18:09
 **/
@Data
public class PayAmountDTO {

    /**
     * 法币类型 1=印尼 2=越南 3=泰国
     */
    @NotNull
    private Integer type;

    /**
     * 代付的IDR
     * @required
     */
    @NotNull
    private Integer amount;

}
