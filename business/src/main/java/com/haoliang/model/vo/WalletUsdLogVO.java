package com.haoliang.model.vo;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.haoliang.common.util.NumberUtil;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;

/**
 * @author Dominick Li
 * @Description 流水
 * @CreateTime 2022/11/14 11:00
 **/
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class WalletUsdLogVO {


    /**
     * 流水日期
     */
    private LocalDateTime createTime;
    /**
     * 流水类型名称
     */
    private String name;

    /**
     * 金额
     */
    private BigDecimal amount;

    /**
     * 币种类型
     */
    private Integer coinId;

    /**
     * 提现中的状态
     */
    private Integer status;

    @JsonIgnore
    private Integer action;

    public String getAmount() {
        return (action == 1 ? "+ " : "- ")+NumberUtil.toPlainString(amount);

    }

    public String getCreateTime() {
        return createTime.toLocalDate().toString();
    }

    /**
     * 类型
     */
    @JsonIgnore
    private Integer type;


}
