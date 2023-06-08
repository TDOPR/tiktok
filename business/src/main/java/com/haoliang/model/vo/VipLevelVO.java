package com.haoliang.model.vo;

import com.haoliang.common.util.NumberUtil;
import lombok.AllArgsConstructor;
import lombok.Data;

import java.math.BigDecimal;
import java.util.List;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/3/5 12:05
 **/
@Data
@AllArgsConstructor
public class VipLevelVO {

    /**
     * 等级
     */
    private int level;

    /**
     * 可抵扣金额
     */
    private BigDecimal deductionsAmount;

    /**
     * 实际购买金额
     */
    private BigDecimal amount;

    /**
     * 开放状态
     */
    private boolean opening;

    /**
     * 已购买的套餐
     */
    private boolean has;

    /**
     * 不能购买的套餐标识
     */
    private boolean noBuy;

    public String getDeductionsAmount() {
        return NumberUtil.toPlainString(deductionsAmount);
    }

    public String getAmount() {
        return NumberUtil.toPlainString(amount);
    }

    /**
     * 显示的套餐文本信息
     */
    private List<String> textList;
}
