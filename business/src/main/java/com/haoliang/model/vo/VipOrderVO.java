package com.haoliang.model.vo;

import com.haoliang.common.util.NumberUtil;
import com.haoliang.model.VipOrders;
import lombok.Data;

import java.math.BigDecimal;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/3/5 16:45
 **/
@Data
public class VipOrderVO {

    /**
     * 购买的vip等级
     */
    private Integer level;

    /**
     * 收益上限总量
     */
    private String total;

    /**
     * 剩余收益余量
     */
    private String allowance;

    /**
     * 已获取
     */
    private String acquired;

    public VipOrderVO(VipOrders vipOrders) {
        this.level = vipOrders.getLevel();
        this.total = vipOrders.getTotal().toPlainString();
        this.acquired = vipOrders.getTotal().subtract(vipOrders.getAllowance()).toPlainString();
        this.allowance = vipOrders.getAllowance().toPlainString();
    }

}
