package com.haoliang.utils;

import com.haoliang.constant.TiktokConfig;
import com.haoliang.enums.VipLevelEnum;
import com.haoliang.model.VipOrders;

import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 * @author Dominick Li
 * @Description 计算每单可收益的USD
 * @CreateTime 2023/3/22 15:23
 **/
public class EarningUtil {

    /**
     * 根据余额计算每单需要扣减的usd
     *
     * @param hasLevel  当前等级
     * @param vipOrders 已购vip套餐信息
     * @return
     */
    public static BigDecimal getEarningByAllowance(VipLevelEnum hasLevel, VipOrders vipOrders) {
        BigDecimal oneEarning = getEarning(hasLevel, vipOrders);
        //当余额不足以做一单的时候,用仅有的金额去计算
        return vipOrders.getAllowance().compareTo(oneEarning) < 0 ? vipOrders.getAllowance() : oneEarning;
    }

    /**
     * 根据余额计算每单需要扣减的usd
     *
     * @param hasLevel  当前等级
     * @param vipOrders 已购vip套餐信息
     * @return
     */
    public static BigDecimal getEarningByFrozen(VipLevelEnum hasLevel, VipOrders vipOrders) {
        BigDecimal oneEarning = getEarning(hasLevel, vipOrders);
        //当余额不足以做一单的时候,用仅有的金额去计算
        return vipOrders.getFrozenAmount().compareTo(oneEarning) < 0 ? vipOrders.getFrozenAmount() : oneEarning;
    }

    /**
     * 计算每单的收益
     * @param hasLevel
     * @param vipOrders
     * @return
     */
    private static BigDecimal getEarning(VipLevelEnum hasLevel, VipOrders vipOrders) {
        VipLevelEnum vipLevelEnum;
        if (vipOrders.getLevel().equals(VipLevelEnum.ZERO.getLevel())) {
            vipLevelEnum = vipOrders.getTotal().compareTo(VipLevelEnum.ZERO.getOutOfSaleAmount())==0? VipLevelEnum.ZERO : VipLevelEnum.ZERO_SECOND_MONTH;
        } else {
            vipLevelEnum = VipLevelEnum.getByLevel(vipOrders.getLevel());
        }
        //如果是最高等级则取用定义的单收益,否则根据以 VIP日收益上限/最高等级VIP的每天做单量上限 计算
        return hasLevel.getLevel().equals(vipOrders.getLevel()) ? hasLevel.getEarnings() : vipLevelEnum.getEarningsLimit().divide(new BigDecimal(hasLevel.getTaskNumLimit()), TiktokConfig.NUMBER_OF_DIGITS, RoundingMode.FLOOR);
    }

}
