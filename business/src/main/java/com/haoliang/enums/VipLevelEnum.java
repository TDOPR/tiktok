package com.haoliang.enums;

import com.haoliang.common.model.ThreadLocalManager;
import com.haoliang.common.util.MessageUtil;
import com.haoliang.constant.TiktokConfig;
import com.haoliang.model.vo.VipLevelVO;
import com.haoliang.utils.BigDecimalUtils;
import lombok.Getter;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/3/5 10:23
 **/
@Getter
public enum VipLevelEnum {

    ZERO(0, 5, new BigDecimal("15")),
    ZERO_SECOND_MONTH(0, 5, new BigDecimal("7.5")),
    ONE(1, new BigDecimal("50"), new BigDecimal("2"), new BigDecimal("75"), 5, true),
    TWO(2, new BigDecimal("150"), new BigDecimal("3"), new BigDecimal("65"), 10, true),
    THREE(3, new BigDecimal("500"), new BigDecimal("4"), new BigDecimal("55"), 15, true),
    FOUR(4, new BigDecimal("1500"), new BigDecimal("5"), new BigDecimal("45"), 20, true),
    FIVE(5, new BigDecimal("3000"), new BigDecimal("6"), new BigDecimal("45"), 25, true),
    SIX(6, new BigDecimal("6000"), new BigDecimal("7"), new BigDecimal("45"), 30, false),
    Seven(7, new BigDecimal("9000"), new BigDecimal("8"), new BigDecimal("45"), 35, false);

    VipLevelEnum(Integer level, BigDecimal amount, BigDecimal num, BigDecimal day, Integer taskNumLimit, boolean opening) {
        this.level = level;
        this.amount = amount;
        this.num = num;
        this.day = day;
        this.outOfSaleAmount = amount.multiply(num);
        this.earningsLimit = BigDecimalUtils.divideSaveTwoDecimal(amount, day);
        this.taskNumLimit = taskNumLimit;
        this.earnings = BigDecimalUtils.divideSaveTwoDecimal(earningsLimit, new BigDecimal(taskNumLimit));
        this.opening = opening;
    }

    VipLevelEnum(Integer level, int taskNumLimit, BigDecimal outOfSaleAmount) {
        this.level = level;
        this.taskNumLimit = taskNumLimit;
        //每天的收益上限等于可获取的除以总金额
        this.earningsLimit = BigDecimalUtils.divideSaveTwoDecimal(outOfSaleAmount, new BigDecimal("30"));
        this.outOfSaleAmount = outOfSaleAmount;
        //每单的收益等于 单量
        this.earnings = BigDecimalUtils.divideSaveTwoDecimal(earningsLimit, new BigDecimal(taskNumLimit));
        this.opening = true;
    }

    /**
     * VIP等级
     */
    private Integer level;

    /**
     * 购买金额
     */
    private BigDecimal amount;

    /**
     * 出局倍率
     */
    private BigDecimal num;

    /**
     * 回本天数
     */
    private BigDecimal day;

    /**
     * 出局金额(收益上限)=购买金额*出局倍率
     */
    private BigDecimal outOfSaleAmount;

    /**
     * 每天任务收益上限
     */
    private BigDecimal earningsLimit;

    /**
     * 每天的接单上限
     */
    private Integer taskNumLimit;

    /**
     * 每单的收益
     */
    private BigDecimal earnings;

    /**
     * 是否开放
     */
    private boolean opening;

    /**
     * 获取页面显示的Vip套餐列表
     *
     * @param level 当前用户的等级
     * @return
     */
    public static List<VipLevelVO> getVipList(Integer level) {
        List<VipLevelVO> list = new ArrayList<>();
        boolean has, noBuy;
        for (VipLevelEnum vipLevelEnum : values()) {
            has = false;
            noBuy = false;
            if (vipLevelEnum.level.equals(VipLevelEnum.ZERO.getLevel())) {
                continue;
            }
            if (vipLevelEnum.level <= level) {
                has = true;
                if (vipLevelEnum.level < level) {
                    noBuy = true;
                }
            }
            list.add(new VipLevelVO(vipLevelEnum.level, BigDecimal.ZERO, vipLevelEnum.amount, vipLevelEnum.opening, has, noBuy, vipLevelEnum.opening ? vipLevelEnum.getTextList() : Collections.emptyList()));
        }
        return list;
    }

    public static List<VipLevelVO> getVipListByZero(BigDecimal usd) {
        List<VipLevelVO> list = new ArrayList<>();
        BigDecimal usdResult;
        for (VipLevelEnum vipLevelEnum : values()) {
            if (vipLevelEnum == ZERO || vipLevelEnum == ZERO_SECOND_MONTH) {
                continue;
            }
            if (vipLevelEnum.level.equals(VipLevelEnum.ONE.getLevel())) {
                usdResult = TiktokConfig.V1_USD.compareTo(usd) > 0 ? usd : TiktokConfig.V1_USD;
            } else if (vipLevelEnum.level.equals(VipLevelEnum.TWO.getLevel())) {
                usdResult = TiktokConfig.V2_USD.compareTo(usd) > 0 ? usd : TiktokConfig.V2_USD;
            } else {
                usdResult = usd;
            }
            if (usdResult.compareTo(vipLevelEnum.getAmount()) > 0) {
                //抵扣价格大于vip的价格则购买vip免费
                usdResult = vipLevelEnum.getAmount();
            }
            list.add(new VipLevelVO(vipLevelEnum.level, usdResult, vipLevelEnum.amount.subtract(usdResult), vipLevelEnum.opening, false, false, vipLevelEnum.opening ? vipLevelEnum.getTextList() : Collections.emptyList()));
        }
        return list;
    }

    /**
     * 获取国际化文本展示的内容
     *
     * @return
     */
    private List<String> getTextList() {
        return Arrays.asList(
                MessageUtil.get("vip.text1", new Object[]{amount.toPlainString()}, ThreadLocalManager.getLanguage()),
                MessageUtil.get("vip.text2", new Object[]{num, outOfSaleAmount.toPlainString()}, ThreadLocalManager.getLanguage()),
                MessageUtil.get("vip.text3", new Object[]{taskNumLimit, earningsLimit.toPlainString()}, ThreadLocalManager.getLanguage())
        );
    }

    public static VipLevelEnum getByLevel(Integer level) {
        for (VipLevelEnum vipLevelEnum : values()) {
            if (vipLevelEnum.getLevel().equals(level)) {
                return vipLevelEnum;
            }
        }
        return null;
    }

}
