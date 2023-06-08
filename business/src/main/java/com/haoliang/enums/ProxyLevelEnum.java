package com.haoliang.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.math.BigDecimal;


/**
 * @author Dominick Li 社区
 * @Description 代理商团队分红枚举
 * @CreateTime 2022/11/1 10:30
 **/
@Getter
@AllArgsConstructor
public enum ProxyLevelEnum {

    //代理商等级基于用户小团队业绩总和去判断是否有资格升级
    ONE(1, 8, 80, new BigDecimal("0.05"), new BigDecimal("0.05"), new BigDecimal("0.001")),
    TWO(2, 10, 240, new BigDecimal("0.08"), new BigDecimal("0.07"), new BigDecimal("0.002")),
    THREE(3, 12, 660, new BigDecimal("0.11"), new BigDecimal("0.09"), new BigDecimal("0.003")),
    FOUR(4, 14, 1500, new BigDecimal("0.14"), new BigDecimal("0.1"), new BigDecimal("0.004")),
    FIVE(5, 16, 3000, new BigDecimal("0.17"), new BigDecimal("0.1"), new BigDecimal("0.005")),
    SIX(6, 18, 6000, new BigDecimal("0.2"), new BigDecimal("0.1"), new BigDecimal("0.006")),
    SEVEN(7, 20, 12000, new BigDecimal("0.25"), new BigDecimal("0.1"), new BigDecimal("0.007")),
    ;

    /**
     * 代理商等级
     */
    private Integer level;

    /**
     * 成为代理商条件的直推用户数
     */
    private Integer generationUserNum;

    /**
     * 成为代理商条件的团队用户数
     */
    private Integer itemUserNum;

    /**
     * 收益比例
     */
    private BigDecimal incomeRatio;

    /**
     * 平级比例
     */
    private BigDecimal lateralIncomeRatio;

    /**
     * 获取团队日持币量比例
     */
    private BigDecimal holdingsRatio;

    public static ProxyLevelEnum getByLevel(Integer level) {
        for (ProxyLevelEnum proxyLevelEnum : values()) {
            if (proxyLevelEnum.getLevel().equals(level)) {
                return proxyLevelEnum;
            }
        }
        return null;
    }

    /**
     * 用户画像显示的标签名称
     */
    public String getLabelName() {
        return level + "星用户";
    }

    /**
     * 根据团队规模获取代理商等级对象
     *
     * @param generationUserNum 直推用户量
     * @param itemUserNum       团队用户量
     * @return 代理商等级对象
     */
    public static ProxyLevelEnum getByItemUserNum(Integer generationUserNum, Integer itemUserNum) {
        if (generationUserNum >= SEVEN.getGenerationUserNum() && itemUserNum >= SEVEN.getItemUserNum()) {
            return SEVEN;
        } else if (generationUserNum >= SIX.getGenerationUserNum() && itemUserNum >= SIX.getItemUserNum()) {
            return SIX;
        } else if (generationUserNum >= FIVE.getGenerationUserNum() && itemUserNum >= FIVE.getItemUserNum()) {
            return FIVE;
        } else if (generationUserNum >= FOUR.getGenerationUserNum() && itemUserNum >= FOUR.getItemUserNum()) {
            return FOUR;
        } else if (generationUserNum >= THREE.getGenerationUserNum() && itemUserNum >= THREE.getItemUserNum()) {
            return THREE;
        } else if (generationUserNum >= TWO.getGenerationUserNum() && itemUserNum >= TWO.getItemUserNum()) {
            return TWO;
        } else if (generationUserNum >= ONE.getGenerationUserNum() && itemUserNum >= ONE.getItemUserNum()) {
            return ONE;
        }
        return null;
    }

}
