package com.haoliang.model.vo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;


/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/11/16 10:40
 **/
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class MyWalletsVO {

    /**
     * 货币信息
     */
    private CoinInfoVo coinInfo;

    /**
     * TTT账号余额
     */
    private String tttBalance;

    /**
     * USD账户余额
     */
    private String usdBalance;

    /**
     * 我的社区
     */
    private MyCommunityVO community;

    /**
     * 社区收益
     */
    private String communityBenefits;

    /**
     * 任务收益
     */
    private String taskBenefits;

    /**
     * 七天待领取的收益
     */
    private String toBeClaimed;

    /**
     * 是否支持法币
     */
    private Boolean supportFiat;

    /**
     * 支付问题联系的客服
     */
    private String customer;
}
