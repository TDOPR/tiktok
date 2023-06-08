package com.haoliang.service;


import com.haoliang.model.FiatRechargeOrder;
import com.haoliang.model.FiatWithdrawOrder;
import com.haoliang.pay.enums.FiatTypeEnum;

import java.util.concurrent.CountDownLatch;

/**
 * @author Dominick Li
 * @CreateTime 2021/7/22 13:45
 * @description 异步调用服务
 **/
public interface AsyncService {

    /**
     * 等级用户的代理商等级
     * @param userId
     * @param countDownLatch
     */
    void updateUserLevelTask(Integer userId, CountDownLatch countDownLatch);

    /**
     * 查询法币充值待支付订单
     * @param fiatRechargeOrder  充值记录
     * @param fiatTypeEnum 枚举类型
     * @param countDownLatch
     */
    void queryRechargeOrder(FiatRechargeOrder fiatRechargeOrder, FiatTypeEnum fiatTypeEnum,CountDownLatch countDownLatch);

    /**
     * 查询法币提现待支付订单
     * @param fiatWithdrawOrder  充值记录
     * @param fiatTypeEnum 枚举类型
     * @param countDownLatch
     */
    void queryWithdrawOrder(FiatWithdrawOrder fiatWithdrawOrder, FiatTypeEnum fiatTypeEnum, CountDownLatch countDownLatch);
}
