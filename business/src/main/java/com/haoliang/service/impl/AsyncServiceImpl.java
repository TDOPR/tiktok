package com.haoliang.service.impl;

import cn.hutool.core.date.TimeInterval;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.haoliang.common.model.HttpResult;
import com.haoliang.enums.ProxyLevelEnum;
import com.haoliang.mapper.AppUserMapper;
import com.haoliang.mapper.TreePathMapper;
import com.haoliang.mapper.UpdateUserLevelTaskMapper;
import com.haoliang.model.AppUsers;
import com.haoliang.model.FiatRechargeOrder;
import com.haoliang.model.FiatWithdrawOrder;
import com.haoliang.pay.enums.FiatTypeEnum;
import com.haoliang.pay.enums.PayStatusEnum;
import com.haoliang.pay.id.IdPayUtils;
import com.haoliang.pay.th.ThPayUtils;
import com.haoliang.pay.vn.VnPayUtils;
import com.haoliang.service.AsyncService;
import com.haoliang.service.FiatRechargeOrderService;
import com.haoliang.service.FiatWithdrawOrderService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.concurrent.CountDownLatch;

/**
 * @author Dominick Li
 * @Description 异步任务处理
 * @CreateTime 2022/11/14 14:41
 **/
@Slf4j
@Service
public class AsyncServiceImpl implements AsyncService {

    @Resource
    private TreePathMapper treePathMapper;

    @Resource
    private UpdateUserLevelTaskMapper updateUserLevelTaskMapper;

    @Resource
    private AppUserMapper appUserMapper;

    @Autowired
    private FiatRechargeOrderService fiatRechargeOrderService;

    @Autowired
    private FiatWithdrawOrderService fiatWithdrawOrderService;


    @Override
    @Async("asyncExecutor")
    public void updateUserLevelTask(Integer userId, CountDownLatch countDownLatch) {
        try {
            TimeInterval timeInterval = new TimeInterval();
            timeInterval.start();
            log.info("start updateUserParentLevel userId={} ", userId);
            Integer generationUserNum = treePathMapper.getGenerationUserNum(userId);
            Integer itemUserNum = treePathMapper.getItemUserNum(userId);

            ProxyLevelEnum proxyLevelEnum = ProxyLevelEnum.getByItemUserNum(generationUserNum, itemUserNum);
            if (proxyLevelEnum != null) {
                UpdateWrapper<AppUsers> updateWrapper = Wrappers.update();
                updateWrapper.lambda()
                        .set(AppUsers::getLevel, proxyLevelEnum.getLevel())
                        .eq(AppUsers::getId, userId);
                appUserMapper.update(null, updateWrapper);
                log.info("updateUserParentLevel userId={},直推有效用户数={},团队有效用户数={} set level={} ,time={} ", userId, generationUserNum, itemUserNum, proxyLevelEnum.getLevel(), timeInterval.interval());
            }
            //更新完后删除当前任务
            updateUserLevelTaskMapper.deleteById(userId);
        } catch (Exception e) {
            log.error("updateUserLevelTask userId={} ,error:{}", userId, e.getMessage());
        } finally {
            countDownLatch.countDown();
        }
    }

    @Override
    @Async("asyncExecutor")
    public void queryRechargeOrder(FiatRechargeOrder fiatRechargeOrder, FiatTypeEnum fiatTypeEnum, CountDownLatch countDownLatch) {
        String orderNumStr = fiatRechargeOrder.getId().toString();
        try {
            HttpResult<Integer> result;
            if (fiatTypeEnum == FiatTypeEnum.ID) {
                result = IdPayUtils.queryPayOrder(orderNumStr);
            } else if (fiatTypeEnum == FiatTypeEnum.VN) {
                result = VnPayUtils.queryPayOrder(orderNumStr);
            } else {
                result = ThPayUtils.queryPayOrder(orderNumStr);
            }
            if (result.isSuccess() && result.getData() != null) {
                PayStatusEnum payStatusEnum = PayStatusEnum.statusOf(result.getData());
                //payStatusEnum=PayStatusEnum.SUCCESS;
                log.info("recharge orderNum={},status={}", orderNumStr, payStatusEnum.getName());
                boolean flag = payStatusEnum == PayStatusEnum.SUCCESS || payStatusEnum == PayStatusEnum.PAY_ERROR;
                if (flag) {
                    fiatRechargeOrderService.editOrders(fiatRechargeOrder, payStatusEnum.getStatus());
                }
            }
        } finally {
            countDownLatch.countDown();
        }
    }

    @Override
    public void queryWithdrawOrder(FiatWithdrawOrder fiatWithdrawOrder, FiatTypeEnum fiatTypeEnum, CountDownLatch countDownLatch) {
        String orderNumStr = fiatWithdrawOrder.getId().toString();
        try {
            HttpResult<Integer> result;
            if (fiatTypeEnum == FiatTypeEnum.ID) {
                result = IdPayUtils.queryProxyPayOrder(orderNumStr);
            } else if (fiatTypeEnum == FiatTypeEnum.VN) {
                result = VnPayUtils.queryProxyPayOrder(orderNumStr);
            } else {
                result = ThPayUtils.queryProxyPayOrder(orderNumStr);
            }
            if (result.isSuccess() && result.getData() != null) {
                PayStatusEnum payStatusEnum = PayStatusEnum.statusOf(result.getData());
                //payStatusEnum=PayStatusEnum.SUCCESS;
                log.info("withdraw orderNum={},status={}", orderNumStr, payStatusEnum.getName());
                boolean flag = payStatusEnum == PayStatusEnum.SUCCESS || payStatusEnum == PayStatusEnum.PAY_ERROR;
                if (flag) {
                    fiatWithdrawOrderService.editOrders(fiatWithdrawOrder, payStatusEnum.getStatus());
                }
            }
        }  finally {
            countDownLatch.countDown();
        }
    }
}
