package com.haoliang.scheduled;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.haoliang.common.annotation.RedisLock;
import com.haoliang.common.config.AppParamProperties;
import com.haoliang.common.config.GlobalProperties;
import com.haoliang.common.enums.BooleanEnum;
import com.haoliang.constant.TiktokConfig;
import com.haoliang.enums.VipLevelEnum;
import com.haoliang.mapper.VipOrdersMapper;
import com.haoliang.model.FiatRechargeOrder;
import com.haoliang.model.FiatWithdrawOrder;
import com.haoliang.model.UpdateUserLevelJob;
import com.haoliang.model.VipOrders;
import com.haoliang.pay.enums.FiatTypeEnum;
import com.haoliang.pay.enums.PayStatusEnum;
import com.haoliang.service.*;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.time.LocalDateTime;
import java.util.List;
import java.util.concurrent.CountDownLatch;

/**
 * @author Dominick Li
 * @Description 常规任务定时任务类
 * @CreateTime 2022/11/25 11:57
 **/
@Component
@Slf4j
public class BusinessScheduledJob {

    @Autowired
    private UpdateUserLevelTaskService updateUserLevelTaskService;

    @Autowired
    private AsyncService asyncService;

    @Autowired
    private AppUserTaskService appUserTaskService;

    @Resource
    private VipOrdersMapper vipOrdersMapper;

    @Autowired
    private StatService statService;

    @Autowired
    private WalletsUsdRechargeService walletsUsdRechargeService;

    @Autowired
    private WalletsUsdWithdrawService walletsUsdWithdrawService;

    @Autowired
    private CurrencyExchangeRatesService currencyExchangeRatesService;

    @Autowired
    private FiatWithdrawOrderService fiatWithdrawOrderService;

    @Autowired
    private FiatRechargeOrderService fiatRechargeOrderService;

    @Autowired
    private WalletTttLogsService walletTttLogsService;

    @Resource
    private AppParamProperties appParamProperties;

    /**
     * 每分钟拉取下有没有任务需要处理
     */
    @Scheduled(fixedDelay = 10000)
    @RedisLock(name = TiktokConfig.UPDATE_USER_LEVEL_TASK_METHOD_NAME)
    public void updateUserLevelTask() {
        List<UpdateUserLevelJob> taskList;
        CountDownLatch countDownLatch;
        Integer taskSize;
        try {
            do {
                taskList = updateUserLevelTaskService.findTask(10);
                taskSize = taskList.size();
                if (taskSize > 0) {
                    countDownLatch = new CountDownLatch(taskSize);
                    for (UpdateUserLevelJob updateUserLevelJob : taskList) {
                        asyncService.updateUserLevelTask(updateUserLevelJob.getUserId(), countDownLatch);
                    }
                    countDownLatch.await();
                }
            } while (taskList.size() > 0);
        } catch (Exception e) {
            e.printStackTrace();
            log.error("updateUserLevelTask error:{}", e.getMessage());
        }
    }

    /**
     * 每天凌晨0点10清除零撸超时的t币流水
     */
    @Scheduled(cron = "0 10 0 * * ?")
    //@Scheduled(fixedDelay = 3000)
    public void clearExpired() {
        walletTttLogsService.clearExpired();
    }


    /**
     * 每天凌晨0点3分清除接单了没有做的任务
     */
    @Scheduled(cron = "0 3 0 * * ?")
    //@Scheduled(fixedDelay = 3000)
    public void clearAppUserTask() {
        appUserTaskService.clearAppUserTask();
    }

    /**
     * 每天凌晨5点统计昨天的数据
     */
    @Scheduled(cron = "0 0 3 * * ?")
    //@Scheduled(fixedDelay = 3000)
    public void statSum() {
        statService.stat();
    }

    /**
     * 每天凌晨 3点30分 更新零撸用户的奖励上限
     */
    //@Scheduled(fixedDelay = 30000)//测试每10秒执行一次
    @Scheduled(cron = "0 30 3 * * ?")
    @RedisLock
    public void updateZeroUserReward() {
        log.info("updateZeroUserReward .....");
        LocalDateTime now = LocalDateTime.now();
        LocalDateTime end = now.minusDays(TiktokConfig.ZERO_LEVEL_FLUSHED_SEARCH_END_DAY);
        List<VipOrders> vipOrdersList = vipOrdersMapper.selectList(new LambdaQueryWrapper<VipOrders>()
                .eq(VipOrders::getLevel, VipLevelEnum.ZERO.getLevel())
                .eq(VipOrders::getValid, BooleanEnum.TRUE.intValue())
                .le(VipOrders::getCreateTime, end)
        );
        VipLevelEnum zeroSecond = VipLevelEnum.ZERO_SECOND_MONTH;
        VipOrders newVipOrders;
        for (VipOrders vipOrders : vipOrdersList) {
            //设置成不可用
            vipOrders.setValid(BooleanEnum.FALSE.intValue());
            vipOrdersMapper.updateById(vipOrders);
            //清除零撸用户已接的单
            appUserTaskService.removeZeroUserTask(vipOrders.getUserId());
            log.info("零撸套餐id={},生效时间={} ,清空额度", vipOrders.getId(), vipOrders.getCreateTime());
            if (vipOrders.getTotal().compareTo(VipLevelEnum.ZERO.getOutOfSaleAmount()) == 0) {
                //如果是第一个月的,则生成第二个月的零撸套餐包
                log.info("零撸用户:{},发放第二个月的零撸套餐", vipOrders.getUserId());
                //第二个月时候更新套餐使用量
                newVipOrders = new VipOrders();
                newVipOrders.setTotal(zeroSecond.getOutOfSaleAmount());
                newVipOrders.setAllowance(zeroSecond.getOutOfSaleAmount());
                newVipOrders.setUserId(vipOrders.getUserId());
                newVipOrders.setLevel(VipLevelEnum.ZERO.getLevel());
                vipOrdersMapper.insert(newVipOrders);
            }
        }
    }

    /**
     * 自动审核任务
     */
    @Scheduled(fixedDelay = 60000)
    public void autoCheckTask() {
        appUserTaskService.checkTaskList();
    }

    /**
     * 区块链 每隔5秒拉取充值记录表任务状态
     */
    @Scheduled(fixedDelay = 5000)
    @RedisLock
    public void scanRechargeData() {
        walletsUsdRechargeService.scanRechargeList();
    }

    /**
     * 每隔30秒拉取提现表任务状态
     */
    @Scheduled(fixedDelay = 30000)
    @RedisLock
    public void scanWithdrawData() {
        walletsUsdWithdrawService.scanWithdrawData();
    }

    /**
     * 每隔5分钟拉取延迟任务状态表记录
     */
    //@Scheduled(fixedDelay = 30000)
    @Scheduled(cron = "0 0/5 * * * ?")
    @RedisLock
    public void scanDelayProxyPayData() {
        fiatWithdrawOrderService.scanDelayProxyPayData();
    }

    /**
     * 每隔5分钟更新一次汇率数据
     */
    @Scheduled(cron = "0 0/5 * * * ?")
//    @Scheduled(fixedDelay = 30000)
    @RedisLock
    public void updateExchangeRates() {
        if (GlobalProperties.isProdEnv()) {
            currencyExchangeRatesService.updateExchangeRates();
        }
    }

    /**
     * 每5分钟查询待支付的充值订单状态
     */
    @Scheduled(cron = "0 0/5 * * * ?")
    //@Scheduled(fixedDelay = 30000)
    @RedisLock
    public void queryRechargeOrder() {
        if (appParamProperties.isEnableQueryOrdersStatus()) {
            LocalDateTime now = LocalDateTime.now();
            //只查找最近
            now = now.minusMinutes(30);
            List<FiatRechargeOrder> fiatRechargeOrderList = fiatRechargeOrderService.list(
                    new LambdaQueryWrapper<FiatRechargeOrder>()
                            .eq(FiatRechargeOrder::getStatus, PayStatusEnum.NO_PAY.getStatus())
                            .ge(FiatRechargeOrder::getLastmodifiedTime, now)
            );
            Integer size = fiatRechargeOrderList.size();
            log.info("半小时内待支付订单数:{}", size);
            if (size == 0) {
                return;
            }

            //法币充值订单
            FiatTypeEnum fiatTypeEnum;
            CountDownLatch countDownLatch = new CountDownLatch(size);
            for (FiatRechargeOrder fiatRechargeOrder : fiatRechargeOrderList) {
                fiatTypeEnum = FiatTypeEnum.typeOf(fiatRechargeOrder.getFiatType());
                if (fiatTypeEnum == null) {
                    countDownLatch.countDown();
                    continue;
                }
                asyncService.queryRechargeOrder(fiatRechargeOrder, fiatTypeEnum, countDownLatch);
            }
        }
    }

    /**
     * 每10分钟查询待支付的充值订单状态
     */
    @Scheduled(cron = "0 0/10 * * * ?")
    //@Scheduled(fixedDelay = 30000)
    @RedisLock
    public void queryWithdrawOrder() {
        if (appParamProperties.isEnableQueryOrdersStatus()) {
            LocalDateTime now = LocalDateTime.now();
            //只查找最近1小时的提现记录
            now = now.minusHours(1);
            List<FiatWithdrawOrder> fiatWithdrawOrderList = fiatWithdrawOrderService.list(
                    new LambdaQueryWrapper<FiatWithdrawOrder>()
                            .eq(FiatWithdrawOrder::getStatus, PayStatusEnum.NO_PAY.getStatus())
                            .ge(FiatWithdrawOrder::getLastmodifiedTime, now)
            );
            Integer size = fiatWithdrawOrderList.size();
            log.info("1小时内提现待支付订单数:{}", size);
            if (size == 0) {
                return;
            }

            //法币充值订单
            FiatTypeEnum fiatTypeEnum;
            CountDownLatch countDownLatch = new CountDownLatch(size);
            for (FiatWithdrawOrder fiatWithdrawOrder : fiatWithdrawOrderList) {
                fiatTypeEnum = FiatTypeEnum.typeOf(fiatWithdrawOrder.getFiatType());
                if (fiatTypeEnum == null) {
                    countDownLatch.countDown();
                    continue;
                }
                asyncService.queryWithdrawOrder(fiatWithdrawOrder, fiatTypeEnum, countDownLatch);
            }
        }
    }

}
