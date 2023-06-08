package com.haoliang.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.haoliang.common.enums.ReturnMessageEnum;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.model.ThreadLocalManager;
import com.haoliang.common.util.JwtTokenUtil;
import com.haoliang.constant.TiktokConfig;
import com.haoliang.enums.FlowingActionEnum;
import com.haoliang.enums.TttLogTypeEnum;
import com.haoliang.mapper.FreezeProxyLogsMapper;
import com.haoliang.model.AppUsers;
import com.haoliang.model.FreezeProxyLogs;
import com.haoliang.model.VipOrders;
import com.haoliang.model.WalletTttLogs;
import com.haoliang.service.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/3/10 17:35
 **/
@Service
public class FreezeProxyLogsServiceImpl extends ServiceImpl<FreezeProxyLogsMapper, FreezeProxyLogs> implements FreezeProxyLogsService {

    @Autowired
    private AppUserService appUserService;

    @Autowired
    private WalletsService walletsService;

    @Autowired
    private VipOrdersService vipOrdersService;

    @Autowired
    private WalletTttLogsService walletTttLogsService;

    @Autowired
    private KLineDataService kLineDataService;


    @Override
    public void insertLogs(Integer userId, TttLogTypeEnum type, BigDecimal amount) {
        this.save(FreezeProxyLogs.builder()
                .createTime(LocalDate.now().minusDays(1))
                .userId(userId)
                .amount(amount)
                .type(type.getValue())
                .build());
    }

    @Override
    public BigDecimal getRecentOneWeek(Integer userId) {
        List<FreezeProxyLogs> freezeProxyLogsList = getOneWeakData(userId);
        BigDecimal result = BigDecimal.ZERO;
        for (FreezeProxyLogs freezeProxyLogs : freezeProxyLogsList) {
            result = result.add(freezeProxyLogs.getAmount());
        }
        return result;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public JsonResult receiveReward() {
        Integer userId = JwtTokenUtil.getUserIdFromToken(ThreadLocalManager.getToken());
        AppUsers appUsers = appUserService.selectColumnsByUserId(userId, AppUsers::getVipLevel);
        List<VipOrders> ordersList = vipOrdersService.findByUserIdOrderByLevelAes(userId, appUsers.getVipLevel());

        if (ordersList.size() == 0) {
            //返回错误信息 提示套餐余额不足
            return JsonResult.failureResult(ReturnMessageEnum.VIP_NOT_PURCHASED);
        }

        //找到待发放奖励的记录,把金额发放给用户
        List<FreezeProxyLogs> freezeProxyLogsList = getOneWeakData(userId);
        HashMap<Integer, BigDecimal> amountMap = new HashMap<>();
        BigDecimal sendAmount;
        List<FreezeProxyLogs> updateList = new ArrayList<>();
        List<FreezeProxyLogs> deleteList = new ArrayList<>();
        //ttt和usd的汇率
        BigDecimal exchange = kLineDataService.getNowExchangeRate();
        BigDecimal totalUsd;
        for (FreezeProxyLogs freezeProxyLogs : freezeProxyLogsList) {
            totalUsd = exchange.multiply(freezeProxyLogs.getAmount());
            for (VipOrders vipOrders : ordersList) {
                if (vipOrders.getAllowance().compareTo(BigDecimal.ZERO) == 0) {
                    continue;
                }
                if (vipOrders.getAllowance().compareTo(totalUsd) > 0) {
                    //余额够用,扣费
                    vipOrders.setAllowance(vipOrders.getAllowance().subtract(totalUsd));
                    totalUsd = BigDecimal.ZERO;
                    break;
                } else {
                    //余额不足
                    totalUsd = totalUsd.subtract(vipOrders.getAllowance());
                    vipOrders.setAllowance(BigDecimal.ZERO);
                }
            }
            if (totalUsd.compareTo(BigDecimal.ZERO) == 0) {
                //够发
                deleteList.add(freezeProxyLogs);
                sendAmount=freezeProxyLogs.getAmount();
            } else {
                //已发放的ttt
                sendAmount = freezeProxyLogs.getAmount().subtract(totalUsd.divide(exchange, TiktokConfig.NUMBER_OF_DIGITS, RoundingMode.FLOOR));
                freezeProxyLogs.setAmount(freezeProxyLogs.getAmount().subtract(sendAmount));
                updateList.add(freezeProxyLogs);
            }
            amountMap.put(freezeProxyLogs.getType(), amountMap.containsKey(freezeProxyLogs.getType()) ? amountMap.get(freezeProxyLogs.getType()).add(sendAmount) : sendAmount);
        }

        List<WalletTttLogs> walletTttLogsList = new ArrayList<>(freezeProxyLogsList.size());
        WalletTttLogs walletTttLogs;
        LocalDateTime now = LocalDateTime.now();
        BigDecimal total = BigDecimal.ZERO;
        for (Map.Entry<Integer, BigDecimal> entry : amountMap.entrySet()) {
            if (entry.getValue().compareTo(BigDecimal.ZERO) == 0) {
                continue;
            }
            walletTttLogs = new WalletTttLogs();
            walletTttLogs.setUserId(userId);
            walletTttLogs.setAction(FlowingActionEnum.INCOME.getValue());
            walletTttLogs.setCreateTime(now);
            walletTttLogs.setType(entry.getKey());
            walletTttLogs.setAmount(entry.getValue());
            walletTttLogsList.add(walletTttLogs);
            total = total.add(entry.getValue());
        }
        //插入ttt动态奖励流水记录
        walletTttLogsService.saveBatch(walletTttLogsList);
        //删除已发放的七天待领取
        this.removeByIds(deleteList);
        //没发完的奖励冻结
        this.updateBatchById(updateList);
        //修改用户ttt账户的余额
        walletsService.lookUpdateWallets(userId, total, FlowingActionEnum.INCOME);
        //更新vip套餐信息
        vipOrdersService.updateBatchById(ordersList);
        return JsonResult.successResult();
    }

    private List<FreezeProxyLogs> getOneWeakData(Integer userId) {
        //找到七天待领取的记录
        LocalDate now = LocalDate.now();
        LocalDate startDate = now.minusDays(7);
        LocalDate endDate = now.minusDays(1);
        return this.list(new LambdaQueryWrapper<FreezeProxyLogs>()
                .eq(FreezeProxyLogs::getUserId, userId)
                .between(FreezeProxyLogs::getCreateTime, startDate, endDate)
        );
    }

}
