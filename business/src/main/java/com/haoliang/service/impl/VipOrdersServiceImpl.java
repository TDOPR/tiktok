package com.haoliang.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.haoliang.common.enums.BooleanEnum;
import com.haoliang.enums.TttLogTypeEnum;
import com.haoliang.enums.VipLevelEnum;
import com.haoliang.mapper.VipOrdersMapper;
import com.haoliang.model.VipOrders;
import com.haoliang.service.FreezeProxyLogsService;
import com.haoliang.service.KLineDataService;
import com.haoliang.service.VipOrdersService;
import com.haoliang.utils.BigDecimalUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/3/21 16:48
 **/
@Service
public class VipOrdersServiceImpl extends ServiceImpl<VipOrdersMapper, VipOrders> implements VipOrdersService {

    @Autowired
    private FreezeProxyLogsService freezeProxyLogsService;

    @Autowired
    private KLineDataService kLineDataService;

    @Override
    public List<VipOrders> findByUserIdOrderByLevelAes(Integer userId, Integer level) {
        if (level.equals(VipLevelEnum.ZERO.getLevel())) {
            return this.list(new LambdaQueryWrapper<VipOrders>()
                    .eq(VipOrders::getUserId, userId)
                    .eq(VipOrders::getLevel, VipLevelEnum.ZERO.getLevel())
                    .eq(VipOrders::getValid, BooleanEnum.TRUE.intValue())
                    .gt(VipOrders::getAllowance, BigDecimal.ZERO)
            );
        }
        return this.list(new LambdaQueryWrapper<VipOrders>()
                .eq(VipOrders::getUserId, userId)
                .gt(VipOrders::getAllowance, BigDecimal.ZERO)
                .gt(VipOrders::getLevel, VipLevelEnum.ZERO.getLevel())
                .orderByAsc(VipOrders::getLevel)
                .orderByAsc(VipOrders::getCreateTime)
        );
    }

    @Transactional(rollbackFor = Exception.class)
    @Override
    public BigDecimal chargebacks(Integer userId, Integer level, BigDecimal amount, TttLogTypeEnum tttLogTypeEnum) {
        //根据汇率计算扣减
        BigDecimal exchange = kLineDataService.getNowExchangeRate();
        BigDecimal totalUsd = exchange.multiply(amount);
        List<VipOrders> list = this.findByUserIdOrderByLevelAes(userId, level);
        List<VipOrders> updateList = new ArrayList<>();
        for (VipOrders vipOrders : list) {
            updateList.add(vipOrders);
            if (vipOrders.getAllowance().compareTo(totalUsd) >= 0) {
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
        this.saveOrUpdateBatch(updateList);
        //如果购买的套餐包次数不足,则需要把放到七天待领取里面去
        if (totalUsd.compareTo(BigDecimal.ZERO) > 0) {
            //未发放完冻结到七天待领取里
            this.freezeProxyLogsService.insertLogs(userId, tttLogTypeEnum, BigDecimalUtils.divideSaveTwoDecimal(totalUsd, exchange));
        }
        //如果套餐金额不够扣费,则计算出已扣费的金额
        return totalUsd.compareTo(BigDecimal.ZERO) == 0 ? amount : amount.subtract(BigDecimalUtils.divideSaveTwoDecimal(totalUsd, exchange));
    }

    @Override
    public List<VipOrders> getListByUserIdAndLevel(Integer userId, Integer level) {
        if (level.equals(VipLevelEnum.ZERO.getLevel())) {
            return this.list(new LambdaQueryWrapper<VipOrders>()
                    .eq(VipOrders::getUserId, userId)
                    .eq(VipOrders::getLevel, VipLevelEnum.ZERO.getLevel())
                    .eq(VipOrders::getValid, BooleanEnum.TRUE.intValue())
            );
        } else {
            return this.list(new LambdaQueryWrapper<VipOrders>()
                    .eq(VipOrders::getUserId, userId)
                    .gt(VipOrders::getLevel, VipLevelEnum.ZERO.getLevel())
                    .gt(VipOrders::getAllowance, BigDecimal.ZERO));
        }
    }

    @Override
    public void clearFrozenAmount() {
        this.baseMapper.clearFrozenAmount();
    }
}
