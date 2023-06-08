package com.haoliang.service.impl;

import cn.hutool.core.collection.CollectionUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.haoliang.constant.TiktokConfig;
import com.haoliang.enums.*;
import com.haoliang.mapper.WalletsUsdRechargeMapper;
import com.haoliang.model.WalletsUsdRecharge;
import com.haoliang.model.usd.EvmRecharge;
import com.haoliang.model.usd.TrxRecharge;
import com.haoliang.pay.enums.CoinUnitEnum;
import com.haoliang.service.*;
import com.haoliang.service.tool.EvmRechargeService;
import com.haoliang.service.tool.TrxRechargService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.List;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/4/4 11:47
 **/
@Slf4j
@Service
public class WalletsUsdRechargeServiceImpl extends ServiceImpl<WalletsUsdRechargeMapper, WalletsUsdRecharge> implements WalletsUsdRechargeService {

    @Autowired
    private WalletsService walletsService;

    @Autowired
    private EvmRechargeService evmRechargeService;

    @Autowired
    private TrxRechargService trxRechargService;

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void scanRechargeList() {
        List<WalletsUsdRecharge> walletsUsdRechargeList = new ArrayList<>();
        WalletsUsdRecharge walletsUsdRecharge;
        //获取充值任务状态是打币成功的
        List<EvmRecharge> list = evmRechargeService.list(
                new LambdaQueryWrapper<EvmRecharge>()
                        .gt(EvmRecharge::getUid, 0)
                        .eq(EvmRecharge::getStatus, RechargeStatusEnum.RECHARGE_SUCCESS.getStatus())
        );

        if (CollectionUtil.isNotEmpty(list)) {
            List<Long> idList = new ArrayList<>();
            //根据区块链地址修改用户钱包金额
            for (EvmRecharge evmRecharge : list) {
                walletsUsdRecharge = WalletsUsdRecharge.builder()
                        .status(RechargeStatusEnum.TO_RECORDED_SUCCESS.getStatus())
                        .userId(evmRecharge.getUid())
                        .coinId(evmRecharge.getCoinId())
                        .coinType(evmRecharge.getCoinUnit())
                        .amount(evmRecharge.getAmount())
                        .actualAmount(evmRecharge.getActualAmount())
                        .txid(evmRecharge.getTxid())
                        .build();
                idList.add(evmRecharge.getId());
                walletsUsdRechargeList.add(walletsUsdRecharge);
                if (evmRecharge.getAmount().compareTo(TiktokConfig.RECHARGE_MIN_LIMIT) >= 0) {
                    //充值到余额里面
                    walletsService.updateUsdWallet(evmRecharge.getActualAmount().setScale(TiktokConfig.NUMBER_OF_DIGITS, RoundingMode.FLOOR), evmRecharge.getUid(), FlowingActionEnum.INCOME, UsdLogTypeEnum.RECHARGE, CoinUnitEnum.USDT);
                    log.info("usd充值成功  链={},任务id={},充币金额={}", evmRecharge.getCoinUnit(), evmRecharge.getId(), evmRecharge.getActualAmount());
                } else {
                    log.info("usd充值金额小于{}USD ,不入账! 链={},任务id={}", TiktokConfig.RECHARGE_MIN_LIMIT, evmRecharge.getCoinUnit(), evmRecharge.getId());
                }
            }
            UpdateWrapper<EvmRecharge> updateWrapper = Wrappers.update();
            updateWrapper.lambda()
                    .set(EvmRecharge::getStatus, RechargeStatusEnum.TO_RECORDED_SUCCESS.getStatus())
                    .in(EvmRecharge::getId, idList);
            evmRechargeService.update(updateWrapper);
        }

        //获取充值任务状态是打币成功的
        List<TrxRecharge> trxList = trxRechargService.list(
                new LambdaQueryWrapper<TrxRecharge>()
                        .eq(TrxRecharge::getStatus, RechargeStatusEnum.RECHARGE_SUCCESS.getStatus())
                        .gt(TrxRecharge::getUid, 0)
        );
        if (CollectionUtil.isNotEmpty(trxList)) {
            List<Long> idList = new ArrayList<>();
            //根据区块链地址修改用户钱包金额
            for (TrxRecharge trxRecharge : trxList) {
                walletsUsdRecharge = WalletsUsdRecharge.builder()
                        .status(RechargeStatusEnum.TO_RECORDED_SUCCESS.getStatus())
                        .userId(trxRecharge.getUid())
                        .coinId(trxRecharge.getCoinId())
                        .coinType(trxRecharge.getCoinUnit())
                        .amount(trxRecharge.getAmount())
                        .actualAmount(trxRecharge.getActualAmount())
                        .txid(trxRecharge.getTxid())
                        .build();
                walletsUsdRechargeList.add(walletsUsdRecharge);
                idList.add(trxRecharge.getId());
                if (trxRecharge.getAmount().compareTo(TiktokConfig.RECHARGE_MIN_LIMIT) >= 0) {
                    walletsService.updateUsdWallet(trxRecharge.getActualAmount().setScale(TiktokConfig.NUMBER_OF_DIGITS, RoundingMode.FLOOR), trxRecharge.getUid(), FlowingActionEnum.INCOME, UsdLogTypeEnum.RECHARGE, CoinUnitEnum.USDT);
                    log.info("usd充值成功  链=TRX,任务id={},充币金额={}", trxRecharge.getId(), trxRecharge.getActualAmount());
                } else {
                    log.info("usd充值金额小于{}USD ,不入账!  链=TRX,任务id={}", TiktokConfig.RECHARGE_MIN_LIMIT, trxRecharge.getId());
                }
            }
            UpdateWrapper<TrxRecharge> updateWrapper = Wrappers.update();
            updateWrapper.lambda()
                    .set(TrxRecharge::getStatus, RechargeStatusEnum.TO_RECORDED_SUCCESS.getStatus())
                    .in(TrxRecharge::getId, idList);
            trxRechargService.update(updateWrapper);
        }
        this.saveBatch(walletsUsdRechargeList);
    }

}
