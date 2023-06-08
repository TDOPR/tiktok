package com.haoliang.service.impl;

import cn.hutool.core.collection.CollectionUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.haoliang.common.config.GlobalProperties;
import com.haoliang.common.constant.CacheKeyPrefixConstants;
import com.haoliang.common.enums.ReturnMessageEnum;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.model.PageParam;
import com.haoliang.common.model.ThreadLocalManager;
import com.haoliang.common.model.vo.PageVO;
import com.haoliang.common.util.IdUtil;
import com.haoliang.common.util.JwtTokenUtil;
import com.haoliang.common.util.redis.RedisUtil;
import com.haoliang.constant.TiktokConfig;
import com.haoliang.enums.*;
import com.haoliang.mapper.WalletsUsdWithdrawMapper;
import com.haoliang.model.FiatWithdrawOrder;
import com.haoliang.model.Wallets;
import com.haoliang.model.WalletsUsdWithdraw;
import com.haoliang.model.condition.WalletsUsdWithdrawCondition;
import com.haoliang.model.dto.AuditCheckDTO;
import com.haoliang.model.dto.UsdtWithdrawalDTO;
import com.haoliang.model.usd.EvmWithdraw;
import com.haoliang.model.usd.TrxWithdraw;
import com.haoliang.model.vo.WalletsUsdWithdrawVO;
import com.haoliang.pay.enums.CoinUnitEnum;
import com.haoliang.pay.enums.FiatTypeEnum;
import com.haoliang.pay.enums.ProxyPayStatusEnum;
import com.haoliang.service.FiatWithdrawOrderService;
import com.haoliang.service.UpdatePwdLogService;
import com.haoliang.service.tool.EvmWithdrawService;
import com.haoliang.service.tool.TrxWithdrawService;
import com.haoliang.service.WalletsService;
import com.haoliang.service.WalletsUsdWithdrawService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * @author Dominick Li
 * @Description USD提现管理
 * @CreateTime 2023/4/3 17:35
 **/
@Slf4j
@Service
public class WalletsUsdWithdrawServiceImpl extends ServiceImpl<WalletsUsdWithdrawMapper, WalletsUsdWithdraw> implements WalletsUsdWithdrawService {

    @Autowired
    private WalletsService walletsService;

    @Autowired
    private EvmWithdrawService evmWithdrawService;

    @Autowired
    private TrxWithdrawService trxWithdrawService;

    @Autowired
    private FiatWithdrawOrderService fiatWithdrawOrderService;

    @Autowired
    private UpdatePwdLogService updatePwdLogService;

    @Override
    public JsonResult pageList(PageParam<EvmWithdraw, WalletsUsdWithdrawCondition> pageParam) {
        if (pageParam.getSearchParam() == null) {
            pageParam.setSearchParam(new WalletsUsdWithdrawCondition());
        }
        IPage<WalletsUsdWithdrawVO> iPage = this.baseMapper.page(pageParam.getPage(), pageParam.getSearchParam());
        for (WalletsUsdWithdrawVO walletsUsdWithdrawVO : iPage.getRecords()) {
            walletsUsdWithdrawVO.setChannel(CoinUnitEnum.getCnNameById(walletsUsdWithdrawVO.getCoinId()));
            BigDecimal feeRate = walletsUsdWithdrawVO.getCoinId().equals(CoinUnitEnum.USDT.getId()) ? CoinNetworkSourceEnum.getFreeByName(walletsUsdWithdrawVO.getCoinType()) : FiatTypeEnum.typeOf(walletsUsdWithdrawVO.getCoinId()).getProxyFeeRate();
            walletsUsdWithdrawVO.setFreeRate(feeRate.multiply(new BigDecimal(100)).setScale(1).toPlainString() + "%");
            if (walletsUsdWithdrawVO.getCoinId().equals(CoinUnitEnum.USDT.getId())) {
                walletsUsdWithdrawVO.setCoinType(CoinNetworkSourceEnum.nameOf(walletsUsdWithdrawVO.getCoinType()).getNetwordName());
            }
        }
        return JsonResult.successResult(new PageVO<>(iPage.getTotal(), iPage.getPages(), iPage.getRecords()));
    }

    @Override
    @Transactional
    public JsonResult check(AuditCheckDTO auditCheckDTO) {
        UpdateWrapper<WalletsUsdWithdraw> wrapper = Wrappers.update();
        wrapper.lambda()
                .set(WalletsUsdWithdraw::getStatus, auditCheckDTO.getState())
                .set(WalletsUsdWithdraw::getAuditTime, LocalDateTime.now())
                .eq(WalletsUsdWithdraw::getId, auditCheckDTO.getId());
        WalletsUsdWithdraw walletsUsdWithdraw = this.getById(auditCheckDTO.getId());
        if (!WithdrawStatusEnum.UNDER_REVIEW.getStatus().equals(walletsUsdWithdraw.getStatus())) {
            //如果不是待审核的状态 表示重复提交
            return JsonResult.failureResult();
        }
        if (auditCheckDTO.getState().equals(WithdrawStatusEnum.CHECK_SUCCESS.getStatus())) {
            //审核通过 触发提现逻辑
            if (walletsUsdWithdraw.getCoinId().equals(CoinUnitEnum.USDT.getId())) {
                wrapper.lambda().set(WalletsUsdWithdraw::getStatus, WithdrawStatusEnum.CHECK_SUCCESS.getStatus());
                //插入提现记录到指定的表中
                Long id = inertUsdtWithdrawal(walletsUsdWithdraw);
                wrapper.lambda().set(WalletsUsdWithdraw::getTxid, id.toString());
            } else {
                FiatWithdrawOrder fiatWithdrawOrder = fiatWithdrawOrderService.getById(walletsUsdWithdraw.getTxid());
                if (fiatWithdrawOrder != null) {
                    fiatWithdrawOrder.setStatus(ProxyPayStatusEnum.DELAY.getStatus());
                    //修改法币记录表状态
                    fiatWithdrawOrderService.updateById(fiatWithdrawOrder);
                }
            }
        } else {
            //取消冻结的金额
            walletsService.unFrozenAmount(walletsUsdWithdraw.getUserId(), walletsUsdWithdraw.getAmount(), walletsUsdWithdraw.getUsdLogsId(), CoinUnitEnum.idOf(walletsUsdWithdraw.getCoinId()));
            if (!walletsUsdWithdraw.getCoinId().equals(CoinUnitEnum.USDT.getId())) {
                //删除法币提现记录
                fiatWithdrawOrderService.removeById(walletsUsdWithdraw.getTxid());
            }
        }
        this.update(wrapper);
        return JsonResult.successResult();
    }


    @Override
    @Transactional(rollbackFor = Exception.class)
    public JsonResult usdtWithdrawal(UsdtWithdrawalDTO userWalletsDTO) {
        JsonResult result = updatePwdLogService.checkUpdateTime();
        if (result.getCode() != HttpStatus.OK.value()) {
            return result;
        }

        CoinNetworkSourceEnum coinNetworkSourceEnum = CoinNetworkSourceEnum.networdNameOf(userWalletsDTO.getNetwordName());
        //查看最近一天的数据
        Integer userId = JwtTokenUtil.getUserIdFromToken(ThreadLocalManager.getToken());

        Long count = this.count(new LambdaQueryWrapper<WalletsUsdWithdraw>()
                .eq(WalletsUsdWithdraw::getUserId, userId)
                .eq(WalletsUsdWithdraw::getCoinId, CoinUnitEnum.USDT.getId())
                .gt(WalletsUsdWithdraw::getCreateTime, LocalDateTime.now().minusDays(1))
        );

        //24小时提币3次限制则返回错误提示
        if (count >= TiktokConfig.WITHDRAW_COUNT_LIMIT) {
            return JsonResult.failureResult(ReturnMessageEnum.WITHDRAW_COUNT_LIMIT);
        }

        if (GlobalProperties.isProdEnv()) {
            String cacheKey = CacheKeyPrefixConstants.CAPTCHA_CODE + userWalletsDTO.getUuid();
            String code = RedisUtil.getCacheObject(cacheKey);
            if (code == null) {
                return JsonResult.failureResult(ReturnMessageEnum.VERIFICATION_CODE_EXPIRE);
            }
            if (!code.equals(userWalletsDTO.getCode())) {
                return JsonResult.failureResult(ReturnMessageEnum.VERIFICATION_CODE_ERROR);
            }
        }

        if (coinNetworkSourceEnum == null) {
            return JsonResult.failureResult(ReturnMessageEnum.UB_SUPPORT_NETWORD);
        }

        Wallets wallets = walletsService.selectColumnsByUserId(userId, Wallets::getUsdWalletAmount);

        //提现金额不能大于钱包余额
        if (userWalletsDTO.getAmount().compareTo(wallets.getUsdWalletAmount()) > 0) {
            return JsonResult.failureResult(ReturnMessageEnum.AMOUNT_EXCEEDS_BALANCE);
        }

        //计算手续费
        BigDecimal free = coinNetworkSourceEnum.getFree().multiply(userWalletsDTO.getAmount()).setScale(TiktokConfig.NUMBER_OF_DIGITS, RoundingMode.UP);
        if (free.compareTo(coinNetworkSourceEnum.getMinFreeAmount()) < 0) {
            //如果手续费低于提现要求的最低,则使用最低限制的
            free = coinNetworkSourceEnum.getMinFreeAmount();
        }
        //需要冻结提现金额
        Long usdLogId = walletsService.frozenAmount(userId, userWalletsDTO.getAmount(), CoinUnitEnum.USDT);

        String address = userWalletsDTO.getAddress();
        if (address.contains(":")) {
            String[] arr = address.split(":");
            address = arr[1];
        }

        //生成提现审核记录
        WalletsUsdWithdraw walletsUsdWithdraw = WalletsUsdWithdraw.builder()
                .usdLogsId(usdLogId)
                .userId(userId)
                .address(address)
                .amount(userWalletsDTO.getAmount())
                .free(free)
                .actualAmount(userWalletsDTO.getAmount().subtract(free))
                .coinType(coinNetworkSourceEnum.getName())
                .coinId(CoinUnitEnum.USDT.getId())
                .build();
        walletsUsdWithdraw.setId(IdUtil.getSnowflakeNextId());

        Long txId = null;

        Integer status = WithdrawStatusEnum.UNDER_REVIEW.getStatus();
        if (TiktokSettingEnum.ENABLED_AUTO_CHECK.boolValue()) {
            if (new BigDecimal(TiktokSettingEnum.CHECK_MIN_AMOUNT.intValue()).compareTo(walletsUsdWithdraw.getAmount()) >= 0) {
                status = WithdrawStatusEnum.SUCCESS.getStatus();
                //根据网络类型添加到对应的提现表中
                txId = inertUsdtWithdrawal(walletsUsdWithdraw);
            }
        }
        walletsUsdWithdraw.setStatus(status);
        walletsUsdWithdraw.setTxid(txId);
        this.save(walletsUsdWithdraw);
        return JsonResult.successResult();
    }

    /**
     * 插入到Usd提现记录中
     */
    @Transactional(rollbackFor = Exception.class)
    public Long inertUsdtWithdrawal(WalletsUsdWithdraw walletsUsdWithdraw) {
        if (CoinNetworkSourceEnum.TRC.getName().equals(walletsUsdWithdraw.getCoinType())) {
            TrxWithdraw trxWithdraw = TrxWithdraw.builder()
                    .status(WithdrawStatusEnum.CHECK_SUCCESS.getStatus())
                    .userId(walletsUsdWithdraw.getUserId())
                    .address(walletsUsdWithdraw.getAddress())
                    .fee(walletsUsdWithdraw.getFree())
                    .actualAmount(walletsUsdWithdraw.getActualAmount())
                    .amount(walletsUsdWithdraw.getAmount())
                    .coinName(CoinUnitEnum.USDT.getName())
                    .coinUnit(walletsUsdWithdraw.getCoinType())
                    .coinId(CoinNetworkSourceEnum.getCoinIdByName(walletsUsdWithdraw.getCoinType()))
                    .build();
            trxWithdrawService.save(trxWithdraw);
            return trxWithdraw.getId();
        } else {
            //生成提现记录
            EvmWithdraw evmWithdraw = EvmWithdraw.builder()
                    .status(WithdrawStatusEnum.CHECK_SUCCESS.getStatus())
                    .userId(walletsUsdWithdraw.getUserId())
                    .address(walletsUsdWithdraw.getAddress())
                    .fee(walletsUsdWithdraw.getFree())
                    .actualAmount(walletsUsdWithdraw.getActualAmount())
                    .amount(walletsUsdWithdraw.getAmount())
                    .coinName(CoinUnitEnum.USDT.getName())
                    .coinUnit(walletsUsdWithdraw.getCoinType())
                    .coinId(CoinNetworkSourceEnum.getCoinIdByName(walletsUsdWithdraw.getCoinType()))
                    .build();
            evmWithdrawService.save(evmWithdraw);
            return evmWithdraw.getId();
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void scanWithdrawData() {

        WalletsUsdWithdraw walletsUsdWithdraw;
        Long usdLogId;
        {
            //获取提现任务状态是打币成功和失败的记录
            UpdateWrapper<EvmWithdraw> updateWrapper;
            List<EvmWithdraw> list = evmWithdrawService.list(
                    new LambdaQueryWrapper<EvmWithdraw>()
                            .select(EvmWithdraw::getId, EvmWithdraw::getUserId, EvmWithdraw::getAmount, EvmWithdraw::getCoinUnit, EvmWithdraw::getStatus)
                            .in(EvmWithdraw::getStatus, Arrays.asList(WithdrawStatusEnum.SUCCESS.getStatus(), WithdrawStatusEnum.BLOCK_COIN_PRINTING_FAILED.getStatus()))
                            .gt(EvmWithdraw::getUserId, 0)
            );
            if (CollectionUtil.isNotEmpty(list)) {
                List<Long> idList = new ArrayList<>();
                for (EvmWithdraw evmWithdraw : list) {
                    try {
                        usdLogId = null;
                        walletsUsdWithdraw = this.getOne(new LambdaQueryWrapper<WalletsUsdWithdraw>()
                                .eq(WalletsUsdWithdraw::getTxid, evmWithdraw.getId().toString())
                                .eq(WalletsUsdWithdraw::getCoinType, evmWithdraw.getCoinUnit())
                        );
                        //log.info("evmWithdraw id={} ,CoinType={}",evmWithdraw.getId(),evmWithdraw.getCoinUnit());
                        if (walletsUsdWithdraw != null) {
                            usdLogId = walletsUsdWithdraw.getUsdLogsId();
                        }
                        if (evmWithdraw.getStatus().equals(WithdrawStatusEnum.SUCCESS.getStatus())) {
                            //重冻结的金额里面扣除提现的金额
                            walletsService.reduceFrozenAmount(evmWithdraw.getUserId(), evmWithdraw.getAmount(), usdLogId);
                        } else {
                            //如果区块链打币失败 则把冻结的金额返还给客户
                            walletsService.unFrozenAmount(evmWithdraw.getUserId(), evmWithdraw.getAmount(), usdLogId, CoinUnitEnum.USDT);
                        }
                        idList.add(evmWithdraw.getId());
                        log.info("usd提现  链={},任务id={},提币结果={},状态码:{}", evmWithdraw.getCoinUnit(), evmWithdraw.getId(), evmWithdraw.getStatus().equals(WithdrawStatusEnum.SUCCESS.getStatus()) ? "成功" : "失败", evmWithdraw.getStatus());
                    } catch (Exception e) {
                        log.error("evmWithdraw error:{}", e.getMessage());
                    }
                }
                if (idList.size() > 0) {
                    //修改任务状态为已结算
                    updateWrapper = Wrappers.update();
                    updateWrapper.lambda()
                            .set(EvmWithdraw::getStatus, WithdrawStatusEnum.TO_AMOUNT_SUCCESS.getStatus())
                            .in(EvmWithdraw::getId, idList);
                    evmWithdrawService.update(updateWrapper);
                }
            }
        }
        //获取提现任务状态是打币成功和失败的记录
        UpdateWrapper<TrxWithdraw> updateWrapper;
        List<TrxWithdraw> list = trxWithdrawService.list(
                new LambdaQueryWrapper<TrxWithdraw>()
                        .select(TrxWithdraw::getId, TrxWithdraw::getUserId, TrxWithdraw::getAmount, TrxWithdraw::getCoinUnit, TrxWithdraw::getStatus)
                        .in(TrxWithdraw::getStatus, Arrays.asList(WithdrawStatusEnum.SUCCESS.getStatus(), WithdrawStatusEnum.BLOCK_COIN_PRINTING_FAILED.getStatus()))
                        .gt(TrxWithdraw::getUserId, 0)
        );
        if (CollectionUtil.isNotEmpty(list)) {
            List<Long> idList = new ArrayList<>();
            for (TrxWithdraw trxWithdraw : list) {
                try {
                    usdLogId = null;
                    walletsUsdWithdraw = this.getOne(new LambdaQueryWrapper<WalletsUsdWithdraw>()
                            .eq(WalletsUsdWithdraw::getTxid, trxWithdraw.getId().toString())
                            .eq(WalletsUsdWithdraw::getCoinType, trxWithdraw.getCoinUnit())
                    );
                    if (walletsUsdWithdraw != null) {
                        usdLogId = walletsUsdWithdraw.getUsdLogsId();
                    }
                    if (trxWithdraw.getStatus().equals(WithdrawStatusEnum.SUCCESS.getStatus())) {
                        //重冻结的金额里面扣除提现的金额
                        walletsService.reduceFrozenAmount(trxWithdraw.getUserId(), trxWithdraw.getAmount(), usdLogId);
                    } else {
                        //如果区块链打币失败 则把冻结的金额返还给客户
                        walletsService.unFrozenAmount(trxWithdraw.getUserId(), trxWithdraw.getAmount(), usdLogId, CoinUnitEnum.USDT);
                    }
                    idList.add(trxWithdraw.getId());
                    log.info("usd提现 链=trx 任务id={},提币结果={},状态码:{}", trxWithdraw.getId(), trxWithdraw.getStatus().equals(WithdrawStatusEnum.SUCCESS.getStatus()) ? "成功" : "失败", trxWithdraw.getStatus());
                } catch (Exception e) {
                    log.error("trxWithdraw error:{}", e.getMessage());
                }
            }
            if (idList.size() > 0) {
                //修改任务状态为已结算
                updateWrapper = Wrappers.update();
                updateWrapper.lambda()
                        .set(TrxWithdraw::getStatus, WithdrawStatusEnum.TO_AMOUNT_SUCCESS.getStatus())
                        .in(TrxWithdraw::getId, idList);
                trxWithdrawService.update(updateWrapper);
            }
        }
    }

    public static void main(String[] args) {
        String address = "0xc40eF7Da9C631E199C09F6E5566c512858EF1eAb";
        if (address.contains(":")) {
            String[] arr = address.split(":");
            address = arr[1];
        }
        System.out.println(address);
    }

}
