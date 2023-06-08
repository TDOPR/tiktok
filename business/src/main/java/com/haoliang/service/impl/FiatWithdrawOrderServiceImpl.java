package com.haoliang.service.impl;

import cn.hutool.core.collection.CollectionUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.haoliang.common.config.AppParamProperties;
import com.haoliang.common.config.GlobalProperties;
import com.haoliang.common.constant.CacheKeyPrefixConstants;
import com.haoliang.common.enums.ReturnMessageEnum;
import com.haoliang.common.model.HttpResult;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.model.ThreadLocalManager;
import com.haoliang.common.util.BeanMapUtil;
import com.haoliang.common.util.IdUtil;
import com.haoliang.common.util.JwtTokenUtil;
import com.haoliang.common.util.redis.RedisUtil;
import com.haoliang.enums.*;
import com.haoliang.mapper.FiatWithdrawOrderMapper;
import com.haoliang.mapper.WalletsUsdWithdrawMapper;
import com.haoliang.model.*;
import com.haoliang.model.dto.BankCodeDTO;
import com.haoliang.model.dto.ProxyPayAmountDTO;
import com.haoliang.model.vo.FiatWithdrawInfoVO;
import com.haoliang.pay.enums.CoinUnitEnum;
import com.haoliang.pay.enums.FiatTypeEnum;
import com.haoliang.pay.id.IdPayUtils;
import com.haoliang.pay.enums.ProxyPayStatusEnum;
import com.haoliang.pay.id.model.IdProxyPayCallBack;
import com.haoliang.pay.id.model.IdProxyPayResult;
import com.haoliang.pay.th.ThPayUtils;
import com.haoliang.pay.th.enums.ThPayStatusEnum;
import com.haoliang.pay.th.model.ThPayCallBack;
import com.haoliang.pay.th.model.ThPayResult;
import com.haoliang.pay.vn.VnPayUtils;
import com.haoliang.pay.vn.model.VnCallBack;
import com.haoliang.pay.vn.model.VnPayResult;
import com.haoliang.pay.vn.enums.VnPayStatusEnum;
import com.haoliang.service.*;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.Resource;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/4/16 17:13
 **/
@Slf4j
@Service
public class FiatWithdrawOrderServiceImpl extends ServiceImpl<FiatWithdrawOrderMapper, FiatWithdrawOrder> implements FiatWithdrawOrderService {

    @Autowired
    private AppUserService appUserService;

    @Autowired
    private WalletsService walletsService;

    @Autowired
    private CurrencyExchangeRatesService currencyExchangeRatesService;

    @Autowired
    private AppParamProperties appParamProperties;

    @Autowired
    private StringRedisTemplate stringRedisTemplate;

    @Autowired
    private UpdatePwdLogService updatePwdLogService;

    @Resource
    private WalletsUsdWithdrawMapper walletsUsdWithdrawMapper;


    @Override
    public JsonResult<List<FiatWithdrawInfoVO>> info() {
        List<FiatWithdrawInfoVO> fiatWithdrawOrderList = new ArrayList<>();
        FiatWithdrawInfoVO fiatWithdrawInfoVO;
        for (FiatTypeEnum fiatTypeEnum : FiatTypeEnum.values()) {
            if (fiatTypeEnum.isOpening()) {
                BigDecimal exchangeRate = currencyExchangeRatesService.getExchangeRatesByType(fiatTypeEnum.getCoinUnit()).getExchangeRateDown();
                fiatWithdrawInfoVO = new FiatWithdrawInfoVO(fiatTypeEnum);
                fiatWithdrawInfoVO.setMin(new BigDecimal(fiatWithdrawInfoVO.getMin()).divide(exchangeRate, 0, RoundingMode.UP).intValue());
                fiatWithdrawInfoVO.setMax(new BigDecimal(fiatWithdrawInfoVO.getMax()).divide(exchangeRate, 0, RoundingMode.FLOOR).intValue());
                fiatWithdrawOrderList.add(fiatWithdrawInfoVO);
                fiatWithdrawInfoVO.setExchangeRate(exchangeRate.toPlainString());
            }
        }
        return JsonResult.successResult(fiatWithdrawOrderList);
    }

    @Override
    public JsonResult pay(ProxyPayAmountDTO amountDTO) {
        JsonResult result = updatePwdLogService.checkUpdateTime();
        if (result.getCode() != HttpStatus.OK.value()) {
            return result;
        }
        //验证码校验
        if (GlobalProperties.isProdEnv()) {
            String cacheKey = CacheKeyPrefixConstants.CAPTCHA_CODE + amountDTO.getUuid();
            String code = RedisUtil.getCacheObject(cacheKey);
            if (code == null) {
                return JsonResult.failureResult(ReturnMessageEnum.VERIFICATION_CODE_EXPIRE);
            }

            if (!code.equals(amountDTO.getCode())) {
                return JsonResult.failureResult(ReturnMessageEnum.VERIFICATION_CODE_ERROR);
            }
        }

        FiatTypeEnum typeEnum = FiatTypeEnum.typeOf(amountDTO.getType());
        if (typeEnum == null) {
            return JsonResult.failureResult();
        }

        Map<String, String> bankMap = typeEnum.getBankList().stream().collect(Collectors.toMap(BankCodeDTO::getCode, BankCodeDTO::getName));

        CurrencyExchangeRates currencyExchangeRates = currencyExchangeRatesService.getExchangeRatesByType(typeEnum.getCoinUnit());
        //获取向下取整的汇率
        BigDecimal exchangeRate = currencyExchangeRates.getExchangeRateDown();

        BigDecimal usdAmount = new BigDecimal(amountDTO.getAmount());
        //计算Usd等值的提现法币金额
        Integer fiatAmount = new BigDecimal(amountDTO.getAmount()).multiply(exchangeRate).setScale(0, RoundingMode.FLOOR).intValue();

        //金额限制判断
        if (fiatAmount < typeEnum.getProxyMin() || fiatAmount > typeEnum.getProxyMax()) {
            log.info("代付 fiatType={} amount={}  not in {} ~ {}", typeEnum.getName(), fiatAmount, typeEnum.getProxyMin(), typeEnum.getProxyMax());
            return JsonResult.failureResult();
        }

        Integer userId = JwtTokenUtil.getUserIdFromToken(ThreadLocalManager.getToken());
        Wallets wallets = walletsService.selectColumnsByUserId(userId, Wallets::getUsdWalletAmount);

        if (amountDTO.getAmount().compareTo(wallets.getUsdWalletAmount().intValue()) > 0) {
            //提现金额不能大于钱包余额
            return JsonResult.failureResult(ReturnMessageEnum.AMOUNT_EXCEEDS_BALANCE);
        }

        //usd扣费比例
        BigDecimal usdFee = usdAmount.multiply(typeEnum.getProxyFeeRate());

        Long orderNo = IdUtil.getSnowflakeNextId();
        LocalDateTime now = LocalDateTime.now();
        //实际提现金额
        BigDecimal actualAmount = new BigDecimal(fiatAmount).multiply(new BigDecimal(1).subtract(typeEnum.getProxyFeeRate()))
                .subtract(new BigDecimal(typeEnum.getBaseFee())).setScale(0, RoundingMode.DOWN);
        //实际提现手续费  提现金额*费率+基础手续费
        BigDecimal fee = actualAmount.multiply(typeEnum.getProxyFeeRate()).add(new BigDecimal(typeEnum.getBaseFee()));

        FiatWithdrawOrder fiatWithdrawOrder = FiatWithdrawOrder.builder()
                .userId(userId)
                .amount(fiatAmount)
                .fee(fee)
                .status(ProxyPayStatusEnum.TO_BE_REVIEWED.getStatus())
                .actualAmount(actualAmount.intValue())
                .fiatType(amountDTO.getType())
                .bankCode(amountDTO.getBankCode())
                .bankNumber(amountDTO.getBankNumber())
                .usdAmount(amountDTO.getAmount())
                .name(amountDTO.getName())
                .exchangeRate(exchangeRate)
                .build();
        fiatWithdrawOrder.setId(orderNo);
        fiatWithdrawOrder.setCreateTime(now);

        Integer status = WithdrawStatusEnum.UNDER_REVIEW.getStatus();
        if (TiktokSettingEnum.ENABLED_AUTO_CHECK.boolValue()) {
            if (TiktokSettingEnum.CHECK_MIN_AMOUNT.intValue() >= amountDTO.getAmount()) {
                status = WithdrawStatusEnum.SUCCESS.getStatus();
                fiatWithdrawOrder.setStatus(ProxyPayStatusEnum.DELAY.getStatus());
            }
        }

        //需要冻结提现金额
        Long usdLogId = walletsService.frozenAmount(userId, usdAmount, CoinUnitEnum.idOf(amountDTO.getType()));

        //生成提现记录
        WalletsUsdWithdraw walletsUsdWithdraw = WalletsUsdWithdraw.builder()
                .usdLogsId(usdLogId)
                .userId(userId)
                .amount(usdAmount)
                .free(usdFee)
                .actualAmount(usdAmount.subtract(usdFee))
                .coinType(bankMap.get(amountDTO.getBankCode()))
                .txid(orderNo)
                .coinId(typeEnum.getType())
                .build();

        walletsUsdWithdraw.setStatus(status);
        walletsUsdWithdrawMapper.insert(walletsUsdWithdraw);
        this.save(fiatWithdrawOrder);
        return JsonResult.successResult();
    }

    @Override
    public void scanDelayProxyPayData() {
        LocalDateTime end = LocalDateTime.now().minusDays(appParamProperties.getDelayPayDay());
        List<FiatWithdrawOrder> fiatWithdrawOrderList = this.list(new LambdaQueryWrapper<FiatWithdrawOrder>()
                .eq(FiatWithdrawOrder::getStatus, ProxyPayStatusEnum.DELAY.getStatus())
                .le(FiatWithdrawOrder::getLastmodifiedTime, end)
        );

        if (CollectionUtil.isNotEmpty(fiatWithdrawOrderList)) {
            log.info("延迟{}天提现订单数={}", appParamProperties.getDelayPayDay(), fiatWithdrawOrderList.size());
            Long usdLogId;
            WalletsUsdWithdraw walletsUsdWithdraw;

            for (FiatWithdrawOrder fiatWithdrawOrder : fiatWithdrawOrderList) {
                boolean flag = callProxyPay(fiatWithdrawOrder);
                log.info("订单号:{} 调用法币提现结果:{}", fiatWithdrawOrder.getId(), flag);
                if (flag) {
                    fiatWithdrawOrder.setStatus(ProxyPayStatusEnum.NO_PAY.getStatus());
                } else {
                    fiatWithdrawOrder.setStatus(ProxyPayStatusEnum.API_ERROR.getStatus());
                    usdLogId = null;
                    walletsUsdWithdraw = walletsUsdWithdrawMapper.selectOne(new LambdaQueryWrapper<WalletsUsdWithdraw>()
                            .eq(WalletsUsdWithdraw::getTxid, fiatWithdrawOrder.getId())
                    );
                    if (walletsUsdWithdraw != null) {
                        usdLogId = walletsUsdWithdraw.getUsdLogsId();
                    }
                    //如果调用提现接口异常则返回冻结金额给用户
                    walletsService.unFrozenAmount(fiatWithdrawOrder.getUserId(), new BigDecimal(fiatWithdrawOrder.getUsdAmount()), usdLogId, CoinUnitEnum.idOf(fiatWithdrawOrder.getFiatType()));
                }
            }
            this.updateBatchById(fiatWithdrawOrderList);
        }
    }

    /**
     * 调用提现接口
     *
     * @param fiatWithdrawOrder 法币提现信息
     * @return
     */
    @Override
    public boolean callProxyPay(FiatWithdrawOrder fiatWithdrawOrder) {
        AppUsers appUsers = this.appUserService.selectColumnsByUserId(fiatWithdrawOrder.getUserId(), AppUsers::getMobile, AppUsers::getEmail);
        FiatTypeEnum fiatTypeEnum = FiatTypeEnum.typeOf(fiatWithdrawOrder.getFiatType());
        if (fiatTypeEnum == FiatTypeEnum.ID) {
            HttpResult<IdProxyPayResult> httpResult = IdPayUtils.proxyPay(fiatWithdrawOrder.getId().toString(), fiatWithdrawOrder.getCreateTime(), fiatWithdrawOrder.getActualAmount().toString(), fiatWithdrawOrder.getBankCode(), fiatWithdrawOrder.getBankNumber(), fiatWithdrawOrder.getName(), appUsers.getMobile(), appUsers.getEmail());
            if (httpResult.isSuccess() && "SUCCESS".equals(httpResult.getData().getPlatRespCode())) {
                IdProxyPayResult idPayResult = httpResult.getData();
                fiatWithdrawOrder.setStatus(ProxyPayStatusEnum.NO_PAY.getStatus());
                Integer free = Integer.parseInt(idPayResult.getFee());
                fiatWithdrawOrder.setFee(new BigDecimal(free));
                fiatWithdrawOrder.setPlatformOrderNo(idPayResult.getPlatOrderNum());
                fiatWithdrawOrder.setActualAmount(fiatWithdrawOrder.getAmount() - free);
                return true;
            }
            log.error("userId={},订单号:{},调用印尼法币代付异常:{}", fiatWithdrawOrder.getUserId(), fiatWithdrawOrder.getId(), httpResult.getMsg());
        } else if (fiatTypeEnum == FiatTypeEnum.VN) {
            HttpResult<VnPayResult> httpResult = VnPayUtils.proxyPay(fiatWithdrawOrder.getId().toString(), fiatWithdrawOrder.getAmount().toString(), fiatWithdrawOrder.getBankCode(), fiatWithdrawOrder.getBankNumber(), fiatWithdrawOrder.getName());
            if (httpResult.isSuccess()) {
                fiatWithdrawOrder.setPlatformOrderNo(httpResult.getData().getSys_order_no());
                fiatWithdrawOrder.setStatus(ProxyPayStatusEnum.NO_PAY.getStatus());
                return true;
            }
            log.error("userId={},订单号:{},调用越南法币代付异常:{}", fiatWithdrawOrder.getUserId(), fiatWithdrawOrder.getId(), httpResult.getMsg());
        } else if (fiatTypeEnum == FiatTypeEnum.TH) {
            HttpResult<ThPayResult> httpResult = ThPayUtils.proxyPay(fiatWithdrawOrder.getId().toString(), fiatWithdrawOrder.getAmount().toString(), fiatWithdrawOrder.getBankCode(), fiatWithdrawOrder.getBankNumber(), fiatWithdrawOrder.getName());
            if (httpResult.isSuccess()) {
                fiatWithdrawOrder.setStatus(ProxyPayStatusEnum.NO_PAY.getStatus());
                return true;
            }
            log.error("userId={},订单号:{},调用泰国法币代付异常:{}", fiatWithdrawOrder.getUserId(), fiatWithdrawOrder.getId(), httpResult.getMsg());
        }
        //调用接口失败
        return false;
    }


    @Override
    public String idCallBack(IdProxyPayCallBack idProxyPayCallBack) {
        //印尼支付回调
        boolean pass = IdPayUtils.verifySign(BeanMapUtil.beanToTreeMap(idProxyPayCallBack));
        if(!GlobalProperties.isProdEnv()){
            pass = true;
        }
        if (pass) {
            Integer resStatus = Integer.parseInt(idProxyPayCallBack.getStatus());
            if (ProxyPayStatusEnum.SUCCESS.getStatus().equals(resStatus) || ProxyPayStatusEnum.PAY_CANCEL.getStatus().equals(resStatus)) {
                //验证签名通过
                FiatWithdrawOrder fiatWithdrawOrder = this.getById(Long.parseLong(idProxyPayCallBack.getOrderNum()));
                if (fiatWithdrawOrder != null) {
                    editOrders(fiatWithdrawOrder, resStatus);
                }
            }
            return "SUCCESS";
        }
        return "verifySign error";
    }


    @Override
    public String vnCallBack(VnCallBack vnCallBack) {
        boolean pass = VnPayUtils.verifySign(BeanMapUtil.beanToStrMap(vnCallBack));
        if(!GlobalProperties.isProdEnv()){
            pass = true;
        }
        if (pass) {
            Integer resStatus = VnPayStatusEnum.nameOf(vnCallBack.getStatus()).getStatus();
            if (ProxyPayStatusEnum.SUCCESS.getStatus().equals(resStatus) || ProxyPayStatusEnum.PAY_CANCEL.getStatus().equals(resStatus)) {
                //验证签名通过
                FiatWithdrawOrder fiatWithdrawOrder = this.getById(Long.parseLong(vnCallBack.getOrder_no()));
                if (fiatWithdrawOrder != null) {
                    editOrders(fiatWithdrawOrder, resStatus);
                }
            }
            return "SUCCESS";
        }
        return "verifySign error";
    }

    @Override
    public String thCallBack(ThPayCallBack thPayCallBack) {
        boolean pass = ThPayUtils.verifySign(BeanMapUtil.beanToStrMap(thPayCallBack));
        if(!GlobalProperties.isProdEnv()){
            pass = true;
        }
        if (pass) {
            Integer resStatus = ThPayStatusEnum.nameOf(thPayCallBack.getState()).getStatus();
            if (ProxyPayStatusEnum.SUCCESS.getStatus().equals(resStatus) || ProxyPayStatusEnum.PAY_CANCEL.getStatus().equals(resStatus)) {
                //验证签名通过
                FiatWithdrawOrder fiatWithdrawOrder = this.getById(Long.parseLong(thPayCallBack.getOrder_no()));
                if (fiatWithdrawOrder != null) {
                    fiatWithdrawOrder.setPlatformOrderNo(thPayCallBack.getOrder_id());
                    editOrders(fiatWithdrawOrder, resStatus);
                }
            }
            return "SUCCESS";
        }
        return "verifySign error";
    }

    /**
     * 修改订单状态
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public void editOrders(FiatWithdrawOrder fiatWithdrawOrder, Integer resStatus) {
        String cacheKey = CacheKeyPrefixConstants.WITHDRAW_ORDERS + fiatWithdrawOrder.getId();
        boolean flag = stringRedisTemplate.opsForValue().setIfAbsent(cacheKey, "1", 15, TimeUnit.SECONDS);
        if (flag) {
            //如果订单的状态不等于成功和失败则处理  避免重复提交
            if (!fiatWithdrawOrder.getStatus().equals(ProxyPayStatusEnum.SUCCESS.getStatus()) && !fiatWithdrawOrder.getStatus().equals(ProxyPayStatusEnum.PAY_CANCEL.getStatus())) {
                WalletsUsdWithdraw walletsUsdWithdraw = walletsUsdWithdrawMapper.selectOne(
                        new LambdaQueryWrapper<WalletsUsdWithdraw>()
                                .eq(WalletsUsdWithdraw::getTxid, fiatWithdrawOrder.getId())
                );
                BigDecimal usdAmount = new BigDecimal(fiatWithdrawOrder.getUsdAmount());
                if (ProxyPayStatusEnum.SUCCESS.getStatus().equals(resStatus)) {
                    walletsService.reduceFrozenAmount(fiatWithdrawOrder.getUserId(), usdAmount, walletsUsdWithdraw.getUsdLogsId());
                } else {
                    walletsService.unFrozenAmount(fiatWithdrawOrder.getUserId(), usdAmount, walletsUsdWithdraw.getUsdLogsId(), CoinUnitEnum.idOf(fiatWithdrawOrder.getFiatType()));
                }
                //修改订单状态
                fiatWithdrawOrder.setStatus(resStatus);
                this.updateById(fiatWithdrawOrder);
            }
        } else {
            log.error("editOrders 未获取到cacheKey={} 锁资源", cacheKey);
        }
    }

}
