package com.haoliang.service.impl;

import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.haoliang.common.config.GlobalProperties;
import com.haoliang.common.constant.CacheKeyPrefixConstants;
import com.haoliang.common.model.HttpResult;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.model.ThreadLocalManager;
import com.haoliang.common.util.BeanMapUtil;
import com.haoliang.common.util.IdUtil;
import com.haoliang.common.util.JwtTokenUtil;
import com.haoliang.pay.enums.CoinUnitEnum;
import com.haoliang.pay.enums.FiatTypeEnum;
import com.haoliang.enums.FlowingActionEnum;
import com.haoliang.enums.UsdLogTypeEnum;
import com.haoliang.mapper.FiatRechargeOrderMapper;
import com.haoliang.model.AppUsers;
import com.haoliang.model.CurrencyExchangeRates;
import com.haoliang.model.FiatRechargeOrder;
import com.haoliang.model.dto.PayAmountDTO;
import com.haoliang.model.vo.PayResultVO;
import com.haoliang.model.vo.StatusVO;
import com.haoliang.pay.id.IdPayUtils;
import com.haoliang.pay.enums.PayStatusEnum;
import com.haoliang.model.vo.FiatRechargeInfoVO;
import com.haoliang.pay.id.model.IdPayCallBack;
import com.haoliang.pay.id.model.IdPayResult;
import com.haoliang.pay.th.ThPayUtils;
import com.haoliang.pay.th.enums.ThPayStatusEnum;
import com.haoliang.pay.th.model.ThPayCallBack;
import com.haoliang.pay.th.model.ThPayResult;
import com.haoliang.pay.vn.VnPayUtils;
import com.haoliang.pay.vn.model.VnCallBack;
import com.haoliang.pay.vn.model.VnPayResult;
import com.haoliang.pay.vn.enums.VnPayStatusEnum;
import com.haoliang.service.AppUserService;
import com.haoliang.service.CurrencyExchangeRatesService;
import com.haoliang.service.FiatRechargeOrderService;
import com.haoliang.service.WalletsService;
import com.haoliang.utils.BigDecimalUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/4/15 14:22
 **/
@Slf4j
@Service
public class FiatRechargeOrderServiceImpl extends ServiceImpl<FiatRechargeOrderMapper, FiatRechargeOrder> implements FiatRechargeOrderService {

    @Autowired
    private AppUserService appUserService;

    @Autowired
    private WalletsService walletsService;

    @Autowired
    private StringRedisTemplate stringRedisTemplate;

    @Autowired
    private CurrencyExchangeRatesService currencyExchangeRatesService;

    @Override
    public JsonResult info() {
        List<FiatRechargeInfoVO> fiatRechargeInfoVOList = new ArrayList<>();
        FiatRechargeInfoVO fiatRechargeInfoVO;
        //金额限制需要减去手续费的额度
        BigDecimal minRate;

        for (FiatTypeEnum fiatTypeEnum : FiatTypeEnum.values()) {
            if (fiatTypeEnum.isOpening()) {
                //获取实时汇率
                BigDecimal exchangeRate = currencyExchangeRatesService.getExchangeRatesByType(fiatTypeEnum.getCoinUnit()).getExchangeRateUp();
                fiatRechargeInfoVO = new FiatRechargeInfoVO();
                BeanUtils.copyProperties(fiatTypeEnum, fiatRechargeInfoVO);
                fiatRechargeInfoVO.setFeeRate(fiatTypeEnum.getFeeRate().toPlainString());
                minRate = new BigDecimal("1").add(fiatTypeEnum.getFeeRate());
                fiatRechargeInfoVO.setMin(new BigDecimal(fiatRechargeInfoVO.getMin()).divide(minRate, 0, RoundingMode.UP).divide(exchangeRate, 0, RoundingMode.UP).intValue());
                //最大向下取整
                fiatRechargeInfoVO.setMax(new BigDecimal(fiatRechargeInfoVO.getMax()).divide(minRate, 0, RoundingMode.FLOOR).divide(exchangeRate, 0, RoundingMode.FLOOR).intValue());
                fiatRechargeInfoVO.setExchangeRate(exchangeRate.toPlainString());
                fiatRechargeInfoVOList.add(fiatRechargeInfoVO);
            }
        }
        return JsonResult.successResult(fiatRechargeInfoVOList);
    }

    @Override
    public JsonResult pay(PayAmountDTO amountDTO) {
        FiatTypeEnum typeEnum = FiatTypeEnum.typeOf(amountDTO.getType());
        if (typeEnum == null) {
            return JsonResult.failureResult();
        }
        //金额限制判断
        if (amountDTO.getAmount() < typeEnum.getMin() || amountDTO.getAmount() > typeEnum.getMax()) {
            log.info("代收金额限制 fiatType={} amount={}  not in {} ~ {}", typeEnum.getName(), amountDTO.getAmount(), typeEnum.getMin(), typeEnum.getMax());
            return JsonResult.failureResult();
        }
        Integer userId = JwtTokenUtil.getUserIdFromToken(ThreadLocalManager.getToken());
        Long orderNo = IdUtil.getSnowflakeNextId();
        String orderNoStr = orderNo.toString();
        LocalDateTime now = LocalDateTime.now();
        AppUsers appUsers = appUserService.selectColumnsByUserId(userId, AppUsers::getEmail, AppUsers::getNickName, AppUsers::getMobile);

        //充值金额里面包含了费率，需要计算出实际充值金额  减去基础服务费再除以费率比例     应付=10000*0.05-6500   收付费=应付/1.05+6500
        BigDecimal fee = new BigDecimal(amountDTO.getAmount()).multiply(typeEnum.getFeeRate()).setScale(2, RoundingMode.UP).add(new BigDecimal(typeEnum.getBaseFee()));
        BigDecimal actualAmount = new BigDecimal(amountDTO.getAmount()).subtract(fee);
        if (amountDTO.getType().equals(FiatTypeEnum.ID.getType())) {
            HttpResult<IdPayResult> httpResult = IdPayUtils.pay(now, orderNoStr, amountDTO.getAmount().toString(), appUsers.getMobile(), appUsers.getEmail());
            if (httpResult.isSuccess()) {
                log.info("印尼代收:  userId={}  ,应付={},手续费={},实际到账={}", userId, amountDTO.getAmount(), fee, actualAmount);
                IdPayResult idPayResult = httpResult.getData();
                FiatRechargeOrder fiatRechargeOrder = FiatRechargeOrder.builder()
                        .platformOrderNo(idPayResult.getPlatOrderNum())
                        .userId(userId)
                        .fiatType(amountDTO.getType())
                        .url(idPayResult.getUrl())
                        .amount(amountDTO.getAmount())
                        .status(PayStatusEnum.NO_PAY.getStatus())
                        //手续费
                        .fee(fee)
                        //实际tiktok入账法币
                        .actualAmount(actualAmount)
                        .build();
                fiatRechargeOrder.setId(orderNo);
                fiatRechargeOrder.setCreateTime(now);
                this.save(fiatRechargeOrder);
                return JsonResult.successResult(new PayResultVO(orderNoStr, idPayResult.getUrl()));
            } else {
                log.info("印尼代收 orderNum={} 请求失败,erroeMsg={}", orderNoStr, httpResult.getMsg());
            }
        } else if (amountDTO.getType().equals(FiatTypeEnum.VN.getType())) {
            HttpResult<VnPayResult> httpResult = VnPayUtils.pay(orderNoStr, amountDTO.getAmount().toString());
            if (httpResult.isSuccess()) {
                VnPayResult vnPayResult = httpResult.getData();
                //充值金额里面包含了费率，需要计算出实际充值金额  减去基础服务费再除以费率比例
                log.info("越南支付: userId={}  应付={},手续费={},实际到账={}", userId, amountDTO.getAmount(), fee, actualAmount);
                FiatRechargeOrder fiatRechargeOrder = FiatRechargeOrder.builder()
                        .platformOrderNo(vnPayResult.getSys_order_no())
                        .userId(userId)
                        .fiatType(amountDTO.getType())
                        .url(vnPayResult.getUrl())
                        .amount(amountDTO.getAmount())
                        .status(PayStatusEnum.NO_PAY.getStatus())
                        //手续费
                        .fee(fee)
                        //实际tiktok入账法币
                        .actualAmount(actualAmount)
                        .build();
                fiatRechargeOrder.setId(orderNo);
                fiatRechargeOrder.setCreateTime(now);
                this.save(fiatRechargeOrder);
                return JsonResult.successResult(new PayResultVO(orderNoStr, vnPayResult.getUrl()));
            } else {
                log.info("越南代收 orderNum={} 请求失败,erroeMsg={}", orderNoStr, httpResult.getMsg());
            }
        } else if (amountDTO.getType().equals(FiatTypeEnum.TH.getType())) {
            HttpResult<ThPayResult> httpResult = ThPayUtils.pay(orderNoStr, amountDTO.getAmount().toString());
            if (httpResult.isSuccess()) {
                ThPayResult vnPayResult = httpResult.getData();
                //充值金额里面包含了费率，需要计算出实际充值金额  减去基础服务费再除以费率比例
                log.info("泰国代收: userId={}  应付={},手续费={},实际到账={}", userId, amountDTO.getAmount(), fee, actualAmount);
                FiatRechargeOrder fiatRechargeOrder = FiatRechargeOrder.builder()
                        .userId(userId)
                        .fiatType(amountDTO.getType())
                        .url(vnPayResult.getData())
                        .amount(amountDTO.getAmount())
                        .status(PayStatusEnum.NO_PAY.getStatus())
                        //手续费
                        .fee(fee)
                        //实际tiktok入账法币
                        .actualAmount(actualAmount)
                        .build();
                fiatRechargeOrder.setId(orderNo);
                fiatRechargeOrder.setCreateTime(now);
                this.save(fiatRechargeOrder);
                return JsonResult.successResult(new PayResultVO(orderNoStr, vnPayResult.getData()));
            } else {
                log.info("泰国代收 orderNum={} 请求失败,erroeMsg={}", orderNoStr, httpResult.getMsg());
            }
        }
        return JsonResult.failureResult();
    }

    @Override
    public JsonResult queryStatus(Long orderNo) {
        FiatRechargeOrder fiatRechargeOrder = this.getById(orderNo);
        return JsonResult.successResult(new StatusVO(fiatRechargeOrder.getStatus()));
    }

    @Override
    public JsonResult cancel(Long orderNo) {
        UpdateWrapper<FiatRechargeOrder> update = Wrappers.update();
        update.lambda()
                .set(FiatRechargeOrder::getStatus, PayStatusEnum.PAY_CANCEL.getStatus())
                .eq(FiatRechargeOrder::getId, orderNo);
        this.update(update);
        return JsonResult.successResult();
    }

    /**
     * 修改订单状态
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void editOrders(FiatRechargeOrder fiatRechargeOrder, Integer resStatus) {
        String cacheKey = CacheKeyPrefixConstants.RECHARGE_ORDERS + fiatRechargeOrder.getId();
        boolean flag = stringRedisTemplate.opsForValue().setIfAbsent(cacheKey, "1", 15, TimeUnit.SECONDS);
        if (flag) {
            if (PayStatusEnum.SUCCESS.getStatus().equals(resStatus) && !fiatRechargeOrder.getStatus().equals(PayStatusEnum.SUCCESS.getStatus())) {
                CurrencyExchangeRates currencyExchangeRates = currencyExchangeRatesService.getExchangeRatesByType(FiatTypeEnum.typeOf(fiatRechargeOrder.getFiatType()).getCoinUnit());
                //充值成功 用法币除以兑美元汇率然后向下取整
                fiatRechargeOrder.setExchangeRate(currencyExchangeRates.getExchangeRate());
                fiatRechargeOrder.setUsdAmount(BigDecimalUtils.divideSaveTwoDecimal(fiatRechargeOrder.getActualAmount(), currencyExchangeRates.getExchangeRateUp()));
                //给客戶钱包添加流水记录
                walletsService.updateUsdWallet(fiatRechargeOrder.getUsdAmount(), fiatRechargeOrder.getUserId(), FlowingActionEnum.INCOME, UsdLogTypeEnum.RECHARGE, CoinUnitEnum.idOf(fiatRechargeOrder.getFiatType()));
            }
            fiatRechargeOrder.setStatus(resStatus);
            this.updateById(fiatRechargeOrder);
        } else {
            log.error("editOrders 未获取到cacheKey={} 锁资源", cacheKey);
        }
    }

    @Override
    public String idCallBack(IdPayCallBack idPayCallBack) {
        //印尼支付回调
        boolean pass = IdPayUtils.verifySign(BeanMapUtil.beanToTreeMap(idPayCallBack));
        if(!GlobalProperties.isProdEnv()){
            pass = true;
        }
        if (pass) {
            //验证签名通过
            PayStatusEnum payStatusEnum = PayStatusEnum.nameOf(idPayCallBack.getStatus());
            Integer resStatus = payStatusEnum.getStatus();
            FiatRechargeOrder fiatRechargeOrder = this.getById(Long.parseLong(idPayCallBack.getOrderNum()));
            if (fiatRechargeOrder != null) {
                editOrders(fiatRechargeOrder, resStatus);
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
            VnPayStatusEnum payStatusEnum = VnPayStatusEnum.nameOf(vnCallBack.getStatus());
            FiatRechargeOrder fiatRechargeOrder = this.getById(Long.parseLong(vnCallBack.getOrder_no()));
            if (fiatRechargeOrder != null) {
                editOrders(fiatRechargeOrder, payStatusEnum.getStatus());
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
            FiatRechargeOrder fiatRechargeOrder = this.getById(Long.parseLong(thPayCallBack.getOrder_no()));
            if (fiatRechargeOrder != null) {
                ThPayStatusEnum thPayStatusEnum = ThPayStatusEnum.nameOf(thPayCallBack.getState());
                fiatRechargeOrder.setPlatformOrderNo(thPayCallBack.getOrder_id());
                editOrders(fiatRechargeOrder, thPayStatusEnum.getStatus());
            }
            return "SUCCESS";
        }
        return "verifySign error";
    }
}
