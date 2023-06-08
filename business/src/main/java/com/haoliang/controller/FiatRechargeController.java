package com.haoliang.controller;

import com.haoliang.common.annotation.IgnoreWebSecurity;
import com.haoliang.common.annotation.RepeatSubmit;
import com.haoliang.common.model.JsonResult;
import com.haoliang.model.dto.PayAmountDTO;
import com.haoliang.pay.id.model.IdPayCallBack;
import com.haoliang.pay.th.model.ThPayCallBack;
import com.haoliang.pay.vn.model.VnCallBack;
import com.haoliang.service.FiatRechargeOrderService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;

/**
 * @author Dominick Li
 * @Description 法币充值
 * @CreateTime 2023/4/15 9:59
 **/
@Slf4j
@RestController
@RequestMapping("/recharge")
public class FiatRechargeController {

    @Autowired
    private FiatRechargeOrderService fiatRechargeOrderService;

    /**
     * 支付的费率提示信息
     */
    @GetMapping
    public JsonResult info() {
        return fiatRechargeOrderService.info();
    }

    /**
     * 支付
     */
    @PostMapping("/pay")
    @RepeatSubmit
    public JsonResult pay(@Valid @RequestBody PayAmountDTO amountDTO) {
        return fiatRechargeOrderService.pay(amountDTO);
    }

    /**
     * 根据订单号查询订单状态
     */
    @GetMapping("/status/{orderNo}")
    public JsonResult queryStatus(@PathVariable Long orderNo) {
        return fiatRechargeOrderService.queryStatus(orderNo);
    }

    /**
     * 取消订单
     */
    @GetMapping("/cancel/{orderNo}")
    public JsonResult cancel(@PathVariable Long orderNo) {
        return fiatRechargeOrderService.cancel(orderNo);
    }

    /**
     * 印尼代收回调
     */
    @PostMapping("/idCallBack")
    @IgnoreWebSecurity
    @RepeatSubmit
    public String idCallBack(@RequestBody IdPayCallBack idPayCallBack) {
        log.info("印尼代付PayCallBack={}", idPayCallBack);
        return fiatRechargeOrderService.idCallBack(idPayCallBack);
    }

    /**
     * 越南代收回调
     */
    @PostMapping("/vnCallBack")
    @IgnoreWebSecurity
    @RepeatSubmit
    public String vnCallBack(@RequestBody VnCallBack vnCallBack) {
        log.info("越南代付CallBack={}", vnCallBack);
        return fiatRechargeOrderService.vnCallBack(vnCallBack);
    }

    /**
     * 泰国代收回调
     */
    @PostMapping("/thCallBack")
    @IgnoreWebSecurity
    @RepeatSubmit
    public String thCallBack(@RequestBody ThPayCallBack thCallBack) {
        log.info("泰国代付CallBack={}", thCallBack);
        return fiatRechargeOrderService.thCallBack(thCallBack);
    }

}
