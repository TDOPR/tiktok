package com.haoliang.controller;

import com.haoliang.common.annotation.IgnoreWebSecurity;
import com.haoliang.common.annotation.RepeatSubmit;
import com.haoliang.common.model.JsonResult;
import com.haoliang.model.dto.ProxyPayAmountDTO;
import com.haoliang.model.vo.FiatWithdrawInfoVO;
import com.haoliang.pay.id.model.IdProxyPayCallBack;
import com.haoliang.pay.th.model.ThPayCallBack;
import com.haoliang.pay.vn.model.VnCallBack;
import com.haoliang.service.FiatWithdrawOrderService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;

/**
 * @author Dominick Li
 * @Description 法币提现
 * @CreateTime 2023/4/16 10:30
 **/
@Slf4j
@RestController
@RequestMapping("/withdraw")
public class FiatWithdrawController {

    @Autowired
    private FiatWithdrawOrderService fiatWithdrawOrderService;

    /**
     * 提现的费率提示信息
     */
    @GetMapping
    public JsonResult<List<FiatWithdrawInfoVO>> info() {
        return fiatWithdrawOrderService.info();
    }

    /**
     * 提现
     */
    @PostMapping("/pay")
    @RepeatSubmit
    public JsonResult pay(@Valid @RequestBody ProxyPayAmountDTO amountDTO) {
        return fiatWithdrawOrderService.pay(amountDTO);
    }

    /**
     * 印尼代付回调
     */
    @PostMapping("/idCallBack")
    @IgnoreWebSecurity
    @RepeatSubmit
    public String idCallBack(@RequestBody IdProxyPayCallBack idProxyPayCallBack) {
        log.info("印尼代付CallBack={}", idProxyPayCallBack);
        return fiatWithdrawOrderService.idCallBack(idProxyPayCallBack);
    }

    /**
     * 越南代付回调
     */
    @PostMapping("/vnCallBack")
    @IgnoreWebSecurity
    @RepeatSubmit
    public String vnCallBack(@RequestBody VnCallBack vnCallBack) {
        log.info("越南代付CallBack={}", vnCallBack);
        return fiatWithdrawOrderService.vnCallBack(vnCallBack);
    }

    /**
     * 泰国代付回调
     */
    @PostMapping("/thCallBack")
    @IgnoreWebSecurity
    @RepeatSubmit
    public String thCallBack(@RequestBody ThPayCallBack thPayCallBack) {
        log.info("泰国代付CallBack={}", thPayCallBack);
        return fiatWithdrawOrderService.thCallBack(thPayCallBack);
    }

}
