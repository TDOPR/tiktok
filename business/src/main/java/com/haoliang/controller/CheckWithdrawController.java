package com.haoliang.controller;

import com.haoliang.common.annotation.PrintLog;
import com.haoliang.common.annotation.RepeatSubmit;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.model.PageParam;
import com.haoliang.model.usd.EvmWithdraw;
import com.haoliang.model.condition.WalletsUsdWithdrawCondition;
import com.haoliang.model.dto.AuditCheckDTO;
import com.haoliang.pay.enums.CoinUnitEnum;
import com.haoliang.service.WalletsUsdWithdrawService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

/**
 * @author Dominick Li
 * @Description 提现审核
 * @CreateTime 2023/1/8 15:31
 **/
@RestController
@RequestMapping("/auditWithdrawal")
public class CheckWithdrawController {

    @Autowired
    private WalletsUsdWithdrawService walletsUsdWithdrawService;

    /**
     * 分页查询
     */
    @PostMapping("/pagelist")
    @PreAuthorize("hasAuthority('examine:taking:list')")
    public JsonResult pageList(@RequestBody PageParam<EvmWithdraw, WalletsUsdWithdrawCondition> pageParam) {
        return walletsUsdWithdrawService.pageList(pageParam);
    }

    /**
     * 审核
     */
    @PrintLog
    @PostMapping("/check")
    @RepeatSubmit
    @PreAuthorize("hasAnyAuthority('examine:taking:pass','examine:taking:reject')")
    public JsonResult check(@RequestBody AuditCheckDTO auditCheckDTO) {
        return walletsUsdWithdrawService.check(auditCheckDTO);
    }

    /**
     * 获取提现渠道类型
     */
    @GetMapping("/channelList")
    public JsonResult getChannelList() {
        return JsonResult.successResult(CoinUnitEnum.getSelectList());
    }
}
