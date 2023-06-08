package com.haoliang.service;

import com.baomidou.mybatisplus.extension.service.IService;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.model.PageParam;
import com.haoliang.model.WalletsUsdWithdraw;
import com.haoliang.model.condition.WalletsUsdWithdrawCondition;
import com.haoliang.model.dto.AuditCheckDTO;
import com.haoliang.model.dto.UsdtWithdrawalDTO;
import com.haoliang.model.usd.EvmWithdraw;

public interface WalletsUsdWithdrawService extends IService<WalletsUsdWithdraw> {

    JsonResult pageList(PageParam<EvmWithdraw, WalletsUsdWithdrawCondition> pageParam);

    JsonResult check(AuditCheckDTO auditCheckDTO);

    JsonResult usdtWithdrawal(UsdtWithdrawalDTO userWalletsDTO);

    void scanWithdrawData();

}
