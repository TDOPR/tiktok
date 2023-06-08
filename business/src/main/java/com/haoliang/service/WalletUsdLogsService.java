package com.haoliang.service;

import com.baomidou.mybatisplus.extension.service.IService;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.model.PageParam;
import com.haoliang.enums.FlowingActionEnum;
import com.haoliang.enums.UsdLogTypeEnum;
import com.haoliang.model.WalletUsdLogs;
import com.haoliang.model.condition.BillDetailsCondition;
import com.haoliang.model.vo.WalletsUsdLogDetailVO;
import com.haoliang.pay.enums.CoinUnitEnum;

import java.math.BigDecimal;

public interface WalletUsdLogsService  extends IService<WalletUsdLogs> {

    /**
     * 插入流水记录
     * @param userId  用户Id
     * @param amount  变更的金额
     * @param flowingActionEnum 收入或支出
     * @param usdLogTypeEnum 流水类型
     * @return 执行结果
     */
    Long insertWalletLogs(Integer userId, BigDecimal amount, FlowingActionEnum flowingActionEnum, UsdLogTypeEnum usdLogTypeEnum);

    /**
     * 插入流水记录
     * @param userId  用户Id
     * @param amount  变更的金额
     * @param flowingActionEnum 收入或支出
     * @param usdLogTypeEnum 流水类型
     * @param coinUnitEnum 充值和提现
     * @return 执行结果
     */
    Long insertWalletLogs(Integer userId, BigDecimal amount, FlowingActionEnum flowingActionEnum, UsdLogTypeEnum usdLogTypeEnum, CoinUnitEnum coinUnitEnum);

    JsonResult<WalletsUsdLogDetailVO> getMybillDetails(PageParam<WalletUsdLogs, BillDetailsCondition> pageParam);
}
