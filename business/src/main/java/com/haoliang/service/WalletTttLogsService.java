package com.haoliang.service;

import com.baomidou.mybatisplus.extension.service.IService;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.model.PageParam;
import com.haoliang.common.model.dto.PageDTO;
import com.haoliang.common.model.dto.TypeDTO;
import com.haoliang.enums.FlowingActionEnum;
import com.haoliang.enums.TttLogTypeEnum;
import com.haoliang.model.WalletTttLogs;
import com.haoliang.model.condition.BillDetailsCondition;
import com.haoliang.model.dto.EarningsDTO;
import com.haoliang.model.dto.TeamTaskDTO;
import com.haoliang.model.vo.*;

import java.math.BigDecimal;
import java.util.List;

public interface WalletTttLogsService extends IService<WalletTttLogs> {

    /**
     * 插入流水记录
     * @param userId  用户Id
     * @param amount  变更的金额
     * @param flowingActionEnum 收入或支出
     * @param tttLogTypeEnum 流水类型
     * @param zero 零撸
     * @return 执行结果
     */
    boolean insertZeroWalletLogs(Integer userId,BigDecimal amount, FlowingActionEnum flowingActionEnum, TttLogTypeEnum tttLogTypeEnum);

    boolean insertWalletLogs(Integer userId,BigDecimal amount, FlowingActionEnum flowingActionEnum, TttLogTypeEnum tttLogTypeEnum);

    /**
     * 根据用户Id获取收益信息
     * @param userId
     * @return
     */
    EarningsDTO getMyEarningsWalletLogs(Integer userId);

    /**
     * 获取我的钱包账单明细
     * @return
     */
    JsonResult<WalletLogsDetailVO> getMybillDetails(PageParam<WalletTttLogs, BillDetailsCondition> pageParam);

    /**
     * 获取动态奖励明细
     * @return
     */
    JsonResult<CommunityRewardDetailVO> communityRewardDetail(PageDTO pageDTO);

    /**
     * 任务收益明细
     */
    JsonResult taskEarningsDetail(TypeDTO pageDTO);

    BigDecimal sumTotalEarnings(Integer userId);

    /**
     * 查询所有用户的任务收益
     * @return
     */
    BigDecimal getSumTaskEarnings();

    /**
     * 获取用户收益信息
     * @return
     */
    AppUserRewardVO getUserReward(Integer userId);

    List<TeamTaskDTO> getSumTaskEarningGroupByUser();

    BigDecimal sumTotalAmountByTypeList(List<Integer> taskTypeList);

    BigDecimal sumYesterdayTotalAmountByTypeList(List<Integer> taskTypeList);

    BigDecimal sumYesterdayTotalZeroAmount();

    /**
     * 清除零撸用户超时的收益
     */
    void clearExpired();

    BigDecimal sumTotalAmountByTypeListAndParentId(List<Integer> taskTypeList, Integer userId);

    BigDecimal sumYesterdayTotalAmountByTypeListAndParentId(List<Integer> taskTypeList, Integer userId);

    BigDecimal sumYesterdayTotalZeroAmountAndParentId(Integer userId);
}
