package com.haoliang.controller;

import com.haoliang.common.annotation.PrintLog;
import com.haoliang.common.annotation.RepeatSubmit;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.model.PageParam;
import com.haoliang.common.model.dto.TypeDTO;
import com.haoliang.model.WalletTttLogs;
import com.haoliang.model.WalletUsdLogs;
import com.haoliang.model.condition.BillDetailsCondition;
import com.haoliang.model.dto.BuyVipDTO;
import com.haoliang.model.dto.UsdtWithdrawalDTO;
import com.haoliang.model.vo.*;
import com.haoliang.service.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;

/**
 * @author Dominick Li
 * @Description 钱包相关接口
 * @CreateTime 2023/5/10 10:28
 **/
@RestController
@RequestMapping("/wallet")
public class WalletController {

    @Autowired
    private WalletsService walletsService;

    @Autowired
    private WalletTttLogsService walletLogService;

    @Autowired
    private WalletUsdLogsService walletUsdLogsService;

    @Autowired
    private WalletsUsdWithdrawService walletsUsdWithdrawService;

    @Autowired
    private FreezeProxyLogsService freezeProxyLogsService;

    /**
     * 我的钱包
     */
    @GetMapping
    @RepeatSubmit
    public JsonResult<MyWalletsVO> getMyWallet() {
        return walletsService.getMyWallet();
    }

    /**
     * 获取Vip套餐列表
     */
    @GetMapping("/getVipList")
    public JsonResult<List<VipLevelVO>> getVipList() {
        return walletsService.getVipList();
    }

    /**
     * 购买Vip
     */
    @PostMapping("/buyVip")
    public JsonResult buyVip(@RequestBody BuyVipDTO buyVipDTO) {
        return walletsService.buyVip(buyVipDTO);
    }

    /**
     * usdt提现
     */
    @PrintLog
    @RepeatSubmit
    @PostMapping("/withdrawal/usdt")
    public JsonResult usdtWithdrawal(@Valid @RequestBody UsdtWithdrawalDTO userWalletsDTO) {
        return walletsUsdWithdrawService.usdtWithdrawal(userWalletsDTO);
    }

    /**
     * ttt账单明细
     */
    @PostMapping("/tttBillDetails")
    public JsonResult<WalletLogsDetailVO> tttBillDetails(@RequestBody PageParam<WalletTttLogs, BillDetailsCondition> pageParam) {
        return walletLogService.getMybillDetails(pageParam);
    }

    /**
     * usd账单明细
     */
    @PostMapping("/usdBillDetails")
    public JsonResult<WalletsUsdLogDetailVO> usdBillDetails(@RequestBody PageParam<WalletUsdLogs, BillDetailsCondition> pageParam) {
        return walletUsdLogsService.getMybillDetails(pageParam);
    }

    /**
     * 社区奖励明细
     */
    @PostMapping("/communityRewardDetail")
    public JsonResult<CommunityRewardDetailVO> communityRewardDetail(@RequestBody TypeDTO pageDTO) {
        return walletLogService.communityRewardDetail(pageDTO);
    }

    /**
     * 任务收益明细
     */
    @PostMapping("/taskEarningsDetail")
    public JsonResult<TaskEarningsDetailVO> taskEarningsDetail(@RequestBody TypeDTO pageDTO) {
        return walletLogService.taskEarningsDetail(pageDTO);
    }

    /**
     * 为用户绑定一条区块链地址
     */
    @GetMapping("/getBlockAddress/{networdName}")
    public JsonResult getBlockAddress(@PathVariable String networdName) {
        return walletsService.getBlockAddress(networdName);
    }

    /**
     * 领取七天待领取的奖励
     */
    @PostMapping("/receive")
    public JsonResult receive() {
        return freezeProxyLogsService.receiveReward();
    }

}
