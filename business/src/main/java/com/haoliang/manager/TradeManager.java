package com.haoliang.manager;

import cn.hutool.core.date.TimeInterval;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.haoliang.common.enums.BooleanEnum;
import com.haoliang.common.model.JsonResult;
import com.haoliang.constant.TiktokConfig;
import com.haoliang.enums.*;
import com.haoliang.mapper.AppUserMapper;
import com.haoliang.mapper.BusinessJobMapper;
import com.haoliang.model.*;
import com.haoliang.model.dto.AppUsersAmountDTO;
import com.haoliang.model.dto.TeamTaskDTO;
import com.haoliang.model.dto.TreePathAmountDTO;
import com.haoliang.model.dto.TreePathLevelDTO;
import com.haoliang.service.*;
import com.haoliang.utils.BigDecimalUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.Resource;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;

/**
 * @author Dominick Li
 * @Description 核心业务类
 * @CreateTime 2022/12/5 16:18
 **/
@Slf4j
@Component
public class TradeManager {

    @Autowired
    private WalletsService walletsService;

    @Autowired
    private TreePathService treePathService;

    @Autowired
    private WalletTttLogsService walletTttLogsService;

    @Autowired
    private VipOrdersService vipOrdersService;

    @Resource
    private BusinessJobMapper businessJobMapper;

    @Resource
    private AppUserMapper appUserMapper;

    @Transactional(rollbackFor = Exception.class)
    public void sendHoldingCoinTask(BusinessJob businessJob) {
        log.info("-------------开始发放持币奖--------------");
        TimeInterval timeInterval = new TimeInterval();
        timeInterval.start();
        List<AppUsersAmountDTO> appUsersAmountDTOList = walletsService.findHoldingCoinUserInfo();
        List<TreePathLevelDTO> treePathList;
        //上一级拿的，当前等级拿的，极差
        BigDecimal lastRate, rate, surplusRate;
        //最大代理商等级
        Integer maxUserLevel;
        BigDecimal amount;

        LocalDateTime yesterday = LocalDateTime.now().minusDays(1);
        long selectTime = timeInterval.intervalRestart();
        //用于汇总发放团队奖的比例
        HashMap<TreePathLevelDTO, BigDecimal> amountResult = new HashMap<>();
        TreePathLevelDTO prevUser;
        //社区等级
        for (AppUsersAmountDTO amountDTO : appUsersAmountDTOList) {
            treePathList = treePathService.getTreePathLevelOrderByLevel(amountDTO.getUserId());
            if (treePathList.size() == 0) {
                continue;
            }
            prevUser = new TreePathLevelDTO(amountDTO.getUserId(), amountDTO.getLevel(), null);
            lastRate = BigDecimal.ZERO;
            maxUserLevel = treePathList.stream().max(Comparator.comparing(x -> x.getUserLevel())).get().getUserLevel();
            //log.info("userId={},item maxUserLevel={}", amountDTO.getUserId(), maxUserLevel);
            for (TreePathLevelDTO treePathLevelDTO : treePathList) {
                if (treePathLevelDTO.getUserLevel() > prevUser.getUserLevel()) {
                    //根据代理商等级获取对应分红比例
                    rate = ProxyLevelEnum.getByLevel(treePathLevelDTO.getUserLevel()).getHoldingsRatio();
                    //极差 减去上一级别拿走的比例
                    surplusRate = rate.subtract(lastRate);
                    //根据极差计算发放团队奖的金额
                    amount = amountDTO.getTotalAmount().multiply(surplusRate);
                    //如果map中存在用户Id,金额累加
                    if (amountResult.containsKey(treePathLevelDTO)) {
                        amountResult.put(treePathLevelDTO, amountResult.get(treePathLevelDTO).add(amount));
                    } else {
                        amountResult.put(treePathLevelDTO, amount);
                    }
                    //重置计算极差需要的上一级比例
                    lastRate = rate;
                    //如果当前处理的供应商等级已是最大等级,跳出内层for循环 (最大级别已拿光分红比例)
                    if (treePathLevelDTO.getUserLevel().equals(maxUserLevel)) {
                        //如果上级是该链上最大的社区等级(拿完了下级的团队奖收益) 则跳出循环 不做后面的逻辑处理
                        break;
                    }
                    prevUser = treePathLevelDTO;
                }
            }
        }

        BigDecimal sendAmount;
        Integer userId;
        WalletTttLogs walletLogs;
        List<WalletTttLogs> walletLogsList = new ArrayList<>();
        for (Map.Entry<TreePathLevelDTO, BigDecimal> entry : amountResult.entrySet()) {
            //更新钱包余额
            sendAmount = entry.getValue();
            userId = entry.getKey().getUserId();
            sendAmount = sendAmount.setScale(2, RoundingMode.FLOOR);
            //返回可以发放的奖励余额
            BigDecimal result = vipOrdersService.chargebacks(userId, entry.getKey().getVipLevel(), sendAmount, TttLogTypeEnum.HOLDING_COINS);
            if (result.compareTo(BigDecimal.ZERO) > 0) {
                //更新钱包余额
                walletsService.lookUpdateWallets(entry.getKey().getUserId(), result, FlowingActionEnum.INCOME);
                walletLogs = WalletTttLogs.builder()
                        .userId(userId)
                        .amount(result)
                        .action(FlowingActionEnum.INCOME.getValue())
                        .type(TttLogTypeEnum.HOLDING_COINS.getValue())
                        .build();
                walletLogs.setCreateTime(yesterday);
                walletLogsList.add(walletLogs);
            }
            log.info("发放持币奖: userId={} ,level={},该发放的金额={},已发放={},冻结={}", userId, entry.getKey().getUserLevel(), sendAmount.toPlainString(), result.toPlainString(), sendAmount.subtract(result).toPlainString());
        }

        long computeTime = timeInterval.intervalRestart();
        //插入钱包流水变更记录
        if (walletLogsList.size() > 0) {
            walletTttLogsService.saveBatch(walletLogsList);
        }
        businessJob.setHoldingCoinTask(BooleanEnum.TRUE.intValue());
        businessJobMapper.insert(businessJob);
        long saveTime = timeInterval.interval();
        log.info("-------------结束发放持币奖 select times:{} ms ,compute times:{} ms,save times:{} ms,total:{} ms--------------", selectTime, computeTime, saveTime, (selectTime + computeTime + saveTime));

    }

    /**
     * 发放代数奖
     */
    @Transactional(rollbackFor = Exception.class)
    public void sendAlgebraTask(BusinessJob businessJob) {
        log.info("-------------开始发放代数奖--------------");
        TimeInterval timeInterval = new TimeInterval();
        timeInterval.start();
        LocalDateTime yesterday = LocalDateTime.now().minusDays(1);

        List<AppUsers> appUsersList = appUserMapper.selectList(new LambdaQueryWrapper<AppUsers>()
                .select(AppUsers::getId, AppUsers::getVipLevel,AppUsers::getLevel)
                .eq(AppUsers::getValid, BooleanEnum.TRUE.intValue())
                .eq(AppUsers::getEnabled, BooleanEnum.TRUE.intValue())
        );
        long selectTime = timeInterval.intervalRestart();

        List<WalletTttLogs> walletLogsList = new ArrayList<>();
        List<TreePathAmountDTO> treePathList;
        BigDecimal sendAmount;
        WalletTttLogs walletLogs;

        for (AppUsers appUsers : appUsersList) {
            //获取社区代数静态收益(只获取有效用户的)
            treePathList = treePathService.getTaskEarningsByUserIdAndLevelList(appUsers.getId(), TiktokConfig.ALGEBRA_LEVEL);
            sendAmount = new BigDecimal("0");
            for (TreePathAmountDTO treePathAmountDTO : treePathList) {
                sendAmount = sendAmount.add(treePathAmountDTO.getTotalAmount().multiply(AlgebraEnum.getRechargeMaxByLevel(treePathAmountDTO.getLevel())));
            }
            //金额大于零则发放给用户
            if (sendAmount.compareTo(BigDecimal.ZERO) > 0) {
                sendAmount = sendAmount.setScale(TiktokConfig.NUMBER_OF_DIGITS, RoundingMode.FLOOR);
                BigDecimal result = vipOrdersService.chargebacks(appUsers.getId(), appUsers.getVipLevel(), sendAmount, TttLogTypeEnum.ALGEBRA);
                if (result.compareTo(BigDecimal.ZERO) > 0) {
                    //更新钱包余额
                    walletsService.lookUpdateWallets(appUsers.getId(), result, FlowingActionEnum.INCOME);
                    walletLogs = WalletTttLogs.builder()
                            .userId(appUsers.getId())
                            .amount(result)
                            .action(FlowingActionEnum.INCOME.getValue())
                            .type(TttLogTypeEnum.ALGEBRA.getValue())
                            .build();
                    walletLogs.setCreateTime(yesterday);
                    walletLogsList.add(walletLogs);
                }
                log.info("发放代数奖: userId={} ,该发放的金额={},已发放={},冻结={} ", appUsers.getId(), sendAmount, result, sendAmount.subtract(result));
            }
        }
        long computeTime = timeInterval.intervalRestart();

        //插入钱包流水变更记录
        if (walletLogsList.size() > 0) {
            walletTttLogsService.saveBatch(walletLogsList);
        }
        businessJob.setAlgebraTask(BooleanEnum.TRUE.intValue());
        businessJobMapper.updateById(businessJob);

        long saveTime = timeInterval.interval();
        log.info("-------------结束发放代数奖 select times:{} ms ,compute times:{} ms,save times:{} ms,total:{} ms--------------", selectTime, computeTime, saveTime, (selectTime + computeTime + saveTime));
    }

    /**
     * 发放分红奖励
     */
    @Transactional(rollbackFor = Exception.class)
    public void sendSpecialTask(BusinessJob businessJob) {
        log.info("-------------开始发放分红奖--------------");
        TimeInterval timeInterval = new TimeInterval();
        timeInterval.start();
        LocalDateTime yesterday = LocalDateTime.now().minusDays(1);

        //供应商等级4和5级才有资格发放分红奖
        List<AppUsers> appUsersList = appUserMapper.selectList(new LambdaQueryWrapper<AppUsers>()
                .select(AppUsers::getId, AppUsers::getVipLevel)
                .eq(AppUsers::getEnabled, BooleanEnum.TRUE.intValue())
                .ge(AppUsers::getLevel, ProxyLevelEnum.FOUR.getLevel()));
        Integer proxyLevelCount = appUsersList.size();
        long selectTime = timeInterval.intervalRestart();

        List<WalletTttLogs> walletLogsList = new ArrayList<>();
        WalletTttLogs walletLogs;

        //获取社区任务的收益
        BigDecimal sumProfit = walletTttLogsService.getSumTaskEarnings();
        if (sumProfit == null || proxyLevelCount == 0 || sumProfit.compareTo(BigDecimal.ZERO) == 0) {
            return;
        }
        //该发放的分红奖励
        BigDecimal sendAmount = BigDecimalUtils.divideSaveTwoDecimal(sumProfit.multiply(TiktokConfig.SPECIAL_AWARD_RATE), new BigDecimal(proxyLevelCount));
        for (AppUsers appUsers : appUsersList) {
            BigDecimal result = vipOrdersService.chargebacks(appUsers.getId(), appUsers.getVipLevel(), sendAmount, TttLogTypeEnum.SPECIAL);
            if (result.compareTo(BigDecimal.ZERO) > 0) {
                //需要用总收益*收益比再除以 有资格拿分红奖的人数
                walletsService.lookUpdateWallets(appUsers.getId(), result, FlowingActionEnum.INCOME);
                walletLogs = WalletTttLogs.builder()
                        .userId(appUsers.getId())
                        .amount(result)
                        .action(FlowingActionEnum.INCOME.getValue())
                        .type(TttLogTypeEnum.SPECIAL.getValue())
                        .build();
                walletLogs.setCreateTime(yesterday);
                walletLogsList.add(walletLogs);
            }
            log.info("发放分红奖: userId={} ,该发放的金额={},已发放={},冻结={} ", appUsers.getId(), sendAmount.toPlainString(), result.toPlainString(), sendAmount.subtract(result).toPlainString());
        }
        long computeTime = timeInterval.intervalRestart();
        //插入钱包流水变更记录
        if (walletLogsList.size() > 0) {
            walletTttLogsService.saveBatch(walletLogsList);
        }
        businessJob.setSpecialTask(BooleanEnum.TRUE.intValue());
        businessJobMapper.updateById(businessJob);
        long saveTime = timeInterval.interval();
        log.info("-------------结束发放分红奖 select times:{} ms ,compute times:{} ms,save times:{} ms,total:{} ms--------------", selectTime, computeTime, saveTime, (selectTime + computeTime + saveTime));
    }


    /**
     * 发放团队奖
     */
    @Transactional(rollbackFor = Exception.class)
    public void sendTeamTask(BusinessJob businessJob) {
        log.info("-------------开始发放团队奖--------------");
        TimeInterval timeInterval = new TimeInterval();
        timeInterval.start();
        LocalDateTime yesterday = LocalDateTime.now().minusDays(1);
        //找到产生了任务收益的用户信息
        List<TeamTaskDTO> teamTaskDTOList = walletTttLogsService.getSumTaskEarningGroupByUser();

        long selectTime = timeInterval.intervalRestart();
        //计算的极差,发放的金额
        BigDecimal lastRate, amount;
        //用于汇总发放团队奖的比例
        HashMap<Integer, BigDecimal> amountResult = new HashMap<>();
        TreePathLevelDTO prevUser;
        //是否出现平级
        Map<Integer, Integer> existsEqLevel;
        //存在平级的用户Id
        Map<TreePathLevelDTO, Integer> existsEqLevelUserId = new HashMap<>();
        List<TreePathLevelDTO> treePathList;
        ProxyLevelEnum proxyLevelEnum;
        boolean eqLevelFlag;
        Map<Integer, Integer> userLevelMap = new HashMap<>();
        for (TeamTaskDTO teamTaskDTO : teamTaskDTOList) {
            prevUser = new TreePathLevelDTO(teamTaskDTO.getUserId(), teamTaskDTO.getLevel(), null);
            //找到用户的所有上级(代理商等级>0并且未出局有资格发放团队奖的),根据代数Level升序
            treePathList = treePathService.getTreePathLevelOrderByLevel(teamTaskDTO.getUserId());
            existsEqLevel = new HashMap<>();
            if (treePathList.size() > 0) {
                lastRate = BigDecimal.ZERO;
                eqLevelFlag = false;
                for (TreePathLevelDTO treePathLevelDTO : treePathList) {
                    userLevelMap.put(treePathLevelDTO.getUserId(), treePathLevelDTO.getVipLevel());
                    proxyLevelEnum = ProxyLevelEnum.getByLevel(treePathLevelDTO.getUserLevel());
                    amount = BigDecimal.ZERO;
                    if (treePathLevelDTO.getUserLevel() > prevUser.getUserLevel()) {
                        //只有上级的团队等级比下级大的有资格获取分红
                        amount = teamTaskDTO.getAmount().multiply(proxyLevelEnum.getIncomeRatio().subtract(lastRate));
                        //重置计算极差需要的上一级比例
                        lastRate = proxyLevelEnum.getIncomeRatio();
                        prevUser = treePathLevelDTO;
                    } else if (treePathLevelDTO.getUserLevel().equals(prevUser.getUserLevel()) && !existsEqLevel.containsKey(treePathLevelDTO.getUserLevel()) && !eqLevelFlag) {
                        //计算平级奖
                        existsEqLevel.put(treePathLevelDTO.getUserLevel(), 1);
                        existsEqLevelUserId.put(treePathLevelDTO, prevUser.getUserId());
                        prevUser = treePathLevelDTO;
                        eqLevelFlag = true;
                    }
                    //如果比例大于0则发放对应金额
                    if (amount.compareTo(BigDecimal.ZERO) > 0) {
                        //如果map中存在用户Id,金额累加
                        if (amountResult.containsKey(treePathLevelDTO.getUserId())) {
                            amountResult.put(treePathLevelDTO.getUserId(), amountResult.get(treePathLevelDTO.getUserId()).add(amount));
                        } else {
                            amountResult.put(treePathLevelDTO.getUserId(), amount);
                        }
                        log.info("userId={} 产生收益={}，上级userId={} 拿取的收益={}", teamTaskDTO.getUserId(), teamTaskDTO.getAmount(), treePathLevelDTO.getUserId(), amount);
                    }
                }
            }
        }

        //平级奖比例
        BigDecimal lateralIncomeRatio, lateralIncomAmount;
        //平级奖
        HashMap<Integer, BigDecimal> eqLevelAmountResult = new HashMap<>();
        //发放平级奖
        for (Map.Entry<TreePathLevelDTO, Integer> entry : existsEqLevelUserId.entrySet()) {
            lateralIncomeRatio = ProxyLevelEnum.getByLevel(entry.getKey().getUserLevel()).getLateralIncomeRatio();
            if (amountResult.containsKey(entry.getValue())) {
                lateralIncomAmount = amountResult.get(entry.getValue()).multiply(lateralIncomeRatio);
                eqLevelAmountResult.put(entry.getKey().getUserId(), lateralIncomAmount);
                log.info("发放团队平级奖: userId={},level={}, eqLevelUserId={} ,amount={}, lateralIncomAmount={}", entry.getKey().getUserId(), entry.getKey().getUserLevel(), entry.getValue(), amountResult.get(entry.getValue()), lateralIncomAmount);
            }
        }

        long computeTime = timeInterval.intervalRestart();
        //合并平级奖到团队奖中
        for (Map.Entry<Integer, BigDecimal> entry : amountResult.entrySet()) {
            if (eqLevelAmountResult.containsKey(entry.getKey())) {
                entry.setValue(entry.getValue().add(eqLevelAmountResult.remove(entry.getKey())));
            }
        }

        //未合并的平级奖单独发放到团队奖励
        for (Map.Entry<Integer, BigDecimal> entry : eqLevelAmountResult.entrySet()) {
            amountResult.put(entry.getKey(), entry.getValue());
        }

        List<WalletTttLogs> walletLogsList = new ArrayList<>();
        WalletTttLogs walletLogs;
        BigDecimal semdAmount;
        for (Map.Entry<Integer, BigDecimal> entry : amountResult.entrySet()) {
            semdAmount = entry.getValue();
            if (semdAmount.compareTo(BigDecimal.ZERO) > 0) {
                semdAmount = semdAmount.setScale(2, RoundingMode.FLOOR);
                BigDecimal result = vipOrdersService.chargebacks(entry.getKey(), userLevelMap.get(entry.getKey()), semdAmount, TttLogTypeEnum.TEAM);
                if (result.compareTo(BigDecimal.ZERO) > 0) {
                    //更新钱包余额
                    walletsService.lookUpdateWallets(entry.getKey(), result, FlowingActionEnum.INCOME);
                    walletLogs = WalletTttLogs.builder()
                            .userId(entry.getKey())
                            .amount(result)
                            .action(FlowingActionEnum.INCOME.getValue())
                            .type(TttLogTypeEnum.TEAM.getValue())
                            .build();
                    walletLogs.setCreateTime(yesterday);
                    walletLogsList.add(walletLogs);
                }
                log.info("发放团队奖: userId={} ,该发放的金额={},已发放={},冻结={} ", entry.getKey(), semdAmount.toPlainString(), result.toPlainString(), semdAmount.subtract(result).toPlainString());
            }
        }

        //插入钱包流水变更记录
        if (walletLogsList.size() > 0) {
            walletTttLogsService.saveBatch(walletLogsList);
        }

        //更新发放团队奖任务完成
        businessJob.setTeamTask(BooleanEnum.TRUE.intValue());
        businessJobMapper.updateById(businessJob);

        long saveTime = timeInterval.interval();
        log.info("-------------结束发放团队奖 select times:{} ms ,compute times:{} ms,save times:{} ms,total:{} ms--------------", selectTime, computeTime, saveTime, (selectTime + computeTime + saveTime));
    }

    public JsonResult test() {
        //插入需要更新测试代理商等级和业绩
        List<UpdateUserLevelJob> updateUserLevelJobList = new ArrayList<>();
        UpdateUserLevelJob updateUserLevelJob;
//        for (Integer userId = 100; userId <= 126; userId++) {
//            updateUserLevelJob = new UpdateUserLevelJob();
//            updateUserLevelJob.setUserId(userId);
//            updateUserLevelJobList.add(updateUserLevelJob);
//        }
//        updateUserLevelTaskService.saveBatch(updateUserLevelJobList);

        LocalDate localDate = LocalDate.now().minusDays(1);
        //删除昨天的代数奖，团队奖信息
        walletTttLogsService.remove(new LambdaQueryWrapper<WalletTttLogs>().ge(WalletTttLogs::getCreateTime, localDate)
                .in(WalletTttLogs::getType, TttLogTypeEnum.getDynamicTypeList())
        );
        //删除之前的任务记录
        businessJobMapper.delete(new LambdaQueryWrapper<BusinessJob>().eq(BusinessJob::getCreateDate, localDate));
        UpdateWrapper<Wallets> updateWrapper = Wrappers.update();
        updateWrapper.lambda()
                .set(Wallets::getWalletAmount, BigDecimal.ZERO)
                .eq(Wallets::getUserId, 10100);
        walletsService.update(updateWrapper);

        BusinessJob businessJob = new BusinessJob();
        businessJob.setCreateDate(localDate);
        //发放分红将
        sendHoldingCoinTask(businessJob);
        //发代数奖励
        sendAlgebraTask(businessJob);
        //发放团队奖
        sendTeamTask(businessJob);
        //发放特级奖
        sendSpecialTask(businessJob);
        return JsonResult.successResult("发放奖励成功,请查看");
    }


}
