package com.haoliang.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.haoliang.common.base.DefaultCondition;
import com.haoliang.common.constant.SystemConstants;
import com.haoliang.common.enums.BooleanEnum;
import com.haoliang.common.enums.RoleTypeEnum;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.model.PageParam;
import com.haoliang.common.model.ThreadLocalManager;
import com.haoliang.common.model.vo.PageVO;
import com.haoliang.common.util.DateUtil;
import com.haoliang.common.util.JwtTokenUtil;
import com.haoliang.common.util.NumberUtil;
import com.haoliang.constant.TiktokConfig;
import com.haoliang.enums.ProxyLevelEnum;
import com.haoliang.enums.TttLogTypeEnum;
import com.haoliang.enums.UsdLogTypeEnum;
import com.haoliang.mapper.*;
import com.haoliang.model.*;
import com.haoliang.model.dto.AppUserCountDTO;
import com.haoliang.model.vo.*;
import com.haoliang.service.KLineDataService;
import com.haoliang.service.StatService;
import com.haoliang.service.WalletTttLogsService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/3/31 16:20
 **/
@Slf4j
@Service
public class StatServiceImpl implements StatService {

    @Resource
    private WalletsMapper walletsMapper;

    @Autowired
    private WalletTttLogsService walletTttLogsService;

    @Resource
    private WalletUsdLogsMapper walletUsdLogsMapper;

    @Resource
    private StatUserMapper statUserMapper;

    @Resource
    private StatInputAndOutputMapper statInputAndOutPutMapper;

    @Resource
    private StatDynamicAndStaticMapper statDynamicAndStaticMapper;

    @Resource
    private StatBalanceMapper statBalanceMapper;

    @Resource
    private AppUserMapper appUserMapper;

    @Resource
    private VipOrdersMapper vipOrdersMapper;

    @Autowired
    private KLineDataService kLineDataService;

    @Resource
    private TreePathMapper treePathMapper;

    @Override
    public void stat() {
        try {
            //统计用户数据
            statUser();
            log.info("statUser success");
        } catch (Exception e) {
            e.printStackTrace();
            log.error("statUser error:{}", e);
        }
        try {
            //统计动静态信息
            statDynamicAndStatic();
            log.info("statDynamicAndStatic success");
        } catch (Exception e) {
            e.printStackTrace();
            log.error("statDynamicAndStatic error:{}", e);
        }
        try {
            //统计余额信息
            statBalance();
            log.info("statBalance success");
        } catch (Exception e) {
            e.printStackTrace();
            log.error("statBalance error:{}", e);
        }
        try {
            //统计出入金信息
            statStatInputAndOutput();
            log.info("statStatInputAndOutput success");
        } catch (Exception e) {
            e.printStackTrace();
            log.error("statStatInputAndOutput error:{}", e);
        }
    }

    /**
     * 统计用户数据
     */
    public void statUser() {
        LocalDate yesterday = DateUtil.getYesterdayLocalDate();
        if (statUserMapper.selectCount(new LambdaQueryWrapper<StatUser>().eq(StatUser::getCreateDate, yesterday)) > 0) {
            //已插入过昨日数据
            return;
        }
        StatUser statUser = buildNewData(LocalDate.now());
        statUser.setCreateDate(yesterday);
        statUserMapper.insert(statUser);
    }

    /**
     * 获取最新的用户统计数据
     */
    private StatUser buildNewData(LocalDate localDate) {
        //查询昨天的用户
        StatUser statUser = statUserMapper.selectOne(new LambdaQueryWrapper<StatUser>().eq(StatUser::getCreateDate, localDate.minusDays(2)));
        int yesterdayNum = 0;
        if (statUser != null) {
            yesterdayNum = statUser.getTotal();
        }
        Integer total, valid, yesterdayAdd;
        //查询总用户数
        total = appUserMapper.selectCount(new LambdaQueryWrapper<AppUsers>()
                        .eq(AppUsers::getEnabled, BooleanEnum.TRUE.intValue())
                        .le(AppUsers::getCreateTime, localDate))
                .intValue();
        //查询有效用户数
        valid = appUserMapper.selectCount(new LambdaQueryWrapper<AppUsers>()
                        .ge(AppUsers::getValid, BooleanEnum.TRUE.intValue())
                        .le(AppUsers::getCreateTime, localDate))
                .intValue();
        //计算新增数量
        yesterdayAdd = total - yesterdayNum;
        return StatUser.builder()
                .total(total)
                .community(appUserMapper.selectCount(
                        new LambdaQueryWrapper<AppUsers>()
                                .ge(AppUsers::getLevel, ProxyLevelEnum.ONE.getLevel())
                                .le(AppUsers::getCreateTime, localDate)).intValue())
                .valid(valid)
                .zero(total - valid)
                .yesterdayAdd(yesterdayAdd)
                .build();
    }

    /**
     * 统计动静态数据
     */
    public void statDynamicAndStatic() {
        LocalDate yesterday = DateUtil.getYesterdayLocalDate();
        if (statDynamicAndStaticMapper.selectCount(new LambdaQueryWrapper<StatDynamicAndStatic>().eq(StatDynamicAndStatic::getCreateDate, yesterday)) > 0) {
            //已插入过昨日数据
            return;
        }
        //昨日动态收益
        BigDecimal dynamic = walletTttLogsService.sumYesterdayTotalAmountByTypeList(TttLogTypeEnum.getDynamicTypeList());
        if (dynamic == null) {
            dynamic = BigDecimal.ZERO;
        }
        //昨日静态收益
        BigDecimal statics = walletTttLogsService.sumYesterdayTotalAmountByTypeList(TttLogTypeEnum.getTaskTypeList());
        if (statics == null) {
            statics = BigDecimal.ZERO;
        }
        statDynamicAndStaticMapper.insert(StatDynamicAndStatic.builder()
                .createDate(yesterday)
                .dynamic(dynamic)
                .statics(statics)
                .build());
    }

    /**
     * 统计余额数据
     */
    public void statBalance() {
        LocalDate yesterday = DateUtil.getYesterdayLocalDate();
        if (statBalanceMapper.selectCount(new LambdaQueryWrapper<StatBalance>().eq(StatBalance::getCreateDate, yesterday)) > 0) {
            //已插入过昨日数据
            return;
        }
        BigDecimal usdAmount = walletsMapper.sumUsdAmount();
        BigDecimal tttAmount = walletsMapper.sumTttAmount();
        //今日Usd转入
        BigDecimal tttToUsd = walletUsdLogsMapper.sumYesterdayTotalAmountByType(UsdLogTypeEnum.TTT_TRANSFER_IN.getValue());
        statBalanceMapper.insert(StatBalance.builder()
                .createDate(yesterday)
                .ttt(tttAmount)
                .usd(usdAmount)
                .tttToUsd(tttToUsd)
                .build());
    }

    /**
     * 统计充值和提现数据
     */
    public void statStatInputAndOutput() {
        LocalDate yesterday = DateUtil.getYesterdayLocalDate();
        if (statInputAndOutPutMapper.selectCount(new LambdaQueryWrapper<StatInputAndOutput>().eq(StatInputAndOutput::getCreateDate, yesterday)) > 0) {
            //已插入过昨日数据
            return;
        }
        //入金
        BigDecimal input = walletUsdLogsMapper.sumYesterdayTotalAmountByType(UsdLogTypeEnum.RECHARGE.getValue());
        //出金
        BigDecimal output = walletUsdLogsMapper.sumYesterdayTotalAmountByType(UsdLogTypeEnum.WITHDRAWAL.getValue());
        //总入金
        BigDecimal totalInput = walletUsdLogsMapper.sumAmountByType(UsdLogTypeEnum.RECHARGE.getValue());
        //总出金
        BigDecimal totalOutput = walletUsdLogsMapper.sumAmountByType(UsdLogTypeEnum.WITHDRAWAL.getValue());
        if (totalInput == null) {
            totalInput = BigDecimal.ZERO;
        }
        if (totalOutput == null) {
            totalOutput = BigDecimal.ZERO;
        }
        //当日不含ttt的TVL=总入金-总出金
        BigDecimal tvl = totalInput.subtract(totalOutput);
        //总ttt转入到usd的金额
        BigDecimal tttToUsd = walletUsdLogsMapper.sumAmountByType(UsdLogTypeEnum.TTT_TRANSFER_IN.getValue());
        if (tttToUsd == null) {
            tttToUsd = BigDecimal.ZERO;
        }
        //总ttt持有量
        BigDecimal totalTtt = walletsMapper.sumTttAmount();
        if (totalTtt == null) {
            totalTtt = BigDecimal.ZERO;
        }
        //计算ttt根据汇率转换成的usd金额
        totalTtt = totalTtt.multiply(kLineDataService.getNowExchangeRate());
        //当日含ttt的tvl=总入金+ttt持有转为usd+ttt转usd-总出金
        BigDecimal tvlTtt = totalInput.subtract(totalOutput).add(tttToUsd).add(totalTtt);

        statInputAndOutPutMapper.insert(StatInputAndOutput.builder()
                .createDate(yesterday)
                .input(input)
                .output(output)
                .tvlTtt(tvlTtt)
                .tvl(tvl)
                .bubble(tvlTtt.subtract(tvl))
                .build());
    }

    @Override
    public JsonResult<PageVO<StatUser>> statUserPage(PageParam<StatUser, DefaultCondition> pageParam) {
        if (pageParam.getSearchParam() == null) {
            pageParam.setSearchParam(new DefaultCondition());
        }
        Page<StatUser> page = this.statUserMapper.selectPage(pageParam.getPage(), pageParam.getSearchParam().buildQueryParam(SystemConstants.ORDER_BY));
        return JsonResult.successResult(new PageVO<>(page));
    }

    @Override
    public JsonResult userNowData() {
        StatUser statUser = buildNewData(LocalDate.now().plusDays(1));
        StatUserVO statUserVO = new StatUserVO();
        BeanUtils.copyProperties(statUser, statUserVO);
        statUserVO.setNow(LocalDateTime.now());
        return JsonResult.successResult(statUserVO);
    }

    @Override
    public JsonResult<PageVO<StatInputAndOutput>> inputAndOutPutPage(PageParam<StatInputAndOutput, DefaultCondition> pageParam) {
        if (pageParam.getSearchParam() == null) {
            pageParam.setSearchParam(new DefaultCondition());
        }
        Page<StatInputAndOutput> page = this.statInputAndOutPutMapper.selectPage(pageParam.getPage(), pageParam.getSearchParam().buildQueryParam(SystemConstants.ORDER_BY));
        return JsonResult.successResult(new PageVO<>(page));
    }

    @Override
    public JsonResult dynamicAndStaticPage(PageParam<StatDynamicAndStatic, DefaultCondition> pageParam) {
        if (pageParam.getSearchParam() == null) {
            pageParam.setSearchParam(new DefaultCondition());
        }
        Page<StatDynamicAndStatic> page = this.statDynamicAndStaticMapper.selectPage(pageParam.getPage(), pageParam.getSearchParam().buildQueryParam(SystemConstants.ORDER_BY));
        List<StatDynamicAndStaticVO> list = new ArrayList<>();
        StatDynamicAndStaticVO statDynamicAndStaticVO;
        for (StatDynamicAndStatic statDynamicAndStatic : page.getRecords()) {
            statDynamicAndStaticVO = new StatDynamicAndStaticVO();
            BeanUtils.copyProperties(statDynamicAndStatic, statDynamicAndStaticVO);
            statDynamicAndStaticVO.setTotal(statDynamicAndStatic.getDynamic().add(statDynamicAndStatic.getStatics()));
            list.add(statDynamicAndStaticVO);
        }
        return JsonResult.successResult(new PageVO<>(page.getTotal(), page.getPages(), list));
    }

    @Override
    public JsonResult balancePage(PageParam<StatBalance, DefaultCondition> pageParam) {
        if (pageParam.getSearchParam() == null) {
            pageParam.setSearchParam(new DefaultCondition());
        }
        Page<StatBalance> page = this.statBalanceMapper.selectPage(pageParam.getPage(), pageParam.getSearchParam().buildQueryParam(SystemConstants.ORDER_BY));
        return JsonResult.successResult(new PageVO<>(page));
    }

    @Override
    public JsonResult fund() {
        BigDecimal input = walletUsdLogsMapper.sumAmountByType(UsdLogTypeEnum.RECHARGE.getValue());
        if (input == null) {
            input = BigDecimal.ZERO;
        }
        BigDecimal output = walletUsdLogsMapper.sumAmountByType(UsdLogTypeEnum.WITHDRAWAL.getValue());
        if (output == null) {
            output = BigDecimal.ZERO;
        }
        //今日入金
        BigDecimal yesterdayInput = walletUsdLogsMapper.sumToDayTotalAmountByType(UsdLogTypeEnum.RECHARGE.getValue());
        //今日出金
        BigDecimal yesterdayOutput = walletUsdLogsMapper.sumToDayTotalAmountByType(UsdLogTypeEnum.WITHDRAWAL.getValue());
        return JsonResult.successResult(FundVO.builder()
                .now(LocalDateTime.now())
                .input(NumberUtil.toPlainString(input))
                .output(NumberUtil.toPlainString(output))
                .tvl(NumberUtil.toPlainString(input.subtract(output)))
                .yesterdayInput(NumberUtil.toPlainString(yesterdayInput))
                .yesterdayOutput(NumberUtil.toPlainString(yesterdayOutput))
                .build());
    }

    @Override
    public BusinessVO getAdminPanel() {
        Integer roleId = JwtTokenUtil.getRoleIdFromToken(ThreadLocalManager.getToken());
        if (RoleTypeEnum.PROXY.getId().equals(roleId)) {
            return getByProxy();
        }
        return getByAdmin();
    }


    /**
     * 获取管理员角色面板
     */
    private BusinessVO getByAdmin() {
        List<PortraitSelectVO> user = new ArrayList<>();
        long totalUser = appUserMapper.selectCount(new LambdaQueryWrapper<>());
        long vaildUser = appUserMapper.selectCount(new LambdaQueryWrapper<AppUsers>().eq(AppUsers::getValid, BooleanEnum.TRUE.intValue()));
        long zeroUser = totalUser - vaildUser;
        user.add(new PortraitSelectVO("总用户", String.valueOf(totalUser)));
        user.add(new PortraitSelectVO("有效用户", String.valueOf(vaildUser)));
        user.add(new PortraitSelectVO("零撸用户", String.valueOf(zeroUser)));
        List<AppUserCountDTO> userCountDTOList = appUserMapper.selectUserCountGroupByLevel();
        Map<Integer, String> userMap = userCountDTOList.stream().collect(Collectors.toMap(AppUserCountDTO::getLevel, AppUserCountDTO::getCount));
        for (ProxyLevelEnum proxyLevelEnum : ProxyLevelEnum.values()) {
            user.add(new PortraitSelectVO(proxyLevelEnum.getLabelName(), userMap.containsKey(proxyLevelEnum.getLevel()) ? userMap.get(proxyLevelEnum.getLevel()) : "0"));
        }
        List<WalletUsdLogs> walletUsdLogsList = walletUsdLogsMapper.sumAmountGroupByType();
        BigDecimal totalInput = BigDecimal.ZERO, totalOut = BigDecimal.ZERO, totalVip = BigDecimal.ZERO, totakTask = BigDecimal.ZERO,
                totalUsd = BigDecimal.ZERO, totalStatic, totalDynamic, totalZero;
        Map<Integer, BigDecimal> groupMap = walletUsdLogsList.stream().collect(Collectors.toMap(WalletUsdLogs::getType, WalletUsdLogs::getAmount));

        UsdLogTypeEnum usdLogTypeEnum;
        for (Map.Entry<Integer, BigDecimal> entry : groupMap.entrySet()) {
            usdLogTypeEnum = UsdLogTypeEnum.valueOf(entry.getKey());
            switch (usdLogTypeEnum) {
                case RECHARGE:
                    totalInput = entry.getValue();
                    break;
                case WITHDRAWAL:
                    totalOut = entry.getValue();
                    break;
                case BUY_VIP:
                    totalVip = entry.getValue();
                    break;
                case BUY_TASK_NUM_PACKAGE:
                    totakTask = entry.getValue();
                    break;
                case TTT_TRANSFER_IN:
                    totalUsd = entry.getValue();
                    break;
            }
        }
        if (totalInput == null) {
            totalInput = BigDecimal.ZERO;
        }
        if (totalOut == null) {
            totalOut = BigDecimal.ZERO;
        }
        totalInput = totalInput.setScale(TiktokConfig.NUMBER_OF_DIGITS, RoundingMode.FLOOR);
        totalOut = totalOut.setScale(TiktokConfig.NUMBER_OF_DIGITS, RoundingMode.FLOOR);
        BigDecimal totalTrusteeship = totalInput.subtract(totalOut);
        totalStatic = walletTttLogsService.sumTotalAmountByTypeList(TttLogTypeEnum.getTaskTypeList());
        if (totalStatic == null) {
            totalStatic = BigDecimal.ZERO;
        }
        totalDynamic = walletTttLogsService.sumTotalAmountByTypeList(TttLogTypeEnum.getDynamicTypeList());
        if (totalDynamic == null) {
            totalDynamic = BigDecimal.ZERO;
        }
        totalZero = vipOrdersMapper.sumAllowanceByZeroLevel();
        if (totalZero == null) {
            totalZero = BigDecimal.ZERO;
        }
        BigDecimal expired = walletTttLogsService.sumTotalAmountByTypeList(Arrays.asList(TttLogTypeEnum.EXPIRED.getValue()));
        if(expired==null){
            expired=BigDecimal.ZERO;
        }

        BigDecimal exchangeRate = kLineDataService.getNowExchangeRate();

        //查看历史
        List<PortraitSelectVO> historical = new ArrayList<>();
        historical.add(new PortraitSelectVO("历史总入金", NumberUtil.toPlainString(totalInput)));
        historical.add(new PortraitSelectVO("历史总出金", NumberUtil.toPlainString(totalOut)));
        historical.add(new PortraitSelectVO("VIP等级消费", NumberUtil.toPlainString(totalVip)));
        historical.add(new PortraitSelectVO("分发任务消费", NumberUtil.toPlainString(totakTask)));
        historical.add(new PortraitSelectVO("总静态发放", NumberUtil.downToTwoBigDecimal(totalStatic.multiply(exchangeRate))));
        historical.add(new PortraitSelectVO("总动态发放", NumberUtil.downToTwoBigDecimal(totalDynamic.multiply(exchangeRate))));
        historical.add(new PortraitSelectVO("总零撸发放", NumberUtil.toPlainString(totalZero)));
        historical.add(new PortraitSelectVO("总TTT转入USD", NumberUtil.toPlainString(totalUsd)));
        historical.add(new PortraitSelectVO("总销毁零撸发放", NumberUtil.downToTwoBigDecimal(expired.multiply(exchangeRate))));
        //查看昨日流水明细
        walletUsdLogsList = walletUsdLogsMapper.sumYesterdayAmountGroupByType();
        groupMap = walletUsdLogsList.stream().collect(Collectors.toMap(WalletUsdLogs::getType, WalletUsdLogs::getAmount));
        totalInput = BigDecimal.ZERO;
        totalOut = BigDecimal.ZERO;
        totalVip = BigDecimal.ZERO;
        totakTask = BigDecimal.ZERO;
        totalUsd = BigDecimal.ZERO;

        for (Map.Entry<Integer, BigDecimal> entry : groupMap.entrySet()) {
            usdLogTypeEnum = UsdLogTypeEnum.valueOf(entry.getKey());
            switch (usdLogTypeEnum) {
                case RECHARGE:
                    totalInput = entry.getValue();
                    break;
                case WITHDRAWAL:
                    totalOut = entry.getValue();
                    break;
                case BUY_VIP:
                    totalVip = entry.getValue();
                    break;
                case BUY_TASK_NUM_PACKAGE:
                    totakTask = entry.getValue();
                    break;
                case TTT_TRANSFER_IN:
                    totalUsd = entry.getValue();
                    break;
            }
        }

        totalStatic = walletTttLogsService.sumYesterdayTotalAmountByTypeList(TttLogTypeEnum.getTaskTypeList());
        if (totalStatic == null) {
            totalStatic = BigDecimal.ZERO;
        }

        totalDynamic = walletTttLogsService.sumYesterdayTotalAmountByTypeList(TttLogTypeEnum.getDynamicTypeList());
        if (totalDynamic == null) {
            totalDynamic = BigDecimal.ZERO;
        }
        totalZero = walletTttLogsService.sumYesterdayTotalZeroAmount();
        if (totalZero == null) {
            totalZero = BigDecimal.ZERO;
        }
        expired = walletTttLogsService.sumYesterdayTotalAmountByTypeList(Arrays.asList(TttLogTypeEnum.EXPIRED.getValue()));
        if(expired==null){
            expired=BigDecimal.ZERO;
        }

        List<PortraitSelectVO> yesterday = new ArrayList<>();
        yesterday.add(new PortraitSelectVO("昨日总入金", NumberUtil.toPlainString(totalInput)));
        yesterday.add(new PortraitSelectVO("昨日总出金", NumberUtil.toPlainString(totalOut)));
        yesterday.add(new PortraitSelectVO("VIP等级消费", NumberUtil.toPlainString(totalVip)));
        yesterday.add(new PortraitSelectVO("分发任务消费", NumberUtil.toPlainString(totakTask)));
        yesterday.add(new PortraitSelectVO("昨日静态发放", NumberUtil.downToTwoBigDecimal(totalStatic.multiply(exchangeRate))));
        yesterday.add(new PortraitSelectVO("昨日动态发放", NumberUtil.downToTwoBigDecimal(totalDynamic.multiply(exchangeRate))));
        yesterday.add(new PortraitSelectVO("昨日零撸发放", NumberUtil.downToTwoBigDecimal(totalZero.multiply(exchangeRate))));
        yesterday.add(new PortraitSelectVO("昨日TTT转入USD", NumberUtil.toPlainString(totalUsd)));
        yesterday.add(new PortraitSelectVO("昨日销毁零撸发放",NumberUtil.downToTwoBigDecimal(expired.multiply(exchangeRate))));
        return BusinessVO.builder()
                .totalTrusteeship(NumberUtil.toPlainString(totalTrusteeship))
                .historical(historical)
                .yesterday(yesterday)
                .user(user)
                .build();
    }

    /**
     * 获取代理商角色的面板
     */
    private BusinessVO getByProxy() {
        Integer userId = JwtTokenUtil.getUserIdFromToken(ThreadLocalManager.getToken());
        List<PortraitSelectVO> user = new ArrayList<>();
        long totalUser =treePathMapper.teamSum(userId);
        long vaildUser = treePathMapper.teamMeshUser(userId);

        long zeroUser = totalUser - vaildUser;
        user.add(new PortraitSelectVO("总用户", String.valueOf(totalUser)));
        user.add(new PortraitSelectVO("有效用户", String.valueOf(vaildUser)));
        user.add(new PortraitSelectVO("零撸用户", String.valueOf(zeroUser)));
        List<AppUserCountDTO> userCountDTOList = appUserMapper.selectUserCountGroupByLevelParentId(userId);
        Map<Integer, String> userMap = userCountDTOList.stream().collect(Collectors.toMap(AppUserCountDTO::getLevel, AppUserCountDTO::getCount));
        for (ProxyLevelEnum proxyLevelEnum : ProxyLevelEnum.values()) {
            user.add(new PortraitSelectVO(proxyLevelEnum.getLabelName(), userMap.containsKey(proxyLevelEnum.getLevel()) ? userMap.get(proxyLevelEnum.getLevel()) : "0"));
        }
        List<WalletUsdLogs> walletUsdLogsList = walletUsdLogsMapper.sumAmountGroupByTypeAndParentId(userId);
        BigDecimal totalInput = BigDecimal.ZERO, totalOut = BigDecimal.ZERO, totalVip = BigDecimal.ZERO, totakTask = BigDecimal.ZERO,
                totalUsd = BigDecimal.ZERO, totalStatic, totalDynamic, totalZero;
        Map<Integer, BigDecimal> groupMap = walletUsdLogsList.stream().collect(Collectors.toMap(WalletUsdLogs::getType, WalletUsdLogs::getAmount));

        UsdLogTypeEnum usdLogTypeEnum;
        for (Map.Entry<Integer, BigDecimal> entry : groupMap.entrySet()) {
            usdLogTypeEnum = UsdLogTypeEnum.valueOf(entry.getKey());
            switch (usdLogTypeEnum) {
                case RECHARGE:
                    totalInput = entry.getValue();
                    break;
                case WITHDRAWAL:
                    totalOut = entry.getValue();
                    break;
                case BUY_VIP:
                    totalVip = entry.getValue();
                    break;
                case BUY_TASK_NUM_PACKAGE:
                    totakTask = entry.getValue();
                    break;
                case TTT_TRANSFER_IN:
                    totalUsd = entry.getValue();
                    break;
            }
        }
        if (totalInput == null) {
            totalInput = BigDecimal.ZERO;
        }
        if (totalOut == null) {
            totalOut = BigDecimal.ZERO;
        }
        totalInput = totalInput.setScale(TiktokConfig.NUMBER_OF_DIGITS, RoundingMode.FLOOR);
        totalOut = totalOut.setScale(TiktokConfig.NUMBER_OF_DIGITS, RoundingMode.FLOOR);
        BigDecimal totalTrusteeship = totalInput.subtract(totalOut);
        totalStatic = walletTttLogsService.sumTotalAmountByTypeListAndParentId(TttLogTypeEnum.getTaskTypeList(),userId);
        if (totalStatic == null) {
            totalStatic = BigDecimal.ZERO;
        }
        totalDynamic = walletTttLogsService.sumTotalAmountByTypeListAndParentId(TttLogTypeEnum.getDynamicTypeList(),userId);
        if (totalDynamic == null) {
            totalDynamic = BigDecimal.ZERO;
        }
        totalZero = vipOrdersMapper.sumAllowanceByZeroLevelByParentId(userId);
        if (totalZero == null) {
            totalZero = BigDecimal.ZERO;
        }
        BigDecimal expired = walletTttLogsService.sumTotalAmountByTypeListAndParentId(Arrays.asList(TttLogTypeEnum.EXPIRED.getValue()),userId);
        if(expired==null){
            expired=BigDecimal.ZERO;
        }

        BigDecimal exchangeRate = kLineDataService.getNowExchangeRate();

        //查看历史
        List<PortraitSelectVO> historical = new ArrayList<>();
        historical.add(new PortraitSelectVO("历史总入金", NumberUtil.toPlainString(totalInput)));
        historical.add(new PortraitSelectVO("历史总出金", NumberUtil.toPlainString(totalOut)));
        historical.add(new PortraitSelectVO("VIP等级消费", NumberUtil.toPlainString(totalVip)));
        historical.add(new PortraitSelectVO("分发任务消费", NumberUtil.toPlainString(totakTask)));
        historical.add(new PortraitSelectVO("总静态发放", NumberUtil.downToTwoBigDecimal(totalStatic.multiply(exchangeRate))));
        historical.add(new PortraitSelectVO("总动态发放", NumberUtil.downToTwoBigDecimal(totalDynamic.multiply(exchangeRate))));
        historical.add(new PortraitSelectVO("总零撸发放", NumberUtil.downToTwoBigDecimal(totalZero)));
        historical.add(new PortraitSelectVO("总TTT转入USD", NumberUtil.toPlainString(totalUsd)));
        historical.add(new PortraitSelectVO("总销毁零撸发放", NumberUtil.downToTwoBigDecimal(expired.multiply(exchangeRate))));
        //查看昨日流水明细
        walletUsdLogsList = walletUsdLogsMapper.sumYesterdayAmountGroupByTypeAndParentId(userId);
        groupMap = walletUsdLogsList.stream().collect(Collectors.toMap(WalletUsdLogs::getType, WalletUsdLogs::getAmount));
        totalInput = BigDecimal.ZERO;
        totalOut = BigDecimal.ZERO;
        totalVip = BigDecimal.ZERO;
        totakTask = BigDecimal.ZERO;
        totalUsd = BigDecimal.ZERO;

        for (Map.Entry<Integer, BigDecimal> entry : groupMap.entrySet()) {
            usdLogTypeEnum = UsdLogTypeEnum.valueOf(entry.getKey());
            switch (usdLogTypeEnum) {
                case RECHARGE:
                    totalInput = entry.getValue();
                    break;
                case WITHDRAWAL:
                    totalOut = entry.getValue();
                    break;
                case BUY_VIP:
                    totalVip = entry.getValue();
                    break;
                case BUY_TASK_NUM_PACKAGE:
                    totakTask = entry.getValue();
                    break;
                case TTT_TRANSFER_IN:
                    totalUsd = entry.getValue();
                    break;
            }
        }

        totalStatic = walletTttLogsService.sumYesterdayTotalAmountByTypeListAndParentId(TttLogTypeEnum.getTaskTypeList(),userId);
        if (totalStatic == null) {
            totalStatic = BigDecimal.ZERO;
        }

        totalDynamic = walletTttLogsService.sumYesterdayTotalAmountByTypeListAndParentId(TttLogTypeEnum.getDynamicTypeList(),userId);
        if (totalDynamic == null) {
            totalDynamic = BigDecimal.ZERO;
        }
        totalZero = walletTttLogsService.sumYesterdayTotalZeroAmountAndParentId(userId);
        if (totalZero == null) {
            totalZero = BigDecimal.ZERO;
        }
        expired = walletTttLogsService.sumYesterdayTotalAmountByTypeListAndParentId(Arrays.asList(TttLogTypeEnum.EXPIRED.getValue()),userId);
        if(expired==null){
            expired=BigDecimal.ZERO;
        }

        List<PortraitSelectVO> yesterday = new ArrayList<>();
        yesterday.add(new PortraitSelectVO("昨日总入金", NumberUtil.toPlainString(totalInput)));
        yesterday.add(new PortraitSelectVO("昨日总出金", NumberUtil.toPlainString(totalOut)));
        yesterday.add(new PortraitSelectVO("VIP等级消费", NumberUtil.toPlainString(totalVip)));
        yesterday.add(new PortraitSelectVO("分发任务消费", NumberUtil.toPlainString(totakTask)));
        yesterday.add(new PortraitSelectVO("昨日静态发放", NumberUtil.downToTwoBigDecimal(totalStatic.multiply(exchangeRate))));
        yesterday.add(new PortraitSelectVO("昨日动态发放", NumberUtil.downToTwoBigDecimal(totalDynamic.multiply(exchangeRate))));
        yesterday.add(new PortraitSelectVO("昨日零撸发放", NumberUtil.downToTwoBigDecimal(totalZero.multiply(exchangeRate))));
        yesterday.add(new PortraitSelectVO("昨日TTT转入USD", NumberUtil.toPlainString(totalUsd)));
        yesterday.add(new PortraitSelectVO("昨日销毁零撸发放", NumberUtil.downToTwoBigDecimal(expired.multiply(exchangeRate))));

        return BusinessVO.builder()
                .totalTrusteeship(NumberUtil.toPlainString(totalTrusteeship))
                .historical(historical)
                .yesterday(yesterday)
                .user(user)
                .build();
    }
}
