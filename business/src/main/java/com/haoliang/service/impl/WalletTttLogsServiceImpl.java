package com.haoliang.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.haoliang.common.enums.BooleanEnum;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.model.PageParam;
import com.haoliang.common.model.ThreadLocalManager;
import com.haoliang.common.model.dto.PageDTO;
import com.haoliang.common.model.dto.TypeDTO;
import com.haoliang.common.util.JwtTokenUtil;
import com.haoliang.common.util.MessageUtil;
import com.haoliang.common.util.NumberUtil;
import com.haoliang.constant.TiktokConfig;
import com.haoliang.enums.FlowingActionEnum;
import com.haoliang.enums.TttLogTypeEnum;
import com.haoliang.enums.VipLevelEnum;
import com.haoliang.mapper.VipOrdersMapper;
import com.haoliang.mapper.WalletTttLogsMapper;
import com.haoliang.mapper.WalletsMapper;
import com.haoliang.model.AppUsers;
import com.haoliang.model.VipOrders;
import com.haoliang.model.WalletTttLogs;
import com.haoliang.model.Wallets;
import com.haoliang.model.condition.BillDetailsCondition;
import com.haoliang.model.dto.DateSection;
import com.haoliang.model.dto.EarningsDTO;
import com.haoliang.model.dto.TeamTaskDTO;
import com.haoliang.model.vo.*;
import com.haoliang.service.AppUserService;
import com.haoliang.service.WalletTttLogsService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import javax.annotation.Resource;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/11/1 18:54
 **/
@Slf4j
@Service
public class WalletTttLogsServiceImpl extends ServiceImpl<WalletTttLogsMapper, WalletTttLogs> implements WalletTttLogsService {

    @Autowired
    private AppUserService appUserService;

    @Resource
    private WalletsMapper walletsMapper;

    @Resource
    private VipOrdersMapper vipOrdersMapper;

    /**
     * 社区奖励类型
     */
    private final List<Integer> dynamicTypeList = TttLogTypeEnum.getDynamicTypeList();

    /**
     * 任务奖励类型
     */
    private final List<Integer> taskTypeList = TttLogTypeEnum.getTaskTypeList();

    @Override
    @Transactional(rollbackFor = Exception.class)
    public boolean insertWalletLogs(Integer userId, BigDecimal amount, FlowingActionEnum flowingActionEnum, TttLogTypeEnum tttLogTypeEnum) {
        //添加钱包流水记录
        WalletTttLogs walletLogs = WalletTttLogs.builder()
                .userId(userId)
                .amount(amount)
                .action(flowingActionEnum.getValue())
                .type(tttLogTypeEnum.getValue())
                .build();
        return this.baseMapper.insert(walletLogs) > 0;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public boolean insertZeroWalletLogs(Integer userId, BigDecimal amount, FlowingActionEnum flowingActionEnum, TttLogTypeEnum tttLogTypeEnum) {
        //添加钱包流水记录
        WalletTttLogs walletLogs = WalletTttLogs.builder()
                .userId(userId)
                .amount(amount)
                .zero(BooleanEnum.TRUE.intValue())
                .action(flowingActionEnum.getValue())
                .type(tttLogTypeEnum.getValue())
                .build();
        return this.baseMapper.insert(walletLogs) > 0;
    }

    @Override
    public EarningsDTO getMyEarningsWalletLogs(Integer userId) {
        BigDecimal communityBenefits = BigDecimal.ZERO, taskBenefits = BigDecimal.ZERO;
        List<Integer> typeList = new ArrayList<>();
        typeList.addAll(dynamicTypeList);
        typeList.addAll(taskTypeList);
        List<WalletTttLogs> walletLogsList = this.baseMapper.sumByTypeList(userId, typeList);
        for (WalletTttLogs walletLogs : walletLogsList) {
            if (taskTypeList.contains(walletLogs.getType())) {
                taskBenefits = taskBenefits.add(walletLogs.getAmount());
            } else {
                communityBenefits = communityBenefits.add(walletLogs.getAmount());
            }
        }
        EarningsDTO earningsDTO = new EarningsDTO();
        earningsDTO.setCommunityBenefits(communityBenefits);
        earningsDTO.setTaskBenefits(taskBenefits);
        return earningsDTO;
    }

    @Override
    public JsonResult<WalletLogsDetailVO> getMybillDetails(PageParam<WalletTttLogs, BillDetailsCondition> pageParam) {
        Integer userId = JwtTokenUtil.getUserIdFromToken(ThreadLocalManager.getToken());

        //查询钱包流水中第一笔流水的时间
        DateSection dateSection = this.baseMapper.getDateSection(userId);
        //获取日期下拉列表
        List<ViewSelectVO> dateSelectVOList = pageParam.getSearchParam().isInit() ? ViewSelectVO.getSelectListByDateSection(dateSection) : null;

        //判断是否查询所有还是根据指定月份查询
        LocalDate beginDate = null, endDate = null;
        if (!pageParam.getSearchParam().getYearMonth().equals("-1")) {
            //分割年月
            String arr[] = pageParam.getSearchParam().getYearMonth().split("-");
            beginDate = LocalDate.of(Integer.parseInt(arr[0]), Integer.parseInt(arr[1]), 1);
            endDate = beginDate.plusMonths(1);
        }

        List<WalletLogVO> resultList = new ArrayList<>();
        long totalPage;

        String languageKey = null;
        Page<WalletTttLogs> pageData = pageParam.getPage();
        if (pageParam.getSearchParam().getType() == -1) {
            //查询所有
            pageData = this.baseMapper.pageByAllType(pageData, userId, beginDate, endDate);
        } else if (pageParam.getSearchParam().getType() == 0) {
            //查询代理收益
            languageKey = TiktokConfig.AGENCY_KEY;
            pageData = this.baseMapper.pageByMerge(pageData, userId, beginDate, endDate, dynamicTypeList);
        } else if (pageParam.getSearchParam().getType() == 1) {
            //查询任务收益
            languageKey = TiktokConfig.TASK_KEY;
            pageData = this.baseMapper.pageByMerge(pageData, userId, beginDate, endDate, taskTypeList);
        } else {
            //查询转换到Usd账户数据
            languageKey = TttLogTypeEnum.TO_USD.getKey();
            LambdaQueryWrapper<WalletTttLogs> lambdaQueryWrapper = new LambdaQueryWrapper<WalletTttLogs>()
                    .select(WalletTttLogs::getCreateTime, WalletTttLogs::getAmount, WalletTttLogs::getAction)
                    .eq(WalletTttLogs::getUserId, userId)
                    .eq(WalletTttLogs::getType, TttLogTypeEnum.TO_USD.getValue())
                    .orderByDesc(WalletTttLogs::getCreateTime);
            pageData = this.page(pageData, lambdaQueryWrapper);
        }

        List<WalletTttLogs> walletLogsList = pageData.getRecords();
        totalPage = pageData.getPages();

        for (WalletTttLogs walletLogs : walletLogsList) {
            if (walletLogs.getCreateTime() == null) {
                continue;
            }
            resultList.add(WalletLogVO.builder()
                    .createTime(walletLogs.getCreateTime().toLocalDate().toString())
                    .amount((walletLogs.getAction() == 1 ? "+ " : "- ") + NumberUtil.toPlainString(walletLogs.getAmount()))
                    .name(languageKey != null ? MessageUtil.get(languageKey, ThreadLocalManager.getLanguage()) : TttLogTypeEnum.getTttDetailText(walletLogs.getType()))
                    .build());
        }

        //计算存入和取出金额
        List<Integer> depositTypeList = new ArrayList<>();
        depositTypeList.addAll(dynamicTypeList);
        depositTypeList.addAll(taskTypeList);
        depositTypeList.add(TttLogTypeEnum.ENROLL_IN_BENEFITS.getValue());

        BigDecimal deposit = this.baseMapper.sumAmountByAndUserIdAndTypeInAndDateRange(userId, depositTypeList, beginDate, endDate);
        BigDecimal takeOut = this.baseMapper.sumAmountByAndUserIdAndTypeInAndDateRange(userId, Arrays.asList(TttLogTypeEnum.TO_USD.getValue(), TttLogTypeEnum.PURCHASE_VIP.getValue()), beginDate, endDate);

        List<ViewSelectVO> viewTypeList = pageParam.getSearchParam().isInit() ? WalletLogsDetailVO.buildTttTypeList() : null;
        return JsonResult.successResult(WalletLogsDetailVO.builder()
                .deposit(NumberUtil.toPlainString(deposit))
                .takeOut(NumberUtil.toPlainString(takeOut))
                .tableList(resultList)
                .typeList(viewTypeList)
                .dateSectionList(dateSelectVOList)
                .totalPage((int) totalPage)
                .build());
    }

    @Override
    public JsonResult<CommunityRewardDetailVO> communityRewardDetail(PageDTO pageDTO) {
        Integer userId = JwtTokenUtil.getUserIdFromToken(ThreadLocalManager.getToken());
        LambdaQueryWrapper<WalletTttLogs> lambdaQueryWrapper = new LambdaQueryWrapper<WalletTttLogs>()
                .select(WalletTttLogs::getCreateTime, WalletTttLogs::getAmount, WalletTttLogs::getType)
                .eq(WalletTttLogs::getUserId, userId)
                .in(WalletTttLogs::getType, dynamicTypeList)
                .orderByDesc(WalletTttLogs::getCreateTime);

        Page<WalletTttLogs> page = this.page(new Page<>(pageDTO.getCurrentPage(), pageDTO.getPageSize()), lambdaQueryWrapper);
        //Page<WalletTttLogs> page = this.baseMapper.pageByMergeAndGroupByType(pageDTO.getPage(), userId, dynamicTypeList);
        List<WalletLogVO> walletLogVOList = new ArrayList<>();

        String typeName;
        for (WalletTttLogs walletLogs : page.getRecords()) {
            typeName = TttLogTypeEnum.getDescByValue(walletLogs.getType());
            walletLogVOList.add(WalletLogVO.builder()
                    .createTime(walletLogs.getCreateTime().toLocalDate().toString())
                    .amount(NumberUtil.toPlainString(walletLogs.getAmount()))
                    .name(typeName)
                    .build());
        }

        if (pageDTO.isInit()) {
            BigDecimal algebra = BigDecimal.ZERO, team = BigDecimal.ZERO, special = BigDecimal.ZERO, has = BigDecimal.ZERO;
            List<WalletTttLogs> walletLogsList = this.baseMapper.sumByTypeList(userId, dynamicTypeList);
            for (WalletTttLogs walletLogs : walletLogsList) {
                if (walletLogs.getType().equals(TttLogTypeEnum.ALGEBRA.getValue())) {
                    algebra = walletLogs.getAmount();
                } else if (walletLogs.getType().equals(TttLogTypeEnum.TEAM.getValue())) {
                    team = walletLogs.getAmount();
                } else if (walletLogs.getType().equals(TttLogTypeEnum.SPECIAL.getValue())) {
                    special = walletLogs.getAmount();
                } else {
                    has = walletLogs.getAmount();
                }
            }
            return JsonResult.successResult(CommunityRewardDetailVO.builder()
                    .list(walletLogVOList)
                    .totalPage((int) page.getPages())
                    .algebra(NumberUtil.toPlainString(algebra))
                    .has(NumberUtil.toPlainString(has))
                    .team(NumberUtil.toPlainString(team))
                    .special(NumberUtil.toPlainString(special))
                    .total(NumberUtil.toPlainString(algebra.add(team).add(special).add(has)))
                    .build());
        }
        return JsonResult.successResult(CommunityRewardDetailVO.builder()
                .list(walletLogVOList)
                .totalPage((int) page.getPages())
                .build());
    }

    @Override
    public JsonResult taskEarningsDetail(TypeDTO pageDTO) {
        Integer userId = JwtTokenUtil.getUserIdFromToken(ThreadLocalManager.getToken());
        LambdaQueryWrapper<WalletTttLogs> lambdaQueryWrapper = new LambdaQueryWrapper<WalletTttLogs>()
                .select(WalletTttLogs::getCreateTime, WalletTttLogs::getAmount, WalletTttLogs::getType)
                .eq(WalletTttLogs::getUserId, userId)
                .in(WalletTttLogs::getType, taskTypeList)
                .orderByDesc(WalletTttLogs::getCreateTime);

        Page<WalletTttLogs> page = this.page(new Page<>(pageDTO.getCurrentPage(), pageDTO.getPageSize()), lambdaQueryWrapper);
        //Page<WalletTttLogs> page = this.baseMapper.pageByMergeAndGroupByType(pageDTO.getPage(), userId, taskTypeList);
        List<WalletLogVO> walletLogVOList = new ArrayList<>();

        String typeName;
        for (WalletTttLogs walletLogs : page.getRecords()) {
            typeName = TttLogTypeEnum.getDescByValue(walletLogs.getType());
            walletLogVOList.add(WalletLogVO.builder()
                    .createTime(walletLogs.getCreateTime().toLocalDate().toString())
                    .amount(NumberUtil.toPlainString(walletLogs.getAmount()))
                    .name(typeName)
                    .build());
        }

        if (pageDTO.isInit()) {
            BigDecimal acquired = BigDecimal.ZERO;
            List<WalletTttLogs> walletLogsList = this.baseMapper.sumByTypeList(userId, taskTypeList);
            for (WalletTttLogs walletLogs : walletLogsList) {
                acquired = acquired.add(walletLogs.getAmount());
            }

            //获取已购买的vip列表
            List<VipOrders> vipOrdersList = vipOrdersMapper.selectList(new LambdaQueryWrapper<VipOrders>()
                    .select(VipOrders::getAllowance, VipOrders::getFrozenAmount, VipOrders::getTotal, VipOrders::getLevel)
                    .eq(VipOrders::getUserId, userId)
                    .gt(VipOrders::getLevel, VipLevelEnum.ZERO.getLevel())
                    .orderByAsc(VipOrders::getLevel, VipOrders::getCreateTime)
            );

            AppUsers appUsers = appUserService.selectColumnsByUserId(userId, AppUsers::getLevel);
            List<VipOrders> vipOrderNewList = new ArrayList<>();
            vipOrderNewList.addAll(vipOrdersList.stream().filter(vipOrders -> vipOrders.getAllowance().compareTo(BigDecimal.ZERO) > 0).collect(Collectors.toList()));
            vipOrderNewList.addAll(vipOrdersList.stream().filter(vipOrders -> vipOrders.getAllowance().compareTo(BigDecimal.ZERO) == 0).collect(Collectors.toList()));

            List<VipOrderVO> vipOrderVOList = new ArrayList<>();
            for (VipOrders vipOrders : vipOrderNewList) {
                vipOrders.setAllowance(vipOrders.getAllowance().add(vipOrders.getFrozenAmount()));
                vipOrderVOList.add(new VipOrderVO(vipOrders));
            }

            BigDecimal total = this.baseMapper.sumTotalAmountByUserIdAndTypeIn(userId, TttLogTypeEnum.getTaskTypeList());
            return JsonResult.successResult(TaskEarningsDetailVO.builder()
                    .list(walletLogVOList)
                    .level(appUsers.getLevel())
                    .vipList(vipOrderVOList)
                    .total(NumberUtil.toPlainString(total))
                    .totalPage((int) page.getPages())
                    .build());
        }
        return JsonResult.successResult(CommunityRewardDetailVO.builder()
                .list(walletLogVOList)
                .totalPage((int) page.getPages())
                .build());
    }


    @Override
    public BigDecimal sumTotalEarnings(Integer userId) {
        List<Integer> typeList = new ArrayList<>();
        typeList.addAll(dynamicTypeList);
        typeList.addAll(taskTypeList);
        return this.baseMapper.sumTotalAmountByUserIdAndTypeIn(userId, typeList);
    }

    @Override
    public BigDecimal getSumTaskEarnings() {
        return this.baseMapper.getSumTaskEarningsByTypeIn(taskTypeList);
    }

    @Override
    public AppUserRewardVO getUserReward(Integer userId) {
        List<WalletTttLogs> walletLogsList = this.baseMapper.sumByType(userId);
        BigDecimal totalDynamic = BigDecimal.ZERO, totalStatic = BigDecimal.ZERO;
        for (WalletTttLogs walletTttLogs : walletLogsList) {
            if (dynamicTypeList.contains(walletTttLogs.getType())) {
                totalDynamic = totalDynamic.add(walletTttLogs.getAmount());
            } else if (taskTypeList.contains(walletTttLogs.getType())) {
                totalStatic = totalStatic.add(walletTttLogs.getAmount());
            }
        }

        walletLogsList = this.baseMapper.sumYesterdayByType(userId);
        BigDecimal yesterdayDynamic = BigDecimal.ZERO, yesterdayStatic = BigDecimal.ZERO;


        for (WalletTttLogs walletTttLogs : walletLogsList) {
            if (dynamicTypeList.contains(walletTttLogs.getType())) {
                yesterdayDynamic = yesterdayDynamic.add(walletTttLogs.getAmount());
            } else if (taskTypeList.contains(walletTttLogs.getType())) {
                yesterdayStatic = yesterdayStatic.add(walletTttLogs.getAmount());
            }
        }
        return AppUserRewardVO.builder()
                .totalDynamicReward(NumberUtil.toPlainString(totalDynamic))
                .dynamicReward(NumberUtil.toPlainString(yesterdayDynamic))
                .staticReward(NumberUtil.toPlainString(yesterdayStatic))
                .totalStaticReward(NumberUtil.toPlainString(totalStatic))
                .build();
    }

    @Override
    public List<TeamTaskDTO> getSumTaskEarningGroupByUser() {
        return this.baseMapper.getSumTaskEarningGroupByUser(taskTypeList);
    }

    @Override
    public BigDecimal sumTotalAmountByTypeList(List<Integer> taskTypeList) {
        return this.baseMapper.sumTotalAmountByTypeList(taskTypeList);
    }

    @Override
    public BigDecimal sumYesterdayTotalAmountByTypeList(List<Integer> typeList) {
        return this.baseMapper.sumYesterdayTotalAmountByTypeList(typeList);
    }

    @Override
    public BigDecimal sumYesterdayTotalZeroAmount() {
        return this.baseMapper.sumYesterdayTotalZeroAmount();
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void clearExpired() {
        //过期流水的时间
        LocalDateTime expiredLimit = LocalDateTime.now().minusDays(TiktokConfig.ZERO_EXPIRED_DAY);
        //找到所有零撸用户
        List<AppUsers> appUserList = appUserService.list(new LambdaQueryWrapper<AppUsers>()
                .select(AppUsers::getId)
                .eq(AppUsers::getValid, BooleanEnum.FALSE.intValue())
        );

        if (CollectionUtils.isEmpty(appUserList)) {
            return;
        }
        //找到过期的流水的记录
        List<WalletTttLogs> walletTttLogsList = this.list(new LambdaQueryWrapper<WalletTttLogs>()
                .select(WalletTttLogs::getUserId, WalletTttLogs::getAmount)
                .in(WalletTttLogs::getUserId, appUserList.stream().map(AppUsers::getId).collect(Collectors.toList()))
                .between(WalletTttLogs::getCreateTime, expiredLimit.minusDays(1), expiredLimit)
                .eq(WalletTttLogs::getAction, FlowingActionEnum.INCOME.getValue())
        );
        if (CollectionUtils.isEmpty(walletTttLogsList)) {
            return;
        }
        Map<Integer, List<WalletTttLogs>> data = walletTttLogsList.stream().collect(Collectors.groupingBy(WalletTttLogs::getUserId));
        List<WalletTttLogs> saveList = new ArrayList<>();
        BigDecimal sum;

        List<Wallets> walletsList = walletsMapper.selectList(new LambdaQueryWrapper<Wallets>().in(Wallets::getUserId, data.keySet()));
        Map<Integer, Wallets> walletsMap = walletsList.stream().collect(Collectors.toMap(Wallets::getUserId, Function.identity()));
        Wallets wallets;
        for (Map.Entry<Integer, List<WalletTttLogs>> entry : data.entrySet()) {
            sum = BigDecimal.ZERO;
            for (WalletTttLogs walletTttLogs : entry.getValue()) {
                sum = sum.add(walletTttLogs.getAmount());
            }
            saveList.add(WalletTttLogs.builder()
                    .action(FlowingActionEnum.EXPENDITURE.getValue())
                    .zero(BooleanEnum.TRUE.intValue())
                    .userId(entry.getKey())
                    .amount(sum)
                    .type(TttLogTypeEnum.EXPIRED.getValue())
                    .build());
            if (walletsMap.containsKey(entry.getKey())) {
                wallets = walletsMap.get(entry.getKey());
                wallets.setWalletAmount(wallets.getWalletAmount().compareTo(sum) >= 0 ? wallets.getWalletAmount().subtract(sum) : BigDecimal.ZERO);
                walletsMapper.updateById(wallets);
                log.info("清除零撸用户：{}超过{}天的收益总金额：{}T", entry.getKey(), TiktokConfig.ZERO_EXPIRED_DAY, sum);
            }
        }
        this.saveBatch(saveList);
    }

    @Override
    public BigDecimal sumTotalAmountByTypeListAndParentId(List<Integer> taskTypeList, Integer userId) {
        return this.baseMapper.sumTotalAmountByTypeListAndParentId(taskTypeList, userId);
    }

    @Override
    public BigDecimal sumYesterdayTotalAmountByTypeListAndParentId(List<Integer> taskTypeList, Integer userId) {
        return this.baseMapper.sumYesterdayTotalAmountByTypeListAndParentId(taskTypeList, userId);
    }

    @Override
    public BigDecimal sumYesterdayTotalZeroAmountAndParentId(Integer userId) {
        return this.baseMapper.sumYesterdayTotalZeroAmountAndParentId(userId);
    }
}
