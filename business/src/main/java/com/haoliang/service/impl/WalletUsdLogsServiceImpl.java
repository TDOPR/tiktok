package com.haoliang.service.impl;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.model.PageParam;
import com.haoliang.common.model.ThreadLocalManager;
import com.haoliang.common.util.JwtTokenUtil;
import com.haoliang.common.util.MessageUtil;
import com.haoliang.common.util.NumberUtil;
import com.haoliang.enums.FlowingActionEnum;
import com.haoliang.enums.UsdLogTypeEnum;
import com.haoliang.mapper.WalletUsdLogsMapper;
import com.haoliang.model.WalletUsdLogs;
import com.haoliang.model.condition.BillDetailsCondition;
import com.haoliang.model.dto.DateSection;
import com.haoliang.model.vo.*;
import com.haoliang.pay.enums.CoinUnitEnum;
import com.haoliang.service.WalletUsdLogsService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.Arrays;
import java.util.List;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/3/5 18:07
 **/
@Service
public class WalletUsdLogsServiceImpl extends ServiceImpl<WalletUsdLogsMapper, WalletUsdLogs> implements WalletUsdLogsService {

    /**
     * 存入类型
     */
    private final List<Integer> depositTypeList = Arrays.asList(UsdLogTypeEnum.RECHARGE.getValue(), UsdLogTypeEnum.TTT_TRANSFER_IN.getValue());

    /**
     * 取出类型
     */
    private final List<Integer> takeOutTypeList = Arrays.asList(UsdLogTypeEnum.WITHDRAWAL.getValue(), UsdLogTypeEnum.BUY_VIP.getValue(), UsdLogTypeEnum.BUY_TASK_NUM_PACKAGE.getValue());

    @Override
    @Transactional(rollbackFor = Exception.class)
    public Long insertWalletLogs(Integer userId, BigDecimal amount, FlowingActionEnum flowingActionEnum, UsdLogTypeEnum usdLogTypeEnum) {
        //添加钱包流水记录
        WalletUsdLogs walletLogs = WalletUsdLogs.builder()
                .userId(userId)
                .amount(amount)
                .action(flowingActionEnum.getValue())
                .type(usdLogTypeEnum.getValue())
                .build();
        this.baseMapper.insert(walletLogs);
        return walletLogs.getId();
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public Long insertWalletLogs(Integer userId, BigDecimal amount, FlowingActionEnum flowingActionEnum, UsdLogTypeEnum usdLogTypeEnum, CoinUnitEnum coinUnitEnum) {
        //添加钱包流水记录
        WalletUsdLogs walletLogs = WalletUsdLogs.builder()
                .userId(userId)
                .amount(amount)
                .coinId(coinUnitEnum.getId())
                .action(flowingActionEnum.getValue())
                .type(usdLogTypeEnum.getValue())
                .build();
        this.baseMapper.insert(walletLogs);
        return walletLogs.getId();
    }

    @Override
    public JsonResult<WalletsUsdLogDetailVO> getMybillDetails(PageParam<WalletUsdLogs, BillDetailsCondition> pageParam) {
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

        long totalPage;

        String languageKey = null;

        Page<WalletUsdLogVO> pageData = this.baseMapper.mypage(pageParam.getPage(), userId, pageParam.getSearchParam().getType(), beginDate, endDate);
        List<WalletUsdLogVO> resultList = pageData.getRecords();
        totalPage = pageData.getPages();

        for (WalletUsdLogVO walletUsdLogVO : resultList) {
            if(walletUsdLogVO.getCoinId()!=null && CoinUnitEnum.USDT.getId().equals(walletUsdLogVO.getCoinId()) && walletUsdLogVO.getType().equals(UsdLogTypeEnum.WITHDRAWAL.getValue())){
                //修改状态
                if(walletUsdLogVO.getStatus()==1){
                    walletUsdLogVO.setStatus(4);
                }else if(walletUsdLogVO.getStatus()==2){
                    walletUsdLogVO.setStatus(5);
                }
            }
            walletUsdLogVO.setName(languageKey != null ? MessageUtil.get(languageKey, ThreadLocalManager.getLanguage()) : UsdLogTypeEnum.getDescByValue(walletUsdLogVO.getType()));
        }

        //计算存入和取出金额
        BigDecimal deposit = this.baseMapper.sumAmountByAndUserIdAndTypeInAndDateRange(userId, depositTypeList, beginDate, endDate);
        BigDecimal takeOut = this.baseMapper.sumAmountByAndUserIdAndTypeInAndDateRange(userId, takeOutTypeList, beginDate, endDate);

        List<ViewSelectVO> viewTypeList = pageParam.getSearchParam().isInit() ? WalletsUsdLogDetailVO.buildUsdTypeList() : null;
        return JsonResult.successResult(WalletsUsdLogDetailVO.builder()
                .deposit(NumberUtil.toPlainString(deposit))
                .takeOut(NumberUtil.toPlainString(takeOut))
                .tableList(resultList)
                .typeList(viewTypeList)
                .dateSectionList(dateSelectVOList)
                .totalPage((int) totalPage)
                .build());
    }


}
