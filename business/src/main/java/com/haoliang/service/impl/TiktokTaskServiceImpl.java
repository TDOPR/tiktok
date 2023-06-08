package com.haoliang.service.impl;

import com.alibaba.fastjson.JSONObject;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.haoliang.common.enums.BooleanEnum;
import com.haoliang.common.enums.LanguageEnum;
import com.haoliang.common.enums.ReturnMessageEnum;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.model.PageParam;
import com.haoliang.common.model.ThreadLocalManager;
import com.haoliang.common.model.dto.PageDTO;
import com.haoliang.common.model.dto.TypeDTO;
import com.haoliang.common.model.vo.PageVO;
import com.haoliang.common.util.*;
import com.haoliang.constant.TiktokConfig;
import com.haoliang.enums.ResponseStatusEnums;
import com.haoliang.enums.TiktokTaskTypeEnum;
import com.haoliang.enums.VipLevelEnum;
import com.haoliang.mapper.TiktokTaskMapper;
import com.haoliang.mapper.TiktokTaskPriceOrdersMapper;
import com.haoliang.mapper.TiktokTaskPricesMapper;
import com.haoliang.model.*;
import com.haoliang.model.condition.TiktokTaskCondition;
import com.haoliang.model.dto.ForceDeleteDTO;
import com.haoliang.model.dto.PublishTiktokTask;
import com.haoliang.model.vo.*;
import com.haoliang.service.*;
import com.haoliang.utils.EarningUtil;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.Resource;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/2/24 11:35
 **/
@Service
public class TiktokTaskServiceImpl extends ServiceImpl<TiktokTaskMapper, TiktokTask> implements TiktokTaskService {

    @Autowired
    private AppUserService appUserService;

    @Autowired
    private WalletsService walletsService;

    @Autowired
    private KLineDataService kLineDataService;

    @Resource
    private TiktokTaskPricesMapper tiktokTaskPricesMapper;

    @Autowired
    private AppUserTaskService appUserTaskService;

    @Resource
    private TiktokTaskPriceOrdersMapper tiktokTaskPriceOrdersMapper;

    @Autowired
    private VipOrdersService vipOrdersService;

    private HashMap<String, String> telegramMap = new HashMap<>();

    {
        telegramMap.put(LanguageEnum.ZH_CN.getName(), "https://t.me/Tiktokguild");
        telegramMap.put(LanguageEnum.EN_US.getName(), "https://t.me/Tiktokguild");
        telegramMap.put(LanguageEnum.VI_VN.getName(), "https://t.me/+B8wLTIWciow4MTNl");
        telegramMap.put(LanguageEnum.TH_TH.getName(), "https://t.me/+BpxtbYjPnVo1OTI1");
        telegramMap.put(LanguageEnum.IN_ID.getName(), "https://t.me/+55VlkhdYTKQ1NmM1");
    }

    @Override
    public JsonResult<PublishTaskVO> getPublishList(TypeDTO pageDTO) {
        Integer userId = JwtTokenUtil.getUserIdFromToken(ThreadLocalManager.getToken());
        AppUsers appUsers = appUserService.selectColumnsByUserId(userId, AppUsers::getVipLevel, AppUsers::getGreenhorn);

        VipLevelEnum vipLevelEnum;

        //可获取任务的总收益,接取每一单的任务收益
        BigDecimal sumAllowance = BigDecimal.ZERO, earnings = BigDecimal.ZERO;
        List<VipOrders> ordersList = vipOrdersService.findByUserIdOrderByLevelAes(userId, appUsers.getVipLevel());
        if (VipLevelEnum.ZERO.getLevel().equals(appUsers.getVipLevel())) {
            if (ordersList.size() > 0) {
                VipOrders vipOrders = ordersList.get(0);
                vipLevelEnum = vipOrders.getTotal().compareTo(VipLevelEnum.ZERO.getOutOfSaleAmount()) == 0 ? VipLevelEnum.ZERO : VipLevelEnum.ZERO_SECOND_MONTH;
                //当余额不足以做一单的时候,用仅有的金额去扣减
                earnings = vipOrders.getAllowance().compareTo(vipLevelEnum.getEarnings()) < 0 ? vipOrders.getAllowance() : vipLevelEnum.getEarnings();
                sumAllowance = vipOrders.getAllowance();
            } else {
                earnings = VipLevelEnum.ZERO.getEarnings();
                sumAllowance = BigDecimal.ZERO;
            }
        } else {
            //计算总共可用的usd余额
            BigDecimal oneEarning;
            Integer maxLevel = appUsers.getVipLevel();

            if (ordersList.size() > 0) {
                maxLevel = ordersList.stream().max(Comparator.comparingInt(VipOrders::getLevel)).get().getLevel();
            }

            VipLevelEnum hasLevel = VipLevelEnum.getByLevel(maxLevel);

            for (VipOrders vipOrders : ordersList) {
                sumAllowance = sumAllowance.add(vipOrders.getAllowance());
                oneEarning = EarningUtil.getEarningByAllowance(hasLevel, vipOrders);
                earnings = earnings.add(oneEarning);
            }

            if (earnings.compareTo(BigDecimal.ZERO) == 0) {
                earnings = hasLevel.getEarnings();
            }
        }
        BigDecimal tttToUsdRate = kLineDataService.getNowExchangeRate();
        //计算任务可获取的ttt收益
        sumAllowance = sumAllowance.divide(tttToUsdRate, TiktokConfig.NUMBER_OF_DIGITS, RoundingMode.FLOOR);
        //每单收益可获取的ttt额度
        BigDecimal amount = earnings.divide(tttToUsdRate, TiktokConfig.NUMBER_OF_DIGITS, RoundingMode.FLOOR);

        Page<TiktokTaskVO> tiktokTaskPage = this.baseMapper.page(pageDTO.getPage(), userId, appUsers.getGreenhorn());

        for (TiktokTaskVO tiktokTask : tiktokTaskPage.getRecords()) {
            if (tiktokTask.getBamount() == null) {
                tiktokTask.setBamount(amount);
            }
            if (tiktokTask.getBuilt().equals(1)) {
                tiktokTask.setOpusId(telegramMap.get(ThreadLocalManager.getLanguage()));
            }
        }
        return JsonResult.successResult(new PublishTaskVO(sumAllowance, tiktokTaskPage.getTotal(), tiktokTaskPage.getPages(), tiktokTaskPage.getRecords()));
    }

    @Override
    public JsonResult<PageVO<MyTiktokTaskVO>> getMyPublishList(PageDTO pageDTO) {
        Page<MyTiktokTaskVO> tiktokTaskPage = this.baseMapper.pageByUserId(pageDTO.getPage(), JwtTokenUtil.getUserIdFromToken(ThreadLocalManager.getToken()));
        return JsonResult.successResult(new PageVO<>(tiktokTaskPage));
    }

    @Override
    public JsonResult getPricesAndBalance() {
        List<TiktokTaskPrices> tiktokTaskPricesList = tiktokTaskPricesMapper.selectList(
                new LambdaQueryWrapper<TiktokTaskPrices>()
                        .select(TiktokTaskPrices::getId, TiktokTaskPrices::getNum, TiktokTaskPrices::getPrice)
                        .eq(TiktokTaskPrices::getVisible, BooleanEnum.TRUE.intValue())
        );
        List<TiktokTaskPricesVO> pricesVOList = new ArrayList<>(tiktokTaskPricesList.size());
        TiktokTaskPricesVO tiktokTaskPricesVO;
        for (TiktokTaskPrices tiktokTaskPrices : tiktokTaskPricesList) {
            tiktokTaskPricesVO = new TiktokTaskPricesVO();
            BeanUtils.copyProperties(tiktokTaskPrices, tiktokTaskPricesVO);
            pricesVOList.add(tiktokTaskPricesVO);
        }
        Integer userId = JwtTokenUtil.getUserIdFromToken(ThreadLocalManager.getToken());
        Wallets wallets = walletsService.selectColumnsByUserId(userId, Wallets::getTotalTaskNum, Wallets::getHasTaskNum);
        JSONObject object = new JSONObject();
        object.put("priceList", pricesVOList);
        object.put("balance", new TaskNumVO(wallets.getTotalTaskNum(), wallets.getHasTaskNum()));
        return JsonResult.successResult(object);
    }

    @Override
    @Transactional
    public JsonResult buyPackage(Integer id) {
        Integer userId = JwtTokenUtil.getUserIdFromToken(ThreadLocalManager.getToken());
        TiktokTaskPrices tiktokTaskPrices = tiktokTaskPricesMapper.selectById(id);
        Wallets wallets = walletsService.selectColumnsByUserId(userId, Wallets::getUsdWalletAmount);
        //余额不足时候不购买
        if (wallets.getUsdWalletAmount().compareTo(new BigDecimal(tiktokTaskPrices.getPrice())) < 0) {
            return JsonResult.failureResult(ReturnMessageEnum.AMOUNT_EXCEEDS_BALANCE);
        }
        //升级成有效用户清除零撸套餐
        appUserTaskService.cleanZeroUserTask(userId);
        //购买套餐次数包
        walletsService.buyTaskNumPackage(userId, tiktokTaskPrices);
        //插入购买次数包记录
        tiktokTaskPriceOrdersMapper.insert(TiktokTaskPriceOrders.builder()
                .priceId(id)
                .userId(userId)
                .build());
        return JsonResult.successResult();
    }

    @Override
    @Transactional
    public JsonResult saveAndPublish(PublishTiktokTask publishTiktokTask) {
        if ((publishTiktokTask.getType() == TiktokTaskTypeEnum.COMMENTS.getCode() || publishTiktokTask.getType() == TiktokTaskTypeEnum.LIKE_TASK.getCode()) &&
                StringUtil.isAnyBlank(publishTiktokTask.getOpusId(), publishTiktokTask.getUsername())) {
            //点赞任务和评论任务需要输入用户名和作品Id
            return JsonResult.failureResult(ReturnMessageEnum.PARAM_CANNOT_BE_NULL);
        } else if (publishTiktokTask.getType() == TiktokTaskTypeEnum.CONCERN_TASK.getCode() && StringUtils.isAnyBlank(publishTiktokTask.getUsername(), publishTiktokTask.getTiktokUserId())) {
            //关注任务只需要输入用户名
            return JsonResult.failureResult(ReturnMessageEnum.PARAM_CANNOT_BE_NULL);
        }

        Integer userId = JwtTokenUtil.getUserIdFromToken(ThreadLocalManager.getToken());
        Wallets wallets = walletsService.selectColumnsByUserId(userId, Wallets::getHasTaskNum);

        //次数超过可用的次数则返回  次数包余额
        if (wallets.getHasTaskNum() < publishTiktokTask.getNum()) {
            return JsonResult.failureResult(ReturnMessageEnum.TASK_NUM_BALANCE);
        }

        //扣减任务包次数
        walletsService.reduceHasTaskNum(userId, publishTiktokTask.getNum());

        //添加任务信息
        TiktokTask tiktokTask = new TiktokTask();
        BeanUtils.copyProperties(publishTiktokTask, tiktokTask);
        if (StringUtil.isBlank(tiktokTask.getOpusId())) {
            tiktokTask.setOpusId("");
        }
        tiktokTask.setHasNum(tiktokTask.getNum());
        tiktokTask.setUserId(userId);
        tiktokTask.setBuilt(0);
        this.save(tiktokTask);
        return JsonResult.successResult();
    }

    @Override
    public JsonResult<TaskNumVO> getCountPackageBalance() {
        Integer userId = JwtTokenUtil.getUserIdFromToken(ThreadLocalManager.getToken());
        Wallets wallets = walletsService.selectColumnsByUserId(userId, Wallets::getTotalTaskNum, Wallets::getHasTaskNum);
        return JsonResult.successResult(new TaskNumVO(wallets.getTotalTaskNum(), wallets.getHasTaskNum()));
    }

    @Override
    public JsonResult pagelist(PageParam<TiktokTask, TiktokTaskCondition> pageParam) {
        if (pageParam.getSearchParam() == null) {
            pageParam.setSearchParam(new TiktokTaskCondition());
        }
        Page<TiktokTask> page = this.page(pageParam.getPage(), pageParam.getSearchParam().buildQueryParam());
        List<AdminTiktokTaskVO> list = new ArrayList<>();
        AdminTiktokTaskVO adminTiktokTaskVO;
        for (TiktokTask tiktokTask : page.getRecords()) {
            adminTiktokTaskVO = new AdminTiktokTaskVO();
            BeanUtils.copyProperties(tiktokTask, adminTiktokTaskVO);
            adminTiktokTaskVO.setUserUse(appUserTaskService.count(new LambdaQueryWrapper<AppUserTask>().eq(AppUserTask::getTaskId, tiktokTask.getId())) > 0);
            list.add(adminTiktokTaskVO);
        }
        return JsonResult.successResult(new PageVO<>(page.getTotal(), page.getPages(), list));
    }

    @Override
    public JsonResult addOrEdit(TiktokTask tiktokTask) {

        this.saveOrUpdate(tiktokTask);
        return JsonResult.successResult();
    }

    @Override
    public JsonResult deleteByIdList(ForceDeleteDTO forceDeleteDTO) {
        List<Long> deleteList = new ArrayList<>();
        if (forceDeleteDTO.isForce()) {
            deleteList.addAll(forceDeleteDTO.getIdList());
            appUserTaskService.remove(new LambdaQueryWrapper<AppUserTask>().eq(AppUserTask::getTaskId, deleteList));
        } else {
            for (Long id : forceDeleteDTO.getIdList()) {
                //删除之前判断任务是否被用户接取
                Long count = appUserTaskService.count(new LambdaQueryWrapper<AppUserTask>().eq(AppUserTask::getTaskId, id));
                if (count == 0) {
                    deleteList.add(id);
                }
            }
        }
        //删除tiktok任务
        this.removeByIds(deleteList);
        forceDeleteDTO.getIdList().removeAll(deleteList);
        if (forceDeleteDTO.getIdList().size() > 0) {
            return JsonResult.failureResult(ResponseStatusEnums.TASK_EXISTS_USER_USE.getCode(), String.format(ResponseStatusEnums.TASK_EXISTS_USER_USE.getName(), forceDeleteDTO.getIdList().size()), forceDeleteDTO.getIdList());
        } else {
            return JsonResult.successResult();
        }
    }
}
