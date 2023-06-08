package com.haoliang.service.impl;

import cn.hutool.core.io.FileUtil;
import com.alibaba.fastjson.JSONObject;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.haoliang.common.config.GlobalProperties;
import com.haoliang.common.enums.BooleanEnum;
import com.haoliang.common.enums.DataSavePathEnum;
import com.haoliang.common.enums.ReturnMessageEnum;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.model.PageParam;
import com.haoliang.common.model.ThreadLocalManager;
import com.haoliang.common.model.vo.PageVO;
import com.haoliang.common.util.IdUtil;
import com.haoliang.common.util.JwtTokenUtil;
import com.haoliang.common.util.StringUtil;
import com.haoliang.constant.TiktokConfig;
import com.haoliang.enums.*;
import com.haoliang.mapper.AppUserTaskMapper;
import com.haoliang.mapper.TiktokTaskMapper;
import com.haoliang.mapper.VipOrdersMapper;
import com.haoliang.model.AppUserTask;
import com.haoliang.model.AppUsers;
import com.haoliang.model.TiktokTask;
import com.haoliang.model.VipOrders;
import com.haoliang.model.condition.AppUserTaskCondition;
import com.haoliang.model.condition.CheckTaskCondition;
import com.haoliang.model.dto.AppUserTaskDTO;
import com.haoliang.model.dto.AuditCheckDTO;
import com.haoliang.model.dto.TiktokCountDTO;
import com.haoliang.model.dto.TiktokTaskDTO;
import com.haoliang.model.vo.AppUserTaskVO;
import com.haoliang.model.vo.CheckTaskVO;
import com.haoliang.service.*;
import com.haoliang.utils.EarningUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FileUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import javax.annotation.Resource;
import java.io.File;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDateTime;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;


/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/2/24 15:50
 **/
@Slf4j
@Service
public class AppUserTaskServiceImpl extends ServiceImpl<AppUserTaskMapper, AppUserTask> implements AppUserTaskService {

    @Autowired
    private VipOrdersService vipOrdersService;

    @Autowired
    private WalletsService walletsService;

    @Autowired
    private KLineDataService kLineDataService;

    @Autowired
    private AppUserService appUserService;

    @Resource
    private TiktokTaskMapper tiktokTaskMapper;

    @Resource
    private VipOrdersMapper vipOrdersMapper;

    @Override
    @Transactional(rollbackFor = Exception.class)
    public JsonResult saveByTaskId(Long id) {
        Integer userId = JwtTokenUtil.getUserIdFromToken(ThreadLocalManager.getToken());
        AppUsers appUsers = appUserService.selectColumnsByUserId(userId, AppUsers::getVipLevel);
        BigDecimal allowance;
        Integer maxLevel;
        if (appUsers.getVipLevel().equals(VipLevelEnum.ZERO.getLevel())) {
            allowance = vipOrdersMapper.sumAllowanceByUserIdAndZeroLevel(userId);
        } else {
            allowance = vipOrdersMapper.sumAllowanceByUserId(userId);
        }
        if (allowance == null || allowance.compareTo(BigDecimal.ZERO) == 0) {
            //可用余额不足
            return JsonResult.failureResult(ResponseStatusEnums.INSUFFICIENT_LIMIT_CODE.getCode(), ReturnMessageEnum.INSUFFICIENT_LIMIT);
        }

        List<VipOrders> vipOrdersList = vipOrdersService.getListByUserIdAndLevel(userId, appUsers.getVipLevel());
        maxLevel = vipOrdersList.stream().max(Comparator.comparingInt(VipOrders::getLevel)).get().getLevel();

        VipLevelEnum hasLevel = VipLevelEnum.getByLevel(maxLevel);
        //判断接单是否达到上限
        int count = this.baseMapper.selectCountByUserIdAndMaxLevel(userId, maxLevel);

        if (count >= hasLevel.getTaskNumLimit()) {
            return JsonResult.failureResult(ReturnMessageEnum.TASK_NUM_LIMIT);
        }

        //从使用的vip套餐里冻结相应的金额
        BigDecimal oneEarning, totalEarning = BigDecimal.ZERO;
        for (VipOrders vipOrders : vipOrdersList) {
            //如果是最高等级则取用定义的单收益,否则根据以 日收益上限/单量 计算
            oneEarning = EarningUtil.getEarningByAllowance(hasLevel, vipOrders);
            totalEarning = totalEarning.add(oneEarning);
            vipOrders.setFrozenAmount(vipOrders.getFrozenAmount().add(oneEarning));
            vipOrders.setAllowance(vipOrders.getAllowance().subtract(oneEarning));
            vipOrdersMapper.updateById(vipOrders);
        }

        //扣除任务数量
        tiktokTaskMapper.reduceNum(id);
        //保存用户的接单信息
        BigDecimal amount = totalEarning.divide(kLineDataService.getNowExchangeRate(), TiktokConfig.NUMBER_OF_DIGITS, RoundingMode.FLOOR);
        AppUserTask appUserTask = new AppUserTask();
        appUserTask.setVipOrderIds(JSONObject.toJSONString(vipOrdersList.stream().map(VipOrders::getId).collect(Collectors.toList())));
        appUserTask.setTaskId(id);
        appUserTask.setAmount(amount);
        appUserTask.setUserId(userId);
        appUserTask.setLevel(maxLevel);
        this.save(appUserTask);
        return JsonResult.successResult();
    }

    @Override
    public JsonResult uploadHeadImage(MultipartFile file, Long id) {
        try {
            AppUserTask appUserTask = this.getOne(new LambdaQueryWrapper<AppUserTask>().select(AppUserTask::getImgUrl).eq(AppUserTask::getId, id));
            String suffix = FileUtil.getSuffix(file.getOriginalFilename());
            String fileName = id + "_" + IdUtil.getSnowflakeNextId() + "." + suffix;
            DataSavePathEnum dataSavePathEnum = DataSavePathEnum.TASK_IMAGE;
            String savePath = dataSavePathEnum.getPath();

            //删除之前上传过的截图
            if (StringUtil.isNoneBlank(appUserTask.getImgUrl())) {
                String headName = appUserTask.getImgUrl().substring(appUserTask.getImgUrl().lastIndexOf("/") + 1);
                FileUtil.del(new File(savePath, headName));
            }

            String url = GlobalProperties.getVirtualPathURL() + StringUtil.replace(savePath, GlobalProperties.getRootPath(), "") + fileName;
            //复制文件流到本地文件
            FileUtils.copyInputStreamToFile(file.getInputStream(), new File(dataSavePathEnum.getFile(), fileName));
            UpdateWrapper<AppUserTask> updateWrapper = Wrappers.update();
            updateWrapper.lambda()
                    .set(AppUserTask::getImgUrl, url)
                    .eq(AppUserTask::getId, id);
            boolean flag = this.update(updateWrapper);
            if (flag) {
                JSONObject object = new JSONObject();
                object.put("url", url);
                return JsonResult.successResult(object);
            }
        } catch (Exception e) {
            e.printStackTrace();
            log.error("upload error:{}", e.getMessage());
        }
        return JsonResult.failureResult();
    }

    @Override
    @Transactional
    public JsonResult submit(Long id) {
        TiktokTaskDTO tiktokTaskDTO = this.baseMapper.getTiktokTaskDTO(id);
        if (StringUtil.isBlank(tiktokTaskDTO.getImgUrl())) {
            return JsonResult.failureResult(ReturnMessageEnum.PLEASE_UPLOAD_IMG);
        }
        //如果提交的任务已是审核中或成功,则不处理
        if (TaskStatusEnum.TO_BE_CHECK.getCode() == tiktokTaskDTO.getStatus() || TaskStatusEnum.SUCCESS.getCode() == tiktokTaskDTO.getStatus()) {
            return JsonResult.failureResult();
        }
        if (tiktokTaskDTO.getBuilt() > 0) {
            //如果是内置任务类型需要查看用户是否做过新手教程
            AppUsers appUsers = appUserService.selectColumnsByUserId(tiktokTaskDTO.getUserId(), AppUsers::getGreenhorn);
            if (BooleanEnum.TRUE.intValue().equals(appUsers.getGreenhorn())) {
                //修改用户做过新手教程
                UpdateWrapper<AppUsers> appUsersUpdateWrapper = Wrappers.update();
                appUsersUpdateWrapper.lambda()
                        .set(AppUsers::getGreenhorn, BooleanEnum.FALSE.intValue())
                        .eq(AppUsers::getId, tiktokTaskDTO.getUserId());
                appUserService.update(appUsersUpdateWrapper);
                //新手教程则自动审核发放任务奖励
//                AuditCheckDTO auditCheckDTO = new AuditCheckDTO();
//                auditCheckDTO.setId(id);
//                auditCheckDTO.setState(TaskStatusEnum.SUCCESS.getCode());
//                return this.checkTask(auditCheckDTO);
            }
        }

        UpdateWrapper<AppUserTask> updateWrapper = Wrappers.update();
        updateWrapper.lambda()
                .set(AppUserTask::getStatus, TaskStatusEnum.TO_BE_CHECK.getCode())
                .eq(AppUserTask::getId, id);
        this.update(updateWrapper);
        return JsonResult.successResult();
    }

    @Override
    public JsonResult<PageVO<AppUserTaskVO>> myList(PageParam<AppUserTaskVO, AppUserTaskCondition> pageParam) {
        Page<AppUserTaskVO> page = this.baseMapper.page(pageParam.getPage(), pageParam.getSearchParam().getStatus(), JwtTokenUtil.getUserIdFromToken(ThreadLocalManager.getToken()));
        return JsonResult.successResult(new PageVO<>(page));
    }


    @Override
    public JsonResult<PageVO<CheckTaskVO>> findCheckTaskList(PageParam<CheckTaskVO, CheckTaskCondition> pageParam) {
        if (pageParam.getSearchParam() == null) {
            pageParam.setSearchParam(new CheckTaskCondition());
        }
        Page<CheckTaskVO> page = this.baseMapper.checkTaskPage(pageParam.getPage(), pageParam.getSearchParam());
        return JsonResult.successResult(new PageVO<>(page));
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public JsonResult checkTask(AuditCheckDTO auditCheckDTO) {
        if (TiktokSettingEnum.ENABLED_TIKTOK_AUTO_CHECK.boolValue()) {
            return JsonResult.successResult("已开启自动审核,如需手动审核请先关闭自动审核功能！");
        }
        AppUserTaskDTO appUserTaskDTO = this.baseMapper.getById(auditCheckDTO.getId());
        if (TaskStatusEnum.SUCCESS.getCode() == auditCheckDTO.getState()) {
            VipLevelEnum hasLevel;
            List<VipOrders> vipOrdersList;
            UpdateWrapper<VipOrders> vipOrdersUpdateWrapper;
            Integer vipOrderLevel = 0;
            vipOrdersList = vipOrdersMapper.selectBatchIds(JSONObject.parseArray(appUserTaskDTO.getVipOrderIds(), Integer.class));
            Integer maxLevel = vipOrdersList.stream().max(Comparator.comparingInt(VipOrders::getLevel)).get().getLevel();
            hasLevel = VipLevelEnum.getByLevel(maxLevel);
            for (VipOrders vipOrders : vipOrdersList) {
                vipOrdersUpdateWrapper = Wrappers.update();
                vipOrdersUpdateWrapper.lambda()
                        .set(VipOrders::getFrozenAmount, vipOrders.getFrozenAmount().subtract(EarningUtil.getEarningByFrozen(hasLevel, vipOrders)))
                        .eq(VipOrders::getId, vipOrders.getId());
                vipOrdersService.update(vipOrdersUpdateWrapper);
                vipOrderLevel = vipOrders.getLevel();
            }
            walletsService.updateTttWallet(appUserTaskDTO.getAmount(), appUserTaskDTO.getUserId(), FlowingActionEnum.INCOME, TttLogTypeEnum.valueOf(TiktokTaskTypeEnum.getTttLogType(appUserTaskDTO.getType())), vipOrderLevel == 0);
        }
        UpdateWrapper<AppUserTask> updateWrapper = Wrappers.update();
        updateWrapper.lambda()
                .set(AppUserTask::getStatus, auditCheckDTO.getState())
                .set(AppUserTask::getAuditTime, LocalDateTime.now())
                .eq(AppUserTask::getId, appUserTaskDTO.getId());
        this.update(updateWrapper);
        return JsonResult.successResult();
    }


    @Override
    @Transactional(rollbackFor = Exception.class)
    public void checkTaskList() {
        if (!TiktokSettingEnum.ENABLED_TIKTOK_AUTO_CHECK.boolValue()) {
            return;
        }
        //只处理比当前时间早指定分钟的任务
        List<AppUserTaskDTO> appUserTaskDTOList = this.baseMapper.getAutoCheckList(TaskStatusEnum.TO_BE_CHECK.getCode(), LocalDateTime.now().minusMinutes(TiktokSettingEnum.TIKTOK_AUTO_CHECK_LAZY_TIME.intValue()));
        if (appUserTaskDTOList.size() == 0) {
            return;
        }
        UpdateWrapper<AppUserTask> updateWrapper;
        VipLevelEnum hasLevel;
        List<VipOrders> vipOrdersList;
        UpdateWrapper<VipOrders> vipOrdersUpdateWrapper;
        Integer vipOrderLevel = 0;
        for (AppUserTaskDTO appUserTaskDTO : appUserTaskDTOList) {
            updateWrapper = Wrappers.update();
            vipOrdersList = vipOrdersMapper.selectBatchIds(JSONObject.parseArray(appUserTaskDTO.getVipOrderIds(), Integer.class));
            if (vipOrdersList.size() == 0) {
                this.removeById(appUserTaskDTO.getId());
                continue;
            }
            Integer maxLevel = vipOrdersList.stream().max(Comparator.comparingInt(VipOrders::getLevel)).get().getLevel();
            hasLevel = VipLevelEnum.getByLevel(maxLevel);
            for (VipOrders vipOrders : vipOrdersList) {
                vipOrdersUpdateWrapper = Wrappers.update();
                vipOrdersUpdateWrapper.lambda()
                        .set(VipOrders::getFrozenAmount, vipOrders.getFrozenAmount().subtract(EarningUtil.getEarningByFrozen(hasLevel, vipOrders)))
                        .eq(VipOrders::getId, vipOrders.getId());
                vipOrdersService.update(vipOrdersUpdateWrapper);
                vipOrderLevel = vipOrders.getLevel();
            }
            walletsService.updateTttWallet(appUserTaskDTO.getAmount(), appUserTaskDTO.getUserId(), FlowingActionEnum.INCOME, TttLogTypeEnum.valueOf(TiktokTaskTypeEnum.getTttLogType(appUserTaskDTO.getType())), vipOrderLevel == 0);
            updateWrapper.lambda()
                    .set(AppUserTask::getStatus, TaskStatusEnum.SUCCESS.getCode())
                    .eq(AppUserTask::getId, appUserTaskDTO.getId());
            this.update(updateWrapper);
        }
        log.info("自动审核了{}条记录!", appUserTaskDTOList.size());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void cleanZeroUserTask(Integer userId) {
        //非有效用户升级成有效用户时候的操作
        AppUsers appUsers = appUserService.selectColumnsByUserId(userId, AppUsers::getValid);
        if (appUsers.getValid().equals(BooleanEnum.FALSE.intValue())) {
            //设置零撸的vip套餐失效
            UpdateWrapper<VipOrders> vipOrdersUpdateWrapper = Wrappers.update();
            vipOrdersUpdateWrapper.lambda()
                    .set(VipOrders::getValid, BooleanEnum.FALSE.intValue())
                    .set(VipOrders::getFrozenAmount, BigDecimal.ZERO)
                    .eq(VipOrders::getUserId, userId);
            vipOrdersService.update(vipOrdersUpdateWrapper);
            this.removeZeroUserTask(userId);
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void removeZeroUserTask(Integer userId) {
        LambdaQueryWrapper queryWrapper = new LambdaQueryWrapper<AppUserTask>()
                .eq(AppUserTask::getUserId, userId)
                .ne(AppUserTask::getStatus, TaskStatusEnum.SUCCESS.getCode());
        List<AppUserTask> taskList = this.list(queryWrapper);
        if (taskList.size() > 0) {
            //找到零撸用户非成功的数据然后返回次数给tiktok任务
            List<Long> tiktokTaskId = taskList.stream().map(AppUserTask::getTaskId).collect(Collectors.toList());
            //增加次数
            tiktokTaskMapper.increaseNum(tiktokTaskId);
        }
        this.remove(queryWrapper);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void clearAppUserTask() {

        //找到tiktok任务并增加要删除对应的次数
        List<TiktokCountDTO> tiktokCountDTOList = this.baseMapper.selectTiktokCountNeStatus(TaskStatusEnum.SUCCESS.getCode());
        TiktokTask tiktokTask;
        for (TiktokCountDTO tiktokCountDTO : tiktokCountDTOList) {
            tiktokTask = tiktokTaskMapper.selectById(tiktokCountDTO.getTaskId());
            if (tiktokTask != null) {
                tiktokTask.setHasNum(tiktokTask.getHasNum() + tiktokCountDTO.getCount());
                tiktokTaskMapper.updateById(tiktokTask);
            }
        }

        //清除非新手已接单未完成的任务
        this.baseMapper.removeTaskNeStatus(TaskStatusEnum.SUCCESS.getCode());

        //从冻结的金额里面返回给套餐余额
        this.vipOrdersService.clearFrozenAmount();
    }

}
