package com.haoliang.service.impl;

import cn.hutool.core.io.FileUtil;
import com.alibaba.fastjson.JSONObject;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.haoliang.common.config.GlobalProperties;
import com.haoliang.common.enums.DataSavePathEnum;
import com.haoliang.common.enums.ReturnMessageEnum;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.model.PageParam;
import com.haoliang.common.model.ThreadLocalManager;
import com.haoliang.common.model.dto.PageDTO;
import com.haoliang.common.model.vo.PageVO;
import com.haoliang.common.util.DateUtil;
import com.haoliang.common.util.JwtTokenUtil;
import com.haoliang.common.util.StringUtil;
import com.haoliang.enums.TaskStatusEnum;
import com.haoliang.mapper.TreatGuestDinnerMapper;
import com.haoliang.model.AppUsers;
import com.haoliang.model.TreatGuestDinner;
import com.haoliang.model.condition.TreatGuestDinnerCondition;
import com.haoliang.model.dto.AuditCheckDTO;
import com.haoliang.model.dto.TreatGuestDinnerDTO;
import com.haoliang.model.vo.TreatGuestDinnerVO;
import com.haoliang.model.vo.TreatGuestDinnerVO2;
import com.haoliang.model.vo.UserValidVO;
import com.haoliang.service.AppUserService;
import com.haoliang.service.TreatGuestDinnerService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FileUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/3/23 10:51
 **/
@Slf4j
@Service
public class TreatGuestDinnerServiceImpl extends ServiceImpl<TreatGuestDinnerMapper, TreatGuestDinner> implements TreatGuestDinnerService {

    @Autowired
    private AppUserService appUserService;

    @Override
    public JsonResult<PageVO<TreatGuestDinnerVO>> getMyPage(PageDTO pageDTO) {
        Integer userId = JwtTokenUtil.getUserIdFromToken(ThreadLocalManager.getToken());
        Page<TreatGuestDinner> page = this.page(pageDTO.getPage(), new LambdaQueryWrapper<TreatGuestDinner>()
                .eq(TreatGuestDinner::getMasterUserId, userId)
                .orderByDesc(TreatGuestDinner::getCreateTime)
        );
        List<TreatGuestDinnerVO> treatGuestDinnerVOList = new ArrayList<>();
        TreatGuestDinnerVO treatGuestDinnerVO;
        for (TreatGuestDinner treatGuestDinner : page.getRecords()) {
            treatGuestDinnerVO = new TreatGuestDinnerVO();
            BeanUtils.copyProperties(treatGuestDinner, treatGuestDinnerVO);
            treatGuestDinnerVO.setItemUserIds(JSONObject.parseArray(treatGuestDinner.getItemUserIds(), Integer.class));
            treatGuestDinnerVOList.add(treatGuestDinnerVO);
        }
        return JsonResult.successResult(new PageVO(page.getTotal(), page.getPages(), treatGuestDinnerVOList));
    }

    @Override
    public JsonResult submitTask(MultipartFile file, MultipartFile img, int[] itemUserIds, String address, String coinType) {
        try {
            AppUsers appUsers;
            for (int userId : itemUserIds) {
                appUsers = appUserService.selectColumnsByUserId(userId, AppUsers::getId);
                if (appUsers == null) {
                    return JsonResult.failureResult(ReturnMessageEnum.INPUT_USER_ID_ERROR.setAndToString(String.valueOf(userId)));
                }
            }

            Integer masterUserId = JwtTokenUtil.getUserIdFromToken(ThreadLocalManager.getToken());
            String suffix = FileUtil.getSuffix(file.getOriginalFilename());
            String name = DateUtil.getDetailTimeIgnoreUnit();
            String fileName = masterUserId + "_" + name + "." + suffix;
            String savePath = DataSavePathEnum.VIDEO.getPath();
            String url = GlobalProperties.getVirtualPathURL() + StringUtil.replace(savePath, GlobalProperties.getRootPath(), "") + fileName;
            //复制视频文件流到本地文件
            FileUtils.copyInputStreamToFile(file.getInputStream(), new File(DataSavePathEnum.VIDEO.getFile(), fileName));

            suffix = FileUtil.getSuffix(img.getOriginalFilename());
            fileName = masterUserId + "_" + name + "." + suffix;

            String imgUrl = GlobalProperties.getVirtualPathURL() + StringUtil.replace(savePath, GlobalProperties.getRootPath(), "") + fileName;
            FileUtils.copyInputStreamToFile(img.getInputStream(), new File(DataSavePathEnum.VIDEO.getFile(), fileName));

            TreatGuestDinner treatGuestDinner = TreatGuestDinner.builder()
                    .status(TaskStatusEnum.TO_BE_CHECK.getCode())
                    .address(address)
                    .coinType(coinType)
                    .videoUrl(url)
                    .imgUrl(imgUrl)
                    .itemUserIds(JSONObject.toJSONString(itemUserIds))
                    .masterUserId(masterUserId)
                    .build();
            this.save(treatGuestDinner);
        } catch (Exception e) {
            e.printStackTrace();
            log.error("submitTask error:{}", e);
            return JsonResult.failureResult();
        }
        return JsonResult.successResult();
    }

    @Override
    public JsonResult edit(Integer id, MultipartFile file, MultipartFile img, int[] itemUserIds, String address, String coinType) {
        try {
            AppUsers appUsers;
            for (int userId : itemUserIds) {
                appUsers = appUserService.selectColumnsByUserId(userId, AppUsers::getId);
                if (appUsers == null) {
                    return JsonResult.failureResult(ReturnMessageEnum.INPUT_USER_ID_ERROR.setAndToString(String.valueOf(userId)));
                }
            }

            Integer masterUserId = JwtTokenUtil.getUserIdFromToken(ThreadLocalManager.getToken());
            TreatGuestDinner treatGuestDinner = this.getById(id);
            treatGuestDinner.setAddress(address);
            treatGuestDinner.setCoinType(coinType);
            treatGuestDinner.setStatus(TaskStatusEnum.TO_BE_CHECK.getCode());
            treatGuestDinner.setItemUserIds(JSONObject.toJSONString(itemUserIds));

            if (file != null) {
                String savePath = DataSavePathEnum.VIDEO.getPath();
                String headName = treatGuestDinner.getVideoUrl().substring(treatGuestDinner.getVideoUrl().lastIndexOf("/") + 1);
                FileUtil.del(new File(savePath, headName));
                headName = treatGuestDinner.getImgUrl().substring(treatGuestDinner.getImgUrl().lastIndexOf("/") + 1);
                FileUtil.del(new File(savePath, headName));

                String name = DateUtil.getDetailTimeIgnoreUnit();

                String suffix = FileUtil.getSuffix(file.getOriginalFilename());
                String fileName = masterUserId + "_" + name + "." + suffix;
                String url = GlobalProperties.getVirtualPathURL() + StringUtil.replace(savePath, GlobalProperties.getRootPath(), "") + fileName;
                //复制文件流到本地文件
                FileUtils.copyInputStreamToFile(file.getInputStream(), new File(DataSavePathEnum.VIDEO.getFile(), fileName));
                treatGuestDinner.setVideoUrl(url);

                suffix = FileUtil.getSuffix(img.getOriginalFilename());
                fileName = masterUserId + "_" + name + "." + suffix;
                url = GlobalProperties.getVirtualPathURL() + StringUtil.replace(savePath, GlobalProperties.getRootPath(), "") + fileName;
                FileUtils.copyInputStreamToFile(img.getInputStream(), new File(DataSavePathEnum.VIDEO.getFile(), fileName));
                treatGuestDinner.setImgUrl(url);
            }
            this.updateById(treatGuestDinner);
        } catch (Exception e) {
            e.printStackTrace();
            log.error("submitTask error:{}", e.getMessage());
            return JsonResult.failureResult();
        }
        return JsonResult.successResult();
    }

    @Override
    public JsonResult editJson(TreatGuestDinnerDTO treatGuestDinnerDTO) {
        AppUsers appUsers;
        for (int userId : treatGuestDinnerDTO.getItemUserIds()) {
            appUsers = appUserService.selectColumnsByUserId(userId, AppUsers::getId);
            if (appUsers == null) {
                return JsonResult.failureResult(ReturnMessageEnum.INPUT_USER_ID_ERROR.setAndToString(String.valueOf(userId)));
            }
        }
        TreatGuestDinner treatGuestDinner = this.getById(treatGuestDinnerDTO.getId());
        treatGuestDinner.setAddress(treatGuestDinnerDTO.getAddress());
        treatGuestDinner.setCoinType(treatGuestDinnerDTO.getCoinType());
        treatGuestDinner.setStatus(TaskStatusEnum.TO_BE_CHECK.getCode());
        treatGuestDinner.setItemUserIds(JSONObject.toJSONString(treatGuestDinnerDTO.getItemUserIds()));
        this.updateById(treatGuestDinner);
        return JsonResult.successResult();
    }

    @Override
    public JsonResult getCheckList(PageParam<TreatGuestDinner, TreatGuestDinnerCondition> pageParam) {
        if (pageParam.getSearchParam() == null) {
            pageParam.setSearchParam(new TreatGuestDinnerCondition());
        }
        Page<TreatGuestDinnerVO2> page = this.baseMapper.page(pageParam.getPage(), pageParam.getSearchParam());
        List<Integer> userIdList;
        List<UserValidVO> userValidList;
        UserValidVO validVO;
        List<AppUsers> appUserList;
        for (TreatGuestDinnerVO2 treatGuestDinnerVO2 : page.getRecords()) {
            userIdList = JSONObject.parseArray(treatGuestDinnerVO2.getItemUserIds(), Integer.class);
            appUserList = appUserService.list(new LambdaQueryWrapper<AppUsers>().in(AppUsers::getId, userIdList));
            userValidList = new ArrayList<>();
            for (AppUsers appUsers : appUserList) {
                validVO = new UserValidVO();
                BeanUtils.copyProperties(appUsers, validVO);
                userValidList.add(validVO);
            }
            treatGuestDinnerVO2.setUserList(userValidList);
        }
        return JsonResult.successResult(new PageVO<>(page));
    }

    @Override
    public JsonResult check(AuditCheckDTO auditCheckDTO) {
        UpdateWrapper<TreatGuestDinner> wrapper = Wrappers.update();
        wrapper.lambda()
                .set(TreatGuestDinner::getStatus, auditCheckDTO.getState())
                .set(TreatGuestDinner::getAuditTime, LocalDateTime.now())
                .eq(TreatGuestDinner::getId, auditCheckDTO.getId());
        this.update(wrapper);
        return JsonResult.successResult();
    }
}
