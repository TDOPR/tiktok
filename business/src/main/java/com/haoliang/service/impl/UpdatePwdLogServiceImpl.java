package com.haoliang.service.impl;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.haoliang.common.enums.ReturnMessageEnum;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.model.ThreadLocalManager;
import com.haoliang.common.util.DateUtil;
import com.haoliang.common.util.JwtTokenUtil;
import com.haoliang.mapper.UpdatePwdLogMapper;
import com.haoliang.model.UpdatePwdLog;
import com.haoliang.service.UpdatePwdLogService;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/5/16 11:23
 **/
@Service
public class UpdatePwdLogServiceImpl extends ServiceImpl<UpdatePwdLogMapper, UpdatePwdLog> implements UpdatePwdLogService {

    @Override
    public JsonResult checkUpdateTime() {
        Integer userId = JwtTokenUtil.getUserIdFromToken(ThreadLocalManager.getToken());
        UpdatePwdLog updatePwdLog = this.getById(userId);
        if (updatePwdLog != null) {
            boolean errorFlag = DateUtil.betweenDays(updatePwdLog.getLastmodifiedTime(), LocalDateTime.now()) == 0;
            if (errorFlag) {
                return JsonResult.failureResult(ReturnMessageEnum.UPDATE_PWD_TIME_LIMIT);
            }
        }
        return JsonResult.successResult();
    }
}
