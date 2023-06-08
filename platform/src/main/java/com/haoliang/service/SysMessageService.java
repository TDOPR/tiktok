package com.haoliang.service;

import com.baomidou.mybatisplus.extension.service.IService;
import com.haoliang.common.model.JsonResult;
import com.haoliang.model.SysMessage;

import javax.servlet.http.HttpServletResponse;

public interface SysMessageService extends IService<SysMessage> {
    JsonResult saveMessage(SysMessage sysMessage);

    void exportJson(Integer type, HttpServletResponse httpServletResponse);
}
