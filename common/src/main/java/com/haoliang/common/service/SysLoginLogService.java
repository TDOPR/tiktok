package com.haoliang.common.service;

import com.baomidou.mybatisplus.extension.service.IService;
import com.haoliang.common.model.SysLoginLog;

public interface SysLoginLogService extends IService<SysLoginLog> {

    Integer getTodayLoginCount();

}
