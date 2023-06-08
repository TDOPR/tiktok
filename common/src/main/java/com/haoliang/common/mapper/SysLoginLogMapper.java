package com.haoliang.common.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.haoliang.common.model.SysLoginLog;

public interface SysLoginLogMapper extends BaseMapper<SysLoginLog> {

    long getTodayLoginCount(String now);

}
