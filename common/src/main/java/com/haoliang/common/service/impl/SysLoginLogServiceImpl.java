package com.haoliang.common.service.impl;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.haoliang.common.mapper.SysLoginLogMapper;
import com.haoliang.common.model.SysLoginLog;
import com.haoliang.common.service.SysLoginLogService;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.time.LocalDate;


@Service
public class SysLoginLogServiceImpl extends ServiceImpl<SysLoginLogMapper, SysLoginLog> implements SysLoginLogService {

    @Resource
    SysLoginLogMapper sysLoginLogMapper;

    @Override
    public Integer getTodayLoginCount() {
        return (int)sysLoginLogMapper.getTodayLoginCount(LocalDate.now().toString());
    }
}
