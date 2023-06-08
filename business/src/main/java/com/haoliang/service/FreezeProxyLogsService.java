package com.haoliang.service;

import com.baomidou.mybatisplus.extension.service.IService;
import com.haoliang.common.model.JsonResult;
import com.haoliang.enums.TttLogTypeEnum;
import com.haoliang.model.FreezeProxyLogs;

import java.math.BigDecimal;

public interface FreezeProxyLogsService extends IService<FreezeProxyLogs> {

    /**
     * 插入到七天待领取
     * @param userId 用户Id
     * @param type 类型
     * @param amount 奖励金额
     */
    void insertLogs(Integer userId, TttLogTypeEnum type, BigDecimal amount);

    /**
     * 查看用户七天待领取的金额 最近一周
     */
    BigDecimal getRecentOneWeek(Integer userId);

    JsonResult receiveReward();
}
