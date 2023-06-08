package com.haoliang.service;

import com.baomidou.mybatisplus.extension.service.IService;
import com.haoliang.model.UpdateUserLevelJob;

import java.util.List;

public interface UpdateUserLevelTaskService extends IService<UpdateUserLevelJob> {

    /**
     * 当用户托管金额充值和取出，购买机器人和升级机器人需要 添加一条延迟任务更新链上的代理商等级
     * @param userId
     */
    void insertListByUserId(Integer userId);


    /**
     * 查询指定条数需要处理的任务
     * @param pageSize 任务数
     */
    List<UpdateUserLevelJob> findTask(Integer pageSize);

    /**
     * 更新所有代理商等级
     */
    void updateAll();
}

