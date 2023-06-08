package com.haoliang.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.haoliang.model.UpdateUserLevelJob;

public interface UpdateUserLevelTaskMapper extends BaseMapper<UpdateUserLevelJob> {

    /**
     * 递归查询用户的所有上级 已废弃
     */
    //List<AppUserTreeDTO> findUserTreeByUserId(Integer userId);

}
