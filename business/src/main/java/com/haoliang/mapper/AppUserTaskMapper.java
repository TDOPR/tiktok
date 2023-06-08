package com.haoliang.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.haoliang.model.AppUserTask;
import com.haoliang.model.condition.CheckTaskCondition;
import com.haoliang.model.dto.AppUserTaskDTO;
import com.haoliang.model.dto.TiktokCountDTO;
import com.haoliang.model.dto.TiktokTaskDTO;
import com.haoliang.model.vo.AppUserTaskVO;
import com.haoliang.model.vo.CheckTaskVO;
import org.apache.ibatis.annotations.Param;

import java.time.LocalDateTime;
import java.util.List;

public interface AppUserTaskMapper extends BaseMapper<AppUserTask> {
    Page<AppUserTaskVO> page(Page<AppUserTaskVO> page, @Param("status") Integer status, @Param("userId") Integer userId);

    Page<CheckTaskVO> checkTaskPage(Page<CheckTaskVO> page, @Param("param") CheckTaskCondition searchParam);

    int selectCountByUserIdAndMaxLevel(@Param("userId") Integer userId, @Param("level") Integer level);

    List<AppUserTaskDTO> getAutoCheckList(@Param("status") Integer status, @Param("localDateTime") LocalDateTime localDateTime);

    AppUserTaskDTO getById(Long id);

    TiktokTaskDTO getTiktokTaskDTO(Long id);

    List<TiktokCountDTO> selectTiktokCountNeStatus(Integer status);

    int removeTaskNeStatus(Integer status);
}
