package com.haoliang.model;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

/**
 * @author Dominick Li
 * @Description 动态更新用户代理商等级和业务的任务表
 * @CreateTime 2022/11/25 11:10
 **/
@Data
@TableName("update_user_level_job")
@NoArgsConstructor
public class UpdateUserLevelJob {

    /**
     * 需要更新代理商等级的用户Id 分钟
     */
    @TableId(type = IdType.NONE)
    private Integer userId;

    /**
     * 任务处理时间,需要延迟指定时间
     */
    private LocalDateTime delayTime;


    public UpdateUserLevelJob(Integer userId, LocalDateTime createTime, Integer minute) {
        this.userId = userId;
        this.delayTime = createTime.plusMinutes(minute);
    }
}
