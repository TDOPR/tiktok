package com.haoliang.model;

import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.haoliang.common.base.BaseModelCID;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;


/**
 * @author Dominick Li
 * @Description 请客吃饭任务管理
 * @CreateTime 2023/3/23 10:28
 **/
@Data
@TableName("treat_guest_dinner")
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class TreatGuestDinner extends BaseModelCID {

    /**
     * 请客的主人用户ID
     */
    private Integer masterUserId;

    /**
     * 新用户ID
     */
    @JsonIgnore
    private String itemUserIds;

    /**
     * 封面图路径
     */
    private String imgUrl;

    /**
     * 视频播放地址
     */
    private String videoUrl;

    /**
     * 报销地址
     */
    private String address;

    /**
     * 链的类型
     */
    private String coinType;

    /**
     * 任务状态 2=待审核 3=驳回  4=已完成
     */
    private Integer status;

    /**
     * 审核时间
     */
    private LocalDateTime auditTime;
}
