package com.haoliang.model.dto;

import lombok.Data;

import javax.validation.constraints.NotNull;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/2/27 17:19
 **/
@Data
public class PublishTiktokTask {

    /**
     * tiktok用户名
     */
    private String username;

    /**
     * 作品Id
     */
    private String opusId;

    /**
     * tiktok用户Id
     */
    private String tiktokUserId;

    /**
     * 数量
     */
    @NotNull
    private Integer num;

    /**
     * 任务类型 1=关注任务 2=评论  3=点赞
     */
    @NotNull
    private Integer type;
}
