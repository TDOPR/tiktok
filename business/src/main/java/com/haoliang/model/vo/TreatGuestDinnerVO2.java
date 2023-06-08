package com.haoliang.model.vo;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.List;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/3/23 19:20
 **/
@Data
public class TreatGuestDinnerVO2 {

    private Integer id;

    private LocalDateTime createTime;

    private LocalDateTime auditTime;

    /**
     * 组织者
     */
    private Integer userId;

    /**
     * 组织者
     */
    private String userName;

    /**
     * 新用户Id
     */
    @JsonIgnore
    private String itemUserIds;

    /**
     * 视频播放地址
     */
    private String videoUrl;

    /**
     * 图片地址
     */
    private String imgUrl;

    /**
     * 报销地址
     */
    private String address;

    /**
     * 链的类型
     */
    private String coinType;

    /**
     * 审核状态
     */
    private Integer status;

    /**
     * 下级用户
     */
    private List<UserValidVO> userList;


}
