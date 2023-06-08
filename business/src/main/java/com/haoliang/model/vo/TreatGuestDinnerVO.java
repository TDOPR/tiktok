package com.haoliang.model.vo;

import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.List;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/3/23 11:27
 **/
@Data
public class TreatGuestDinnerVO {

    private Integer id;

    @JsonFormat(pattern = "yyyy-MM-dd")
    private LocalDateTime createTime;

    /**
     * 组织者
     */
    private Integer masterUserId;

    /**
     * 新用户Id
     */
    private List<Integer> itemUserIds;

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
     * 任务状态 2=待审核 3=未通过  4=通过
     */
    private Integer status;

}
