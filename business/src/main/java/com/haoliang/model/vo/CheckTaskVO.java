package com.haoliang.model.vo;

import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/3/1 14:42
 **/
@Data
public class CheckTaskVO {

    /**
     * 任务Id
     */
    @JsonFormat(shape = JsonFormat.Shape.STRING)
    private Long id;

    /**
     * 关注 任务类型
     */
    private Integer type;

    /**
     * 用户邮箱
     */
    private String email;

    /**
     * 用户Id
     */
    private Integer userId;

    /**
     * tiktok账号
     */
    private String username;

    /**
     * 作品Id
     */
    private String opusId;

    /**
     * 金额
     */
    private BigDecimal amount;

    /**
     * 待审需要的截图文件存储路径
     */
    private String imgUrl;

    /**
     * 申请时间
     */
    private LocalDateTime createTime;

    /**
     * 审核时间
     */
    private LocalDateTime auditTime;

    /**
     * 任务状态
     */
    private Integer status;

}
