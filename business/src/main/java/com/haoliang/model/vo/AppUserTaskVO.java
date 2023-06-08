package com.haoliang.model.vo;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.haoliang.common.util.NumberUtil;
import lombok.Data;

import java.math.BigDecimal;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/2/27 11:59
 **/
@Data
public class AppUserTaskVO {

    /**
     * 用户接取的任务Id
     */
    @JsonFormat(shape = JsonFormat.Shape.STRING)
    private Long id;

    /**
     * 创建时间
     */
    private String createTime;

    /**
     * tiktokName
     */
    private String username;

    /**
     * tiktok用户Id
     */
    private String tiktokUserId;

    /**
     * 作品Id
     */
    private String opusId;

    /**
     * 作品类型
     */
    private Integer  type;

    /**
     * 待审需要的截图文件存储路径
     */
    private String imgUrl;

    /**
     * 金额
     */
    private String amount;

    /**
     * 内置任务类型
     */
    private Integer built;

    /**
     * 金额
     */
    @JsonIgnore
    private BigDecimal dAmount;

    public String getAmount() {
        return NumberUtil.toPlainString(dAmount);
    }
}
