package com.haoliang.model.vo;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.haoliang.common.enums.BooleanEnum;
import com.haoliang.common.util.NumberUtil;
import lombok.Data;

import java.math.BigDecimal;
import java.util.List;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/2/24 14:40
 **/
@Data
public class TiktokTaskVO {
    /**
     * 任务Id
     */
    @JsonFormat(shape = JsonFormat.Shape.STRING)
    private Long id;
    /**
     * tiktok用户名
     */
    private String username;
    /**
     * tiktok用户ID
     */
    private String tiktokUserId;
    /**
     * 作品Id
     */
    private String opusId;
    /**
     * 剩余的数量
     */
    private Integer hasNum;
    /**
     * 类型
     */
    private Integer type;
    /**
     * 接单状态 1=已接  0=未接
     */
    private Integer status;
    /**
     * 0=非内置 1=Telegram任务 2=Facebook任务 3=Twitter任务
     */
    private Integer built;

    /**
     * 金额
     */
    @JsonIgnore
    private BigDecimal bamount;

    public String getAmount() {
        return NumberUtil.toPlainString(bamount);
    }

    public Integer getStatus() {
        return status != null ? BooleanEnum.TRUE.intValue() : BooleanEnum.FALSE.intValue();
    }
}
