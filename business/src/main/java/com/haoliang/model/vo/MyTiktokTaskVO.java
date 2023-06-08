package com.haoliang.model.vo;

import lombok.Data;

import java.time.LocalDateTime;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/2/24 14:40
 **/
@Data
public class MyTiktokTaskVO {

    /**
     * 总量
     */
    private Integer num;
    /**
     * 剩余的数量
     */
    private Integer hasNum;
    /**
     * 类型
     */
    private Integer type;
    /**
     * 时间
     */
    private LocalDateTime createTime;

    public String getCreateTime() {
        return createTime.toLocalDate().toString();
    }
}
