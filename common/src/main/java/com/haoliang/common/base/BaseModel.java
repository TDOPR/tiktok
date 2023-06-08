package com.haoliang.common.base;

import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Data;

import java.time.LocalDateTime;

/**
 * @Date 2019/3/13 14:18
 * @Created by LeeJunMing
 * 基类
 */
@Data
public  class BaseModel{

    private static final long serialVersionUID = 2863256929817929823L;

    /**
     *  唯一标识
     */
    @JsonFormat(shape = JsonFormat.Shape.STRING)
    @TableId(type = IdType.ASSIGN_ID)
    private Long id;

    /**
     * 创建时间
     * @ignore
     */
    @TableField(fill = FieldFill.INSERT)
    private LocalDateTime createTime;

    /**
     * 修改时间
     * @ignore
     */
    @TableField(fill = FieldFill.INSERT_UPDATE)
    private LocalDateTime lastmodifiedTime;

}
