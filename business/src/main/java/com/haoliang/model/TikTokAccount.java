package com.haoliang.model;

import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.Data;
import org.springframework.data.annotation.Id;

import java.time.LocalDateTime;

/**
 * @author Dominick Li
 * @Description 绑定的tiktok账号
 * @CreateTime 2023/3/13 15:44
 **/
@Data
@TableName("tiktok_account")
public class TikTokAccount {

    @Id
    private String id;

    @JsonIgnore
    private Integer userId;

    private String username;

    private Integer active;

    /**
     * 创建时间
     */
    @TableField(fill = FieldFill.INSERT)
    @JsonIgnore
    private LocalDateTime createTime;
}
