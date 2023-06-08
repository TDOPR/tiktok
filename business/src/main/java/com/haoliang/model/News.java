package com.haoliang.model;

import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.haoliang.common.base.BaseModelCID;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author Dominick Li
 * @Description 文章和公告
 * @CreateTime 2023/3/2 15:17
 **/
@Data
@TableName("news")
@NoArgsConstructor
@AllArgsConstructor
public class News extends BaseModelCID {
    /**
     * 存储路径
     */
    @JsonIgnore
    private String path;
    /**
     * 类型 1=文章 2=公告
     */
    private Integer type;
    /**
     * 新闻的背景图
     */
    private String bannerUrl;

    /**
     * 使用启用 1=启用 0=禁用
     */
    private Integer enabled;

    /**
     * 标题
     */
    @TableField(exist = false)
    private String title;

    /**
     * 简介
     */
    @TableField(exist = false)
    private String description;

    /**
     * 来源
     */
    @TableField(exist = false)
    private String source;

    /**
     * 内容
     */
    @TableField(exist = false)
    private String info;

    /**
     * 是否强制通知 只有公告类型包含此属性  1=强制通知
     */
    private Integer forceStatus;

}
