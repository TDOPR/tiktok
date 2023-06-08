package com.haoliang.model;

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
@TableName("article")
@NoArgsConstructor
@AllArgsConstructor
public class Article extends BaseModelCID {
    /**
     * 存储路径
     */
    @JsonIgnore
    private String path;
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
    private String title;

    /**
     * 简介
     */
    private String description;

    /**
     * 来源
     */
    private String source;

    /**
     * 内容
     */
    private String info;

    /**
     * 语种
     */
    private Integer language;

}
