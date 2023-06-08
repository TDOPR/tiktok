package com.haoliang.model.vo;

import lombok.Data;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/5/8 11:09
 **/
@Data
public class ArticleInfoVO {

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

}
