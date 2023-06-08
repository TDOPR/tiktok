package com.haoliang.model.vo;

import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/11/10 12:28
 **/
@Data
@NoArgsConstructor
public class HomeVO {

    /**
     * 背景图
     */
    private List<String> bannerList;

    /**
     * 公会平台介绍
     */
    private String platformDesc;

    /**
     * 商业计划书
     */
    private String businessPlan;

    /**
     * TTT币白皮书
     */
    private String whitePaper;

    /**
     * 公会产品介绍
     */
    private String productDesc;

    /**
     * 公会任务模型
     */
    private String jobModel;

    /**
     * 新闻
     */
    private List<ArticleHomeVO> newsList;

    /**
     * 新闻总页数
     */
    private Integer totalPage;

    /**
     * 公告
     */
    private HomeNoticeVO notice;

}
