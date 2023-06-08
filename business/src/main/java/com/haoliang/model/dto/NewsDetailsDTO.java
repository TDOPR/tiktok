package com.haoliang.model.dto;

import lombok.Data;

/**
 * @author Dominick Li
 * @Description 存放国际化信息
 * @CreateTime 2023/3/3 10:07
 **/
@Data
public class NewsDetailsDTO {

    /**
     * 国际化信息 zh_CN=中文 en_US=英语 in_ID=印度尼西亚文  th_TH=泰语 vi_VN=越南语
     */
    private String language;

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
     * 富文本内容
     */
    private String info;

}
