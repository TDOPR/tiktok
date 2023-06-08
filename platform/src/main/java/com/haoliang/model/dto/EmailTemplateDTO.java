package com.haoliang.model.dto;

import lombok.Data;

/**
 * @author Dominick Li
 * @description 重xml中获取发送邮件的模版
 **/
@Data
public class EmailTemplateDTO {

    /**
     * 邮件标题
     */
    private String title;

    /**
     * 邮件内容
     */
    private String content;

    /**
     * 发送给谁
     */
    private String to;
}
