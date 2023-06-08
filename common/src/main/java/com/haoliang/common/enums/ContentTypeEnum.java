package com.haoliang.common.enums;

/**
 * @author Dominick Li
 * @CreateTime 2021/1/26 11:19
 * @description 响应文件类型
 **/
public enum ContentTypeEnum {

    OCTET_STREAM("application/octet-stream","二进制流数据"),
    JPG("image/jpeg","jpg图片格式"),
    PNG("image/png","png图片格式"),
    WORD("application/msword","Word文档格式"),
    PDF("application/pdf ","pdf格式");

    ContentTypeEnum(String value, String name){
        this.value=value;
        this.name=name;
    }

    String value;

    String name;

    public String getValue() {
        return value;
    }
}
