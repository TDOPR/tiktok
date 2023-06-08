package com.haoliang.model.vo;

import lombok.Data;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/1/7 10:06
 **/
@Data
public class AppVersionVO {

    /**
     * 最新的版本号
     */
    private String version;

    /**
     * 下载地址
     */
    private String downloadAddress;

    /**
     * 更新说明
     */
    private String updateDesc;

}
