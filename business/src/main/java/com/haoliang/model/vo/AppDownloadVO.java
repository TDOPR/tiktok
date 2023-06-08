package com.haoliang.model.vo;

import lombok.Builder;
import lombok.Data;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/4/10 18:54
 **/
@Data
@Builder
public class AppDownloadVO {

    private String ios;

    private String android;

    public AppDownloadVO(String ios, String android) {
        this.ios = ios;
        this.android = android;
    }
}
