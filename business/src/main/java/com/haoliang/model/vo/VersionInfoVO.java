package com.haoliang.model.vo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author Dominick Li
 * @Description 最新版本信息
 * @CreateTime 2023/1/7 10:07
 **/
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class VersionInfoVO {

    /**
     * ios 版本信息
     */
    private AppVersionVO ios;

    /**
     * android 版本信息
     */
    private AppVersionVO android;

    /**
     * 服务条款
     */
    private String termsOfService;

    /**
     * 隐私政策
     */
    private String privacyPolicy;

}
