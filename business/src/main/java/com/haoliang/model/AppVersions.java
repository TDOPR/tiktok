package com.haoliang.model;

import com.baomidou.mybatisplus.annotation.TableName;
import com.haoliang.common.base.BaseModelCIDNoModifyTime;
import lombok.Data;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

/**
 * @author Dominick Li
 * @Description 系统版本表
 * @CreateTime 2022/11/22 9:36
 **/
@Data
@TableName("app_versions")
public class AppVersions extends BaseModelCIDNoModifyTime {


    /**
     * 系统名称 ios 、android
     */
    @NotBlank
    private String systemName;

    /**
     * 版本号
     */
    @NotBlank
    private String version;

    /**
     * 功能更新说明
     * 如需要换行 请再记事本中编辑好再复制到数据库中
     */
    private String znUpdateDesc;

    /**
     * 英语 功能更新说明
     */
    private String enUpdateDesc;

    /**
     * 葡萄牙语 功能更新说明
     */
    private String inUpdateDesc;

    /**
     * 西班牙语 功能更新说明
     */
    private String thUpdateDesc;

    /**
     * 西班牙语 功能更新说明
     */
    private String viUpdateDesc;

    /**
     * app下载地址
     */
    @NotBlank
    private String downloadAddress;

    /**
     * 激活版本  1=当前最新版本  0=旧版本
     */
    private Integer active;

    /**
     * 是否强制升级  1=强制更新  0=需要确认才能更新
     */
    @NotNull
    private Integer forceUpdate;
}
