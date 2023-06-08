package com.haoliang.model.dto;

import lombok.Data;

import javax.validation.constraints.NotNull;

/**
 * @author Dominick Li
 * @Description 检测版本更新
 * @CreateTime 2022/12/8 15:07
 **/
@Data
public class CheckVersionDTO {

    /**
     * 当前版本号
     */
    @NotNull
    private String version;

    /**
     * 系统名称
     */
    @NotNull
    private String systemName;

}
