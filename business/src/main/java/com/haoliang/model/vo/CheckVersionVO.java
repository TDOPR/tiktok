package com.haoliang.model.vo;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/12/8 15:10
 **/
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonInclude(JsonInclude.Include.NON_EMPTY)
public class CheckVersionVO {

    /**
     * 是否有新版本
     */
    private Boolean flag;

    /**
     * 最新的版本号
     */
    private String version;

    /**
     * 下载地址
     */
    private String downloadAddress;

    /**
     * 是否强制升级 true=强制  false=需要确认才能更新
     */
    private Boolean force;

    /**
     * 更新说明
     */
    private String updateDesc;

}
