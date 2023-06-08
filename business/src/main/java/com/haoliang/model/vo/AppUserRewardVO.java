package com.haoliang.model.vo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;

/**
 * @Description
 * @Author Dominick Li
 * @CreateTime 2023/3/26 15:23
 **/
@Data
@Builder
@AllArgsConstructor
public class AppUserRewardVO {

    /**
     * 总动态收益
     */
    private String totalDynamicReward;

    /**
     * 总静态收益
     */
    private String totalStaticReward;

    /**
     * 动态收益
     */
    private String dynamicReward;

    /**
     * 静态收益
     */
    private String staticReward;

}
