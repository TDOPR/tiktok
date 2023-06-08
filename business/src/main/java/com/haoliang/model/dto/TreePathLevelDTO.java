package com.haoliang.model.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/1/8 11:02
 **/
@Data
@NoArgsConstructor
@AllArgsConstructor
public class TreePathLevelDTO {

    /**
     * 上级用户Id
     */
    private Integer userId;

    /**
     * 代理商等级
     */
    private Integer userLevel;
    /**
     * vip等级
     */
    private Integer vipLevel;

}
