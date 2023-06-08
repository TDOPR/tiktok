package com.haoliang.model.vo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author Dominick Li
 * @Description 社区信息
 * @CreateTime 2022/11/15 17:25
 **/
@Data
@Builder
public class MyCommunityVO {
    /**
     * 直推有效的用户数
     */
    private Integer validUser;

    /**
     * 有效 + 无效
     */
    private Integer allUser;

    /**
     * 社区用户有效用户
     */
    private Integer meshUser;

    /**
     * 星级用户 在社区用户的基础上有等级
     */
    private Integer starUser;

}
