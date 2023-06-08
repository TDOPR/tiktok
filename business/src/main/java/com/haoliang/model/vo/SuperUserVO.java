package com.haoliang.model.vo;

import lombok.Data;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/11/10 12:08
 **/
@Data
public class SuperUserVO {

    /**
     * 邮箱账号
     */
    private String email;

    /**
     * 用户昵称
     */
    private String nickName;

    /**
     * 用户等级
     */
    private Integer level;

    /**
     * 上级团队数量
     */
    private Integer superTeamSum;
}
