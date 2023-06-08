package com.haoliang.model.vo;

import com.haoliang.model.AppUsers;
import com.haoliang.model.SysUser;
import lombok.Data;


@Data
public class TokenVO {

    /**
     * 身份证认证token信息
     */
    private String token;

    /**
     * 用户昵称
     */
    private String nickName;

    /**
     * 手机号
     */
    private String mobile;

    /**
     * 邮箱号
     */
    private String email;

    public TokenVO(String token, SysUser sysUser) {
        this.token = token;
        this.email = sysUser.getEmail();
        this.nickName = sysUser.getName();
        this.mobile = sysUser.getMobile();
    }

    public TokenVO(String token, AppUsers appUsers) {
        this.token = token;
        this.email = appUsers.getEmail();
        this.nickName = appUsers.getNickName();
        this.mobile = appUsers.getMobile();
    }

}
