package com.haoliang.model;

import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.haoliang.common.base.BaseModelCID;
import lombok.Data;


/**
 * @author Dominick Li
 * @description 系统用户表
 **/
@Data
@TableName("sys_user")
public class SysUser extends BaseModelCID {

    /**
     * 用户名
     *
     * @required
     */
    private String username;

    /**
     * 密码
     *
     * @required
     */
    private String password;

    /**
     * 加密用得盐
     *
     * @ignore
     */
    @JsonIgnore
    private String salt;

    /**
     * 谷歌验证器的秘钥
     */
    private String googleSecret;

    /**
     * 用户昵称
     *
     * @required
     */
    private String name;

    /**
     * 邮箱号
     */
    private String email;

    /**
     * 手机号
     */
    private String mobile;

    /**
     * 用户状态 1=正常 0=禁用
     *
     * @required
     */
    private Integer enabled = 1;

    /**
     * 角色编号
     *
     * @required
     */
    private Integer roleId;

    /**
     * 机构编号
     *
     * @ignore
     */
    @JsonIgnore
    private Integer channelId;

    /**
     * 逻辑删除字段 1=删除 0=未删除
     *
     * @ignore
     */
    @JsonIgnore
    private Integer deleted = 0;


}

