package com.haoliang.model;

import com.baomidou.mybatisplus.annotation.TableName;
import com.haoliang.common.base.BaseModelCID;
import lombok.Data;

import javax.validation.constraints.Email;
import javax.validation.constraints.NotEmpty;

/**
 * @author Dominick Li
 * @Description 业务用户表  客户
 * @CreateTime 2022/11/1 10:30
 **/
@Data
@TableName("app_users")
public class AppUsers extends BaseModelCID {

    /**
     * 邮箱账号
     */
    @NotEmpty
    private String email;

    /**
     * 密码
     */
    @NotEmpty
    private String password;

    /**
     * 手机号
     */
    private String mobile;

    /**
     * 加密用得盐
     *
     * @ignore
     */
    private String salt;

    /**
     * 头像
     */
    private String headImage;

    /**
     * 用户名
     */
    private String nickName;

    /**
     * 用户状态 1=正常 0=禁用
     */
    private Integer enabled;

    /**
     * 登录次数
     */
    private Integer loginCount;

    /**
     * 邀请码
     */
    private String inviteCode;

    /**
     * 邀请人Id
     */
    private Integer inviteId;

    /**
     * 代理商等级
     */
    private Integer level;

    /**
     * 个人等级
     */
    private Integer vipLevel;

    /**
     * 是否有效用户(消费过 未消费) 1=有效  0=无效
     */
    private Integer valid;

    /**
     * 新手教程 1=新手 0=非新手
     */
    private Integer greenhorn;

    /**
     * 是否为代理商角色 1=是 (可以登录后台)
     */
    private Integer proxyRole;

}
