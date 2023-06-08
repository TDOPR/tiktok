package com.haoliang.common.model;

import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.haoliang.common.base.BaseModelCIDNoModifyTime;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author Dominick Li
 * @description 登录日志表
 **/
@Data
@TableName("sys_loginlog" )
@NoArgsConstructor
public class SysLoginLog extends BaseModelCIDNoModifyTime {


    /**
     * 用户名
     */
    private String username;

    /**
     * 登录Ip地址
     */
    private String ipAddr;

    /**
     * 用户类型 1=系统用户 2=app用户
     */
    @JsonIgnore
    private Integer userType;

    public SysLoginLog(String username, String ipAddr,Integer userType){
        this.ipAddr=ipAddr;
        this.username=username;
        this.userType=userType;
    }
}
