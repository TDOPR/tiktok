package com.haoliang.model.vo;

import lombok.Data;

import java.time.LocalDateTime;

/**
 * @author Dominick Li
 * @Description 客户详细信息
 * @CreateTime 2022/12/9 9:32
 **/
@Data
public class MyDetailVO {
    /**
     * 用户Id
     */
    private Integer userId;
    /**
     * 头像
     */
    private String headImage;
    /**
     * 用户昵称
     */
    private String nickName;
    /**
     * 用户邀请码
     */
    private String inviteCode;
    /**
     * 社区等级
     */
    private Integer level;
    /**
     * 个人等级
     */
    private Integer vipLevel;
    /**
     * 是否有效
     */
    private boolean valid;
    /**
     * 手机号所属国际地区编码
     */
    private String countryCode = "";
    /**
     * 手机号
     */
    private String mobile;
    /**
     * 官网地址
     */
    private String officialWebsite;
    /**
     * 注册时间
     */
    private LocalDateTime createTime;

}
