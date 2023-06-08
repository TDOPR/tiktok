package com.haoliang.model.vo;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.haoliang.common.util.NumberUtil;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/11/11 18:51
 **/
@Data
public class AppUsersVO {

    /**
     * 用户Id
     */
    private Integer id;

    /**
     * 上级Id
     */
    private Integer parentId;

    /**
     * 邮箱账号
     */
    private String email;

    /**
     * 用户昵称
     */
    private String nickName;

    /**
     * 用户状态 1=正常 0=禁用
     */
    private Integer enabled;

    /**
     * 用户等级
     */
    private Integer level;

    /**
     * Vip等级
     */
    private Integer vipLevel;

    /**
     * 手机号
     */
    private String mobile;

    /**
     * 头像
     */
    private String headImage;

    /**
     * 邀请码
     */
    private String inviteCode;

    /**
     * 是否有效用户(消费过 未消费) 1=有效  0=无效
     */
    private Integer valid;

    /**
     * 是否代理商
     */
    private Integer proxyRole;

    @JsonIgnore
    private BigDecimal ttt;

    @JsonIgnore
    private BigDecimal usd;

    @JsonIgnore
    private BigDecimal recharge;

    @JsonIgnore
    private BigDecimal withdraw;

    @JsonIgnore
    private BigDecimal consume;

    /**
     * 下级数量
     */
    private Integer subordinateNumber;

    /**
     * 用户创建时间
     */
    private LocalDateTime createTime;

    /**
     * 钱包信息
     */
    private AppUserWalletVO wallet;

    /**
     * 奖励信息
     */
    private AppUserRewardVO reward;

    /**
     *团队关系
     */
    private MyCommunityAdminVO community;

}
