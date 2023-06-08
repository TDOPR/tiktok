package com.haoliang.model.vo;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * @author Dominick Li
 * @Description 审核列表数据
 * @CreateTime 2022/11/11 16:42
 **/
@Data
@NoArgsConstructor
@AllArgsConstructor
public class WalletsUsdWithdrawVO {

    /**
     * 任务Id
     */
    @JsonFormat(shape = JsonFormat.Shape.STRING)
    private Long id;

    /**
     * 单号
     */
    private String txid;

    /**
     * 用户Id
     */
    private Integer userId;

    /**
     * 用户昵称
     */
    private String nickName;

    /**
     * 类型
     */
    @JsonIgnore
    private Integer coinId;

    /**
     * 提现渠道
     */
    private String channel;

    /**
     * 提现网络链路
     */
    private String coinType;

    /**
     * 提现金额(包含手续费)
     */
    private BigDecimal amount;

    /**
     * 费率
     */
    private String freeRate;

    /**
     * 创建时间
     */
    private LocalDateTime createTime;

    /**
     * 审核时间
     */
    private LocalDateTime auditTime;

    /**
     * 审核状态
     */
    private Integer status;

}
