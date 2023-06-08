package com.haoliang.model.usd;

import com.baomidou.mybatisplus.annotation.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * 用户提现记录表
 */
@Data
@Builder
@TableName("evm_token_withdraw")
@NoArgsConstructor
@AllArgsConstructor
public class EvmWithdraw {

    /**
     * 唯一标识
     */
    @TableId(type = IdType.AUTO)
    private Long id;

    /**
     * 用户id
     */
    @TableField(value = "uid")
    private Integer userId;

    /**
     * 币种Id
     */
    @TableField(value = "coin_id")
    private Integer coinId;

    /**
     * 币种名称
     */
    @TableField(value = "coin_name")
    private String coinName;

    /**
     * 币种类型 (充值网络类型)
     */
    @TableField(value = "coin_type")
    private String coinUnit;

    /**
     * 钱包地址
     */
    private String address;

    /**
     * 交易id
     */
    private String txid;


    /**
     * 提现金额(包含手续费)
     */
    @TableField(value = "num")
    private BigDecimal amount;

    /**
     * 手续费
     */
    private BigDecimal fee;

    /**
     * 实际提现金额
     */
    @TableField(value = "mum")
    private BigDecimal actualAmount;

    /**
     * 链上手续费花费
     */
    @TableField(value = "chain_fee")
    private BigDecimal chainFee;

    /**
     * 区块链高度
     */
    @TableField(value = "block_num")
    private Integer blockNum;

    /**
     * 备注
     */
    private String remark;

    /**
     * 当前审核级别
     */
    private Integer step;

    /**
     * 状态：0-审核中;1-成功;2-拒绝;3-撤销;4-审核通过;5-打币中;6;-待区块确认;7-区块打币失败 8=已结算
     */
    private Integer status;

    /**
     * 审核时间
     */
    @TableField(value = "audit_time")
    private LocalDateTime auditTime;

    /**
     * 创建时间
     */
    @TableField(value = "create_time", fill = FieldFill.INSERT)
    private LocalDateTime createTime;

    /**
     * 修改时间
     */
    @TableField(value = "last_update_time", fill = FieldFill.INSERT_UPDATE)
    private LocalDateTime lastmodifiedTime;

    /**
     * 输出Id
     */
    @TableField(value = "out_id")
    private Long outId;

    /**
     * 日志id
     */
    @TableField(value = "monet_log_id")
    private Integer monetLogId;

}
