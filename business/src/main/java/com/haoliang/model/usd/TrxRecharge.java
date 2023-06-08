package com.haoliang.model.usd;

import com.baomidou.mybatisplus.annotation.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * 充值记录
 */
@Data
@Builder
@TableName("trx_token_recharge")
@NoArgsConstructor
@AllArgsConstructor
public class TrxRecharge {

    /**
     * 唯一标识
     */
    @TableId(type = IdType.AUTO)
    private Long id;

    /**
     * 用户ID
     */
    private Integer uid;

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
     * 状态：0-待入账(法币充值为下单成功待支付)；1-充值成功，2到账失败，3到账成功
     */
    private Integer status;

    /**
     * 交易订单id
     */
    private String txid;

    /**
     * 提现金额(包含手续费)
     */
    @TableField(value = "num")
    private BigDecimal amount;

    /**
     * 实际充值金额
     */
    @TableField(value = "mum")
    private BigDecimal actualAmount;

    /**
     * 区块链高度
     */
    @TableField(value = "block_number")
    private Integer blockNum;

    /**
     * 创建时间
     */
    @TableField(value = "created", fill = FieldFill.INSERT)
    private LocalDateTime createTime;

    /**
     * 修改时间
     */
    @TableField(value = "last_update_time", fill = FieldFill.INSERT_UPDATE)
    private LocalDateTime lastmodifiedTime;

    /**
     * 备注
     */
    private String remark;

}
