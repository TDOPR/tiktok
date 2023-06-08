package com.haoliang.model.usd;

import com.baomidou.mybatisplus.annotation.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/12/6 16:46
 **/
@Data
@Builder
@TableName("evm_user_wallet")
@NoArgsConstructor
@AllArgsConstructor
public class EvmUserWallet {

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
     * 网络名称
     */
    @TableField(value = "coin_type")
    private String coinType;

    /**
     * 钱包地址
     */
    private String address;

    /**
     * 钱包地址
     */
    @TableField(value = "lower_address")
    private String lowerAddress;

    /**
     * keystore
     */
    private String keystore;

    /**
     * 秘钥
     */
    private String password;

    /**
     * 是否可用：E可用，D不可用
     */
    private String valid;

    /**
     * 创建时间
     */
    @TableField(value = "create_time", fill = FieldFill.INSERT)
    private LocalDateTime createTime;

    /**
     * 修改时间
     */
    @TableField(value = "update_time", fill = FieldFill.INSERT_UPDATE)
    private LocalDateTime lastmodifiedTime;

}
