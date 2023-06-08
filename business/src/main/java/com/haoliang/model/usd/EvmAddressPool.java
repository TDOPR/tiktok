package com.haoliang.model.usd;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

/**
 * 区块链地址池表
 */
@Data
@TableName("evm_token_address_pool")
public class EvmAddressPool {

    /**
     * 唯一标识
     */
    @TableId(type = IdType.ASSIGN_UUID)
    private Long id;

    /**
     * 货币Id
     */
    @TableField(value = "coin_id")
    private Integer coinId;

    /**
     * 区块链地址
     */
    private String address;

    /**
     * keystore
     */
    private String keystore;

    /**
     * 秘钥
     */
    private String pwd;

    /**
     *地址网络类型
     */
    @TableField(value = "coin_type")
    private String coinType;

}
