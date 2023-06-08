package com.haoliang.model.vo;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * @author Dominick Li
 * @Description 审核列表数据
 * @CreateTime 2022/11/11 16:42
 **/
@Data
public class EvmRechargeVO {

    /**
     * 邮箱号
     */
    private String  email;

    /**
     * 钱包地址
     */
    private String address;

    /**
     * 币种Id
     */
    @JsonIgnore
    private Integer coinId;

    /**
     * 币种名称
     */
    private String coinName;

    /**
     * 实际充值金额
     */
    private BigDecimal mum;

    /**
     * 订单状态
     */
    @JsonIgnore
    private Integer status;

    /**
     * 订单状态名称
     */
    private String statusName;

    /**
     * 创建时间
     */
    private LocalDateTime createTime;
}
