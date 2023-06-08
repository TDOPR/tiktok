package com.haoliang.model.vo;

import com.haoliang.enums.CoinNetworkSourceEnum;
import lombok.Data;

import java.math.BigDecimal;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/12/6 17:20
 **/
@Data
public class BlockAddressVo {

    /**
     * 网络名称
     */
    private String networdName;

    /**
     * 地址
     */
    private String address;

    /**
     * 提现手续费率
     */
    private BigDecimal free;

    /**
     * 最低手续费限制
     */
    private BigDecimal minFreeAmount;

    /**
     * 提现最低提现金额
     */
    private BigDecimal minAmount;

    public BlockAddressVo(String address, CoinNetworkSourceEnum coinNetworkSourceEnum) {
        this.networdName = coinNetworkSourceEnum.getNetwordName();
        this.address = address == null ? "" : address;
        this.free = coinNetworkSourceEnum.getFree();
        this.minFreeAmount = coinNetworkSourceEnum.getMinFreeAmount();
        this.minAmount=this.minFreeAmount.add(new BigDecimal(1));
    }

}
