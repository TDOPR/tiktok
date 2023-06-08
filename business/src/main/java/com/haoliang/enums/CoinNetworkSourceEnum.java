package com.haoliang.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

/**
 * @author Dominick Li
 * @Description 数字货币网络来源
 * @CreateTime 2022/12/6 16:25
 **/
@AllArgsConstructor
@Getter
public enum CoinNetworkSourceEnum {

    BSC(1, "BSC", new BigDecimal("0.03"), new BigDecimal("1"),"BEP-20"),
    ETH(2, "ETH", new BigDecimal("0.05"), new BigDecimal("3"),"ERC-20"),
    TRC(3, "TRX", new BigDecimal("0.03"), new BigDecimal("1"),"TRC-20");

    private int coinId;

    private String name;

    /**
     * 提现手续费
     */
    private BigDecimal free;

    /**
     * 提现最低手续费k扣除要求
     */
    private BigDecimal minFreeAmount;

    /**
     * 充值网络名称
     */
    private String networdName;

    /**
     * 根据币种名称获取币种Id
     */
    public static int getCoinIdByName(String name) {
        for (CoinNetworkSourceEnum coinNetworkSourceEnum : values()) {
            if (coinNetworkSourceEnum.getName().equals(name)) {
                return coinNetworkSourceEnum.getCoinId();
            }
        }
        return 1;
    }

    /**
     * 根据充值网络名称获取对应的手续费
     *
     * @param name
     * @return
     */
    public static BigDecimal getFreeByName(String name) {
        for (CoinNetworkSourceEnum coinNetworkSourceEnum : values()) {
            if (coinNetworkSourceEnum.getName().equals(name)) {
                return coinNetworkSourceEnum.getFree();
            }
        }
        return BigDecimal.ZERO;
    }

    public static CoinNetworkSourceEnum networdNameOf(String networdName) {
        for (CoinNetworkSourceEnum coinNetworkSourceEnum : values()) {
            if (coinNetworkSourceEnum.getNetwordName().equals(networdName)) {
                return coinNetworkSourceEnum;
            }
        }
        return null;
    }

    public static CoinNetworkSourceEnum nameOf(String name) {
        for (CoinNetworkSourceEnum coinNetworkSourceEnum : values()) {
            if (coinNetworkSourceEnum.getName().equals(name)) {
                return coinNetworkSourceEnum;
            }
        }
        return null;
    }

    public static List<String> getSelectList() {
        List<String> list = new ArrayList<>();
        for (CoinNetworkSourceEnum coinNetworkSourceEnum : values()) {
            list.add(coinNetworkSourceEnum.getNetwordName());
        }
        return list;
    }
}
