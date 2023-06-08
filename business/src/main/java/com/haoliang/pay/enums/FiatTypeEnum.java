package com.haoliang.pay.enums;

import com.haoliang.common.model.ThreadLocalManager;
import com.haoliang.common.util.MessageUtil;
import com.haoliang.model.dto.BankCodeDTO;
import com.haoliang.pay.id.enums.IdBankCodeEnum;
import com.haoliang.pay.th.enums.ThBankCodeEnum;
import com.haoliang.pay.vn.enums.VnBankCodeEnum;
import lombok.AllArgsConstructor;
import lombok.Getter;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

/**
 * @author Dominick Li
 * @Description 法币支付相关参数
 * @CreateTime 2022/11/1 16:01
 **/
@Getter
@AllArgsConstructor
public enum FiatTypeEnum {

    //印尼代付最低最高限制是扣除手续费后的
    ID(1, "印度尼西亚", "coin.id", "IDR", new BigDecimal("0.05"), new BigDecimal("0.02"), 6500, 3000, 100000000, 16837,
            51027000, 5, IdBankCodeEnum.getBankCodeList(), true),
    VN(2, "越南", "coin.vn", "VND", new BigDecimal("0.06"), new BigDecimal("0.03"), 0, 100000, 80000000,
            100000, 80000000, 5, VnBankCodeEnum.getBankCodeList(), true),
    TH(3, "泰国", "coin.th", "THB",new BigDecimal("0.08"),new BigDecimal("0.015"),0,
            100,200000,100,200000,5, ThBankCodeEnum.getBankCodeList(),true
    );

    FiatTypeEnum(int type, String name, String coinName, String coinUnit) {
        this.type = type;
        this.name = name;
        this.coinName = coinName;
        this.coinUnit = coinUnit;
        this.opening = false;
    }

    private Integer type;

    private String name;

    private String coinName;

    private String coinUnit;

    /**
     * 费率
     */
    private BigDecimal feeRate;

    /**
     * 代付费率
     */
    private BigDecimal proxyFeeRate;

    /**
     * 基础费用 代收和代付一样
     */
    private Integer baseFee;

    /**
     * 最低金额
     */
    private Integer min;

    /**
     * 最大金额
     */
    private Integer max;

    /**
     * 代付最低金额
     */
    private Integer proxyMin;

    /**
     * 代付最低金额
     */
    private Integer proxyMax;

    /**
     * 失效时间 单位(分钟)
     */
    private Integer expiryPeriod;

    /**
     * 提现支持的银行编码
     */
    private List<BankCodeDTO> bankList;

    /**
     * 是否开放
     */
    private boolean opening;

    public static FiatTypeEnum typeOf(Integer type) {
        for (FiatTypeEnum fiatTypeEnum : values()) {
            if (fiatTypeEnum.type.equals(type)) {
                return fiatTypeEnum;
            }
        }
        return null;
    }

    public String getCoinName() {
        return MessageUtil.get(coinName, ThreadLocalManager.getLanguage());
    }

    /**
     * 获取所有的货币名称
     *
     * @return
     */
    public static List<String> getCoinUtilList() {
        List<String> coinUtilList = new ArrayList<>();
        for (FiatTypeEnum fiatTypeEnum : values()) {
            coinUtilList.add(fiatTypeEnum.getCoinUnit());
        }
        return coinUtilList;
    }
}
