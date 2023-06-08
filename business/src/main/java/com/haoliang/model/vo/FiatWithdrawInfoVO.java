package com.haoliang.model.vo;

import com.haoliang.pay.enums.FiatTypeEnum;
import com.haoliang.model.dto.BankCodeDTO;
import lombok.Data;
import org.springframework.beans.BeanUtils;

import java.util.List;


/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/4/15 10:07
 **/
@Data
public class FiatWithdrawInfoVO {

    public FiatWithdrawInfoVO(FiatTypeEnum fiatTypeEnum) {
        BeanUtils.copyProperties(fiatTypeEnum, this);
        this.feeRate=fiatTypeEnum.getProxyFeeRate().toPlainString();
        this.min=fiatTypeEnum.getProxyMin();
        this.max=fiatTypeEnum.getProxyMax();

    }

    /**
     * 法币类型  1=印尼 2=越南 3=泰国
     */
    private Integer type;

    /**
     * 提现费率
     */
    private String feeRate;

    /**
     * 单笔最小代收金额 10000
     */
    private Integer min;

    /**
     * 单笔最大代收金额
     */
    private Integer max;

    /**
     * 基础手续费   手续费=基础手续费+(充值金额 * 费率)  例如充值10000 则手续费=6500+10000*0.05=7000
     */
    private Integer baseFee;

    /**
     * 币种名称
     */
    private String coinName;

    /**
     * 币种单位
     */
    private String coinUnit;

    /**
     * USD 转 法币的汇率
     */
    private String exchangeRate;

    /**
     * 提现银行列表
     */
    private List<BankCodeDTO> bankList;

}
