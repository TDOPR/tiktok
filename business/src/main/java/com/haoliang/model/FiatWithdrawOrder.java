package com.haoliang.model;

import com.baomidou.mybatisplus.annotation.TableName;
import com.haoliang.common.base.BaseModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/4/15 11:12
 **/
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@TableName("fiat_withdraw_order")
public class FiatWithdrawOrder extends BaseModel {

    /**
     * 用户Id
     */
    private Integer userId;

    /**
     * 法币类型  1=印尼 2=越南 3=泰国
     */
    private Integer fiatType;

    /**
     * 平台订单
     */
    private String platformOrderNo;

    /**
     * 提现状态 0=待处理 1=已受理 2=提现成功 4=提现失败,5=银行代付中
     */
    private Integer status;

    /**
     * 银行名称
     */
    private String bankCode;

    /**
     * 银行卡号
     */
    private String bankNumber;

    /**
     * 持卡人姓名
     */
    private String name;

    /**
     * 提现的法币
     */
    private Integer amount;

    /**
     * 手续费 法币
     */
    private BigDecimal fee;

    /**
     * 实际提现到账 法币
     */
    private Integer actualAmount;

    /**
     * 提现的usd金额
     */
    private Integer usdAmount;

    /**
     * usd换法币的汇率
     */
    private BigDecimal exchangeRate;

}
