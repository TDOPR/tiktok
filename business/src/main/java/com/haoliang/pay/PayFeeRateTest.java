package com.haoliang.pay;

import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 * @author Dominick Li
 * @Description 充值费率计算
 * @CreateTime 2023/4/19 14:57
 **/
public class PayFeeRateTest {

    public static void main(String[] args) {
        //充值的USD
        BigDecimal usdNumber = new BigDecimal(1);
        //usd兑法币的汇率
        BigDecimal exchange = new BigDecimal("14843.05");
        //法币金额
        BigDecimal amount = usdNumber.multiply(exchange);
        //基础服务费
        BigDecimal baseFee = new BigDecimal("6500");
        //费率
        BigDecimal feeRate = new BigDecimal("0.05");
        //应付法币 = (amount+baseFee)/(1-feeRate) 向上取整
        BigDecimal pay = amount.add(baseFee).divide(new BigDecimal(1).subtract(feeRate), 0, RoundingMode.UP);
        //手续费=   (应付法币pay*feeRate)+baseFee 向上取整
        BigDecimal fee = pay.multiply(feeRate).add(baseFee).setScale(0,RoundingMode.UP);
        System.out.println("手续费=" + fee.toPlainString());
        System.out.println("应付" + pay.toPlainString());
    }
}
