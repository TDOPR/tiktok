package com.haoliang.common.util;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.DecimalFormat;

/**
 * @author Dominick Li
 * @CreateTime 2020/6/19 9:52
 * @description 财报金额列校验工具
 **/

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class NumberUtil {

    @NoArgsConstructor
    @AllArgsConstructor
    public enum AmountTypeEnum {
        USD("美元格式", "$1110.11", "$0.00"),
        MONEY("千分号+小数点", "1,110.11", "##,##0.00"),
        TWO_DECIMAL("小数点无逗号", "1110.11", "0.00"),
        INT("无小数无逗号", "1110", "0"),
        MONEY_INT("千分号+无小数", "1,110", "##,##0"),
        PERCENTAGE("两位小数的百分比", "10.11%", "0.00%");

        private String desc;

        private String demo;

        private String format;

        public String getFormat() {
            return format;
        }

    }

    public static String formatBigDecimalToRateStr(BigDecimal bigDecimal) {
        return formartMoney(bigDecimal, AmountTypeEnum.PERCENTAGE);
    }

    public static String formatToRate(BigDecimal bigDecimal) {
        return formartMoney(bigDecimal, AmountTypeEnum.TWO_DECIMAL);
    }

    /**
     * 格式化 乡下取整去掉小数
     *
     * @param money
     * @return
     */
    public static String downToInt(BigDecimal money) {
        return formartMoney(money, AmountTypeEnum.INT);
    }

    /**
     * 乡下取整保留两位小数
     *
     * @param money
     * @return
     */
    public static String downToTwoBigDecimal(BigDecimal money) {
        return formartMoney(money, AmountTypeEnum.TWO_DECIMAL);
    }

    /**
     * 转换成金额类型
     */
    public static String toMoeny(BigDecimal money) {
        return formartMoney(money, AmountTypeEnum.MONEY);
    }

    /**
     * 格式化成两位小数百分比并向上取整
     */
    public static String toPercentageUP(BigDecimal money) {
        return formartMoney(money, AmountTypeEnum.PERCENTAGE, RoundingMode.UP);
    }

    /**
     * 格式化成两位小数百分比并向下取整
     */
    public static String toTwoDecimal(BigDecimal money) {
        return formartMoney(money, AmountTypeEnum.TWO_DECIMAL);
    }

    /**
     * 格式化成两位小数百分比并向下取整 十
     */
    public static String toUSD(BigDecimal money) {
        return formartMoney(money, AmountTypeEnum.USD);
    }

    /**
     * 获取十的N次方
     *
     * @param n
     * @return
     */
    public static int getTenNResult(int n) {
        int result = 1;
        for (int i = 1; i < n; i++) {
            result *= 10;
        }
        return result;
    }

    /**
     * 格式化金额
     * 默认向下取整
     */
    private static String formartMoney(BigDecimal money, AmountTypeEnum amountType) {
        if (money == null) {
            money = BigDecimal.ZERO;
        }
        DecimalFormat decimalFormat = new DecimalFormat(amountType.getFormat());
        decimalFormat.setRoundingMode(RoundingMode.FLOOR);
        return formartMoney(money, amountType, RoundingMode.FLOOR);
    }

    /**
     * 格式化金额
     *
     * @param money        金额
     * @param amountType   格式化类型
     * @param roundingMode 舍入模式
     * @return 格式化后的金额
     */
    private static String formartMoney(BigDecimal money, AmountTypeEnum amountType, RoundingMode roundingMode) {
        if (money == null) {
            money = BigDecimal.ZERO;
        }
        DecimalFormat decimalFormat = new DecimalFormat(amountType.getFormat());
        decimalFormat.setRoundingMode(roundingMode);
        return decimalFormat.format(money);
    }

    public static String toPlainString(BigDecimal decimal){
        if (decimal == null) {
            decimal = BigDecimal.ZERO;
        }
        return decimal.toPlainString();
    }

    public static void main(String[] args) {
//        BigDecimal one = new BigDecimal("1111.3599");
//        System.out.println(formartMoney(one, AmountTypeEnum.MONEY));
//        System.out.println(formartMoney(one, AmountTypeEnum.TWO_DECIMAL));
//        System.out.println(formartMoney(one, AmountTypeEnum.INT));
//        System.out.println(formartMoney(one, AmountTypeEnum.MONEY_INT));
        //System.out.println(getTenNResult(5));
        BigDecimal decimal=new BigDecimal("20.00");
        System.out.println(decimal.toPlainString());
    }

}
