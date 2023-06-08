package com.haoliang.test;

import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/11/18 17:50
 **/
public class TestBigDecimal {

    public static void main(String[] args) {
        //小数加减乘除用BigDecimal
        BigDecimal b1 = new BigDecimal("1.8");
        BigDecimal b2 = new BigDecimal("0.1");
        //小数相加
        BigDecimal b3 = b1.add(b2);
        //小数相减
        BigDecimal b4 = b1.subtract(b2);
        //小数相乘
        BigDecimal b5 = b1.multiply(b2);
        //小数相除 指定保留2位小数
        BigDecimal b6=b1.divide(b2,2, RoundingMode.HALF_UP);
        //设置保留指定位小数
        BigDecimal b7=b1.divide(b2,3, RoundingMode.HALF_UP);
        System.out.println("1.8 + 0.1 = " + b3);
        System.out.println("1.8 - 0.1 = " + b4);
        System.out.println("1.8 * 0.1 = " + b5);
        System.out.println("1.8 / 0.1 = " + b6);
        System.out.println("1.8 保留3位小数 = " + b7);
    }

}
