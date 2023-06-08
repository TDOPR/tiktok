//package com.haoliang.pay.id.enums;
//
//import lombok.AllArgsConstructor;
//import lombok.Getter;
//
//import java.math.BigDecimal;
//
///**
// * 代收支持的银行
// */
//@AllArgsConstructor
//@Getter
//public enum IdPayChannelEnum {
//
//    MANDIRI("MANDIRI", new BigDecimal("0.05"), new BigDecimal("6500"), new BigDecimal("10000"), new BigDecimal("100000000")),
//    BNI("BNI", new BigDecimal("0.05"), new BigDecimal("6500"), new BigDecimal("10000"), new BigDecimal("100000000")),
//    CIMB("CIMB", new BigDecimal("0.05"), new BigDecimal("6500"), new BigDecimal("10000"), new BigDecimal("100000000")),
//    BRI("BRI", new BigDecimal("0.05"), new BigDecimal("6500"), new BigDecimal("10000"), new BigDecimal("100000000")),
//    PERMATA("PERMATA", new BigDecimal("0.05"), new BigDecimal("6500"), new BigDecimal("10000"), new BigDecimal("100000000"));
//
//    /**
//     * 银行名称
//     */
//    private String name;
//
//    /**
//     * 费率
//     */
//    private BigDecimal free;
//
//    /**
//     * 单笔基础费用
//     */
//    private BigDecimal minFree;
//
//    /**
//     * 单笔最小代收金额
//     */
//    private BigDecimal minAmount;
//
//    /**
//     * 单笔最大 代收金额
//     */
//    private BigDecimal maxAmount;
//
//}
