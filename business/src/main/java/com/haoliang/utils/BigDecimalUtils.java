package com.haoliang.utils;

import com.haoliang.constant.TiktokConfig;

import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/4/13 9:39
 **/
public class BigDecimalUtils {

    /**
     * 保留两位小数向下取整
     * @param A
     * @param B
     * @return
     */
    public static BigDecimal divideSaveTwoDecimal(BigDecimal A, BigDecimal B) {
        return A.divide(B, TiktokConfig.NUMBER_OF_DIGITS, RoundingMode.FLOOR);
    }

}
