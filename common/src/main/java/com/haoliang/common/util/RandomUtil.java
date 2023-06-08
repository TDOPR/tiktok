package com.haoliang.common.util;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/11/8 16:05
 **/
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class RandomUtil extends cn.hutool.core.util.RandomUtil {

    /**
     * 生成随机数
     * @param min      最小值
     * @param max      最大值
     * @param accuracy 小数精度
     * @param divid    除的倍率
     * @return
     */
    public static BigDecimal generate(int min, int max, int accuracy, int divid) {
        double num = (Math.random() * (min - max) + max) / divid;
        BigDecimal d = new BigDecimal(num);
        d = d.setScale(accuracy, RoundingMode.HALF_UP);
        return d;
    }


    public static void main(String[] args) {
        System.out.println("first:" + randomInt(0, 4) + ":" + randomInt(0, 4));
        System.out.println("second:" + randomInt(2, 4) + ":" + randomInt(0, 1));
//        //校验工具的准确性
//        int count=0;
//        int min=0;
//        int max=0;
//
//        for (int i = 0; i < 100000; i++) {
//            BigDecimal num = generateRobotYieldRate(35, 55);
//            if (num.doubleValue() < 0.0035 || num.doubleValue() > 0.0055) {
//                count++;
//            }
//            if (num.compareTo(new BigDecimal(0.0035)) == 0) {
//                min++;
//            }
//            if (num.compareTo(new BigDecimal(0.0055)) == 0) {
//                max++;
//            }
//        }
//
//        System.out.println(count);
//        System.out.println(min);
//        System.out.println(max);

    }

}
