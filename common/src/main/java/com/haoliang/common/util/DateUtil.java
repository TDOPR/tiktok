package com.haoliang.common.util;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import java.lang.management.ManagementFactory;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.Date;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class DateUtil extends cn.hutool.core.date.DateUtil {

    public static final String EXCEL_DATE_FORMAT = "yyyy年MM月dd日";
    public static final String EXCEL_DATETIME_FORMAT = "yyyy年MM月dd日 HH时mm分";
    public static final String SIMPLE_DATETIME_FORMAT = "yyyy-MM-dd HH:mm:ss";
    public static final String DETAIL_FORMAT_NO_UNIT = "yyyyMMddHHmmssSSS";

    public static final String SIMPLE_DATETIME_NUMBER = "yyyyMMddHHmmss";

    public static String formatDate(Date date, String format) {
        SimpleDateFormat myFormat = new SimpleDateFormat(format);
        return myFormat.format(date);
    }

    /**
     * 把LocalDate转成日期 K线
     */
    public static Date getNowDate() {
        return parse(LocalDate.now().toString());
    }


    /***
     * 生成详细日期，不要单位。格式YYMMDDhhmmss
     * @return
     */
    public static String getDetailTimeIgnoreUnit() {
        return LocalDateTime.now().format(DateTimeFormatter.ofPattern(DETAIL_FORMAT_NO_UNIT));
    }

    /**
     * 获取服务器启动时间
     */
    public static Date getServerStartDate() {
        long time = ManagementFactory.getRuntimeMXBean().getStartTime();
        return new Date(time);
    }

    public static String getDatePoor(Date endDate, Date nowDate) {
        long nd = 1000 * 24 * 60 * 60;
        long nh = 1000 * 60 * 60;
        long nm = 1000 * 60;
        // long ns = 1000;
        // 获得两个时间的毫秒时间差异
        long diff = endDate.getTime() - nowDate.getTime();
        // 计算差多少天
        long day = diff / nd;
        // 计算差多少小时
        long hour = diff % nd / nh;
        // 计算差多少分钟
        long min = diff % nd % nh / nm;
        // 计算差多少秒//输出结果
        // long sec = diff % nd % nh % nm / ns;
        return day + "天" + hour + "小时" + min + "分钟";
    }

    /**
     * 获取昨天的时间
     *
     * @return
     */
    public static LocalDate getYesterdayLocalDate() {
        return LocalDate.now().minusDays(1);
    }


    public static String getNowDateTimeNumber() {
        return formatDate(new Date(), SIMPLE_DATETIME_NUMBER);
    }

    /**
     * 计算两个日期之前相差的月数
     *
     * @param startLocalDate 开始日期
     * @param endLocalDate   结束日期
     * @return
     */
    public static int betweenMonths(LocalDate startLocalDate, LocalDate endLocalDate) {
        LocalDate start = LocalDate.of(startLocalDate.getYear(), startLocalDate.getMonth(), 1);
        LocalDate end = LocalDate.of(endLocalDate.getYear(), endLocalDate.getMonth(), 1);
        return (int) ChronoUnit.MONTHS.between(start, end);
    }
    /**
     * 计算两个日期之前相差的天数
     * @param startLocalDateTime 开始日期
     * @param endLocalDateTime   结束日期
     * @return
     */
    public static int betweenDays(LocalDateTime startLocalDateTime, LocalDateTime endLocalDateTime) {
        return (int) ChronoUnit.DAYS.between(startLocalDateTime, endLocalDateTime);
    }

    /**
     * 检查日期是否失效了
     */
    public static boolean checkIsInvalid(Date dealine) {
        Date nowDate = new Date();
        if (dealine.getTime() < nowDate.getTime()) {
            return true;
        }
        return false;
    }

    /**
     * 获取时间戳
     */
    public static long getTime(LocalDateTime localDateTime) {
        return localDateTime.toInstant(ZoneOffset.of("+8")).toEpochMilli();
    }

    public static void main(String[] args) {
        //当前当前日期时间字符串  格式yyyy-MM-dd hh:mm:ss
        System.out.println(now());
        //今天日期 yyyy-MM-dd
        System.out.println(today());

        //格式化日期字符串为日期类型 支持下面几种
        //yyyy-MM-dd hh:mm:ss
        //yyyy-MM-dd
        //hh:mm:ss
        //yyyy-MM-dd hh:mm
        //System.out.println(parse(LocalDate.now().toString()));
        LocalDateTime start=LocalDateTime.of(2023,5,15,11,0);
        System.out.println(betweenDays(start,LocalDateTime.now()));
    }

}
