package com.haoliang.common.util;

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;

/**
 * @author Dominick Li
 * @CreateTime 2021/5/18 14:07
 * @description 计算两个日期间隔时间
 **/
public class DateIntervalUtils {

    public static void main(String[] args)  throws Exception{
        SimpleDateFormat simpleDateFormat=new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        System.out.println(getIntervalTimeStr(simpleDateFormat.parse("2021-09-26 10:10:03"),new Date())); //秒
    }


    public static String getIntervalTimeStr(Date start, Date end) {
        if (start == null) {
            return "";
        }
        return new DateIntervalUtils(start, end).toString();
    }

    private int year, month, day, hour, minute, second;

    private Date start, end;

    private Calendar sCal = Calendar.getInstance();

    private Calendar eCal = Calendar.getInstance();

    public DateIntervalUtils(Date start, Date end) {
        this.start = start;
        this.end = end;
        sCal.setTime(start);
        eCal.setTime(end);
        year = eCal.get(Calendar.YEAR) - sCal.get(Calendar.YEAR);
        month = eCal.get(Calendar.MONTH) - sCal.get(Calendar.MONTH);
        day = eCal.get(Calendar.DAY_OF_MONTH) - sCal.get(Calendar.DAY_OF_MONTH);
        hour = eCal.get(Calendar.HOUR_OF_DAY) - sCal.get(Calendar.HOUR_OF_DAY);
        minute = eCal.get(Calendar.MINUTE) - sCal.get(Calendar.MINUTE);
        second = eCal.get(Calendar.SECOND) - sCal.get(Calendar.SECOND);
        this.calcInterval();
    }


    public DateIntervalUtils calcInterval() {
        if (month < 0) {
            descYear();
        }
        if (day < 0) {
            descMonth();
        }
        if (hour < 0) {
            descDay();
        }
        if (minute < 0) {
            descHour();
        }
        if (second < 0) {
            descMintue();
        }
        return this;
    }

    private void descYear() {
        year--;
        month += 12;
    }

    private void descMonth() {
        if (--month < 0) {
            descYear();
        }
        day += calDaysOfLastMonth();
    }

    private void descDay() {
        if (--day < 0) {
            descMonth();
        }
        hour += 24;
    }

    private void descHour() {
        if (--hour < 0) {
            descDay();
        }
        minute += 60;
    }

    private void descMintue() {
        if (--minute < 0) {
            descHour();
        }
        second += 60;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        if (year > 0) {
            sb.append(year).append("年");
        }
        if (month > 0) {
            sb.append(month).append("月");
        }
        if (day > 0) {
            sb.append(day).append("天");
        }
        if (hour > 0) {
            sb.append(hour).append("小时");
        }
        if (minute > 0) {
            sb.append(minute).append("分钟");
        }
        if(second>0){
            sb.append(second).append("秒");
        }
        return sb.toString();
    }

    private int calDaysOfLastMonth() {
        Calendar calendar = Calendar.getInstance();
        calendar.set(Calendar.YEAR, eCal.get(Calendar.YEAR));
        calendar.set(Calendar.MONTH, eCal.get(Calendar.MONTH));
        calendar.add(Calendar.MONTH, -1);
        return calendar.getActualMaximum(Calendar.DATE);
    }

}
