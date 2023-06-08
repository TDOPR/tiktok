package com.haoliang.controller.monitor.server;


import com.haoliang.common.util.ArithUtil;
import com.haoliang.common.util.DateUtil;

import java.lang.management.ManagementFactory;
import java.util.Date;

/**
 * JVM相关信息
 *
 * @author ruoyi
 */
public class Jvm {
    /**
     * 当前JVM占用的内存总数(M)
     */
    private double total;

    /**
     * JVM最大可用内存总数(M)
     */
    private double max;

    /**
     * JVM空闲内存(M)
     */
    private double free;

    /**
     * JDK版本
     */
    private String version;

    /**
     * JDK路径
     */
    private String home;

    public double getTotal() {
        return ArithUtil.div(total, (1024 * 1024), 2);
    }

    public void setTotal(double total) {
        this.total = total;
    }

    public double getMax() {
        return ArithUtil.div(max, (1024 * 1024), 2);
    }

    public void setMax(double max) {
        this.max = max;
    }

    public double getFree() {
        return ArithUtil.div(free, (1024 * 1024), 2);
    }

    public void setFree(double free) {
        this.free = free;
    }

    public double getUsed() {
        return ArithUtil.div(total - free, (1024 * 1024), 2);
    }

    public double getUsage() {
        return ArithUtil.mul(ArithUtil.div(total - free, total, 4), 100);
    }

    /**
     * 获取JDK名称
     */
    public String getName() {
        return ManagementFactory.getRuntimeMXBean().getVmName();
    }

    public String getVersion() {
        return version;
    }

    public void setVersion(String version) {
        this.version = version;
    }

    public String getHome() {
        return home;
    }

    public void setHome(String home) {
        this.home = home;
    }

    /**
     * JDK启动时间
     */
    public String getStartTime() {
        return DateUtil.formatDate(DateUtil.getServerStartDate(), DateUtil.SIMPLE_DATETIME_FORMAT);
    }

    /**
     * JDK运行时间
     */
    public String getRunTime() {
        return DateUtil.getDatePoor(new Date(), DateUtil.getServerStartDate());
    }

    /**
     * 运行参数
     */
    public String getInputArgs() {
        return ManagementFactory.getRuntimeMXBean().getInputArguments().toString();
    }
}
