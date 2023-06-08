package com.haoliang.test;

import cn.hutool.core.date.TimeInterval;
import cn.hutool.core.thread.ThreadUtil;
import lombok.extern.slf4j.Slf4j;

/**
 * @author Dominick Li
 * @Description 统计每个接口耗时情况
 * @CreateTime 2022/12/6 11:01
 **/
@Slf4j
public class TimeIntervalTest {
    public static void main(String[] args) {
        TimeInterval timeInterval = new TimeInterval();
        timeInterval.start();
        ThreadUtil.sleep(800);
        long select = timeInterval.intervalRestart();
        ThreadUtil.sleep(1800);
        long compute = timeInterval.intervalRestart();
        ThreadUtil.sleep(500);
        long save = timeInterval.interval();
        log.info("------------- select times:{} ms ,compute times:{} ms,save times:{} ms,total:{} ms--------------", select, compute, save, (select + compute + save));
    }
}
