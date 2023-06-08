package com.haoliang.controller;

import com.alibaba.fastjson.JSONObject;
import com.haoliang.common.annotation.RepeatSubmit;
import com.haoliang.common.model.JsonResult;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.client.RestTemplate;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * @Description
 * @Author Dominick Li
 * @CreateTime 2022/10/21 17:14
 **/
@RestController
@RequestMapping("/")
public class TestRepeatSubmitController {

    @GetMapping("/testRepeatSubmit/{id}")
    @RepeatSubmit
    public JsonResult testRepeatSubmit(@PathVariable Integer id) throws Exception {
        //模拟业务处理休眠1秒
        Thread.sleep(1000L);
        return JsonResult.successResult();
    }

    public static void main(String[] args) throws Exception {
        ///设置线程池最大执行20个线程并发执行任务
        int threadSize = 20;
        //AtomicInteger通过CAS操作能保证统计数量的原子性
        AtomicInteger successCount = new AtomicInteger(0);
        CountDownLatch downLatch = new CountDownLatch(20);
        ExecutorService fixedThreadPool = Executors.newFixedThreadPool(threadSize);
        for (int i = 0; i < threadSize; i++) {
            int finalI = i;
            fixedThreadPool.submit(() -> {
                RestTemplate restTemplate = new RestTemplate();
                JsonResult result = restTemplate.getForObject("http://localhost:9999/testRepeatSubmit/1", JsonResult.class);
                //String str = restTemplate.getForObject("http://localhost:8032/test/1", String.class);
                if (result.getCode() == HttpStatus.OK.value()) {
                    successCount.getAndIncrement();
                }
                System.out.println(JSONObject.toJSONString(result));
                downLatch.countDown();
            });
            //模拟网络传输时间
            Thread.sleep(100);
        }
        //等待所有线程都执行完任务
        downLatch.await();
        fixedThreadPool.shutdown();
        System.out.println("总共有" + successCount.get() + "个线程请求成功!");

    }
}
