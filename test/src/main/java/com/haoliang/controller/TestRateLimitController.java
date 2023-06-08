package com.haoliang.controller;

import com.haoliang.common.annotation.RateLimit;
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
 * @Description 测试分布式接口限流
 * @Author Dominick Li
 * @CreateTime 2022/10/21 15:37
 **/
@RestController
@RequestMapping("/")
public class TestRateLimitController {

    @GetMapping("/testRateLimit/{id}")
    @RateLimit(value = 3,time = 1)
    public String testRateLimit(@PathVariable String id) {
       return "success";
    }

    public static void main(String[] args) throws Exception {
        int threadSize = 20;
        //AtomicInteger通过CAS操作能保证统计数量的原子性
        AtomicInteger successCount = new AtomicInteger(0);
        CountDownLatch downLatch = new CountDownLatch(20);
        ExecutorService fixedThreadPool = Executors.newFixedThreadPool(threadSize);
        for (int i = 0; i < threadSize; i++) {
            fixedThreadPool.submit(() -> {
                RestTemplate restTemplate = new RestTemplate();
                String str = restTemplate.getForObject("http://localhost:9999/testRateLimit", String.class);
                if ("success".equals(str)) {
                    successCount.incrementAndGet();
                }
                System.out.println(str);
                downLatch.countDown();
            });
        }
        //等待所有线程都执行完任务
        downLatch.await();
        fixedThreadPool.shutdown();
        System.out.println("总共有" + successCount.get() + "个线程获得到了令牌!");
    }

}
