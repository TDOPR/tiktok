package com.haoliang.controller;

import com.haoliang.common.annotation.RedisLock;
import com.haoliang.common.util.redis.RedisUtil;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.client.RestTemplate;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 * @Description 使用分布式锁实现秒杀系统
 * @Author Dominick Li
 * @CreateTime 2022/10/21 15:37
 **/
@RestController
@RequestMapping("/")
public class TestRedisLockController {

    @GetMapping("/testRedidLock")
    @RedisLock(retry = 3)
    public String testRedidLock() {
        Integer shopNumber = RedisUtil.getCacheObject("shop:number");
        if (shopNumber > 0) {
            shopNumber--;
            RedisUtil.setCacheObject("shop:number", shopNumber);
            return "抢购成功!";
        } else {
            return "余量不足!";
        }
    }

    public static void main(String[] args) {
        int threadSize = 30;
        ExecutorService fixedThreadPool = Executors.newFixedThreadPool(threadSize);
        for (int i = 0; i < threadSize; i++) {
            fixedThreadPool.submit(() -> {
                RestTemplate restTemplate = new RestTemplate();
                String str = restTemplate.getForObject("http://localhost:9999/testRedidLock", String.class);
                System.out.println(str);
            });
        }
    }

}
