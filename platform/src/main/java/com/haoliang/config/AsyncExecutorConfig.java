package com.haoliang.config;

import lombok.extern.slf4j.Slf4j;
import org.springframework.aop.interceptor.AsyncUncaughtExceptionHandler;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.scheduling.annotation.AsyncConfigurer;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Component;

import java.util.StringJoiner;
import java.util.concurrent.Executor;
import java.util.concurrent.ThreadPoolExecutor;

/**
 * @Description 异步线程池
 * @Author Dominick Li
 * @CreateTime 2022/10/21 9:57
 **/
@Component
@EnableAsync
@Slf4j
public class AsyncExecutorConfig implements AsyncConfigurer {

    @Value("${asyncExecutor.prefixName}")
    String prefixName;

    @Value("${asyncExecutor.codeSize}")
    Integer codeSize;

    @Value("${asyncExecutor.maxSize}")
    Integer maxSize;

    @Value("${asyncExecutor.queueSize}")
    Integer maxQueueSize;

    @Bean("asyncExecutor")
    @Override
    public Executor getAsyncExecutor() {

        ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
        // 设置核心线程数
        executor.setCorePoolSize(codeSize);
        // 设置最大线程数
        executor.setMaxPoolSize(maxSize);
        if (maxQueueSize > 0) {
            // 设置队列容量
            executor.setQueueCapacity(maxQueueSize);
        }
        // 设置线程活跃时间（秒）
        executor.setKeepAliveSeconds(60);

        //设置线程池中任务的等待时间，如果超过这个时候还没有销毁就强制销毁，以确保应用最后能够被关闭，而不是阻塞住
        executor.setAwaitTerminationSeconds(60);

        // 设置默认线程名称
        executor.setThreadNamePrefix(prefixName);//线程前缀名称

        /**
         * 拒绝策略，默认是AbortPolicy
         * AbortPolicy：丢弃任务并抛出RejectedExecutionException异常
         * DiscardPolicy：丢弃任务但不抛出异常
         * DiscardOldestPolicy：丢弃最旧的处理程序，然后重试，如果执行器关闭，这时丢弃任务
         * CallerRunsPolicy：执行器执行任务失败，则在策略回调方法中执行任务，如果执行器关闭，这时丢弃任务
         */
        // 设置拒绝策略
        executor.setRejectedExecutionHandler(new ThreadPoolExecutor.CallerRunsPolicy());//配置拒绝策略

        return executor;
    }

    @Override
    public AsyncUncaughtExceptionHandler getAsyncUncaughtExceptionHandler() {
        return (ex, method, params) -> {
            StringJoiner stringJoiner = new StringJoiner(",");
            for (Object param : params) {
                stringJoiner.add(param.toString());
            }
            log.error("异步线程执行失败。方法：[{}],参数：[{}],异常信息[{}] : ", method, stringJoiner.toString(), ex.getMessage(), ex);
        };
    }

}
