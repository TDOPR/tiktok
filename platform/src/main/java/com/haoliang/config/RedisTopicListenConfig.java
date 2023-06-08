//package com.haoliang.config;
//
//import com.haoliang.service.SysDictionaryService;
//import lombok.extern.slf4j.Slf4j;
//import org.springframework.beans.factory.annotation.Autowired;
//import org.springframework.beans.factory.annotation.Value;
//import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
//import org.springframework.context.annotation.Bean;
//import org.springframework.context.annotation.Configuration;
//import org.springframework.data.redis.connection.Message;
//import org.springframework.data.redis.connection.MessageListener;
//import org.springframework.data.redis.connection.RedisConnectionFactory;
//import org.springframework.data.redis.listener.PatternTopic;
//import org.springframework.data.redis.listener.RedisMessageListenerContainer;
//import org.springframework.data.redis.listener.adapter.MessageListenerAdapter;
//
//import javax.annotation.PostConstruct;
//
///**
// * @author Dominick Li
// * @Description
// * @CreateTime 2023/5/4 16:29
// **/
//@Slf4j
//@Configuration
//@ConditionalOnProperty(name = "app.singleDeploy", havingValue = "false")
//public class RedisTopicListenConfig {
//
//    @Value("${spring.redis.topic}")
//    private String topicName;
//
//    @Autowired
//    private SysDictionaryService sysDictionaryService;
//
//
//    @PostConstruct
//    void init(){
//        System.out.println("多节点加载reids监控用于刷新字典缓存!");
//    }
//
//    /**
//     * 收取订阅频道的消息
//     * 用于清除一级缓存
//     */
//    @Bean
//    MessageListenerAdapter listenerAdapter() {
//        return new MessageListenerAdapter(new MessageListener() {
//            @Override
//            public void onMessage(Message message, byte[] bytes) {
//                try {
//                    //刷新字典缓存
//                    sysDictionaryService.reloadSetting();
//                } catch (Exception e) {
//                    log.error("listenerAdapter error:{}", e.getMessage());
//                }
//            }
//        });
//    }
//
//    /**
//     * 订阅一个频道
//     */
//    @Bean
//    RedisMessageListenerContainer container(RedisConnectionFactory connectionFactory, MessageListenerAdapter listenerAdapter) {
//        RedisMessageListenerContainer container = new RedisMessageListenerContainer();
//        container.setConnectionFactory(connectionFactory);
//        container.addMessageListener(listenerAdapter, new PatternTopic(topicName));
//        return container;
//    }
//
//}
