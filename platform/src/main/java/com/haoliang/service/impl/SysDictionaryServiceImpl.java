package com.haoliang.service.impl;


import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.haoliang.common.config.SysSettingParam;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.util.SpringUtil;
import com.haoliang.mapper.SysDictionaryMapper;
import com.haoliang.model.SysDictionary;
import com.haoliang.service.SysDictionaryService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;

import javax.annotation.PostConstruct;
import java.util.List;
import java.util.Map;

/**
 * @author Dominick Li
 * @CreateTime 2021/6/18 15:10
 * @description
 **/
@Slf4j
@Service
public class SysDictionaryServiceImpl extends ServiceImpl<SysDictionaryMapper, SysDictionary> implements SysDictionaryService {

    @Value("${spring.redis.topic}")
    private String topicName;

    @Value("${app.singleDeploy}")
    private boolean singleDeploy;

    /**
     * bean初始化缓存字典信息
     */
    @PostConstruct
    public void init() {
        reload();
    }

    @Override
    public JsonResult modifyBaseDictionary(Map<String, String> data) {
        SysDictionary dictionary;
        for (Map.Entry<String, String> entry : data.entrySet()) {
            dictionary = this.getOne(new LambdaQueryWrapper<SysDictionary>().eq(SysDictionary::getDicKey, entry.getKey()));
            dictionary.setDicValue(entry.getValue());
            this.saveOrUpdate(dictionary);
            SysSettingParam.put(dictionary.getDicKey(), dictionary.getDicValue());
        }
        if (!singleDeploy) {
            RedisTemplate redisTemplate = SpringUtil.getBean(RedisTemplate.class);
            redisTemplate.convertAndSend(topicName, "refreshDictionary");
            log.info("redisTemplate.convertAndSend:{} success", topicName);
        }
        return JsonResult.successResult();
    }

    @Override
    public JsonResult reloadSetting() {
        reload();
        return JsonResult.successResult();
    }

    private void reload() {
        List<SysDictionary> dictionaryList = this.list();

        if (dictionaryList.size() > 0) {
            SysSettingParam.setLoading(true);
            log.info("初始化把字典信息加入缓存中...");
        }

        dictionaryList.forEach(dictionary -> {
            SysSettingParam.put(dictionary.getDicKey(), dictionary.getDicValue());
        });
    }
}
