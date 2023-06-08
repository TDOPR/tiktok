package com.haoliang.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.haoliang.common.config.AppParamProperties;
import com.haoliang.common.constant.CacheKeyPrefixConstants;
import com.haoliang.constant.TiktokConfig;
import com.haoliang.mapper.TreePathMapper;
import com.haoliang.mapper.UpdateUserLevelTaskMapper;
import com.haoliang.model.UpdateUserLevelJob;
import com.haoliang.service.AsyncService;
import com.haoliang.service.UpdateUserLevelTaskService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/11/25 11:17
 **/
@Slf4j
@Service
public class UpdateUserLevelTaskServiceImpl extends ServiceImpl<UpdateUserLevelTaskMapper, UpdateUserLevelJob> implements UpdateUserLevelTaskService {

    @Resource
    private TreePathMapper treePathMapper;

    @Resource
    private AsyncService asyncService;

    @Resource
    private RedisTemplate redisTemplate;

    @Resource
    AppParamProperties appParamProperties;

    @Override
    public void insertListByUserId(Integer userId) {
        //获取所有有效的上级用户Id
        List<Integer> parentIdList = treePathMapper.getAllAncestorIdByUserId(userId);
        log.info("userId={} 需要更新上级社区等级", userId, parentIdList);
        List<UpdateUserLevelJob> updateUserLevelJobList = new ArrayList<>();
        LocalDateTime now = LocalDateTime.now();
        for (Integer id : parentIdList) {
            updateUserLevelJobList.add(new UpdateUserLevelJob(id, now, appParamProperties.getDelayTime()));
        }
        if (updateUserLevelJobList.size() > 0) {
            this.saveOrUpdateBatch(updateUserLevelJobList);
        }
    }

    @Override
    public List<UpdateUserLevelJob> findTask(Integer pageSize) {
        Page<UpdateUserLevelJob> updateUserLevelTaskPage = new Page<UpdateUserLevelJob>(1, pageSize);
        IPage<UpdateUserLevelJob> page = this.page(updateUserLevelTaskPage,
                new LambdaQueryWrapper<UpdateUserLevelJob>()
                        .le(UpdateUserLevelJob::getDelayTime, LocalDateTime.now())
                        .orderByAsc(UpdateUserLevelJob::getDelayTime));
        return page.getRecords();
    }

    @Override
    public void updateAll() {
        String cacheKey = CacheKeyPrefixConstants.DISTRIBUTED_LOCK + TiktokConfig.UPDATE_USER_LEVEL_TASK_METHOD_NAME;
        boolean flag = false;
        try {
            //通过分布式锁避免当前任务触发时候和定时任务的冲突
            flag = redisTemplate.opsForValue().setIfAbsent(cacheKey, "1", 30, TimeUnit.SECONDS);
            if (flag) {
                List<UpdateUserLevelJob> list = this.list();
                int size = list.size();
                log.info("需要更新业绩的记录数:{}", size);
                if (size > 0) {
                    CountDownLatch countDownLatch = new CountDownLatch(size);
                    for (UpdateUserLevelJob updateUserLevelJob : list) {
                        asyncService.updateUserLevelTask(updateUserLevelJob.getUserId(), countDownLatch);
                    }
                    countDownLatch.await();
                    log.info("updateAll success");
                }
            }
        } catch (Exception e) {
            log.error("updateAll error:{}", e.getMessage());
        } finally {
            if (flag) {
                //如果拿到锁的情况释放锁
                redisTemplate.delete(cacheKey);
            }
        }
    }
}
