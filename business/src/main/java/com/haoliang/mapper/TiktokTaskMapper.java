package com.haoliang.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.haoliang.model.TiktokTask;
import com.haoliang.model.vo.MyTiktokTaskVO;
import com.haoliang.model.vo.TiktokTaskVO;
import org.apache.ibatis.annotations.Param;

import java.util.List;

public interface TiktokTaskMapper  extends BaseMapper<TiktokTask> {
    int reduceNum(Long taskId);

    Page<TiktokTaskVO> page(Page<TiktokTaskVO> page,@Param("userId") Integer userId,@Param("greenhorn") Integer greenhorn);

    Page<MyTiktokTaskVO> pageByUserId(Page page, @Param("userId") Integer userId);

    void increaseNum(List<Long> idList);
}
