package com.haoliang.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.haoliang.model.Test;
import org.apache.ibatis.annotations.Param;

import java.util.List;

public interface TestMapper extends BaseMapper<Test> {

    List<Test> findAllByName(@Param("name")String name);

}
