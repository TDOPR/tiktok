package com.haoliang.service.impl;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.haoliang.mapper.TestMapper;
import com.haoliang.model.Test;
import com.haoliang.service.TestService;
import org.springframework.stereotype.Service;

/**
 * @Description
 * @Author Dominick Li
 * @CreateTime 2022/10/18 17:35
 **/
@Service
public class TestServiceImpl extends ServiceImpl<TestMapper,Test> implements TestService {


}
