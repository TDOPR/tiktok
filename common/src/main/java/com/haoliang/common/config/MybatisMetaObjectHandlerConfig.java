package com.haoliang.common.config;

import com.baomidou.mybatisplus.core.handlers.MetaObjectHandler;
import org.apache.ibatis.reflection.MetaObject;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;

/**
 * @author Dominick Li
 * @Description 数据填充
 * @CreateTime 2022/10/26 10:24
 **/
@Component
public class MybatisMetaObjectHandlerConfig implements MetaObjectHandler {

    @Override
    public void insertFill(MetaObject metaObject) {
        // 该属性为空，可以进行填充
        if (this.getFieldValByName("createTime", metaObject) == null) {
            this.setFieldValByName("createTime", LocalDateTime.now(), metaObject);
        }
        this.setFieldValByName("lastmodifiedTime", LocalDateTime.now(), metaObject);
    }

    @Override
    public void updateFill(MetaObject metaObject) {
        this.setFieldValByName("lastmodifiedTime", LocalDateTime.now(), metaObject);
    }

}
