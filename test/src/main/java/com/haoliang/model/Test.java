package com.haoliang.model;

import com.baomidou.mybatisplus.annotation.TableName;
import com.haoliang.common.base.BaseModelCID;
import lombok.Data;



/**
 * @Description
 * @Author Dominick Li
 * @CreateTime 2022/10/18 17:36
 **/
@Data
@TableName("test")
public class Test extends BaseModelCID {

    /**
     * 账号
     */
    private String name;

    /**
     * 密码
     */
    private String password;

}
