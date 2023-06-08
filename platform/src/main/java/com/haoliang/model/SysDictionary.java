package com.haoliang.model;

import com.baomidou.mybatisplus.annotation.TableName;
import com.haoliang.common.base.BaseModelCID;
import lombok.Data;

/**
 * @author Dominick Li
 * @description 系统字典表
 **/
@Data
@TableName("sys_dictionary")
public class SysDictionary extends BaseModelCID {

    /**
     * 字典的key
     */
    private String dicKey;

    /**
     * 字典的value
     */
    private String dicValue;

    /**
     * 字典名称
     */
    private String dicName;

    /**
     * 备注
     */
    private String remark;


}
