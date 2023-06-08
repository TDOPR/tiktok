package com.haoliang.common.model;

import com.baomidou.mybatisplus.annotation.TableName;
import com.haoliang.common.base.BaseModelCIDNoModifyTime;
import lombok.Data;


/**
 * @author Dominick Li
 * @description 操作日志表
 **/
@Data
@TableName("sys_operationlog")
public class SysOperationLog extends BaseModelCIDNoModifyTime {

    /**
     * 操作员
     */
    private String username;
    /**
     * 模块
     */
    private String module;
    /**
     * 操作
     */
    private String description;
    /**
     * 详细信息
     */
    private String content;
    /**
     * ip地址
     */
    private String ipAddr;

}
