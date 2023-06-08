package com.haoliang.common.model;

import com.baomidou.mybatisplus.annotation.TableName;
import com.haoliang.common.base.BaseModelCIDNoModifyTime;
import lombok.Data;

/**
 * @author Dominick Li
 * @description 错误日志表
 **/
@Data
@TableName("sys_errorlog")
public class SysErrorLog extends BaseModelCIDNoModifyTime {

    public SysErrorLog(){

    }

    public SysErrorLog(String errorType, String position, String errorMsg, String serverIp){
        this.errorType=errorType;
        this.position=position;
        this.errorMsg=errorMsg;
        this.ipAddr=serverIp;
    }

    /**
     * 错误信息
     */
    private String errorMsg;

    /**
     * ip地址
     */
    private String ipAddr;

    /**
     * 错误类型
     */
    private String errorType;

    /**
     * 异常发生的位置
     */
    private String position;


}
