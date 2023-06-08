package com.haoliang.model.vo;

import com.haoliang.controller.monitor.server.SystemServer;
import lombok.Data;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/11/23 17:01
 **/
@Data
public class AdminHomeVO {
    /**
     * 服务器信息
     */
    private SystemServer system;

    /**
     * 业务数据
     */
    private BusinessVO business;
}
