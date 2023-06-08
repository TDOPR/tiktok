package com.haoliang.service.impl;

import com.haoliang.common.model.JsonResult;
import com.haoliang.config.WebSocketConfig;
import com.haoliang.controller.monitor.server.SystemServer;
import com.haoliang.model.vo.AdminHomeVO;
import com.haoliang.model.vo.BusinessVO;
import com.haoliang.service.AppUserService;
import com.haoliang.service.StatService;
import com.haoliang.service.SystemService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import javax.annotation.PostConstruct;
import javax.annotation.Resource;

/**
 * @author Dominick Li
 * @CreateTime 2020/7/14 20:53
 * @description 获取数据统计信息
 **/
@Service
public class SystemServiceImpl implements SystemService {

    @Autowired
    private StatService statService;

    @Resource
    private WebSocketConfig webSocketConfig;

    @Value("${app.ip}")
    private String ip;

    private String wsAddress;

    @PostConstruct
    public void init() {
        wsAddress = String.format("%s://%s:%s%s", webSocketConfig.isSsl() ? "wss" : "ws", ip, webSocketConfig.getPort(), webSocketConfig.getAdminwsPath());
    }

    @Override
    public JsonResult<BusinessVO> getHomeInfo() {
        return JsonResult.successResult(statService.getAdminPanel());
    }

    @Override
    public JsonResult<SystemServer> getServerInfo() {
        return JsonResult.successResult(new SystemServer());
    }
}
