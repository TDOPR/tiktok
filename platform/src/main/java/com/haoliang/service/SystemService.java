package com.haoliang.service;


import com.haoliang.common.model.JsonResult;
import com.haoliang.controller.monitor.server.SystemServer;
import com.haoliang.model.vo.AdminHomeVO;
import com.haoliang.model.vo.BusinessVO;

public interface SystemService {

    JsonResult<BusinessVO> getHomeInfo();

    JsonResult<SystemServer> getServerInfo();
}
