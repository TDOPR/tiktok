package com.haoliang.service.impl;

import com.alibaba.fastjson.JSONObject;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.util.ResponseUtil;
import com.haoliang.mapper.SysMessageMapper;
import com.haoliang.model.SysMessage;
import com.haoliang.service.SysMessageService;
import org.springframework.stereotype.Service;

import javax.servlet.http.HttpServletResponse;
import java.util.List;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/11/7 9:48
 **/
@Service
public class SysMessageServiceImpl extends ServiceImpl<SysMessageMapper, SysMessage> implements SysMessageService {

    @Override
    public JsonResult saveMessage(SysMessage sysMessage) {
        SysMessage exists = this.getOne(new LambdaQueryWrapper<SysMessage>().eq(SysMessage::getKeyName, sysMessage.getKeyName()));
        if (exists != null && !exists.getId().equals(sysMessage.getId())) {
            return JsonResult.failureResult(String.format("key={%s} exists", sysMessage.getKeyName()));
        }
        return JsonResult.build(this.saveOrUpdate(sysMessage));
    }

    @Override
    public void exportJson(Integer type, HttpServletResponse httpServletResponse) {

        List<SysMessage> sysMessageList;
        if (type == -1) {
            sysMessageList = this.list();
        } else {
            sysMessageList = this.list(new LambdaQueryWrapper<SysMessage>().eq(SysMessage::getType, type));
        }
        //对数据进行分组
        JSONObject data = new JSONObject();
        JSONObject zhCn = new JSONObject();
        JSONObject zhTw = new JSONObject();
        JSONObject enUs = new JSONObject();
        data.put("zhCn", zhCn);
        data.put("zhTw", zhTw);
        data.put("enUs", enUs);

        for (SysMessage sysMessage : sysMessageList) {
            zhCn.put(sysMessage.getKeyName(), sysMessage.getZhCn());
            zhTw.put(sysMessage.getKeyName(), sysMessage.getZhTw());
            enUs.put(sysMessage.getKeyName(), sysMessage.getEnUs());
        }
        ResponseUtil.exportJson(httpServletResponse, data.toJSONString(), "msg");
    }
}
