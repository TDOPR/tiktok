package com.haoliang.controller;

import com.haoliang.common.model.JsonResult;
import com.haoliang.common.util.MessageUtil;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RestController;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/12/8 16:48
 **/
@RestController
public class TestI18nController {


    /**
     * 测试国际化
     * en_US 英文
     * zh_CN 中文
     */
    @GetMapping("/testi18n/{language}")
    public JsonResult test(@PathVariable String language) {
        Object[] params = new Object[2];
        params[0] = "敏";
        params[1] = "啊";
        return JsonResult.successResult(MessageUtil.get("test", params,language));
    }
}
