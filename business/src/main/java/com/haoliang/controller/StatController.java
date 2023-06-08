package com.haoliang.controller;

import com.haoliang.common.base.DefaultCondition;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.model.PageParam;
import com.haoliang.model.StatBalance;
import com.haoliang.model.StatDynamicAndStatic;
import com.haoliang.model.StatInputAndOutput;
import com.haoliang.model.StatUser;
import com.haoliang.service.StatService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

/**
 * @author Dominick Li
 * @Description 数据统计
 * @CreateTime 2023/3/31 18:30
 **/
@RequestMapping("/stat")
@RestController
public class StatController {

    @Autowired
    private StatService statService;

    /**
     * 用户数据
     * @param pageParam
     * @return
     */
    @PostMapping("/userPage")
    @PreAuthorize("hasAuthority('data:user:list')")
    public JsonResult userPage(@RequestBody PageParam<StatUser, DefaultCondition> pageParam) {
        return statService.statUserPage(pageParam);
    }

    /**
     * 获取用户关键指标
     */
    @GetMapping("/userIndex")
    @PreAuthorize("hasAuthority('data:user:list')")
    public JsonResult userNowData() {
        return statService.userNowData();
    }

    /**
     * 获取资金指标数据
     */
    @GetMapping("/fund")
    public JsonResult fund(){
        return statService.fund();
    }

    /**
     * 出入金管理
     */
    @PostMapping("/inputAndOutPutPage")
    @PreAuthorize("hasAuthority('data:fund:access')")
    public JsonResult inputAndOutPutPage(@RequestBody PageParam<StatInputAndOutput, DefaultCondition> pageParam) {
        return statService.inputAndOutPutPage(pageParam);
    }

    /**
     * 动静态管理
     */
    @PostMapping("/dynamicAndStaticPage")
    @PreAuthorize("hasAuthority('data:fund:status')")
    public JsonResult dynamicAndStaticPage(@RequestBody PageParam<StatDynamicAndStatic, DefaultCondition> pageParam) {
        return statService.dynamicAndStaticPage(pageParam);
    }

    /**
     * 余额持有管理
     */
    @PostMapping("/balancePage")
    @PreAuthorize("hasAuthority('data:fund:hold')")
    public JsonResult balancePage(@RequestBody PageParam<StatBalance, DefaultCondition> pageParam) {
        return statService.balancePage(pageParam);
    }

}

