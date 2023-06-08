package com.haoliang.controller;

import com.haoliang.common.annotation.DynamicApi;
import com.haoliang.common.annotation.RepeatSubmit;
import com.haoliang.common.model.JsonResult;
import com.haoliang.manager.TradeManager;
import com.haoliang.model.dto.BatchAddSubUserDTO;
import com.haoliang.service.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/4/10 18:51
 **/
@RestController
@RequestMapping("/test")
public class TestApiController {

    @Autowired
    private AppUserService appUserService;

    @Autowired
    private TradeManager tradeManager;

    @Autowired
    private KLineDataService kLineDataService;

    @Autowired
    private StatService statService;

    @Autowired
    private WalletTttLogsService walletTttLogsService;

    @Autowired
    private AppUserTaskService appUserTaskService;

    /**
     * 批量添加用户
     */
    @RepeatSubmit
    @GetMapping("/batchAddUser")
    @DynamicApi
    public synchronized JsonResult batchAddUser() {
        System.out.println("测试批量添加用户");
        return appUserService.batchAddUser();
    }

    /**
     * 添加指定数量的下级用户
     */
    @RepeatSubmit(interval = 5000)
    @PostMapping("/batchAddSubUser")
    @DynamicApi
    public JsonResult batchAddSubUser(@RequestBody BatchAddSubUserDTO batchAddSubUserDTO) {
        //System.out.println("测试批量添加下级");
        return appUserService.batchAddSubUser(batchAddSubUserDTO);
    }

    /**
     * 测试发放持币奖、代数奖、团队奖和分红将
     */
    @GetMapping("/")
    @RepeatSubmit(interval = 10000)
    @DynamicApi
    public JsonResult test() {
        return tradeManager.test();
    }

    @GetMapping("/insertStat")
    @DynamicApi
    public JsonResult insertStat() {
        statService.stat();
        return JsonResult.successResult();
    }

    @GetMapping("/insertK")
    @DynamicApi
    public JsonResult insertKLine() {
        return kLineDataService.insertTestData();
    }

    @GetMapping("/getNowK")
    @DynamicApi
    public JsonResult getNowK() {
        return JsonResult.successResult(kLineDataService.getKLineNowData());
    }

    @GetMapping("/clearExpired")
    @DynamicApi
    public JsonResult clearExpired() {
        walletTttLogsService.clearExpired();
        return JsonResult.successResult();
    }

    @GetMapping("/clearAppUserTask")
    @DynamicApi
    public JsonResult clearAppUserTask() {
        appUserTaskService.clearAppUserTask();
        return JsonResult.successResult();
    }

}
