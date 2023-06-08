package com.haoliang.controller;

import com.haoliang.common.annotation.RepeatSubmit;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.model.PageParam;
import com.haoliang.common.model.dto.PageDTO;
import com.haoliang.common.model.dto.TypeDTO;
import com.haoliang.common.model.vo.PageVO;
import com.haoliang.model.TiktokTask;
import com.haoliang.model.condition.AppUserTaskCondition;
import com.haoliang.model.condition.CheckTaskCondition;
import com.haoliang.model.condition.TiktokTaskCondition;
import com.haoliang.model.dto.AuditCheckDTO;
import com.haoliang.model.dto.ForceDeleteDTO;
import com.haoliang.model.dto.PublishTiktokTask;
import com.haoliang.model.vo.*;
import com.haoliang.service.AppUserTaskService;
import com.haoliang.service.TiktokTaskService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import javax.validation.Valid;

/**
 * @author Dominick Li
 * @Description TikTok任务发布管理
 * @CreateTime 2023/2/24 11:44
 **/
@RestController
@RequestMapping("/tiktok")
public class TikTokTaskController {

    @Autowired
    private TiktokTaskService tiktokTaskService;

    @Autowired
    private AppUserTaskService appUserTaskService;

    /**
     * 查看列表
     */
    @PostMapping("/pagelist")
    @PreAuthorize("hasAuthority('publish:tiktok:list')")
    public JsonResult pagelist(@RequestBody PageParam<TiktokTask, TiktokTaskCondition> pageParam) {
        return tiktokTaskService.pagelist(pageParam);
    }

    /**
     * 后端添加或修改
     */
    @PostMapping
    @PreAuthorize("hasAnyAuthority('publish:tiktok:add','publish:tiktok:edit')")
    public JsonResult addOrEdit(@RequestBody TiktokTask tiktokTask) {
        return tiktokTaskService.addOrEdit(tiktokTask);
    }

    /**
     * 删除
     */
    @PostMapping("/delete")
    @PreAuthorize("hasAuthority('publish:tiktok:list')")
    public JsonResult delete(@RequestBody ForceDeleteDTO forceDeleteDTO) {
        return tiktokTaskService.deleteByIdList(forceDeleteDTO);
    }

    /**
     * C端查看已发布的任务
     * https://twitter.com/Tiktokguild
     * https://www.facebook.com/profile.php?id=100091423895551
     * https://t.me/Tiktokguild
     */
    @PostMapping("/getPublishList")
    public JsonResult<PublishTaskVO> getPublishList(@RequestBody TypeDTO pageDTO) {
        return tiktokTaskService.getPublishList(pageDTO);
    }

    /**
     * 接单
     */
    @PostMapping("/accessTask/{id}")
    @RepeatSubmit
    public JsonResult accessTask(@PathVariable Long id) {
        return appUserTaskService.saveByTaskId(id);
    }

    /**
     * 获取发布任务的套餐
     */
    @GetMapping("/getPricesAndBalance")
    public JsonResult getPricesAndBalance() {
        return tiktokTaskService.getPricesAndBalance();
    }

    /**
     * 查看次数包余量
     * @return
     */
    @GetMapping("/getCountPackageBalance")
    public JsonResult<TaskNumVO> getCountPackageBalance() {
        return tiktokTaskService.getCountPackageBalance();
    }

    /**
     * 购买次数包 套餐
     */
    @PostMapping("/buyPackage/{id}")
    public JsonResult buyPackage(@PathVariable Integer id) {
        return tiktokTaskService.buyPackage(id);
    }

    /**
     * 发布任务
     */
    @PostMapping("/publish")
    public JsonResult publish(@Valid @RequestBody PublishTiktokTask publishTiktokTask) {
        return tiktokTaskService.saveAndPublish(publishTiktokTask);
    }

    /**
     * 查看自己发布的任务
     */
    @PostMapping("/getMyPublishList")
    public JsonResult<PageVO<MyTiktokTaskVO>> getPublishList(@RequestBody PageDTO pageDTO) {
        return tiktokTaskService.getMyPublishList(pageDTO);
    }

    /**
     * 根据任务状态查看我的任务列表
     */
    @PostMapping("/myList")
    public JsonResult<PageVO<AppUserTaskVO>> myList(@RequestBody PageParam<AppUserTaskVO, AppUserTaskCondition> pageParam) {
        return appUserTaskService.myList(pageParam);
    }

    /**
     * 上传图片
     */
    @PostMapping("/uploadImage")
    public JsonResult uploadImage(@RequestParam("file") MultipartFile file, @RequestParam("id") Long id) {
        return appUserTaskService.uploadHeadImage(file, id);
    }

    /**
     * 已接单任务提交
     */
    @PostMapping("/submit/{id}")
    public JsonResult submit(@PathVariable Long id) {
        return appUserTaskService.submit(id);
    }


    /**
     * 查看需要审核的tiktok任务
     */
    @PostMapping("/checkTaskList")
    public JsonResult findCheckTaskList(@RequestBody PageParam<CheckTaskVO, CheckTaskCondition> pageParam) {
        return appUserTaskService.findCheckTaskList(pageParam);
    }

    /**
     * 审核
     */
    @PostMapping("/check")
    public JsonResult check(@RequestBody AuditCheckDTO auditCheckDTO) {
        return appUserTaskService.checkTask(auditCheckDTO);
    }

}
