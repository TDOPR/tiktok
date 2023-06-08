package com.haoliang.controller;

import com.haoliang.common.annotation.IgnoreWebSecurity;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.model.PageParam;
import com.haoliang.common.model.dto.PageDTO;
import com.haoliang.enums.CoinNetworkSourceEnum;
import com.haoliang.model.TreatGuestDinner;
import com.haoliang.model.condition.TreatGuestDinnerCondition;
import com.haoliang.model.dto.AuditCheckDTO;
import com.haoliang.model.dto.TreatGuestDinnerDTO;
import com.haoliang.service.TreatGuestDinnerService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;


/**
 * @author Dominick Li
 * @Description 请客吃饭
 * @CreateTime 2023/3/23 11:36
 **/
@RestController
@RequestMapping("/guestdinner")
public class TreatGuestDinnerController {

    @Autowired
    private TreatGuestDinnerService treatGuestDinnerService;

    /**
     * 获取链的类型
     */
    @IgnoreWebSecurity
    @GetMapping("/getCoinTypeList")
    public JsonResult getCoinTypeList() {
        return JsonResult.successResult(CoinNetworkSourceEnum.getSelectList());
    }

    /**
     * 添加
     */
    @PostMapping
    public JsonResult submitTask(MultipartFile file, MultipartFile img, int[] itemUserIds, String address, String coinType) {
        return treatGuestDinnerService.submitTask(file, img, itemUserIds, address, coinType);
    }

    /**
     * 编辑
     */
    @PostMapping("/edit")
    public JsonResult edit(Integer id, @RequestParam(name = "file", required = false) MultipartFile file, @RequestParam(name = "img", required = false) MultipartFile img,
                           int[] itemUserIds, String address, String coinType) {
        return treatGuestDinnerService.edit(id, file, img, itemUserIds, address, coinType);
    }

    @PostMapping("/editJson")
    public JsonResult edit(@RequestBody TreatGuestDinnerDTO treatGuestDinnerDTO) {
        return treatGuestDinnerService.editJson(treatGuestDinnerDTO);
    }

    /**
     * 获取我的提交请客吃饭列表
     */
    @PostMapping("/pagelist")
    public JsonResult getGuestList(@RequestBody PageDTO pageDTO) {
        return treatGuestDinnerService.getMyPage(pageDTO);
    }

    /**
     * 查看已提交的任务
     */
    @PostMapping("/getCheckList")
    @PreAuthorize("hasAuthority('examine:dinner:list')")
    public JsonResult getCheckList(@RequestBody PageParam<TreatGuestDinner, TreatGuestDinnerCondition> pageParam) {
        return treatGuestDinnerService.getCheckList(pageParam);
    }

    @PostMapping("/check")
    @PreAuthorize("hasAnyAuthority('examine:dinner:pass','examine:dinner:reject')")
    public JsonResult check(@RequestBody AuditCheckDTO auditCheckDTO){
        return treatGuestDinnerService.check(auditCheckDTO);
    }

}
