package com.haoliang.controller;

import com.haoliang.common.model.JsonResult;
import com.haoliang.common.model.PageParam;
import com.haoliang.common.model.dto.IntIdListDTO;
import com.haoliang.common.model.vo.PageVO;
import com.haoliang.model.AppVersions;
import com.haoliang.model.condition.AppVersionsCondition;
import com.haoliang.service.AppVersionService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;


/**
 * @author Dominick Li
 * @Description 版本管理 页面暂未支持
 * @CreateTime 2023/1/7 10:19
 **/
@RestController
@RequestMapping("/version")
public class AppVersionController {

    @Autowired
    private AppVersionService appVersionService;

    /**
     * 分页查询
     */
    @PostMapping("/pagelist")
    //@PreAuthorize("hasAuthority('sys:customer:list')")
    public JsonResult pageList(@RequestBody PageParam<AppVersions, AppVersionsCondition> pageParam) {
        if (pageParam.getSearchParam() == null) {
            pageParam.setSearchParam(new AppVersionsCondition());
        }
        return JsonResult.successResult(new PageVO<>(appVersionService.page(pageParam.getPage(), pageParam.getSearchParam().buildQueryParam())));
    }

    /**
     * 发布版本
     */
    @GetMapping("/active/{id}")
    public JsonResult active(@PathVariable Integer id) {
        return appVersionService.active(id);
    }

    /**
     * 删除版本信息
     */
    @PostMapping("/delete")
    public JsonResult deleteByIdList(@RequestBody IntIdListDTO intIdListDTO) {
        return appVersionService.deleteByIdList(intIdListDTO);
    }

    /**
     * 保存版本信息
     */
    @PostMapping
    public JsonResult save(@RequestBody AppVersions appVersions) {
        return JsonResult.build(appVersionService.saveOrUpdate(appVersions));
    }

}
