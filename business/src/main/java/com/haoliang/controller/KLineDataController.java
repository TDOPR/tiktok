package com.haoliang.controller;

import com.haoliang.common.base.DefaultCondition;
import com.haoliang.common.constant.SystemConstants;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.model.PageParam;
import com.haoliang.common.model.vo.PageVO;
import com.haoliang.model.KLineData;
import com.haoliang.service.KLineDataService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * @author Dominick Li
 * @Description ttt转usd汇率管理
 * @CreateTime 2023/3/15 18:14
 **/
@RestController
@RequestMapping("/klinedata")
public class KLineDataController {

    @Autowired
    private KLineDataService kLineDataService;

    /**
     * 分页查询
     */
    @PostMapping("/pagelist")
    @PreAuthorize("hasAuthority('settings:rate:list')")
    public JsonResult<PageVO<KLineData>> pageList(@RequestBody PageParam<KLineData, DefaultCondition> pageParam) {
        if (pageParam.getSearchParam() == null) {
            pageParam.setSearchParam(new DefaultCondition());
        }
        return JsonResult.successResult(new PageVO<>(kLineDataService.page(pageParam.getPage(), pageParam.getSearchParam().buildQueryParam(SystemConstants.ORDER_BY))));
    }

    /**
     * 添加或修改
     */
    @PostMapping
    @PreAuthorize("hasAnyAuthority('settings:rate:add','settings:rate:edit')")
    public JsonResult saveOrUpdate(@RequestBody KLineData kLineData) {
        return kLineDataService.addAndEdit(kLineData);
    }

}
