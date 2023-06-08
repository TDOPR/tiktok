package com.haoliang.controller.system;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.haoliang.common.annotation.OperationLog;
import com.haoliang.common.constant.OperationAction;
import com.haoliang.common.constant.OperationModel;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.model.PageParam;
import com.haoliang.common.model.dto.IntIdListDTO;
import com.haoliang.common.model.vo.PageVO;
import com.haoliang.model.SysMessage;
import com.haoliang.model.condition.SysMessageCondition;
import com.haoliang.service.SysMessageService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletResponse;

/**
 * 国际化接口
 *
 * @author Dominick Li
 * @CreateTime 2022/11/7 9:45
 **/
@RestController
@RequestMapping("/msg")
public class SysMessageController {

    @Autowired
    private SysMessageService sysMessageService;

    /**
     * 分页查询
     */
    @PostMapping("/pagelist")
    @PreAuthorize("hasAuthority('sys:msg:list')")
    public JsonResult<PageVO<SysMessage>> queryByCondition(@RequestBody PageParam<SysMessage, SysMessageCondition> pageParam) {
        if (pageParam.getSearchParam() == null) {
            pageParam.setSearchParam(new SysMessageCondition());
        }
        IPage<SysMessage> iPage = sysMessageService.page(pageParam.getPage(), pageParam.getSearchParam().buildQueryParam());
        return JsonResult.successResult(new PageVO<>(iPage));
    }

    /**
     * 批量删除
     *
     * @param idList Id数组
     */
    @OperationLog(module = OperationModel.SYS_MSG, description = OperationAction.REMOVE)
    @PostMapping("/delete")
    @PreAuthorize("hasAuthority('sys:msg:remove')")
    public JsonResult deleteByIds(@RequestBody IntIdListDTO intIdListDTO) {
        return JsonResult.build(sysMessageService.removeByIds(intIdListDTO.getIdList()));
    }

    /**
     * 新增或修改
     */
    @OperationLog(module = OperationModel.SYS_MSG, description = OperationAction.ADD_OR_EDIT)
    @PostMapping
    @PreAuthorize("hasAnyAuthority('sys:msg:add','sys:msg:edit')")
    public JsonResult save(@RequestBody SysMessage sysMessage) {
        return sysMessageService.saveMessage(sysMessage);
    }

    /**
     * 导出成json文件
     *
     * @param type -1 全部 0=管理端 1=客户端
     */
    @PreAuthorize("hasAuthority('sys:msg:export')")
    @GetMapping("/export/{type}")
    public void exportJson(@PathVariable Integer type, HttpServletResponse httpServletResponse) {
        sysMessageService.exportJson(type, httpServletResponse);
    }


}
