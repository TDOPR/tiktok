package com.haoliang.controller.system;

import com.haoliang.common.annotation.OperationLog;
import com.haoliang.common.constant.OperationAction;
import com.haoliang.common.constant.OperationModel;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.model.PageParam;
import com.haoliang.common.model.dto.IntIdListDTO;
import com.haoliang.model.SysRole;
import com.haoliang.model.condition.SysRoleCondition;
import com.haoliang.model.dto.SysRoleDTO;
import com.haoliang.service.SysRoleService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

/**
 * 角色管理
 *
 * @author Dominick Li
 **/
@RestController
@RequestMapping("/role")
public class SysRoleController {

    @Autowired
    private SysRoleService roleService;

    /**
     * 分页查询
     */
    @PostMapping("/pagelist")
    @PreAuthorize("hasAuthority('sys:role:list')")
    public JsonResult queryByCondition(@RequestBody PageParam<SysRole, SysRoleCondition> pageParam) {
        return roleService.queryByCondition(pageParam);
    }

    /**
     * 新增或修改
     */
    @OperationLog(module = OperationModel.SYS_ROLE, description = OperationAction.ADD_OR_EDIT)
    @PostMapping
    @PreAuthorize("hasAnyAuthority('sys:role:add','sys:role:edit')")
    public JsonResult save(@RequestBody SysRoleDTO sysRoleDTO) {
        return roleService.saveRole(sysRoleDTO);
    }

    /**
     * 批量删除
     *
     * @param idList Id数组
     */
    @OperationLog(module = OperationModel.SYS_ROLE, description = OperationAction.REMOVE)
    @PostMapping("/delete")
    @PreAuthorize("hasAuthority('sys:role:remove')")
    public JsonResult deleteByIds(@RequestBody IntIdListDTO intIdListDTO) {
        return roleService.deleteByIdList(intIdListDTO.getIdList());
    }



    /**
     * 导出角色信息
     */
    @PostMapping("/export")
    @PreAuthorize("hasAuthority('sys:role:export')")
    public JsonResult exportRoles(@RequestBody SysRoleCondition sysRoleCondition) {
        return roleService.export(sysRoleCondition);
    }


    /**
     * 获取添加用户时需要的角色下拉列表信息
     */
    @GetMapping("/getSelectList")
    public JsonResult getRoleAndChannel() {
        return roleService.findSelectList();
    }


}
