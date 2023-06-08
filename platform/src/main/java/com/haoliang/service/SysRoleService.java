package com.haoliang.service;


import com.baomidou.mybatisplus.extension.service.IService;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.model.PageParam;
import com.haoliang.model.SysRole;
import com.haoliang.model.dto.SysRoleDTO;
import com.haoliang.model.condition.SysRoleCondition;
import com.haoliang.model.vo.SelectVO;

import java.util.List;

public interface SysRoleService extends IService<SysRole> {

    JsonResult<List<SelectVO>> findSelectList();

    JsonResult queryByCondition(PageParam<SysRole, SysRoleCondition> pageParam);

    JsonResult saveRole(SysRoleDTO sysRoleDTO);

    JsonResult deleteByIdList(List<Integer> idList);

    JsonResult export(SysRoleCondition sysRoleCondition);
}
