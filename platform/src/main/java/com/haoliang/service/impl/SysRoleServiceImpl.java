package com.haoliang.service.impl;


import com.alibaba.excel.support.ExcelTypeEnum;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.haoliang.common.config.GlobalProperties;
import com.haoliang.common.constant.OperationModel;
import com.haoliang.common.enums.BooleanEnum;
import com.haoliang.common.enums.DataSavePathEnum;
import com.haoliang.common.enums.ReturnMessageEnum;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.model.PageParam;
import com.haoliang.common.model.vo.PageVO;
import com.haoliang.common.util.DateUtil;
import com.haoliang.common.util.StringUtil;
import com.haoliang.common.util.excel.ExcelUtil;
import com.haoliang.common.enums.RoleTypeEnum;
import com.haoliang.mapper.SysRoleMapper;
import com.haoliang.mapper.SysRoleMenuMapper;
import com.haoliang.mapper.SysUserMapper;
import com.haoliang.model.SysRole;
import com.haoliang.model.SysRoleMenu;
import com.haoliang.model.condition.SysRoleCondition;
import com.haoliang.model.dto.SysRoleDTO;
import com.haoliang.model.vo.ExportRoleVO;
import com.haoliang.model.vo.RoleVO;
import com.haoliang.model.vo.SelectVO;
import com.haoliang.service.SysRoleService;
import io.jsonwebtoken.lang.Collections;
import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.List;

/**
 * @author Dominick Li
 * @CreateTime 2020/10/12 13:51
 * @description
 **/
@Service
public class SysRoleServiceImpl extends ServiceImpl<SysRoleMapper, SysRole> implements SysRoleService {

    @Resource
    private SysUserMapper sysUserMapper;

    @Resource
    private SysRoleMenuMapper sysRoleMenuMapper;

    @Override
    public JsonResult queryByCondition(PageParam<SysRole, SysRoleCondition> pageParam) {
        if (pageParam.getSearchParam() == null) {
            pageParam.setSearchParam(new SysRoleCondition());
        }
        IPage<SysRole> iPage = this.page(pageParam.getPage(), pageParam.getSearchParam().buildQueryParam());
        List<SysRole> sysRoles = iPage.getRecords();
        List<RoleVO> roleVOList = new ArrayList<>();
        RoleVO roleVO;
        for (SysRole sysRole : sysRoles) {
            roleVO = new RoleVO();
            roleVO.setUserStr(StringUtil.List2Str(sysUserMapper.findAllByUsernameByRoleId(sysRole.getId()), ","));
            BeanUtils.copyProperties(sysRole, roleVO);
            roleVO.setMenuIds(sysRoleMenuMapper.findAllMenuIdByRoleId(sysRole.getId(), BooleanEnum.TRUE.intValue()));
            roleVO.setSemiMenuIds(sysRoleMenuMapper.findAllMenuIdByRoleId(sysRole.getId(), BooleanEnum.FALSE.intValue()));
            roleVO.setBuiltIn(RoleTypeEnum.idOf(sysRole.getId())!=null);
            roleVOList.add(roleVO);
        }
        return JsonResult.successResult(new PageVO(iPage.getTotal(), iPage.getPages(), roleVOList));
    }

    @Override
    public JsonResult saveRole(SysRoleDTO sysRole) {
        if (sysRole.getId() == null) {
            //角色名称和编码不能重复
            SysRole exists = this.getOne(new LambdaQueryWrapper<SysRole>().eq(SysRole::getRoleName, sysRole.getRoleName()));
            if (exists != null) {
                return JsonResult.failureResult(ReturnMessageEnum.ROLE_NAME_EXISTS);
            }
            exists = this.getOne(new LambdaQueryWrapper<SysRole>().eq(SysRole::getRoleCode, sysRole.getRoleCode()));
            if (exists != null) {
                return JsonResult.failureResult(ReturnMessageEnum.ROLE_CODE_EXISTS);
            }
            SysRole newRole = new SysRole();
            BeanUtils.copyProperties(sysRole, newRole);
            this.save(newRole);
            sysRole.setId(newRole.getId());
        } else {
            SysRole oldSysRole = this.getById(sysRole.getId());
            oldSysRole.setRoleCode(sysRole.getRoleCode());
            oldSysRole.setRoleName(sysRole.getRoleName());
            oldSysRole.setEnabled(sysRole.getEnabled());
            this.saveOrUpdate(oldSysRole);
            sysRoleMenuMapper.delete(new LambdaQueryWrapper<SysRoleMenu>().eq(SysRoleMenu::getRoleId, sysRole.getId()));
        }

        List<Integer> menuIds = sysRole.getMenuIds();
        if (!Collections.isEmpty(menuIds)) {
            menuIds.forEach(menuId -> {
                SysRoleMenu sysRoleMenu = new SysRoleMenu(sysRole.getId(), menuId, 1);
                sysRoleMenuMapper.insert(sysRoleMenu);
            });
        }

        menuIds = sysRole.getSemiMenuIds();
        if (!Collections.isEmpty(menuIds)) {
            menuIds.forEach(menuId -> {
                SysRoleMenu sysRoleMenu = new SysRoleMenu(sysRole.getId(), menuId, 0);
                sysRoleMenuMapper.insert(sysRoleMenu);
            });
        }
        return JsonResult.successResult();
    }


    @Override
    public JsonResult<List<SelectVO>> findSelectList() {
        List<SysRole> sysRoleList = this.list();
        List<SelectVO> monthSelectVOList = new ArrayList<>();
        sysRoleList.forEach(sysRole -> {
            if (!sysRole.getId().equals(RoleTypeEnum.ADMIN.getId())) {
                monthSelectVOList.add(new SelectVO(sysRole.getId(), sysRole.getRoleName()));
            }
        });
        return JsonResult.successResult(monthSelectVOList);
    }

    @Override
    @Transactional
    public JsonResult deleteByIdList(List<Integer> idList) {
        //查询角色是否被用户引用
        List<Integer> existsId = sysUserMapper.findExistsByRoleId(idList);
        StringBuilder sb = new StringBuilder();
        if (existsId.size() > 0) {
            List<String> roleName = sysRoleMenuMapper.findRoleNameByIdIn(existsId);
            for (Object str : roleName) {
                sb.append(str).append(",");
            }
            if (sb.length() > 0) {
                sb.deleteCharAt(sb.length() - 1);
            }
            idList.removeAll(existsId);
        }

        if (idList.size() > 0) {
            //删除角色并且删除相关的角色菜单表
            sysRoleMenuMapper.delete(new LambdaQueryWrapper<SysRoleMenu>().in(SysRoleMenu::getRoleId, idList));
        }

        this.removeByIds(idList);
        if (sb.length() > 0) {
            return JsonResult.failureResult("[" + sb + "] has been used by the user and cannot be deleted!");
        } else {
            return JsonResult.successResult();
        }
    }

    @Override
    public JsonResult export(SysRoleCondition sysRoleCondition) {
        PageParam<SysRole, SysRoleCondition> pageParam = new PageParam<>();
        pageParam.setPageSize(ExcelUtil.LIMITT);
        pageParam.setSearchParam(sysRoleCondition);

        JsonResult<PageVO<RoleVO>> jsonResult = this.queryByCondition(pageParam);
        PageVO<RoleVO> data = jsonResult.getData();
        List<ExportRoleVO> exportUserVOList = new ArrayList<>(data.getContent().size());
        for (RoleVO roleVO : data.getContent()) {
            exportUserVOList.add(new ExportRoleVO(roleVO));
        }

        DataSavePathEnum.EXCEL_TMP.mkdirs();
        String filePath = String.format("%s%s_%s%s", DataSavePathEnum.EXCEL_TMP.getPath(), OperationModel.SYS_ROLE, DateUtil.getDetailTimeIgnoreUnit(), ExcelTypeEnum.XLSX.getValue());
        Object url = GlobalProperties.getVirtualPathURL() + StringUtil.replace(filePath, GlobalProperties.getRootPath(), "");
        ExcelUtil.exportData(ExportRoleVO.class, OperationModel.SYS_ROLE, exportUserVOList, filePath);
        return JsonResult.successResult(url);
    }
}
