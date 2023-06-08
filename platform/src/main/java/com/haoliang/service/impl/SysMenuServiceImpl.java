package com.haoliang.service.impl;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.haoliang.common.constant.CacheKeyPrefixConstants;
import com.haoliang.common.enums.ReturnMessageEnum;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.util.JwtTokenUtil;
import com.haoliang.common.enums.RoleTypeEnum;
import com.haoliang.mapper.SysMenuMapper;
import com.haoliang.mapper.SysRoleMenuMapper;
import com.haoliang.model.SysMenu;
import com.haoliang.model.dto.MenuIdDTO;
import com.haoliang.model.vo.MenuVO;
import com.haoliang.model.vo.RouterVO;
import com.haoliang.service.SysMenuService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.cache.annotation.CacheConfig;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.List;

/**
 * @Description
 * @date 2021-04-15 14:53
 */
@Slf4j
@Service
@CacheConfig(cacheNames = CacheKeyPrefixConstants.SYS_MENU)
public class SysMenuServiceImpl extends ServiceImpl<SysMenuMapper, SysMenu> implements SysMenuService {

    @Resource
    private SysRoleMenuMapper sysRoleMenuMapper;

    //@Cacheable(key = "'findAll'")
    @Override
    public JsonResult findAll() {
        return JsonResult.successResult(this.baseMapper.findAllByParentIdOrderBySortIndexAsc());
    }

    @Override
    public JsonResult<RouterVO> findAllByRoleId(Integer roleId) {
        return JsonResult.successResult(findRouterByRoleId(roleId));
    }

    public RouterVO findRouterByRoleId(Integer roleId) {
        RouterVO routerVO = new RouterVO();
        List<MenuVO> sysMenus;
        List<String> authorityList;
        if (roleId.equals(RoleTypeEnum.ADMIN.getId())) {
            sysMenus = this.baseMapper.findAllByParentIOrderBySortIndexAsc();
            authorityList = this.baseMapper.findAllAuthority();
        } else {
            sysMenus = this.baseMapper.findAllByParentIdAndRoleIdOrderBySortIndexAsc(roleId);
            authorityList = this.baseMapper.findAllAuthorityByRoleId(roleId);
        }
        routerVO.setMenuList(sysMenus);
        routerVO.setAuthorityList(authorityList);
        return routerVO;
    }


    @Override
    public JsonResult reloadMenu(List<SysMenu> sysMenuList, String token) {
        SysMenu curSysMenu, subSysMenu;
        List<SysMenu> saveSysMenuList = new ArrayList<>();
        for (int i = 1; i <= sysMenuList.size(); i++) {
            curSysMenu = sysMenuList.get(i - 1);
            curSysMenu.setParentId(0);
            curSysMenu.setSortIndex(i);
            saveSysMenuList.add(curSysMenu);
            for (int j = 1; j <= curSysMenu.getChildren().size(); j++) {
                subSysMenu = curSysMenu.getChildren().get(j - 1);
                subSysMenu.setSortIndex(j);
                subSysMenu.setParentId(curSysMenu.getId());
                saveSysMenuList.add(subSysMenu);
            }
        }
        this.saveOrUpdateBatch(saveSysMenuList);
        return this.findAllByRoleId(JwtTokenUtil.getRoleIdFromToken(token));
    }


    @Override
    @CacheEvict(allEntries = true)
    public JsonResult deleteByIdList(List<Integer> idList) {
        Integer id = idList.get(0);
        Integer existsId = sysRoleMenuMapper.findAllByMenuIdIn(id);
        //只能删除单个
        if (existsId != null) {
            return JsonResult.failureResult(ReturnMessageEnum.MENU_EXISTS_USE);
        } else {
            //根据id查询子菜单Id
            List<MenuIdDTO> menuIdDTOList = this.baseMapper.findIdByParentId(id);
            List<Integer> allId = new ArrayList<>();
            allId.add(id);
            for (MenuIdDTO menuIdDTO : menuIdDTOList) {
                recursionId(menuIdDTO, allId);
            }
            this.removeByIds(allId);
            return JsonResult.successResult();
        }

    }

    /**
     * 递归获取树节点的所有Id
     *
     * @param menuIdDTO 当前节点
     * @param allId     id存储容器
     */
    private void recursionId(MenuIdDTO menuIdDTO, List<Integer> allId) {
        allId.add(menuIdDTO.getId());
        for (MenuIdDTO child : menuIdDTO.getChildren()) {
            recursionId(child, allId);
        }
    }

    @Override
    @CacheEvict(allEntries = true)
    public JsonResult saveMenu(SysMenu sysMenu) {
        return JsonResult.build(this.saveOrUpdate(sysMenu));
    }

    @Override
    public JsonResult getTree() {
        return JsonResult.successResult(this.baseMapper.getTree());
    }

    //@Cacheable(key = "'authority:' +#roleId")
    @Override
    public List<String> findAuthorityByRoleId(Integer roleId) {
        List<String> authorityList;
        if (roleId.equals(RoleTypeEnum.ADMIN.getId())) {
            authorityList = this.baseMapper.findAllAuthority();
        } else {
            authorityList = this.baseMapper.findAllAuthorityByRoleId(roleId);
        }
        return authorityList;
    }

    @Override
    @CacheEvict(allEntries = true)
    public void updateRoleMenu() {
        log.info("reload  authority cache");
    }
}
