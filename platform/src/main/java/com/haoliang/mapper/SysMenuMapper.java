package com.haoliang.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.haoliang.model.SysMenu;
import com.haoliang.model.dto.MenuIdDTO;
import com.haoliang.model.vo.MenuTreeVO;
import com.haoliang.model.vo.MenuVO;
import org.apache.ibatis.annotations.Param;

import java.util.List;

public interface SysMenuMapper extends BaseMapper<SysMenu> {

    List<SysMenu> findAllByParentIdOrderBySortIndexAsc();

    List<MenuVO> findAllByParentIdAndRoleIdOrderBySortIndexAsc(@Param("roleId") Integer roleId);

    String findMenuNameById(@Param("id") Integer id);

    Integer findMaxSortIndex();

    List<String> findAllAuthorityByRoleId(@Param("roleId") Integer roleId);

    List<String> findAllAuthority();

    List<MenuVO> findAllByParentIOrderBySortIndexAsc();

    List<MenuIdDTO> findIdByParentId(@Param("parentId") Integer parentId);

    List<MenuTreeVO> getTree();
}
