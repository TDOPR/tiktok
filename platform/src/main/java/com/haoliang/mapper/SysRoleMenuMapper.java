package com.haoliang.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.haoliang.model.SysRoleMenu;
import org.apache.ibatis.annotations.Param;

import java.util.List;

public interface SysRoleMenuMapper extends BaseMapper<SysRoleMenu> {

    Integer findAllByMenuIdIn(@Param("menuId") Integer menuId);

    List<Integer> findAllMenuIdByRoleId(@Param("roleId") Integer id, @Param("checked") Integer checked);

    List<String> findRoleNameByIdIn(@Param("idList") List<Integer> existsId);
}
