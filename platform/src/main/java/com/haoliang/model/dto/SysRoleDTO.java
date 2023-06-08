package com.haoliang.model.dto;

import com.haoliang.model.SysRole;
import lombok.Data;

import java.util.List;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/10/27 10:12
 **/
@Data
public class SysRoleDTO extends SysRole {

    /**
     * 角色关联的菜单Id数组
     */
    private List<Integer> menuIds;

    /**
     * 半选的菜单
     */
    private List<Integer> semiMenuIds;

}
