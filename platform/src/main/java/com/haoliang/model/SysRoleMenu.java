package com.haoliang.model;

import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

/**
 * @author Dominick Li
 * @description 角色菜单关联表
 **/
@Data
@TableName("sys_role_menu")
@NoArgsConstructor
public class SysRoleMenu {

    /**
     * 角色编号
     */
    private Integer roleId;

    /**
     * 菜单编号
     */
    private Integer menuId;

    /**
     * 是否选中状态
     */
    private Integer checked;

    @TableField(fill = FieldFill.INSERT)
    private LocalDateTime createTime;

    public SysRoleMenu(Integer roleId, Integer menuId, Integer checked) {
        this.roleId = roleId;
        this.menuId = menuId;
        this.checked = checked;
    }
}
