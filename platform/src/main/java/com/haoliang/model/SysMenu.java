package com.haoliang.model;

import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.haoliang.common.base.BaseModelCID;
import lombok.Data;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.util.List;


/**
 * @author Dominick Li
 * @description 系统菜单表
 **/
@Data
@TableName("sys_menu")
public class SysMenu extends BaseModelCID {

    /**
     * 访问路径
     *
     * @required
     */
    @NotEmpty
    private String path;

    /**
     * 菜单名称
     *
     * @required
     */
    @NotEmpty
    private String title;

    /**
     * 菜单图标样式
     *
     * @required
     */
    @NotEmpty
    private String icon;

    /**
     * 父菜单编号
     */
    @JsonFormat(shape = JsonFormat.Shape.STRING)
    private Integer parentId;

    /**
     * 显示顺序
     *
     * @required
     */
    @NotNull
    private Integer sortIndex = 1;

    /**
     * 组件路径
     *
     * @required
     */
    @NotEmpty
    private String importStr;

    /**
     * 菜单类型  1=目录,2=菜单,3=权限
     *
     * @required
     */
    @NotNull
    private Integer type;

    /**
     * 是否外链 1=外链,0=内部菜单
     */
    private Integer outlink;

    /**
     * 显示状态 1=可见,0=隐藏
     */
    private Integer display;

    /**
     * 权限字符
     */
    private String authorityStr;

    /**
     * 子菜单列表
     */
    @TableField(exist = false)
    private List<SysMenu> children;

}
