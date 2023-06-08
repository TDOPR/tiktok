package com.haoliang.model.vo;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Data;

import java.util.List;

/**
 * @author Dominick Li
 * @Description 角色拥有的菜单模型
 * @CreateTime 2022/11/3 16:53
 **/
@Data
public class MenuVO {

    /**
     * 菜单Id
     */
    private Integer id;

    /**
     * 访问路径
     */
    private String path;

    /**
     * 菜单名称
     */
    private String title;

    /**
     * 菜单图标样式
     */
    @JsonInclude(JsonInclude.Include.NON_EMPTY)
    private String icon;

    /**
     * 组件路径
     */
    private String importStr;

    /**
     * 菜单类型  1=目录,2=菜单,3=权限
     */
    private Integer type;

    /**
     * 是否外链 1=外链,0=内部菜单
     */
    private Integer outlink;

    /**
     * 子菜单列表
     */
    @JsonInclude(JsonInclude.Include.NON_EMPTY)
    private List<MenuVO> children;


}

