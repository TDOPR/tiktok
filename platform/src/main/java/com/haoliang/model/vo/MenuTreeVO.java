package com.haoliang.model.vo;

import lombok.Data;

import java.util.List;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/11/8 17:06
 **/
@Data
public class MenuTreeVO {

    /**
     * 菜单id
     */
    private Integer id;

    /**
     * 菜单名称
     */
    private String title;

    /**
     * 子菜单
     */
    private List<MenuTreeVO> children;

}
