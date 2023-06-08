package com.haoliang.model.vo;

import lombok.Data;

import java.util.List;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/11/3 14:35
 **/
@Data
public class RouterVO {

    /**
     * 菜单
     */
    private List<MenuVO> menuList;

    /**
     * 权限
     */
    private List<String> authorityList;
}
