package com.haoliang.model.vo;

import lombok.Data;

import java.util.List;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/5/12 11:52
 **/
@Data
public class ItemInfoVO {

    /**
     * 上级邮箱
     */
    private String parent;

    /**
     * 下级邮箱
     */
    private List<String> childList;

}
