package com.haoliang.model.dto;

import lombok.Data;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/11/25 11:02
 **/
@Data
public class AppUserTreeDTO {

    /**
     * 当前节点Id
     */
    private Integer cid;

    /**
     * 父节点Id
     */
    private Integer pid;

}
