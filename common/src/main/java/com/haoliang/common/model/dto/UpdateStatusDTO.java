package com.haoliang.common.model.dto;

import lombok.Data;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/10/26 18:34
 **/
@Data
public class UpdateStatusDTO {

    /**
     * 唯一id
     */
    private Integer id;

    /**
     * 状态标识 1=启用 0=禁用
     */
    private Integer enabled;

}
