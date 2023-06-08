package com.haoliang.model.dto;

import lombok.Data;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/11/11 14:49
 **/
@Data
public class AuditCheckDTO {

    /**
     * 审核流水Id
     */
    private Long id;

    /**
     * 审核状态
     */
    private Integer state;

}
