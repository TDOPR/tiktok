package com.haoliang.model.dto;

import lombok.Data;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/3/28 15:49
 **/
@Data
public class BatchAddSubUserDTO {

    private Integer userId;

    /**
     * 直推
     */
    private Integer number;

    /**
     * 二代
     */
    private Integer secondNumber;

}
