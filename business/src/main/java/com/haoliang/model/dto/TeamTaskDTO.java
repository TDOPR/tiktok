package com.haoliang.model.dto;

import lombok.Data;

import java.math.BigDecimal;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/3/17 15:51
 **/
@Data
public class TeamTaskDTO {

    private BigDecimal amount;

    private Integer userId;

    private Integer level;

}
