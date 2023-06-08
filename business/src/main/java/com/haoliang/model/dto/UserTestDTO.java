package com.haoliang.model.dto;

import lombok.Data;

import java.math.BigDecimal;
import java.util.List;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/12/14 10:22
 **/
@Data
public class UserTestDTO {

    private Integer userId;

    private BigDecimal amount=BigDecimal.ZERO;

    private Integer level=0;

    public UserTestDTO(Integer userId) {
        this.userId = userId;
    }
    private List<UserTestDTO> childList;

}
