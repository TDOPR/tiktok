package com.haoliang.model.dto;

import lombok.Data;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/4/11 15:32
 **/
@Data
public class TiktokTaskDTO {

    private Integer userId;

    private String imgUrl;

    private Integer built;

    private Integer status;
}
