package com.haoliang.common.model.dto;

import lombok.Data;

import java.util.List;

/**
 * @author Dominick Li
 * @CreateTime 2021/7/12 20:06
 * @description
 **/
@Data
public class IntIdListDTO {

    /**
     * 唯一标识数组
     */
    private List<Integer> idList;
}
