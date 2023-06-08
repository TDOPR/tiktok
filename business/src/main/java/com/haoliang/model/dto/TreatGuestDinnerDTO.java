package com.haoliang.model.dto;

import lombok.Data;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/3/23 18:48
 **/
@Data
public class TreatGuestDinnerDTO {
    private Integer id;
    private int[] itemUserIds;
    private String address;
    private String coinType;
}
