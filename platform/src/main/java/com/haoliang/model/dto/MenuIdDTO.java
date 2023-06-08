package com.haoliang.model.dto;


import lombok.Data;

import java.util.List;

@Data
public class MenuIdDTO {

    private Integer id;

    private List<MenuIdDTO> children;

}
