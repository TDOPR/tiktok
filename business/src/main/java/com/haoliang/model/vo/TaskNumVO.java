package com.haoliang.model.vo;

import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.AllArgsConstructor;
import lombok.Data;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/3/3 17:12
 **/
@Data
@AllArgsConstructor
public class TaskNumVO {

    @JsonFormat(shape = JsonFormat.Shape.STRING)
    private Long totalNum;

    @JsonFormat(shape = JsonFormat.Shape.STRING)
    private Long hasNum;

}
