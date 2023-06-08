package com.haoliang.model.vo;

import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Data;

import java.time.LocalDateTime;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/11/22 11:15
 **/
@Data
public class SysNoticeVO {

    private Integer id;

    private String title;

    private String description;

    @JsonFormat(pattern = "yyyy-MM-dd")
    private LocalDateTime createTime;
    //23490
}
