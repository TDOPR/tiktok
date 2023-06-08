package com.haoliang.model.vo;

import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Data;

import java.time.LocalDateTime;

/**
 * @author Dominick Li
 * @Description 强制通知返回的内容
 * @CreateTime 2022/11/22 11:15
 **/
@Data
public class HomeNoticeVO {

    private Integer id;

    private String title;

    private String info;

    @JsonFormat(pattern = "yyyy-MM-dd HH:mm")
    private LocalDateTime createTime;
}
