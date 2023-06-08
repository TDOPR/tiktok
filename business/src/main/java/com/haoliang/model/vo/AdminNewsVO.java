package com.haoliang.model.vo;


import com.baomidou.mybatisplus.annotation.TableField;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Data;

import java.time.LocalDateTime;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/3/29 18:54
 **/
@Data
public class AdminNewsVO {

    private Integer id;

    private LocalDateTime createTime;

    private LocalDateTime lastmodifiedTime;

    private Integer enabled;

    private String title;

    private Integer forceStatus;

    @JsonInclude(JsonInclude.Include.NON_EMPTY)
    private String bannerUrl;

}
