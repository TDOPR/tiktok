package com.haoliang.model.vo;

import com.haoliang.model.News;
import lombok.Data;
import org.springframework.beans.BeanUtils;

import java.time.LocalDateTime;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/5/8 10:50
 **/
@Data
public class ArticleVO {

    private Integer id;

    private LocalDateTime createTime;

    private LocalDateTime lastmodifiedTime;

    private Integer enabled;

    private String title;

    private String bannerUrl;

}
