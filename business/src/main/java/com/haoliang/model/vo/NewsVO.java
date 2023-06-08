package com.haoliang.model.vo;

import com.haoliang.model.News;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.beans.BeanUtils;

import java.time.LocalDate;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/3/2 16:22
 **/
@Data
@NoArgsConstructor
public class NewsVO {

    public NewsVO(News news) {
        BeanUtils.copyProperties(news, this);
        this.createTime = news.getCreateTime().toLocalDate();
    }

    private Integer id;

    private String title;

    private String source;

    private String description;
    private LocalDate createTime;

    private String bannerUrl;

}
