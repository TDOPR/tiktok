package com.haoliang.model.vo;

import com.haoliang.model.Article;
import com.haoliang.model.News;
import lombok.Data;
import org.springframework.beans.BeanUtils;

import java.time.LocalDate;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/5/8 12:27
 **/
@Data
public class ArticleHomeVO {

    public ArticleHomeVO(Article article) {
        BeanUtils.copyProperties(article, this);
        this.createTime = article.getCreateTime().toLocalDate();
    }

    private Integer id;

    private String title;

    private String source;

    private String description;
    private LocalDate createTime;

    private String bannerUrl;

}
