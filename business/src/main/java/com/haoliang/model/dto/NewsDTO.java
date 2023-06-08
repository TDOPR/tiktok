package com.haoliang.model.dto;

import lombok.Data;

import java.util.List;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/3/3 10:00
 **/
@Data
public class NewsDTO {
    /**
     * 文章Id
     */
    private Integer id;
    /**
     * 新闻的背景图
     */
    private String filePath;
    /**
     * 每个国际化语种对应一条数据
     */
    private List<NewsDetailsDTO> list;

}
