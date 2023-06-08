package com.haoliang.model.vo;

import lombok.Data;

import java.util.List;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/3/28 10:27
 **/
@Data
public class NewsInfoVO {
    /**
     * 富文本内容
     */
    private String info;
    /**
     * 相关推荐
     */
    private List<NewsVO> list;
}
