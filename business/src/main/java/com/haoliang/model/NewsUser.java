package com.haoliang.model;

import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

/**
 * @author Dominick Li
 * @Description 公告和用户之间的引用关系
 * @CreateTime 2022/11/22 10:27
 **/
@Data
@TableName("news_user")
public class NewsUser {

    /**
     * 公告Id
     */
    private Integer newsId;

    /**
     * app用户Id
     */
    private Integer userId;

}
