package com.haoliang.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.haoliang.model.Article;
import com.haoliang.model.vo.NewsVO;

import java.util.List;

public interface ArticleMapper  extends BaseMapper<Article> {

    List<NewsVO> randomLimit(int id, int language, int limitSize);

}
