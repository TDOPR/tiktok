package com.haoliang.service;

import com.baomidou.mybatisplus.extension.service.IService;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.model.PageParam;
import com.haoliang.common.model.dto.IntIdListDTO;
import com.haoliang.common.model.dto.UpdateStatusDTO;
import com.haoliang.model.Article;
import com.haoliang.model.condition.ArticleCondition;
import com.haoliang.model.vo.ArticleInfoVO;

import org.springframework.web.multipart.MultipartFile;


public interface ArticleService  extends IService<Article> {

    JsonResult pageList(PageParam<Article, ArticleCondition> pageParam);

    JsonResult<ArticleInfoVO> getInfoById(Integer id);

    JsonResult delete(IntIdListDTO intIdListDTO);

    JsonResult editStatus(UpdateStatusDTO updateStatusDTO);

    JsonResult saveAndUpload(Integer id, MultipartFile file, String title, String description, String info,String source, Integer language);

    JsonResult getInfoAndRandomLimit(Integer id);

}
