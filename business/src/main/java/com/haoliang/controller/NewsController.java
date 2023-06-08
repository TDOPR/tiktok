package com.haoliang.controller;

import com.haoliang.common.annotation.OperationLog;
import com.haoliang.common.constant.OperationAction;
import com.haoliang.common.constant.OperationModel;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.model.PageParam;
import com.haoliang.common.model.dto.IntIdListDTO;
import com.haoliang.common.model.dto.UpdateStatusDTO;
import com.haoliang.constant.TiktokConfig;
import com.haoliang.model.News;
import com.haoliang.model.condition.NewsCondition;
import com.haoliang.model.dto.NewsDTO;
import com.haoliang.service.ArticleService;
import com.haoliang.service.NewsService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

/**
 * @author Dominick Li
 * @Description 文章管理旧版
 * @CreateTime 2023/3/2 18:37
 **/
@RestController
@RequestMapping("/news")
public class NewsController {

    @Autowired
    private NewsService newsService;

    @Autowired
    private ArticleService articleService;

    /**
     * 分页查询
     */
    @PostMapping("/pagelist")
    @PreAuthorize("hasAuthority('publish:article:list')")
    public JsonResult pageList(@RequestBody PageParam<News, NewsCondition> pageParam) {
        if (pageParam.getSearchParam() == null) {
            pageParam.setSearchParam(new NewsCondition());
        }
        pageParam.getSearchParam().setType(TiktokConfig.NEWS_TYPE);
        return newsService.getPageByZN(pageParam);
    }

    /**
     * 根据id获取新闻内容
     */
    @GetMapping("/getTextInfo/{id}")
    public JsonResult getById(@PathVariable Integer id) {
        return newsService.getListById(id);
    }

    /**
     * 上传banner图
     */
    @PostMapping("/uploadBanner")
    public JsonResult uploadBanner(MultipartFile file) {
        return newsService.uploadBanner(file);
    }

    /**
     * 添加或修改新闻内容
     */
    @PostMapping
    @PreAuthorize("hasAnyAuthority('publish:article:add','publish:article:edit')")
    public JsonResult saveOrUpdate(@RequestBody NewsDTO newsDTO) {
        return newsService.saveOrUpdate(newsDTO);
    }

    /**
     * 批量删除
     */
    @PostMapping("/delete")
    @OperationLog(module = OperationModel.ARTICLE, description = OperationAction.REMOVE)
    @PreAuthorize("hasAuthority('publish:article:remove')")
    public JsonResult delete(@RequestBody IntIdListDTO intIdListDTO) {
        return newsService.delete(intIdListDTO);
    }

    /**
     * 发布或取消发布
     */
    @PostMapping("/editStatus")
    @OperationLog(module = OperationModel.ARTICLE, description = OperationAction.ENABLED_OR_DIABLED)
    @PreAuthorize("hasAnyAuthority('publish:article:show','publish:article:hide')")
    public JsonResult editStatus(@RequestBody UpdateStatusDTO updateStatusDTO) {
        return newsService.editStatus(updateStatusDTO);
    }

    /**
     * 获取文本内容和相关报道
     */
    @GetMapping("/getInfoById/{id}")
    public JsonResult getInfo(@PathVariable Integer id) {
        return articleService.getInfoAndRandomLimit(id);
    }

}
