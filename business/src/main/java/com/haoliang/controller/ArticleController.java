package com.haoliang.controller;

import com.haoliang.common.annotation.OperationLog;
import com.haoliang.common.constant.OperationAction;
import com.haoliang.common.constant.OperationModel;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.model.PageParam;
import com.haoliang.common.model.dto.IntIdListDTO;
import com.haoliang.common.model.dto.UpdateStatusDTO;
import com.haoliang.model.Article;
import com.haoliang.model.condition.ArticleCondition;
import com.haoliang.model.vo.ArticleInfoVO;
import com.haoliang.service.ArticleService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

/**
 * @author Dominick Li
 * @Description 文章管理
 * @CreateTime 2023/5/8 10:43
 **/
@RequestMapping("/article")
@RestController
public class ArticleController {

    @Autowired
    private ArticleService articleService;

    /**
     * 查看文章列表
     */
    @PostMapping("/pagelist")
    @PreAuthorize("hasAuthority('publish:article:list')")
    public JsonResult pageList(@RequestBody PageParam<Article, ArticleCondition> pageParam) {
        return articleService.pageList(pageParam);
    }

    /**
     * 查看文章文本内容
     */
    @GetMapping("/getTextInfo/{id}")
    public JsonResult<ArticleInfoVO> getById(@PathVariable Integer id) {
        return articleService.getInfoById(id);
    }

    /**
     * 添加或修改
     *
     * @param id          唯一标识
     * @param language    语种
     * @param file        封面图
     * @param title       标题
     * @param description 简介
     * @param info        内容
     * @param source      来源
     * @return
     */
    @PostMapping
    @PreAuthorize("hasAnyAuthority('publish:article:add','publish:article:edit')")
    public JsonResult saveOrUpdate(@RequestParam(required = false) Integer id, @RequestParam Integer language,
                                   @RequestParam(required = false) MultipartFile file,
                                   @RequestParam String title, @RequestParam String description,
                                   @RequestParam String info, @RequestParam String source) {
        return articleService.saveAndUpload(id, file, title, description, info, source, language);
    }

    /**
     * 批量删除
     */
    @PostMapping("/delete")
    @OperationLog(module = OperationModel.ARTICLE, description = OperationAction.REMOVE)
    @PreAuthorize("hasAuthority('publish:article:remove')")
    public JsonResult delete(@RequestBody IntIdListDTO intIdListDTO) {
        return articleService.delete(intIdListDTO);
    }

    /**
     * 修改文章状态
     */
    @PostMapping("/editStatus")
    @OperationLog(module = OperationModel.ARTICLE, description = OperationAction.ENABLED_OR_DIABLED)
    @PreAuthorize("hasAnyAuthority('publish:article:show','publish:article:hide')")
    public JsonResult editStatus(@RequestBody UpdateStatusDTO updateStatusDTO) {
        return articleService.editStatus(updateStatusDTO);
    }
}
