package com.haoliang.controller;

import com.haoliang.common.annotation.OperationLog;
import com.haoliang.common.constant.OperationAction;
import com.haoliang.common.constant.OperationModel;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.model.PageParam;
import com.haoliang.common.model.dto.IntIdListDTO;
import com.haoliang.common.model.dto.PageDTO;
import com.haoliang.common.model.dto.UpdateStatusDTO;
import com.haoliang.common.model.vo.PageVO;
import com.haoliang.constant.TiktokConfig;
import com.haoliang.model.News;
import com.haoliang.model.condition.NewsCondition;
import com.haoliang.model.dto.NewsDTO;
import com.haoliang.model.vo.SysNoticeVO;
import com.haoliang.service.NewsService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;


/**
 * @author Dominick Li
 * @Description 公告管理
 * @CreateTime 2023/3/31 18:30
 **/
@RestController
@RequestMapping("/notice")
public class SysNoticeController {

    @Autowired
    private NewsService newsService;

    /**
     * 分页
     */
    @PostMapping("/pagelist")
    @PreAuthorize("hasAuthority('publish:notice:list')")
    public JsonResult pageList(@RequestBody PageParam<News, NewsCondition> pageParam) {
        if (pageParam.getSearchParam() == null) {
            pageParam.setSearchParam(new NewsCondition());
        }
        pageParam.getSearchParam().setType(TiktokConfig.NOTICE_TYPE);
        return newsService.getPageByZN(pageParam);
    }

    /**
     * 根据id查看内容
     */
    @GetMapping("/getTextInfo/{id}")
    public JsonResult getById(@PathVariable Integer id) {
        return newsService.getListById(id);
    }

    /**
     * 添加或修改
     */
    @PostMapping
    @PreAuthorize("hasAnyAuthority('publish:notice:add','publish:notice:edit')")
    public JsonResult saveOrUpdate(@RequestBody NewsDTO newsDTO) {
        return newsService.saveOrUpdateNotice(newsDTO);
    }

    /**
     * 删除
     */
    @PostMapping("/delete")
    @OperationLog(module = OperationModel.NOTICE, description = OperationAction.REMOVE)
    @PreAuthorize("hasAuthority('publish:notice:remove')")
    public JsonResult delete(@RequestBody IntIdListDTO intIdListDTO) {
        return newsService.deleteNotice(intIdListDTO);
    }

    /**
     * 发布和取消发布
     */
    @PostMapping("/editStatus")
    @OperationLog(module = OperationModel.NOTICE, description = OperationAction.ENABLED_OR_DIABLED)
    @PreAuthorize("hasAnyAuthority('publish:notice:show','publish:notice:hide')")
    public JsonResult editStatus(@RequestBody UpdateStatusDTO updateStatusDTO) {
        return newsService.editStatus(updateStatusDTO);
    }

    /**
     * 获取公告内容
     */
    @GetMapping("/{id}")
    public JsonResult getTextById(@PathVariable Integer id) {
        return newsService.getTextById(id);
    }

    /**
     * 根据用户Id获取公告列表
     */
    @PostMapping("/mylist")
    public JsonResult<PageVO<SysNoticeVO>> findMyNoticeList(@RequestBody PageDTO pageDTO) {
        return newsService.findMyNoticeList(pageDTO);
    }

    /**
     * 删除用户关联的公告消息
     */
    @DeleteMapping("/{id}")
    public JsonResult deleteUserNoticeById(@PathVariable Integer id) {
        return newsService.deleteUserNoticeById(id);
    }

    /**
     * 设置强制通知
     */
    @PostMapping("/force")
    public JsonResult force(@RequestBody UpdateStatusDTO updateStatusDTO){
        return newsService.forceNotice(updateStatusDTO);
    }

}
