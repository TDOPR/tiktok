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
import com.haoliang.model.Video;
import com.haoliang.model.condition.VideoCondition;
import com.haoliang.model.vo.VideoVO;
import com.haoliang.service.VideoService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

/**
 * @author Dominick Li
 * @Description 相关视频管理
 * @CreateTime 2023/5/10 10:28
 **/
@RequestMapping("/video")
@RestController
public class VideoController {

    @Autowired
    private VideoService videoService;

    /**
     * 分页查询
     */
    @PostMapping("/pagelist")
    @PreAuthorize("hasAuthority('publish:video:list')")
    public JsonResult pageList(@RequestBody PageParam<Video, VideoCondition> pageParam) {
        return videoService.pageList(pageParam);
    }

    /**
     * 添加或修改
     * @param id       唯一标识
     * @param title    标题
     * @param language 语种
     * @param file     视频
     * @return
     */
    @PostMapping
    @PreAuthorize("hasAnyAuthority('publish:video:add','publish:video:edit')")
    public JsonResult<PageVO<Video>> saveOrUpdate(@RequestParam(required = false) Integer id,
                                                  @RequestParam String title, @RequestParam Integer language,
                                                  @RequestParam(required = false) MultipartFile file,
                                                  @RequestParam(required = false) MultipartFile img) {
        return videoService.saveAndUpload(id, title, language, file, img);
    }

    /**
     * 批量删除
     */
    @PostMapping("/delete")
    @OperationLog(module = OperationModel.ARTICLE, description = OperationAction.REMOVE)
    //@PreAuthorize("hasAuthority('publish:video:remove')")
    public JsonResult delete(@RequestBody IntIdListDTO intIdListDTO) {
        return videoService.delete(intIdListDTO);
    }

    /**
     * 修改视频状态
     */
    @PostMapping("/editStatus")
    @OperationLog(module = OperationModel.ARTICLE, description = OperationAction.ENABLED_OR_DIABLED)
    @PreAuthorize("hasAnyAuthority('publish:video:show','publish:video:hide')")
    public JsonResult editStatus(@RequestBody UpdateStatusDTO updateStatusDTO) {
        return videoService.editStatus(updateStatusDTO);
    }

    /**
     * 获取客户端相关视频分页数据
     */
    @PostMapping("/list")
    public JsonResult<PageVO<VideoVO>> findMyNoticeList(@RequestBody PageDTO pageDTO) {
        return videoService.clientPage(pageDTO);
    }

}
