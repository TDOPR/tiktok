package com.haoliang.service;

import com.baomidou.mybatisplus.extension.service.IService;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.model.PageParam;
import com.haoliang.common.model.dto.IntIdListDTO;
import com.haoliang.common.model.dto.PageDTO;
import com.haoliang.common.model.dto.UpdateStatusDTO;
import com.haoliang.model.Video;
import com.haoliang.model.condition.VideoCondition;
import org.springframework.web.multipart.MultipartFile;

public interface VideoService extends IService<Video> {
    
    JsonResult pageList(PageParam<Video, VideoCondition> pageParam);

    JsonResult saveAndUpload(Integer id, String title, Integer language, MultipartFile file,MultipartFile img);

    JsonResult delete(IntIdListDTO intIdListDTO);

    JsonResult editStatus(UpdateStatusDTO updateStatusDTO);

    JsonResult clientPage(PageDTO pageDTO);
}
