package com.haoliang.service;

import com.baomidou.mybatisplus.extension.service.IService;
import com.haoliang.common.base.DefaultCondition;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.model.PageParam;
import com.haoliang.common.model.dto.IntIdListDTO;
import com.haoliang.common.model.dto.PageDTO;
import com.haoliang.common.model.dto.TypeDTO;
import com.haoliang.common.model.dto.UpdateStatusDTO;
import com.haoliang.common.model.vo.PageVO;
import com.haoliang.model.News;
import com.haoliang.model.condition.NewsCondition;
import com.haoliang.model.dto.NewsDTO;
import com.haoliang.model.vo.SysNoticeVO;
import org.springframework.web.multipart.MultipartFile;

public interface NewsService extends IService<News>  {
    /**
     * 查找中文的国际化新闻
     * @param pageParam
     */
    JsonResult getPageByZN(PageParam<News, NewsCondition> pageParam);

    JsonResult saveOrUpdate(NewsDTO newsDTO);

    JsonResult getInfoById(Integer id);

    JsonResult uploadBanner(MultipartFile file);

    JsonResult delete(IntIdListDTO intIdListDTO);

    JsonResult editStatus(UpdateStatusDTO updateStatusDTO);

    JsonResult saveOrUpdateNotice(NewsDTO newsDTO);

    JsonResult getTextById(Integer id);

    JsonResult<PageVO<SysNoticeVO>> findMyNoticeList(PageDTO pageDTO);

    JsonResult deleteUserNoticeById(Integer id);

    JsonResult deleteNotice(IntIdListDTO intIdListDTO);

    JsonResult getListById(Integer id);

    JsonResult forceNotice(UpdateStatusDTO updateStatusDTO);
}
