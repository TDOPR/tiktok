package com.haoliang.service;

import com.baomidou.mybatisplus.extension.service.IService;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.model.PageParam;
import com.haoliang.common.model.dto.PageDTO;
import com.haoliang.common.model.dto.TypeDTO;
import com.haoliang.common.model.vo.PageVO;
import com.haoliang.model.TiktokTask;
import com.haoliang.model.condition.TiktokTaskCondition;
import com.haoliang.model.dto.ForceDeleteDTO;
import com.haoliang.model.dto.PublishTiktokTask;
import com.haoliang.model.vo.MyTiktokTaskVO;
import com.haoliang.model.vo.PublishTaskVO;
import com.haoliang.model.vo.TaskNumVO;

import java.util.List;

public interface TiktokTaskService  extends IService<TiktokTask> {

    JsonResult<PublishTaskVO> getPublishList(TypeDTO pageDTO);

    JsonResult<PageVO<MyTiktokTaskVO>> getMyPublishList(PageDTO pageDTO);

    JsonResult getPricesAndBalance();

    JsonResult buyPackage(Integer id);

    JsonResult saveAndPublish(PublishTiktokTask publishTiktokTask);

    JsonResult<TaskNumVO> getCountPackageBalance();

    JsonResult pagelist(PageParam<TiktokTask, TiktokTaskCondition> pageParam);

    JsonResult addOrEdit(TiktokTask tiktokTask);

    JsonResult deleteByIdList(ForceDeleteDTO forceDeleteDTO);

}
