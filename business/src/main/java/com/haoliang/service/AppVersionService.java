package com.haoliang.service;

import com.baomidou.mybatisplus.extension.service.IService;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.model.dto.IntIdListDTO;
import com.haoliang.model.AppVersions;
import com.haoliang.model.dto.CheckVersionDTO;
import com.haoliang.model.vo.CheckVersionVO;

public interface AppVersionService extends IService<AppVersions> {

    JsonResult active(Integer id);

    JsonResult deleteByIdList(IntIdListDTO intIdListDTO);

    JsonResult<CheckVersionVO> checkVersion(CheckVersionDTO checkVersionDTO);
}
