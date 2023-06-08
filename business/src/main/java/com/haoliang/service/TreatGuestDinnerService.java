package com.haoliang.service;

import com.baomidou.mybatisplus.extension.service.IService;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.model.PageParam;
import com.haoliang.common.model.dto.PageDTO;
import com.haoliang.common.model.vo.PageVO;
import com.haoliang.model.TreatGuestDinner;
import com.haoliang.model.condition.TreatGuestDinnerCondition;
import com.haoliang.model.dto.AuditCheckDTO;
import com.haoliang.model.dto.TreatGuestDinnerDTO;
import com.haoliang.model.vo.TreatGuestDinnerVO;
import org.springframework.web.multipart.MultipartFile;

public interface TreatGuestDinnerService extends IService<TreatGuestDinner> {

    JsonResult<PageVO<TreatGuestDinnerVO>> getMyPage(PageDTO pageDTO);

    JsonResult submitTask(MultipartFile file, MultipartFile img , int[] itemUserIds, String address, String coinType);

    JsonResult edit(Integer id, MultipartFile file, MultipartFile img, int[] itemUserIds, String address, String coinType);

    JsonResult getCheckList(PageParam<TreatGuestDinner, TreatGuestDinnerCondition> pageParam);

    JsonResult editJson(TreatGuestDinnerDTO treatGuestDinnerDTO);

    JsonResult check(AuditCheckDTO auditCheckDTO);
}
