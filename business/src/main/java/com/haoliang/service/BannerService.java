package com.haoliang.service;

import com.baomidou.mybatisplus.extension.service.IService;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.model.PageParam;
import com.haoliang.common.model.dto.LongIdListDTO;
import com.haoliang.common.model.vo.PageVO;
import com.haoliang.model.Banner;
import com.haoliang.model.condition.BannerCondition;
import com.haoliang.model.vo.BannerVO;
import org.springframework.web.multipart.MultipartFile;

public interface BannerService extends IService<Banner> {

    JsonResult deleteByIdList(LongIdListDTO longIdListDTO);

    JsonResult saveAndUpload(Long id,MultipartFile zhFile,MultipartFile enFile,MultipartFile inFile,MultipartFile thFile,MultipartFile viFile, String name, Integer sortIndex, Integer enabled);

    JsonResult<PageVO<BannerVO>> myPage(PageParam<Banner, BannerCondition> pageParam);
}
