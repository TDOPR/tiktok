package com.haoliang.service.impl;

import cn.hutool.core.io.FileUtil;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.haoliang.common.enums.DataSavePathEnum;
import com.haoliang.common.enums.LanguageEnum;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.model.PageParam;
import com.haoliang.common.model.dto.LongIdListDTO;
import com.haoliang.common.model.vo.PageVO;
import com.haoliang.common.util.IdUtil;
import com.haoliang.mapper.BannerMapper;
import com.haoliang.model.Banner;
import com.haoliang.model.condition.BannerCondition;
import com.haoliang.model.vo.BannerVO;
import com.haoliang.service.BannerService;
import org.apache.commons.io.FileUtils;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/1/9 10:20
 **/
@Service
public class BannerServiceImpl extends ServiceImpl<BannerMapper, Banner> implements BannerService {

    @Override
    public JsonResult deleteByIdList(LongIdListDTO longIdListDTO) {
        for (Long id : longIdListDTO.getIdList()) {
            FileUtil.del(new File(DataSavePathEnum.BANNER.getPath() + id));
        }
        this.removeByIds(longIdListDTO.getIdList());
        return JsonResult.successResult();
    }

    @Override
    public JsonResult saveAndUpload(Long id, MultipartFile zhFile, MultipartFile enFile, MultipartFile inFile, MultipartFile thFile, MultipartFile viFile, String name, Integer sortIndex, Integer enabled) {
        try {
            Banner banner;
            if (id == null) {
                banner = new Banner();
                banner.setId(IdUtil.getSnowflakeNextId());
            } else {
                banner = this.getById(id);
            }
            banner.setName(name);
            banner.setSortIndex(sortIndex);
            banner.setEnabled(enabled);
            if (zhFile != null) {
                banner.setZhPath(getSavePath(banner.getZhPath(), banner.getId(), LanguageEnum.ZH_CN.getName(), zhFile));
            }
            if (enFile != null) {
                banner.setEnPath(getSavePath(banner.getEnPath(), banner.getId(), LanguageEnum.EN_US.getName(), enFile));
            }
            if (inFile != null) {
                banner.setInPath(getSavePath(banner.getInPath(), banner.getId(), LanguageEnum.IN_ID.getName(), inFile));
            }
            if (thFile != null) {
                banner.setThPath(getSavePath(banner.getThPath(), banner.getId(), LanguageEnum.TH_TH.getName(), thFile));
            }
            if (viFile != null) {
                banner.setViPath(getSavePath(banner.getViPath(), banner.getId(), LanguageEnum.VI_VN.getName(), viFile));
            }
            this.saveOrUpdate(banner);
            return JsonResult.successResult();
        } catch (Exception e) {
            log.error("banner: saveAndUpload error:{}", e);
            return JsonResult.failureResult();
        }
    }

    /**
     * 保存图片
     */
    private String getSavePath(String oldPath, Long id, String language, MultipartFile file) throws Exception {
        FileUtil.del(oldPath);
        //获取文件后缀
        String suffix = FileUtil.getSuffix(file.getOriginalFilename());
        String saveFileName = language + "." + suffix;
        String savePath = DataSavePathEnum.BANNER.getPath() + id.toString() + "/" + saveFileName;
        File saveFile = new File(savePath);
        //复制文件流到本地文件
        FileUtils.copyInputStreamToFile(file.getInputStream(), saveFile);
        return savePath;
    }


    @Override
    public JsonResult<PageVO<BannerVO>> myPage(PageParam<Banner, BannerCondition> pageParam) {
        if (pageParam.getSearchParam() == null) {
            pageParam.setSearchParam(new BannerCondition());
        }
        Page<Banner> bannerPage = this.page(pageParam.getPage(), pageParam.getSearchParam().buildQueryParam());
        List<BannerVO> bannerVOList = new ArrayList<>();
        for (Banner banner : bannerPage.getRecords()) {
            bannerVOList.add(new BannerVO(banner));
        }
        return JsonResult.successResult(new PageVO<>(bannerPage.getTotal(), bannerPage.getPages(), bannerVOList));
    }
}
