package com.haoliang.controller;

import com.haoliang.common.model.JsonResult;
import com.haoliang.common.model.PageParam;
import com.haoliang.common.model.dto.LongIdListDTO;
import com.haoliang.common.model.vo.PageVO;
import com.haoliang.model.Banner;
import com.haoliang.model.condition.BannerCondition;
import com.haoliang.model.vo.BannerVO;
import com.haoliang.service.BannerService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

/**
 * @author Dominick Li
 * @Description 客户端轮播图管理
 * @CreateTime 2023/1/8 15:31
 **/
@RestController
@RequestMapping("/banner")
public class BannerController {

    @Autowired
    private BannerService bannerService;

    /**
     * 分页查询
     */
    @PostMapping("/pagelist")
    @PreAuthorize("hasAuthority('publish:banner:list')")
    public JsonResult<PageVO<BannerVO>> pageList(@RequestBody PageParam<Banner, BannerCondition> pageParam) {
        return bannerService.myPage(pageParam);
    }

    /**
     * 上传banner图信息
     *
     * @param zhFile    图片
     * @param name      名称
     * @param sortIndex 排序
     * @return
     */
    @PostMapping("/upload")
    @PreAuthorize("hasAnyAuthority('publish:banner:add','publish:banner:edit')")
    public JsonResult upload(@RequestParam(required = false) Long id,
                             @RequestParam(value = "zhFile", required = false) MultipartFile zhFile,
                             @RequestParam(value = "enFile", required = false) MultipartFile enFile,
                             @RequestParam(value = "inFile", required = false) MultipartFile inFile,
                             @RequestParam(value = "thFile", required = false) MultipartFile thFile,
                             @RequestParam(value = "viFile", required = false) MultipartFile viFile,
                             @RequestParam String name, @RequestParam Integer sortIndex, @RequestParam Integer enabled) {
        return bannerService.saveAndUpload(id, zhFile,enFile,inFile,thFile,viFile, name, sortIndex, enabled);
    }

    /**
     * 删除banner图
     */
    @PostMapping("/delete")
    @PreAuthorize("hasAuthority('publish:banner:remove')")
    public JsonResult deleteByIdList(@RequestBody LongIdListDTO longIdListDTO) {
        return bannerService.deleteByIdList(longIdListDTO);
    }
}
