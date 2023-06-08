package com.haoliang.service.impl;

import cn.hutool.core.io.FileUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.haoliang.common.config.GlobalProperties;
import com.haoliang.common.enums.BooleanEnum;
import com.haoliang.common.enums.DataSavePathEnum;
import com.haoliang.common.enums.LanguageEnum;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.model.PageParam;
import com.haoliang.common.model.ThreadLocalManager;
import com.haoliang.common.model.dto.IntIdListDTO;
import com.haoliang.common.model.dto.PageDTO;
import com.haoliang.common.model.dto.UpdateStatusDTO;
import com.haoliang.common.model.vo.PageVO;
import com.haoliang.common.util.ExtractVideoFirstFrameUtil;
import com.haoliang.common.util.StringUtil;
import com.haoliang.mapper.VideoMapper;
import com.haoliang.model.Video;
import com.haoliang.model.condition.VideoCondition;
import com.haoliang.model.vo.VideoVO;
import com.haoliang.service.VideoService;
import org.apache.commons.io.FileUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/5/10 10:26
 **/
@Service
public class VideoServiceImpl extends ServiceImpl<VideoMapper, Video> implements VideoService {

    @Override
    public JsonResult pageList(PageParam<Video, VideoCondition> pageParam) {
        if (pageParam.getSearchParam() == null) {
            pageParam.setSearchParam(new VideoCondition());
        }
        Page<Video> page = this.page(pageParam.getPage(), pageParam.getSearchParam().buildQueryParam());
        return JsonResult.successResult(new PageVO<>(page));
    }

    @Override
    public JsonResult saveAndUpload(Integer id, String title, Integer language, MultipartFile file, MultipartFile img) {
        try {
            Video video;
            DataSavePathEnum dataSavePathEnum = DataSavePathEnum.GUILD_VIDEO;
            if (id == null) {
                if (file == null) {
                    return JsonResult.failureResult("添加必须上传视频！");
                }
                video = new Video();
                video.setLanguage(language);
                this.save(video);
                video.setFolderPath(dataSavePathEnum.getPath() + video.getId() + "/");
            } else {
                video = this.getById(id);
            }
            video.setTitle(title);
            if (file != null) {
                //获取文件后缀
                String saveFileName = video.getId() + "." + FileUtil.getSuffix(file.getOriginalFilename());
                File saveFile = new File(video.getFolderPath(), saveFileName);
                //复制文件流到本地文件
                FileUtils.copyInputStreamToFile(file.getInputStream(), saveFile);
                String url = GlobalProperties.getVirtualPathURL() + StringUtil.replace(video.getFolderPath(), GlobalProperties.getRootPath(), "") + saveFileName;
                video.setPlayUrl(url);
                if (img == null) {
                    saveFileName = video.getId() + ".jpg";
                    boolean flag = ExtractVideoFirstFrameUtil.ffmpegExtractImage(saveFile.getAbsolutePath(), video.getFolderPath() + saveFileName);
                    if (flag) {
                        video.setBannerUrl(GlobalProperties.getVirtualPathURL() + StringUtil.replace(video.getFolderPath(), GlobalProperties.getRootPath(), "") + saveFileName);
                    }
                }
            }
            if (img != null) {
                String saveFileName = video.getId() + "." + FileUtil.getSuffix(img.getOriginalFilename());
                File saveFile = new File(video.getFolderPath(), saveFileName);
                //复制文件流到本地文件
                FileUtils.copyInputStreamToFile(img.getInputStream(), saveFile);
                String url = GlobalProperties.getVirtualPathURL() + StringUtil.replace(video.getFolderPath(), GlobalProperties.getRootPath(), "") + saveFileName;
                video.setBannerUrl(url);
            }
            this.saveOrUpdate(video);
            return JsonResult.successResult();
        } catch (Exception e) {
            log.error("video: saveAndUpload error:{}", e);
            return JsonResult.failureResult();
        }
    }

    @Override
    public JsonResult delete(IntIdListDTO intIdListDTO) {
        List<Video> videoList = this.list(new LambdaQueryWrapper<Video>()
                .select(Video::getFolderPath)
                .in(Video::getId, intIdListDTO.getIdList())
        );
        for (Video video : videoList) {
            //删除banner图片
            FileUtil.del(new File(video.getFolderPath()));
        }
        this.removeBatchByIds(intIdListDTO.getIdList());
        return JsonResult.successResult();
    }

    @Override
    public JsonResult editStatus(UpdateStatusDTO updateStatusDTO) {
        UpdateWrapper<Video> updateWrapper = Wrappers.update();
        updateWrapper.lambda().set(Video::getEnabled, updateStatusDTO.getEnabled())
                .eq(Video::getId, updateStatusDTO.getId());
        this.update(updateWrapper);
        return JsonResult.successResult();
    }

    @Override
    public JsonResult clientPage(PageDTO pageDTO) {
        LanguageEnum languageEnum = LanguageEnum.nameOf(ThreadLocalManager.getLanguage());
        Integer language = languageEnum == LanguageEnum.ZH_CN ? languageEnum.getType() : LanguageEnum.EN_US.getType();
        Page<Video> page = this.page(pageDTO.getPage(), new LambdaQueryWrapper<Video>()
                .eq(Video::getLanguage, language)
                .eq(Video::getEnabled, BooleanEnum.TRUE.intValue())
                .orderByDesc(Video::getCreateTime));
        List<VideoVO> videoList = new ArrayList<>();
        VideoVO videoVO;
        for (Video video : page.getRecords()) {
            videoVO = new VideoVO();
            BeanUtils.copyProperties(video, videoVO);
            videoList.add(videoVO);
        }
        return JsonResult.successResult(new PageVO<>(page.getTotal(), page.getPages(), videoList));
    }

}
