package com.haoliang.service.impl;

import cn.hutool.core.io.FileUtil;
import com.alibaba.fastjson.JSONObject;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.haoliang.common.config.AppParamProperties;
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
import com.haoliang.common.util.IdUtil;
import com.haoliang.common.util.JwtTokenUtil;
import com.haoliang.common.util.StringUtil;
import com.haoliang.constant.TiktokConfig;
import com.haoliang.mapper.NewsMapper;
import com.haoliang.mapper.NewsUserMapper;
import com.haoliang.model.News;
import com.haoliang.model.NewsUser;
import com.haoliang.model.condition.NewsCondition;
import com.haoliang.model.dto.NewsDTO;
import com.haoliang.model.dto.NewsDetailsDTO;
import com.haoliang.model.vo.*;
import com.haoliang.service.NewsService;
import org.apache.commons.io.FileUtils;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import javax.annotation.Resource;
import java.io.File;
import java.util.ArrayList;
import java.util.List;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/3/2 18:38
 **/
@Service
public class NewsServiceImpl extends ServiceImpl<NewsMapper, News> implements NewsService {

    @Resource
    private NewsUserMapper newsUserMapper;

    @Override
    public JsonResult getPageByZN(PageParam<News, NewsCondition> pageParam) {
        Page<News> page = this.baseMapper.page(pageParam.getPage(), LanguageEnum.ZH_CN.getName().toLowerCase(), pageParam.getSearchParam());
        List<AdminNewsVO> adminNewsVOList = new ArrayList<>();
        AdminNewsVO adminNewsVO;
        for (News news : page.getRecords()) {
            adminNewsVO = new AdminNewsVO();
            BeanUtils.copyProperties(news, adminNewsVO);
            adminNewsVOList.add(adminNewsVO);
        }
        return JsonResult.successResult(new PageVO(page.getTotal(), page.getPages(), adminNewsVOList));
    }

    @Override
    @Transactional
    public JsonResult saveOrUpdate(NewsDTO newsDTO) {
        String url = null, filePath = null;
        if (StringUtil.isNotBlank(newsDTO.getFilePath())) {
            File file = new File(newsDTO.getFilePath());
            if (!file.exists()) {
                return JsonResult.failureResult("未找到上传的图片");
            }
            DataSavePathEnum dataSavePathEnum = DataSavePathEnum.BANNER;
            String fileName = IdUtil.simpleUUID() + "." + FileUtil.getSuffix(file);
            File newFile = new File(dataSavePathEnum.getFile(), fileName);
            filePath = newFile.getAbsolutePath();
            FileUtil.move(file, newFile, true);
            url = GlobalProperties.getVirtualPathURL() + StringUtil.replace(dataSavePathEnum.getPath(), GlobalProperties.getRootPath(), "") + fileName;
        }
        //添加文章
        if (newsDTO.getId() == null) {
            News news = new News();
            news.setBannerUrl(url);
            news.setPath(filePath);
            news.setType(TiktokConfig.NEWS_TYPE);
            news.setEnabled(BooleanEnum.TRUE.intValue());
            this.save(news);
            //循环插入多个语种数据
            for (NewsDetailsDTO detail : newsDTO.getList()) {
                detail.setLanguage(detail.getLanguage().toLowerCase());
                this.baseMapper.insertNewsByLanguage(news.getId(), detail);
            }
        } else {
            if (url != null) {
                //修改文章
                UpdateWrapper<News> newsUpdateWrapper = Wrappers.update();
                newsUpdateWrapper.lambda()
                        .set(News::getBannerUrl, url)
                        .set(News::getPath, filePath)
                        .eq(News::getId, newsDTO.getId());
                this.update(newsUpdateWrapper);
            }
            //修改文章内容
            for (NewsDetailsDTO detail : newsDTO.getList()) {
                detail.setLanguage(detail.getLanguage().toLowerCase());
                this.baseMapper.updateNewsByLanguage(newsDTO.getId(), detail);
            }
        }
        return JsonResult.successResult();
    }

    @Override
    public JsonResult getInfoById(Integer id) {
        NewsInfoVO newsInfoVO = this.baseMapper.getInfoById(id, ThreadLocalManager.getLanguage().toLowerCase());
        List<NewsVO> newsVOList = this.baseMapper.randomLimit(id, ThreadLocalManager.getLanguage().toLowerCase(), 3);
        newsInfoVO.setList(newsVOList);
        Document doc = Jsoup.parseBodyFragment(newsInfoVO.getInfo());
        //得到第一个article标签内容
        List<Element> elementList = doc.getElementsByTag("article");
        if (elementList != null && elementList.size() > 0) {
            Element element = doc.getElementsByTag("article").get(0);
            newsInfoVO.setInfo(element.html());
        }
        return JsonResult.successResult(newsInfoVO);
    }

    @Override
    public JsonResult uploadBanner(MultipartFile file) {
        try {
            //获取文件后缀
            String suffix = FileUtil.getSuffix(file.getOriginalFilename());
            String saveFileName = IdUtil.simpleUUID() + "." + suffix;
            File saveFile = new File(DataSavePathEnum.TMP.getFile(), saveFileName);
            //复制文件流到本地文件
            FileUtils.copyInputStreamToFile(file.getInputStream(), saveFile);
            JSONObject object = new JSONObject();
            object.put("filePath", saveFile.getAbsolutePath());
            return JsonResult.successResult(object);
        } catch (Exception e) {
            log.error("banner: saveAndUpload error:{}", e);
            return JsonResult.failureResult();
        }
    }

    @Override
    @Transactional
    public JsonResult delete(IntIdListDTO intIdListDTO) {
        List<News> newsList = this.listByIds(intIdListDTO.getIdList());
        for (News news : newsList) {
            //删除banner图片
            if (news.getPath() != null) {
                FileUtil.del(new File(news.getPath()));
            }
        }
        for (LanguageEnum languageEnum : LanguageEnum.values()) {
            this.baseMapper.removeByLanguageAndIdIn(intIdListDTO.getIdList(), languageEnum.getName().toLowerCase());
        }
        this.removeBatchByIds(newsList);
        return JsonResult.successResult();
    }

    @Override
    public JsonResult editStatus(UpdateStatusDTO updateStatusDTO) {
        UpdateWrapper<News> updateWrapper = Wrappers.update();
        updateWrapper.lambda().set(News::getEnabled, updateStatusDTO.getEnabled())
                .eq(News::getId, updateStatusDTO.getId());
        this.update(updateWrapper);
        return JsonResult.successResult();
    }

    @Override
    public JsonResult saveOrUpdateNotice(NewsDTO newsDTO) {
        //添加文章
        if (newsDTO.getId() == null) {
            News news = new News();
            news.setType(TiktokConfig.NOTICE_TYPE);
            news.setEnabled(BooleanEnum.TRUE.intValue());
            this.save(news);
            //循环插入多个语种数据
            for (NewsDetailsDTO detail : newsDTO.getList()) {
                detail.setLanguage(detail.getLanguage().toLowerCase());
                this.baseMapper.insertNewsByLanguage(news.getId(), detail);
            }
            //插入公告和用户关联表
            this.baseMapper.insertBatchNoticeUser(news.getId());
        } else {
            //修改文章内容
            for (NewsDetailsDTO detail : newsDTO.getList()) {
                detail.setLanguage(detail.getLanguage().toLowerCase());
                this.baseMapper.updateNewsByLanguage(newsDTO.getId(), detail);
            }
        }
        return JsonResult.successResult();
    }

    @Override
    public JsonResult getTextById(Integer id) {
        NewsInfoVO newsInfoVO = this.baseMapper.getInfoById(id, ThreadLocalManager.getLanguage().toLowerCase());
        JSONObject object = new JSONObject();
        object.put("htmlText", newsInfoVO.getInfo());
        return JsonResult.successResult(object);
    }

    @Override
    public JsonResult<PageVO<SysNoticeVO>> findMyNoticeList(PageDTO pageDTO) {
        Integer userId = JwtTokenUtil.getUserIdFromToken(ThreadLocalManager.getToken());
        Page<SysNoticeVO> page = this.baseMapper.findMyNoticeList(pageDTO.getPage(), userId, ThreadLocalManager.getLanguage().toLowerCase());
        return JsonResult.successResult(new PageVO<>(page));
    }

    @Override
    public JsonResult deleteUserNoticeById(Integer id) {
        Integer userId = JwtTokenUtil.getUserIdFromToken(ThreadLocalManager.getToken());
        newsUserMapper.delete(new LambdaQueryWrapper<NewsUser>()
                .eq(NewsUser::getNewsId, id)
                .eq(NewsUser::getUserId, userId)
        );
        return JsonResult.successResult();
    }

    @Override
    public JsonResult deleteNotice(IntIdListDTO intIdListDTO) {
        //删除国际化文本信息
        for (LanguageEnum languageEnum : LanguageEnum.values()) {
            this.baseMapper.removeByLanguageAndIdIn(intIdListDTO.getIdList(), languageEnum.getName().toLowerCase());
        }
        //删除通知和用户的引用关系
        newsUserMapper.delete(new LambdaQueryWrapper<NewsUser>()
                .in(NewsUser::getNewsId, intIdListDTO.getIdList()));
        this.removeBatchByIds(intIdListDTO.getIdList());
        return JsonResult.successResult();
    }

    @Override
    public JsonResult getListById(Integer id) {
        List<NewsDetailsDTO> list = new ArrayList<>();
        NewsDetailsDTO newsDetailsDTO;
        for (LanguageEnum languageEnum : LanguageEnum.values()) {
            newsDetailsDTO = this.baseMapper.getById(id, languageEnum.getName().toLowerCase());
            if (newsDetailsDTO == null) {
                newsDetailsDTO = new NewsDetailsDTO();
            }
            newsDetailsDTO.setLanguage(languageEnum.getName());
            list.add(newsDetailsDTO);
        }
        return JsonResult.successResult(list);
    }

    @Override
    public JsonResult forceNotice(UpdateStatusDTO updateStatusDTO) {
        if (updateStatusDTO.getEnabled().equals(BooleanEnum.TRUE.intValue())) {
            //如果设置强制通知 取消之前已设置的
            UpdateWrapper<News> unForceUpdate = Wrappers.update();
            unForceUpdate.lambda()
                    .set(News::getForceStatus, BooleanEnum.FALSE.intValue())
                    .eq(News::getForceStatus, BooleanEnum.TRUE.intValue());
            this.update(unForceUpdate);
        }
        //修改通知
        UpdateWrapper<News> forceUpdate = Wrappers.update();
        forceUpdate.lambda()
                .set(News::getForceStatus, updateStatusDTO.getEnabled())
                .eq(News::getId, updateStatusDTO.getId());
        this.update(forceUpdate);
        return JsonResult.successResult();
    }
}
