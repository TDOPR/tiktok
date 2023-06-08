package com.haoliang.service.impl;

import cn.hutool.core.io.FileUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.haoliang.common.config.GlobalProperties;
import com.haoliang.common.enums.DataSavePathEnum;
import com.haoliang.common.enums.LanguageEnum;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.model.PageParam;
import com.haoliang.common.model.ThreadLocalManager;
import com.haoliang.common.model.dto.IntIdListDTO;
import com.haoliang.common.model.dto.UpdateStatusDTO;
import com.haoliang.common.model.vo.PageVO;
import com.haoliang.common.util.IdUtil;
import com.haoliang.common.util.StringUtil;
import com.haoliang.mapper.ArticleMapper;
import com.haoliang.model.Article;
import com.haoliang.model.condition.ArticleCondition;
import com.haoliang.model.vo.ArticleInfoVO;
import com.haoliang.model.vo.ArticleVO;
import com.haoliang.model.vo.NewsInfoVO;
import com.haoliang.model.vo.NewsVO;
import com.haoliang.service.ArticleService;
import org.apache.commons.io.FileUtils;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/5/8 10:40
 **/
@Service
public class ArticleServiceImpl extends ServiceImpl<ArticleMapper, Article> implements ArticleService {

    @Override
    public JsonResult pageList(PageParam<Article, ArticleCondition> pageParam) {
        if (pageParam.getSearchParam() == null) {
            pageParam.setSearchParam(new ArticleCondition());
        }
        Page<Article> articlePage = this.page(pageParam.getPage(), pageParam.getSearchParam().buildQueryParam());
        List<ArticleVO> articleVOList = new ArrayList<>();
        ArticleVO articleVO;
        for (Article article : articlePage.getRecords()) {
            articleVO = new ArticleVO();
            BeanUtils.copyProperties(article, articleVO);
            articleVOList.add(articleVO);
        }
        return JsonResult.successResult(new PageVO<>(articlePage.getTotal(), articlePage.getPages(), articleVOList));
    }

    @Override
    public JsonResult<ArticleInfoVO> getInfoById(Integer id) {
        Article article = this.getById(id);
        ArticleInfoVO articleInfoVO = new ArticleInfoVO();
        BeanUtils.copyProperties(article, articleInfoVO);
        return JsonResult.successResult(articleInfoVO);
    }

    @Override
    public JsonResult delete(IntIdListDTO intIdListDTO) {
        List<Article> articleList = this.list(new LambdaQueryWrapper<Article>()
                .select(Article::getPath)
                .in(Article::getId, intIdListDTO.getIdList())
        );
        for (Article article : articleList) {
            //删除banner图片
            if (article.getPath() != null) {
                FileUtil.del(new File(article.getPath()));
            }
        }
        this.removeBatchByIds(intIdListDTO.getIdList());
        return JsonResult.successResult();
    }

    @Override
    public JsonResult editStatus(UpdateStatusDTO updateStatusDTO) {
        UpdateWrapper<Article> updateWrapper = Wrappers.update();
        updateWrapper.lambda().set(Article::getEnabled, updateStatusDTO.getEnabled())
                .eq(Article::getId, updateStatusDTO.getId());
        this.update(updateWrapper);
        return JsonResult.successResult();
    }

    @Override
    public JsonResult saveAndUpload(Integer id, MultipartFile file, String title, String description, String info,String source, Integer language) {
        try {
            Article article;
            if (id == null) {
                if(file==null){
                    return JsonResult.failureResult("添加必须上传封面图！");
                }
                article = new Article();
                article.setLanguage(language);
            } else {
                article = this.getById(id);
            }
            article.setTitle(title);
            article.setDescription(description);
            article.setInfo(info);
            article.setSource(source);
            if (file != null) {
                //获取文件后缀
                DataSavePathEnum dataSavePathEnum = DataSavePathEnum.ARTICLE;
                String suffix = FileUtil.getSuffix(file.getOriginalFilename());
                String saveFileName = IdUtil.simpleUUID() + "." + suffix;
                File saveFile = new File(dataSavePathEnum.getFile(), saveFileName);
                //复制文件流到本地文件
                FileUtils.copyInputStreamToFile(file.getInputStream(), saveFile);
                String url = GlobalProperties.getVirtualPathURL() + StringUtil.replace(dataSavePathEnum.getPath(), GlobalProperties.getRootPath(), "") + saveFileName;
                article.setPath(saveFile.getAbsolutePath());
                article.setBannerUrl(url);
            }
            this.saveOrUpdate(article);
            return JsonResult.successResult();
        } catch (Exception e) {
            log.error("banner: saveAndUpload error:{}", e);
            return JsonResult.failureResult();
        }
    }

    @Override
    public JsonResult getInfoAndRandomLimit(Integer id) {
        Integer type= LanguageEnum.getType(ThreadLocalManager.getLanguage());
        Article article=this.getById(id);
        NewsInfoVO newsInfoVO = new NewsInfoVO();
        newsInfoVO.setInfo(article.getInfo());
        List<NewsVO> newsVOList = this.baseMapper.randomLimit(id, type, 3);
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
}
