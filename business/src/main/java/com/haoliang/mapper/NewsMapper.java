package com.haoliang.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.haoliang.model.News;
import com.haoliang.model.condition.NewsCondition;
import com.haoliang.model.dto.NewsDetailsDTO;
import com.haoliang.model.vo.HomeNoticeVO;
import com.haoliang.model.vo.NewsInfoVO;
import com.haoliang.model.vo.NewsVO;
import com.haoliang.model.vo.SysNoticeVO;
import org.apache.ibatis.annotations.Param;

import java.util.List;

public interface NewsMapper extends BaseMapper<News> {

    Page<News> page(Page page, @Param("language") String language, @Param("param") NewsCondition condition);

    Page<News> clientPage(Page page, @Param("language") String language);

    int insertNewsByLanguage(@Param("id") Integer id, @Param("news") NewsDetailsDTO detail);

    NewsInfoVO getInfoById(@Param("id") Integer id, @Param("language") String language);

    List<NewsVO> randomLimit(@Param("id") Integer id, @Param("language") String language, @Param("limitSize") Integer limitSize);

    int removeByLanguageAndIdIn(@Param("idList") List<Integer> idList, @Param("language") String language);

    int updateNewsByLanguage(@Param("id") Integer id, @Param("news") NewsDetailsDTO detai);

    int insertBatchNoticeUser(Integer noticeId);

    Page<SysNoticeVO> findMyNoticeList(Page page, @Param("userId")Integer userId,  @Param("language") String language);

    NewsDetailsDTO getById(Integer id, String language);

    HomeNoticeVO selectForceNotice(String language);
}
