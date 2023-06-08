package com.haoliang.service;

import com.baomidou.mybatisplus.extension.service.IService;
import com.haoliang.common.model.JsonResult;
import com.haoliang.model.KLineData;
import com.haoliang.model.vo.KLineDataInfoVO;
import com.haoliang.model.vo.KLineNowDataVO;

import java.math.BigDecimal;
import java.time.LocalDate;

public interface KLineDataService extends IService<KLineData> {

    void insertNow(LocalDate localDate);

    KLineNowDataVO getKLineNowData();

    JsonResult<KLineDataInfoVO> getKLineData();

    BigDecimal getNowExchangeRate();

    JsonResult insertTestData();

    JsonResult addAndEdit(KLineData kLineData);
}
