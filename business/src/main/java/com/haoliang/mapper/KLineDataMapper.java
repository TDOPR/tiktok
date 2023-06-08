package com.haoliang.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.haoliang.model.KLineData;

import java.math.BigDecimal;

public interface KLineDataMapper extends BaseMapper<KLineData> {
    BigDecimal getNowExchangeRate();
}
