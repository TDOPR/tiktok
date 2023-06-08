package com.haoliang.model.vo;

import com.haoliang.model.KLineData;
import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.util.List;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/3/8 12:09
 **/
@Data
@Builder
public class KLineDataInfoVO {

    /**
     * 涨跌幅
     */
    private String upDownRange;

    /**
     * 最新价
     */
    private String close;

    /**
     * 当日成交量
     */
    private String vol;


    /**
     * K线数据
     */
    private List<KLineDataVO> list;


}
