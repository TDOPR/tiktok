package com.haoliang.model.vo;


import com.haoliang.common.util.NumberUtil;
import com.haoliang.model.KLineData;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.ZoneOffset;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/3/15 18:38
 **/
@Data
@NoArgsConstructor
public class KLineDataVO {

    public KLineDataVO(KLineData kLineData) {
        this.sid = kLineData.getCreateDate().atStartOfDay().toInstant(ZoneOffset.of("+8")).toEpochMilli();
        this.open = NumberUtil.toPlainString(kLineData.getKopen());
        this.high = NumberUtil.toPlainString(kLineData.getKhigh());
        this.low = NumberUtil.toPlainString(kLineData.getKlow());
        this.close = NumberUtil.toPlainString(kLineData.getKclose());
        this.vol = NumberUtil.toPlainString(kLineData.getKvol());
    }

    /**
     * 当天开盘价
     */
    private String open;

    /**
     * 当天最高价
     */
    private String high;

    /**
     * 当日最低价
     */
    private String low;

    /**
     * 当日收盘价
     */
    private String close;

    /**
     * 当日成交量
     */
    private String vol;

    private Long sid;

}
