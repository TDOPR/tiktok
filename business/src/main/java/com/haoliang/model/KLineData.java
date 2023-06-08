package com.haoliang.model;

import com.alibaba.fastjson.annotation.JSONField;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.ZoneOffset;

/**
 * @author Dominick Li
 * @Description ttt转usd汇率
 * @CreateTime 2023/3/2 10:52
 **/
@Data
@Builder
@TableName("kline_data")
@NoArgsConstructor
@AllArgsConstructor
public class KLineData {

    /**
     * 唯一标识
     */
    @TableId(type = IdType.AUTO)
    private Integer id;

    /**
     * 创建日期
     */
    private LocalDate createDate;

    /**
     * 当天开盘价
     */
    @JsonProperty("open")
    @JSONField(name = "open")
    private BigDecimal kopen;

    /**
     * 当天最高价
     */
    @JsonProperty("high")
    @JSONField(name = "high")
    private BigDecimal khigh;

    /**
     * 当日最低价
     */
    @JsonProperty("low")
    @JSONField(name = "low")
    private BigDecimal klow;

    /**
     * 当日收盘价
     */
    @JsonProperty("close")
    @JSONField(name = "close")
    private BigDecimal kclose;

    /**
     * 当日成交量
     */
    @JsonProperty("vol")
    @JSONField(name = "vol")
    private BigDecimal kvol;

}
