package com.haoliang.model.vo;

import com.alibaba.fastjson.annotation.JSONField;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.math.BigDecimal;
import java.time.LocalDate;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/3/31 17:53
 **/
public class KLineClientVO {


    /**
     * 创建日期
     */
    private LocalDate createDate;

    /**
     * 当天开盘价
     */
    private String kopen;

    /**
     * 当天最高价
     */
    private String khigh;

    /**
     * 当日最低价
     */
    private String klow;

    /**
     * 当日收盘价
     */
    private String kclose;

    /**
     * 当日成交量
     */
    private String kvol;

}
