package com.haoliang.common.base;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Data;
import org.springframework.util.StringUtils;

import java.util.Date;

/**
 * @author Dominick Li
 * @Description 封装页面的查询条件
 * @CreateTime 2022/11/4 15:55
 **/
@Data
public abstract class BaseCondition<T> {

    /**
     * 范围 开始时间 格式: yyyy-MM-dd
     */
    //@JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private T beginDate;

    /**
     * 范围 结束时间
     */
    //@JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private T endDate;

    /**
     * 查询条件构造器
     *
     * @ignore
     */
    private QueryWrapper queryWrapper;

    private String oderBy = "createTime";

    /**
     * 构建通用查询条件
     */
    protected void buildBaseQueryWrapper() {
        //初始化并且加载日期条件
        this.init();
        //设置排序条件
        this.queryWrapper.orderByDesc(oderBy);
    }


    protected void init() {
        if (this.queryWrapper == null) {
            this.queryWrapper = new QueryWrapper<>();
            //排序条件
            if (!StringUtils.isEmpty(this.getBeginDate())) {
                //大于或等于传入时间
                this.queryWrapper.ge(oderBy, this.getBeginDate());
            }
            if (!StringUtils.isEmpty(this.getEndDate())) {
                //endDate = DateUtil.getDateStrIncrement(endDate, 1, TimeUnit.DAYS);
                //小于或等于传入时间
                this.queryWrapper.le(oderBy, endDate);
            }
        }
    }


    /**
     * 构建查询条件
     */
    public abstract QueryWrapper buildQueryParam();

    /**
     * 构建查询条件
     */
    public QueryWrapper buildQueryParam(String orderBy) {
        this.oderBy = orderBy;
        this.buildBaseQueryWrapper();
        return this.getQueryWrapper();
    }


}
