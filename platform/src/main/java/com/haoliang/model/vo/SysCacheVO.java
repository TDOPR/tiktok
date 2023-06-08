package com.haoliang.model.vo;

import com.haoliang.common.util.StringUtil;
import lombok.Data;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/11/7 17:28
 **/
@Data
public class SysCacheVO {


    /**
     * 缓存名称
     */
    private String cacheName = "";

    /**
     * 缓存键名
     */
    private String cacheKey = "";

    /**
     * 缓存内容
     */
    private String cacheValue = "";

    /**
     * 备注
     */
    private String remark = "";

    public SysCacheVO(String cacheName, String remark) {
        this.cacheName = cacheName;
        this.remark = remark;
    }

    public SysCacheVO(String cacheName, String cacheKey, String cacheValue) {
        this.cacheName = StringUtil.replace(cacheName, ":", "");
        this.cacheKey = StringUtil.replace(cacheKey, cacheName, "");
        this.cacheValue = cacheValue;
    }
}
