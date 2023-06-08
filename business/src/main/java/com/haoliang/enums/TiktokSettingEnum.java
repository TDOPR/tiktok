package com.haoliang.enums;

import com.haoliang.common.config.SysSettingParam;

/**
 * 系统设置缓存中设置项的key枚举类
 */
public enum TiktokSettingEnum {

    ENABLED_AUTO_CHECK("enabledAutoCheck", "提现任务自动审核开关"),
    CHECK_MIN_AMOUNT("checkMinAmount", "自动审核金额门槛"),
    ENABLED_TIKTOK_AUTO_CHECK("enabledTiktokAutoCheck", "tiktok任务自动审核开关"),
    TIKTOK_AUTO_CHECK_LAZY_TIME("tiktokAutoCheckLazyTime", "tiktok自动审核延迟时间"),
    RECOMMENDED_ADDRESS("recommendedAddress", "推荐地址"),
    SUPPORT_FIAT("supportFiat","是否支持法币"),
    IOS("ios","ios下载地址"),
    ANDROID("android","安卓下载地址");

    TiktokSettingEnum(String key, String name) {
        this.key = key;
        this.name = name;
    }

    private String key;
    private String name;

    /**
     * 获取字符串值
     *
     * @return
     */
    public String stringValue() {
        return SysSettingParam.getDictionaryParam().get(key);
    }

    /**
     * 获取整形
     */
    public Integer intValue() {
        String value = stringValue();
        return value != null ? Integer.parseInt(value) : 0;
    }

    /**
     * 获取bool值
     */
    public boolean boolValue() {
        String value = stringValue();
        return value != null ? Boolean.parseBoolean(value) : false;
    }
}
