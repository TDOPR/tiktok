package com.haoliang.common.config;


import com.haoliang.common.enums.SysSettingKeyEnum;

import java.util.HashMap;

public class SysSettingParam {

    private static boolean loading = false;

    private static final HashMap<String, String> DICTIONARY_PARAM = new HashMap();

    public static HashMap<String, String> getDictionaryParam() {
        return DICTIONARY_PARAM;
    }

    public static void put(String key, String value) {
        DICTIONARY_PARAM.put(key, value);
    }

    public static boolean isLoading() {
        return loading;
    }

    public static void setLoading(boolean loading) {
        SysSettingParam.loading = loading;
    }

    public static Integer getLoginLogSaveDay() {
        return Integer.parseInt(DICTIONARY_PARAM.get(SysSettingKeyEnum.LOGIN_LOG_SAVE_DAY.getKey()));
    }

    public static Integer getOperationLogSaveDay() {
        return Integer.parseInt(DICTIONARY_PARAM.get(SysSettingKeyEnum.OPERATION_LOG_SAVE_DAY.getKey()));
    }

    public static Integer getErrorLogSaveDay() {
        return Integer.parseInt(DICTIONARY_PARAM.get(SysSettingKeyEnum.ERROR_LOG_SAVE_DAY.getKey()));
    }

    public static boolean isEnableSso() {
        return Boolean.parseBoolean(DICTIONARY_PARAM.get(SysSettingKeyEnum.ENABLE_SSO.getKey()));
    }

    public static Integer getThresholdSize() {
        return Integer.parseInt(DICTIONARY_PARAM.get(SysSettingKeyEnum.THRESHOLD_SIZE.getKey()));
    }

    public static String getInitPassword() {
        return DICTIONARY_PARAM.get(SysSettingKeyEnum.INIT_PASSWORD.getKey());
    }
}
