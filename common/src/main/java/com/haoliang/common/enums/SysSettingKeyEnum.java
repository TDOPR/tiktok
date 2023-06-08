package com.haoliang.common.enums;

public enum SysSettingKeyEnum {

    LOGIN_LOG_SAVE_DAY("loginLogSaveDay", "登录日志保存天数"),
    OPERATION_LOG_SAVE_DAY("operationLogSaveDay", "操作日志保存天数"),
    ERROR_LOG_SAVE_DAY("errorLogSaveDay", "错误日志保存天数"),
    ENABLE_SSO("enableSso", "是否启用单点登录"),
    INIT_PASSWORD("initPassword","初始密码"),
    THRESHOLD_SIZE("thresholdSize", "硬盘使用率超过x%发送邮件通知");

    SysSettingKeyEnum(String key, String name) {
        this.key = key;
        this.name = name;
    }

    private String key;
    private String name;

    public String getKey() {
        return key;
    }


}
