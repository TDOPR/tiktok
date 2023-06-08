package com.haoliang.enums;

import com.haoliang.common.model.ThreadLocalManager;
import com.haoliang.common.util.MessageUtil;
import lombok.Getter;

@Getter
public enum UsdLogTypeEnum {

    RECHARGE(1, "充值"),
    WITHDRAWAL(2, "提现"),
    BUY_VIP(3, "购买VIP等级"),
    BUY_TASK_NUM_PACKAGE(4, "购买任务次数包套餐"),
    TTT_TRANSFER_IN(5,"TTT账户转入")
    ;
    private Integer value;

    private String name;

    private String key;

    public Integer getValue() {
        return value;
    }

    /**
     * 国际化信息文件里的Key前缀
     */
    private final static String prefix = "usdLogType.";

    UsdLogTypeEnum(Integer value, String name) {
        this.value = value;
        this.name = name;
        this.key = prefix + value;
    }

    public static String getDescByValue(Integer value) {
        for (UsdLogTypeEnum usdLogTypeEnum : values()) {
            if (usdLogTypeEnum.getValue().equals(value)) {
                return usdLogTypeEnum.toString();
            }
        }
        return "";
    }

    public static UsdLogTypeEnum valueOf(Integer value) {
        for (UsdLogTypeEnum usdLogTypeEnum : values()) {
            if (usdLogTypeEnum.getValue().equals(value)) {
                return usdLogTypeEnum;
            }
        }
        return null;
    }


    @Override
    public String toString() {
        return MessageUtil.get(key, ThreadLocalManager.getLanguage());
    }
}
