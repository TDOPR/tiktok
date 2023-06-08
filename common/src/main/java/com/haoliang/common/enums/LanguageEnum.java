package com.haoliang.common.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.Locale;

@Getter
@AllArgsConstructor
public enum LanguageEnum {

    ZH_CN(1, "zh_CN", "中文/中国"),
    EN_US(2, "en_US", "英语/美国"),
    IN_ID(3, "in_ID", "印度尼西亚文/印度尼西亚"),
    TH_TH(4, "th_TH", "泰语/泰国"),
    VI_VN(5, "vi_VN", "越南语/越南");

    /**
     * 类型Id
     */
    private Integer type;

    /**
     * 语言_国家缩写
     */
    private String name;

    /**
     * 描述
     */
    private String desc;

    /**
     * 根据语言名称获取对应枚举
     *
     * @param name
     * @return 默认返回中文
     */
    public static LanguageEnum nameOf(String name) {
        for (LanguageEnum languageEnum : values()) {
            if (languageEnum.getName().equals(name)) {
                return languageEnum;
            }
        }
        return ZH_CN;
    }

    public static Locale getLocale(String name) {
        System.out.println("国际化语言=" + name + "," + nameOf(name).getName());
        return new Locale(nameOf(name).getName());
    }

    public static Integer getType(String language) {
        return nameOf(language).getType();
    }
}
