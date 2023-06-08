package com.haoliang.common.util;


import com.haoliang.common.constant.LanguageKeyConstants;
import com.haoliang.common.model.ThreadLocalManager;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import org.springframework.context.MessageSource;

import java.util.Locale;

/**
 * @author Dominick Li
 * @Description 国际化转换工具类
 * @CreateTime 2022/12/8 16:28
 **/
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class MessageUtil {


    /**
     * 根据key信息获取对应语言的内容
     *
     * @param key      消息key值
     * @param language 语言_国家
     * @return
     */
    public static String get(String key, String language) {
        if (StringUtil.isNotBlank(language)) {
            String arrs[] = language.split("_");
            if (arrs.length == 2) {
                return get(key, new Locale(arrs[0], arrs[1]));
            }
        }
        return get(key, Locale.getDefault());
    }

    private static String get(String key, Locale language) {
        return getInstance().getMessage(key, new String[0], language);
    }

    public static String get(String key, Object[] params, String language) {
        if (StringUtil.isNotBlank(language)) {
            String arrs[] = language.split("_");
            if (arrs.length == 2) {
                return get(key, params, new Locale(arrs[0], arrs[1]));
            }
        }
        return get(key, params, Locale.getDefault());
    }

    private static String get(String key, Object[] params, Locale language) {
        return getInstance().getMessage(key, params, language);
    }

    private static MessageSource getInstance() {
        return Lazy.messageSource;
    }


    /**
     * 根据国际化语言获取对应的月份名称
     */
    public static String getMonthStrByLanguage(Integer month) {
        String key = LanguageKeyConstants.MONTH + month;
        return MessageUtil.get(key, ThreadLocalManager.getLanguage());
    }

    /**
     * 使用懒加载方式实例化messageSource国际化工具
     */
    private static class Lazy {
        private static final MessageSource messageSource = SpringUtil.getBean(MessageSource.class);
    }

}
