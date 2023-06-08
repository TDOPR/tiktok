package com.haoliang.enums;


import lombok.Getter;

import java.util.ArrayList;
import java.util.List;

@Getter
public enum CountryTelephoneCode {

    CN("中国", "86", "", "", "",""),
    TH("泰国", "66", "aD9mfd", "xzLirl", "1000","656057718"),//OK
    VN("越南", "84", "jqhcDa", "LOp8JY", "1000","705962809"),
    ID("印度尼西亚", "62", "ftjRyr", "gKxB1T", "1000","81188896555"),
    MS("马来西亚", "60", "OmlBGB", "moE3SA", "1000","148727234");

    CountryTelephoneCode(String name, String code, String appKey, String appSecret, String appCode,String testPhone) {
        this.name = name;
        this.code = code;
        this.appKey = appKey;
        this.appSecret = appSecret;
        this.appCode = appCode;
        this.testPhone=testPhone;
    }

    private String name;

    private String code;

    private String appKey;

    private String appSecret;

    private String appCode;

    private String testPhone;

    private static List<String> codeList;

    public String getCode() {
        return code;
    }

    public static List<String> getCountryTelephoneCode() {
        if (codeList == null) {
            //初始化
            codeList = new ArrayList<>();
            for (CountryTelephoneCode countryTelephoneCode : values()) {
                codeList.add(countryTelephoneCode.getCode());
            }
        }
        return codeList;
    }
}
