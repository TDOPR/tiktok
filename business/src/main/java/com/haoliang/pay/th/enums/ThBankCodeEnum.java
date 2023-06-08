package com.haoliang.pay.th.enums;

import com.haoliang.model.dto.BankCodeDTO;
import lombok.Getter;

import java.util.ArrayList;
import java.util.List;


@Getter
public enum ThBankCodeEnum {

    SCB("101", "SCB"),
    KBANK("102", "KBANK"),
    KTB("103", "KTB"),
    BBL("104", "BBL"),
    TMB("105", "TMB"),
    GSB("106", "GSB"),
    BAY("107", "BAY"),
    CIMB("108", "CIMB"),
    TBANK("109", "TBANK"),
    BAAC("110", "BAAC"),
    LHBA("111", "LHBA"),
    GHB("112", "GHB"),
    TIB("113", "TIB"),
    ;

    ThBankCodeEnum(String code, String name) {
        this.code = code;
        this.name = name;
    }

    private String code;

    private String name;

    private static List<BankCodeDTO> bankCodeList;

    public static List<BankCodeDTO> getBankCodeList() {
        if (bankCodeList == null) {
            bankCodeList = new ArrayList<>();
            for (ThBankCodeEnum thBankCodeEnum : values()) {
                bankCodeList.add(new BankCodeDTO(thBankCodeEnum.getCode(), thBankCodeEnum.getName()));
            }
        }
        return bankCodeList;
    }

}
