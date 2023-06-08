package com.haoliang.pay.vn.enums;

import com.haoliang.model.dto.BankCodeDTO;
import lombok.Getter;

import java.util.ArrayList;
import java.util.List;

@Getter
public enum VnBankCodeEnum {

    ACB("V_ACB", "ACB"),
    MBBANK("V_MBBANK", "MBBANK"),
    VIETCOMBANK("V_Vietcombank", "VIETCOMBANK"),
    SACOMBANK("V_SACOMBANK", "SACOMBANK"),
    ;

    VnBankCodeEnum(String code, String name) {
        this.code = code;
        this.name = name;
    }

    private String code;

    private String name;

    private static List<BankCodeDTO> bankCodeList;

    public static List<BankCodeDTO> getBankCodeList() {
        if (bankCodeList == null) {
            bankCodeList = new ArrayList<>();
            for (VnBankCodeEnum bankCodeEnum : values()) {
                bankCodeList.add(new BankCodeDTO(bankCodeEnum.getCode(), bankCodeEnum.getName()));
            }
        }
        return bankCodeList;
    }

}
