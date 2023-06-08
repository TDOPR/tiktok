package com.haoliang.pay.id.enums;

import com.haoliang.model.dto.BankCodeDTO;
import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.ArrayList;
import java.util.List;

/**
 * 印尼代付支持的银行编码
 */
@Getter
public enum IdBankCodeEnum {

    BRI("002", "BRI"),
    BNI("009", "BNI"),
    CIMB("022", "CIMB"),
    MANDIRI("008", "MANDIRI"),
    PERMATA("013", "PERMATA");

    IdBankCodeEnum(String code, String name) {
        this.code = code;
        this.name = name;
    }

    private String code;

    private String name;

    private static List<BankCodeDTO> bankCodeList;

    public static List<BankCodeDTO> getBankCodeList() {
        if (bankCodeList == null) {
            bankCodeList = new ArrayList<>();
            for (IdBankCodeEnum bankCodeEnum : values()) {
                bankCodeList.add(new BankCodeDTO(bankCodeEnum.getCode(), bankCodeEnum.getName()));
            }
        }
        return bankCodeList;
    }
}
