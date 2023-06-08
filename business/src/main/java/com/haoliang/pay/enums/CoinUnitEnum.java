package com.haoliang.pay.enums;

import com.haoliang.model.vo.PortraitSelectVO;
import com.haoliang.model.vo.ViewSelectVO;
import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.ArrayList;
import java.util.List;

/**
 * @author Dominick Li
 * @Description 提现的货币类型  网络来源
 * @CreateTime 2022/11/1 16:01
 **/
@Getter
@AllArgsConstructor
public enum CoinUnitEnum {

    USDT(0, "USDT", "区块链"),
    FIAT_ID(1, "ID", "印尼盾"),
    FIAT_VN(2, "VN", "越南盾"),
    FIAT_TH(3, "TH", "泰铢");

    /**
     * 类型id
     */
    private Integer Id;

    /**
     * 货币名称
     */
    private String name;

    private String cnName;


    public static CoinUnitEnum idOf(Integer id) {
        for (CoinUnitEnum coinUnitEnum : values()) {
            if (coinUnitEnum.getId().equals(id)) {
                return coinUnitEnum;
            }
        }
        return null;
    }

    public static String getNameById(Integer id) {
        for (CoinUnitEnum coinUnitEnum : values()) {
            if (coinUnitEnum.getId().equals(id)) {
                return coinUnitEnum.getName();
            }
        }
        return "";
    }

    public static String getCnNameById(Integer id) {
        for (CoinUnitEnum coinUnitEnum : values()) {
            if (coinUnitEnum.getId().equals(id)) {
                return coinUnitEnum.getCnName();
            }
        }
        return "";
    }

    public static List<PortraitSelectVO> getSelectList() {
        List<PortraitSelectVO> list = new ArrayList<>();
        for (CoinUnitEnum coinUnitEnum : values()) {
            list.add(new PortraitSelectVO(coinUnitEnum.getCnName(), coinUnitEnum.getId().toString()));
        }
        return list;
    }


}
