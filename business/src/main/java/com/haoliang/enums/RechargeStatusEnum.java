package com.haoliang.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * 充值状态枚举
 */
@AllArgsConstructor
@Getter
public enum RechargeStatusEnum {

    RECHARGE_SUCCESS(0,"充值成功"),
    TO_RECORDED_SUCCESS(1,"到账成功"),
    TO_BE_PAY(2,"待支付")
    ;

    private Integer status;
    private String desc;

    public static String getDescByStatus(Integer status){
        for(RechargeStatusEnum rechargeStatusEnum :values()){
            if(rechargeStatusEnum.getStatus().equals(status)){
                return rechargeStatusEnum.getDesc();
            }
        }
        return "";
    }

}
