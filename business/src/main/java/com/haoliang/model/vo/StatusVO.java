package com.haoliang.model.vo;

import lombok.Data;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/4/15 11:53
 **/
@Data
public class StatusVO {

    public StatusVO(Integer status) {
        this.status = status;
    }

    /**
     * 订单状态 1=未支付,2=支付成功,3=撤销,4=支付失败
     */
    private Integer status;
}
