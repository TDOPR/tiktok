package com.haoliang.pay.th.model;

import lombok.Data;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/4/20 10:00
 **/
@Data
public class ThPayResult {

    private Integer state;

    private String msg;

    private String data;

}
