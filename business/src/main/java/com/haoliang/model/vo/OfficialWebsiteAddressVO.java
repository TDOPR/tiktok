package com.haoliang.model.vo;

import lombok.Data;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/12/7 16:51
 **/
@Data
public class OfficialWebsiteAddressVO {

    /**
     * 官网地址
     */
    private String openAddress;

    public OfficialWebsiteAddressVO(String openAddress) {
        this.openAddress = openAddress;
    }
}
