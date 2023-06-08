package com.haoliang.model.vo;

import lombok.Builder;
import lombok.Data;

import java.util.List;

/**
 * @author Dominick Li
 * @Description 业务
 * @CreateTime 2022/11/23 17:14
 **/
@Data
@Builder
public class BusinessVO {

    /**
     * 平台锁仓量
     */
    private String totalTrusteeship;

    /**
     * 历史数据
     */
    private List<PortraitSelectVO> historical;

    /**
     * 昨日数据
     */
    private List<PortraitSelectVO> yesterday;

    /**
     * 用户数据
     */
    private List<PortraitSelectVO> user;

}
