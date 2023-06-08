package com.haoliang.model.vo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;

import java.util.List;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/11/22 14:42
 **/
@Data
@Builder
@AllArgsConstructor
public class TaskEarningsDetailVO {

    /**
     * 代理收益
     */
    private List<WalletLogVO> list;

    /**
     * 总页数
     */
    private Integer totalPage;

    /**
     * 总收益
     */
    private String total;

    /**
     * 社区等级
     */
    private Integer level;

    /**
     * vip套餐收益明细
     */
    private List<VipOrderVO> vipList;

}
