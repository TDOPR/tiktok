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
public class CommunityRewardDetailVO {


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
     * 代数奖
     */
    private String algebra;

    /**
     * 团队奖
     */
    private String team;

    /**
     * 分红奖
     */
    private String special;

    /**
     * 持币奖
     */
    private String has;

}
