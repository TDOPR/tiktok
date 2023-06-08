package com.haoliang.model.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.util.List;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/11/15 18:27
 **/
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class MyTeamAmountDTO {

    /**
     * 小团队用户信息
     */
    private List<Integer> minItemUserIdList;

    /**
     * 大团队的用户信息
     */
    private List<Integer> maxItemUserIdList;

    /**
     * 小团队的业绩
     */
    private BigDecimal minItemAmount;

    /**
     * 大团队的业绩
     */
    private BigDecimal maxItemAmount;


}
