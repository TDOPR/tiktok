package com.haoliang.model.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.util.List;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/11/15 11:31
 **/
@Data
@NoArgsConstructor
@AllArgsConstructor
public class TeamAmountDTO {

    /**
     * 团队总业绩
     */
    private BigDecimal itemIncome;

    /**
     * 下表
     */
    private Integer index;

    /**
     * 用户ID
     */
    private List<Integer> userIdList;

    public TeamAmountDTO(BigDecimal itemIncome, Integer index) {
        this.itemIncome = itemIncome;
        this.index = index;
    }
}
