package com.haoliang.model.dto;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.haoliang.common.util.NumberUtil;
import lombok.Data;

import java.math.BigDecimal;
import java.util.List;

/**
 * @author Dominick Li
 * @Description 用户关系树结构
 * @CreateTime 2022/11/14 17:30
 **/
@Data
public class TreeUserIdDTO {

    /**
     * 邮箱号
     * @required
     */
    private String email;
    /**
     * 当前用户的Id
     * @required
     */
    @JsonIgnore
    private Integer descendant;

    /**
     * 社区等级
     * @required
     */
    private Integer level;

    /**
     * 钱包余额
     * @required
     */
    private BigDecimal walletAmount;


    /**
     * 用户下级集合
     * @required
     */
    @JsonInclude(JsonInclude.Include.NON_EMPTY)
    private List<TreeUserIdDTO> childList;


    public String getWalletAmount() {
        return NumberUtil.downToTwoBigDecimal(walletAmount);
    }

}
