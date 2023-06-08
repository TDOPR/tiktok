package com.haoliang.model.vo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author Dominick Li
 * @Description 流水
 * @CreateTime 2022/11/14 11:00
 **/
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class WalletLogVO {

    /**
     * 流水日期
     */
    private String createTime;

    /**
     * 流水类型名称
     */
    private String name;

    /**
     * 金额
     */
    private String amount;

}
