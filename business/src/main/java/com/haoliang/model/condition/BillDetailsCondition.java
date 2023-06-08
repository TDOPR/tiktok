package com.haoliang.model.condition;

import lombok.Data;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/3/6 14:24
 **/
@Data
public class BillDetailsCondition {

    /**
     * ttt流水 -1=查询所有  0=代理收益 1=任务收益 2=转出到USD
     * usd流水 -1=查询所有
     */
    private Integer type;

    /**
     * 年月 -1=查询所有 '2022-11'
     */
    private String yearMonth;

    /**
     * 是否首次加载
     */
    private boolean init=true;

}
