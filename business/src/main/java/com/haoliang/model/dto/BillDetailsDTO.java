package com.haoliang.model.dto;

import com.haoliang.common.model.dto.TypeDTO;
import lombok.Data;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/11/17 12:02
 **/
@Data
public class BillDetailsDTO extends TypeDTO {

    /**
     * -1查询所有  0=代理收益 1=任务收益 2=转出到USD
     */
    private Integer type;

    /**
     * 年月 -1=查询所有 '2022-11'
     */
    private String yearMonth;

}
