package com.haoliang.model.dto;

import lombok.Data;

import java.time.LocalDate;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/11/17 16:45
 **/
@Data
public class DateSection {

    /**
     * 第一步流水日期
     */
    private LocalDate minDate;

    /**
     * 最近一笔流水记录
     */
    private LocalDate maxDate;

}
