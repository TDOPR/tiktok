package com.haoliang.model.vo;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;


/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/4/3 13:31
 **/
@Data
@Builder
public class FundVO {

    private LocalDateTime now;

    private String tvl;

    private String input;

    private String output;

    private String yesterdayInput;

    private String yesterdayOutput;
}
