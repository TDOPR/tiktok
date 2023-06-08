package com.haoliang.model.vo;

import com.baomidou.mybatisplus.annotation.TableField;
import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Data;

import java.time.LocalDate;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/12/12 10:11
 **/
@Data
public class StrategyVO {

    /**
     * 创建时间
     */
    @JsonFormat(pattern = "yyyy.MM.dd")
    private LocalDate createDate;

    /**
     * 策略名称
     */
    @TableField(exist = false)
    private String strategyName;

    /**
     * 基准货币(QC)
     */
    private String qc;

    /**
     * 计价货币(BC)
     */
    private String bc;

    /**
     * 交叉货币对(CRO)
     */
    private String cro;

    /**
     * 派生交易量(DERN)
     */
    private String dern;

    /**
     * 量化指标TI
     */
    private String ti;

    /**
     * 生态基数EIPM
     */
    private String eipM;

    /**
     * 生态基数EIPN
     */
    private String eipN;

    /**
     * 生态基数EIP
     */
    private String eip;

}
