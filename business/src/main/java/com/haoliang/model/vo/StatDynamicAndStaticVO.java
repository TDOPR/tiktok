package com.haoliang.model.vo;

import com.haoliang.model.StatDynamicAndStatic;
import lombok.Data;

import java.math.BigDecimal;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/3/31 19:25
 **/
@Data
public class StatDynamicAndStaticVO extends StatDynamicAndStatic {

    private BigDecimal total;

}
