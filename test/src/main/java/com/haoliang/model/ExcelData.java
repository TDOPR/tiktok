package com.haoliang.model;

import com.alibaba.excel.annotation.ExcelProperty;
import com.alibaba.excel.annotation.write.style.HeadStyle;
import com.alibaba.excel.enums.poi.FillPatternTypeEnum;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @Description
 * @Author Dominick Li
 * @CreateTime 2022/10/24 10:26
 **/
@Data
@NoArgsConstructor
@AllArgsConstructor
public class ExcelData {

    @HeadStyle(fillPatternType = FillPatternTypeEnum.SOLID_FOREGROUND, fillForegroundColor = 14)
    @ExcelProperty(value = "用户Id",order = 2)
    private Integer id;

    @ExcelProperty(value = "用户名",order = 1)
    private String username;

    //@ExcelProperty("年龄") //如果不设置默认会以字段名称为列标题
    private Integer age;

}
