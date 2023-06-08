package com.haoliang.common.util.excel;

import com.alibaba.excel.converters.Converter;
import com.alibaba.excel.metadata.GlobalConfiguration;
import com.alibaba.excel.metadata.data.WriteCellData;
import com.alibaba.excel.metadata.property.ExcelContentProperty;

import java.text.SimpleDateFormat;
import java.util.Date;

/**
 * @Description
 * @Author Dominick Li
 * @CreateTime 2022/10/24 19:11
 **/
public class DateConverter implements Converter<Date> {

    private static final String PATTERN_YYYY_MM_DD = "yyyy-MM-dd HH:mm:ss";

    @Override
    public Class<?> supportJavaTypeKey() {
        return Date.class;
    }

    @Override
    public WriteCellData<?> convertToExcelData(Date value, ExcelContentProperty contentProperty, GlobalConfiguration globalConfiguration) throws Exception {
        SimpleDateFormat sdf = new SimpleDateFormat(PATTERN_YYYY_MM_DD);
        String dateValue = sdf.format(value);
        return new WriteCellData<>(dateValue);
    }
}
