package com.haoliang.common.util.excel;

import com.alibaba.excel.converters.Converter;
import com.alibaba.excel.metadata.GlobalConfiguration;
import com.alibaba.excel.metadata.data.WriteCellData;
import com.alibaba.excel.metadata.property.ExcelContentProperty;

import java.sql.Timestamp;


/**
 * @Description 自定义 excel 类型转行
 * @Author Dominick Li
 * @CreateTime 2022/10/24 18:36
 **/
public class TimestampConverter implements Converter<Timestamp> {

    /**
     * 开启对 Timestamp 类型的支持
     */
    @Override
    public Class<?> supportJavaTypeKey() {
        return Timestamp.class;
    }


    /**
     * 自定义对 Character 类型数据的处理
     * 我这里就拿 String 去包装了下
     */
    @Override
    public WriteCellData<?> convertToExcelData(Timestamp value, ExcelContentProperty contentProperty, GlobalConfiguration globalConfiguration) throws Exception {
        return new WriteCellData<Timestamp>(String.valueOf(value));
    }
}