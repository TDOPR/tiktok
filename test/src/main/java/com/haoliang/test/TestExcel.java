package com.haoliang.test;

import com.alibaba.excel.util.ListUtils;
import com.haoliang.common.util.DateUtil;
import com.haoliang.common.util.excel.ExcelUtil;

import java.util.List;

/**
 * @Description
 * @Author Dominick Li
 * @CreateTime 2022/10/24 10:49
 **/
public class TestExcel {
    public static void main(String[] args) throws Exception {
        //导出
//        List<ExcelData> excelDataList= Arrays.asList(new ExcelData(1,"张三",18),new ExcelData(2,"李四",19));
//        ExcelUtil.exportData(ExcelData.class,"所有数据",excelDataList,"E:\\java\\test.xlsx");
//
//        //导入
//        List<ExcelData> readExcelDataList= ExcelUtil.importData("E:\\java\\test.xlsx",ExcelData.class);
//        System.out.println(JSONObject.toJSON(readExcelDataList));
        List<List<String>> head = ListUtils.newArrayList();
        List<String> head0 = ListUtils.newArrayList();
        head0.add("字符串" + System.currentTimeMillis());
        List<String> head1 = ListUtils.newArrayList();
        head1.add("数字" + System.currentTimeMillis());
        List<String> head2 = ListUtils.newArrayList();
        head2.add("id");
        head.add(head0);
        head.add(head1);
        head.add(head2);

        List<List<Object>> list = ListUtils.newArrayList();
        for (int i = 0; i < 10; i++) {
            List<Object> data = ListUtils.newArrayList();
            data.add("字符串" + i);
            data.add(DateUtil.getDetailTimeIgnoreUnit());
            data.add(i);
            list.add(data);
        }
        ExcelUtil.exportData("测试",head,list,"E:\\java\\test2.xlsx");
    }
}
