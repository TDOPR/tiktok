package com.haoliang.controller;

import com.alibaba.fastjson.JSONObject;
import com.haoliang.common.util.excel.ExcelUtil;
import com.haoliang.model.ExcelData;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletResponse;
import java.util.Arrays;
import java.util.List;

/**
 * @Description
 * @Author Dominick Li
 * @CreateTime 2022/10/24 10:24
 **/
@RestController
@RequestMapping("/")
public class TestExcelController {

    @GetMapping("/export")
    public void exportExcel(HttpServletResponse response){
        List<ExcelData>  excelDataList= Arrays.asList(new ExcelData(1,"张三",18),new ExcelData(2,"李四",19));
        ExcelUtil.exportData(ExcelData.class,"所有数据",excelDataList,response);
    }

    @GetMapping("/import")
    public void importExcel(MultipartFile file) throws Exception{
        List<ExcelData> list= ExcelUtil.importData(file.getInputStream(), ExcelData.class);
        System.out.println(JSONObject.toJSON(list));
    }

}
