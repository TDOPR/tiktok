package com.haoliang.common.util.excel;

import com.alibaba.excel.EasyExcel;
import com.alibaba.excel.support.ExcelTypeEnum;
import com.alibaba.excel.write.metadata.style.WriteCellStyle;
import com.alibaba.excel.write.metadata.style.WriteFont;
import com.alibaba.excel.write.style.HorizontalCellStyleStrategy;
import com.haoliang.common.util.DateUtil;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.poi.ss.usermodel.BorderStyle;
import org.apache.poi.ss.usermodel.HorizontalAlignment;
import org.apache.poi.ss.usermodel.VerticalAlignment;

import javax.servlet.http.HttpServletResponse;
import java.io.FileInputStream;
import java.io.InputStream;
import java.net.URLEncoder;
import java.util.List;

/**
 * @Description Excel读写工具类
 * https://easyexcel.opensource.alibaba.com/docs/current/quickstart/write  阿里easyexcel文档地址
 * @Author Dominick Li
 * @CreateTime 2022/10/24 10:22
 **/
@Slf4j
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class ExcelUtil {

    public static final Integer LIMITT=100000;


    /**
     * 到处数据到本地文件
     *
     * @param clazz     数据的类型
     * @param sheetName 表头
     * @param list      数据
     * @param pathName 本地目录
     */
    public static void exportData(Class clazz, String sheetName, List list, String pathName) {
        EasyExcel.write(pathName)
                .registerWriteHandler(getHorizontalCellStyleStrategy())
                .registerWriteHandler(new MyLongestMatchColumnWidthStyleStrategy())
                .head(clazz)
                .excelType(ExcelTypeEnum.XLS)
                .sheet(sheetName)
                .doWrite(list);
    }

    /**
     * 导出数据
     *
     * @param clazz     数据类型
     * @param sheetName excel名称
     * @param list      数据
     * @param response  输出流
     */
    public static void exportData(Class clazz, String sheetName, List list, HttpServletResponse response) {
        try {
            String fileName = sheetName + "_" + DateUtil.getDetailTimeIgnoreUnit() + ".xlsx";
            response.setContentType("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet");
            response.setCharacterEncoding("utf-8");
            // 这里URLEncoder.encode可以防止中文乱码
            fileName = URLEncoder.encode(fileName, "UTF-8").replaceAll("\\+", "%20");
            response.setHeader("Content-disposition", "attachment;filename*=utf-8''" + fileName + ".xlsx");
            EasyExcel.write(response.getOutputStream())
                    .registerWriteHandler(getHorizontalCellStyleStrategy())
                    .registerWriteHandler(new MyLongestMatchColumnWidthStyleStrategy())
                    .registerConverter(new TimestampConverter())
                    .registerConverter(new DateConverter())
                    .head(clazz)
                    .excelType(ExcelTypeEnum.XLSX)
                    .sheet(sheetName)
                    .doWrite(list);
        } catch (Exception e) {
            log.error("exportData error:{}", e);
        }
    }

    /**
     * 不创建对象导出数据到本地
     */
    public static void exportData(String sheetName, List<List<String>> head, List<List<Object>> data, String pathName) {
        EasyExcel.write(pathName)
                .registerWriteHandler(getHorizontalCellStyleStrategy())
                .registerWriteHandler(new MyLongestMatchColumnWidthStyleStrategy())
                .registerConverter(new TimestampConverter())
                .registerConverter(new DateConverter())
                .head(head)
                .excelType(ExcelTypeEnum.XLSX)
                .sheet(sheetName)
                .doWrite(data);
    }

    /**
     * 不创建对象导出数据到response流中
     */
    public static void exportData(String sheetName, List<List<String>> head, List<List<Object>> data, HttpServletResponse response) {
        try {
            String fileName = sheetName + "_" + DateUtil.getDetailTimeIgnoreUnit() + ".xlsx";
            response.setContentType("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet");
            response.setCharacterEncoding("utf-8");
            // 这里URLEncoder.encode可以防止中文乱码
            fileName = URLEncoder.encode(fileName, "UTF-8").replaceAll("\\+", "%20");
            response.setHeader("Content-disposition", "attachment;filename*=utf-8''" + fileName + ".xlsx");
            EasyExcel.write(response.getOutputStream())
                    .registerWriteHandler(getHorizontalCellStyleStrategy())
                    .registerWriteHandler(new MyLongestMatchColumnWidthStyleStrategy())
                    .registerConverter(new TimestampConverter())
                    .registerConverter(new DateConverter())
                    .head(head)
                    .excelType(ExcelTypeEnum.XLSX)
                    .sheet(sheetName).doWrite(data);
        } catch (Exception e) {
            log.error("exportData error:{}", e);
        }
    }

    /**
     * 导入数据
     *
     * @param filePath 文件路径
     * @param clazz    数据类型
     * @return
     */
    public static <T> List<T> importData(String filePath, Class clazz) throws Exception {
        return importData(new FileInputStream(filePath), clazz);
    }

    /**
     * 导入数据
     *
     * @param inputStream 文件流
     * @param clazz       数据类型
     */
    public static <T> List<T> importData(InputStream inputStream, Class clazz) {
        return EasyExcel.read(inputStream).head(clazz).autoCloseStream(false).sheet().doReadSync();
    }

    /**
     * 使用校验监听器 异步导入 同步返回
     *
     * @param is         输入流
     * @param clazz      对象类型
     * @param isValidate 是否 Validator 检验 默认为是
     * @return 转换后集合
     */
    public static <T> ExcelResult<T> importExcel(InputStream is, Class<T> clazz, boolean isValidate) {
        DefaultExcelListener<T> listener = new DefaultExcelListener<>(isValidate);
        EasyExcel.read(is, clazz, listener).sheet().doRead();
        return listener.getExcelResult();
    }


    /**
     * 设置表头和内容样式
     */
    public static HorizontalCellStyleStrategy getHorizontalCellStyleStrategy() {
        WriteCellStyle contentWriteCellStyle = new WriteCellStyle();
        //垂直居中,水平居中
        contentWriteCellStyle.setVerticalAlignment(VerticalAlignment.CENTER);
        contentWriteCellStyle.setHorizontalAlignment(HorizontalAlignment.CENTER);
        contentWriteCellStyle.setBorderLeft(BorderStyle.THIN);
        contentWriteCellStyle.setBorderTop(BorderStyle.THIN);
        contentWriteCellStyle.setBorderRight(BorderStyle.THIN);
        contentWriteCellStyle.setBorderBottom(BorderStyle.THIN);

        //设置 自动换行
        contentWriteCellStyle.setWrapped(true);
        // 字体策略
        WriteFont contentWriteFont = new WriteFont();
        // 字体大小
        contentWriteFont.setFontHeightInPoints((short) 12);
        contentWriteCellStyle.setWriteFont(contentWriteFont);
        //头策略使用默认 设置字体大小
        WriteCellStyle headWriteCellStyle = new WriteCellStyle();
        WriteFont headWriteFont = new WriteFont();
        headWriteFont.setFontHeightInPoints((short) 12);
        headWriteCellStyle.setWriteFont(headWriteFont);
        return new HorizontalCellStyleStrategy(headWriteCellStyle, contentWriteCellStyle);
    }
}


