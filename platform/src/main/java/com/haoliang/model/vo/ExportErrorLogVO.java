package com.haoliang.model.vo;

import com.alibaba.excel.annotation.ExcelProperty;
import com.haoliang.common.model.SysErrorLog;
import com.haoliang.common.util.DateUtil;
import lombok.Data;
import org.springframework.beans.BeanUtils;


/**
 * @Description
 * @Author Dominick Li
 * @CreateTime 2022/10/24 19:25
 **/
@Data
public class ExportErrorLogVO {

    public ExportErrorLogVO(SysErrorLog sysErrorLog) {
        BeanUtils.copyProperties(sysErrorLog, this);
        this.createTime = DateUtil.format(sysErrorLog.getCreateTime(), DateUtil.EXCEL_DATETIME_FORMAT);
    }

    @ExcelProperty("创建日期")
    private String createTime;

    @ExcelProperty("节点IP")
    private String ipAddr;

    @ExcelProperty("错误类型")
    private String errorType;

    @ExcelProperty("错误内容")
    private String errorMsg;

    @ExcelProperty("异常发生的位置")
    private String position;

}
