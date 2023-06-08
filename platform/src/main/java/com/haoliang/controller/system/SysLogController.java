package com.haoliang.controller.system;


import com.alibaba.excel.support.ExcelTypeEnum;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.haoliang.common.annotation.OperationLog;
import com.haoliang.common.config.GlobalProperties;
import com.haoliang.common.constant.OperationAction;
import com.haoliang.common.constant.OperationModel;
import com.haoliang.common.enums.DataSavePathEnum;
import com.haoliang.common.model.*;
import com.haoliang.common.model.dto.IntIdListDTO;
import com.haoliang.common.model.vo.PageVO;
import com.haoliang.common.service.SysErrorLogService;
import com.haoliang.common.service.SysLoginLogService;
import com.haoliang.common.service.SysOperationLogService;
import com.haoliang.common.util.DateUtil;
import com.haoliang.common.util.StringUtil;
import com.haoliang.common.util.excel.ExcelUtil;
import com.haoliang.model.condition.SysErrorLogCondition;
import com.haoliang.model.condition.SysLoginLogCondition;
import com.haoliang.model.condition.SysOperationLogCondition;
import com.haoliang.model.vo.ExportErrorLogVO;
import com.haoliang.model.vo.ExportLoginLogVO;
import com.haoliang.model.vo.ExportOperationLogVO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.ArrayList;
import java.util.List;

/**
 * 日志管理
 *
 * @author Dominick Li
 **/
@RestController
@RequestMapping("/log")
public class SysLogController {

    @Autowired
    private SysErrorLogService sysErrorLogService;

    @Autowired
    private SysOperationLogService sysOperationLogService;

    @Autowired
    private SysLoginLogService sysLoginLogService;

    /**
     * 分页查询登录日志
     */
    @PostMapping("/loginlog/pagelist")
    @PreAuthorize("hasAuthority('sys:loginlog:list')")
    public JsonResult<PageVO<SysLoginLog>> loginlogList(@RequestBody PageParam<SysLoginLog, SysLoginLogCondition> pageParam) {
        if (pageParam.getSearchParam() == null) {
            pageParam.setSearchParam(new SysLoginLogCondition());
        }
        IPage<SysLoginLog> iPage = sysLoginLogService.page(pageParam.getPage(), pageParam.getSearchParam().buildQueryParam());
        return JsonResult.successResult(new PageVO<>(iPage));
    }

    /**
     * 批量删除登录日志
     *
     * @param idList id数组  格式[1,2,3]
     */
    @OperationLog(module = OperationModel.SYS_LOGIN_LOG, description = OperationAction.REMOVE)
    @PostMapping("/loginlog/delete")
    @PreAuthorize("hasAuthority('sys:loginlog:remove')")
    public JsonResult deleteLoginLogsByIds(@RequestBody IntIdListDTO intIdListDTO) {
        return JsonResult.build(sysLoginLogService.removeByIds(intIdListDTO.getIdList()));
    }

    /**
     * 导出登录日志
     *
     * @return 文件二进制流
     */
    @PostMapping("/loginlog/export")
    @PreAuthorize("hasAuthority('sys:loginlog:export')")
    public JsonResult exportLoginLog(@RequestBody SysLoginLogCondition sysLoginLogCondition) {
        PageParam<SysLoginLog, SysLoginLogCondition> pageParam = new PageParam<>();
        pageParam.setPageSize(ExcelUtil.LIMITT);
        pageParam.setSearchParam(sysLoginLogCondition);

        IPage<SysLoginLog> iPage = sysLoginLogService.page(pageParam.getPage(), pageParam.getSearchParam().buildQueryParam());
        List<SysLoginLog> sysLoginLogList = iPage.getRecords();
        List<ExportLoginLogVO> exportLoginLogVOList = new ArrayList<>(sysLoginLogList.size());
        for (SysLoginLog sysLoginLog : sysLoginLogList) {
            exportLoginLogVOList.add(new ExportLoginLogVO(sysLoginLog));
        }

        DataSavePathEnum.EXCEL_TMP.mkdirs();
        String filePath = String.format("%s%s_%s%s", DataSavePathEnum.EXCEL_TMP.getPath(), OperationModel.SYS_LOGIN_LOG, DateUtil.getDetailTimeIgnoreUnit(), ExcelTypeEnum.XLSX.getValue());
        Object url = GlobalProperties.getVirtualPathURL() + StringUtil.replace(filePath, GlobalProperties.getRootPath(), "");
        ExcelUtil.exportData(ExportLoginLogVO.class, OperationModel.SYS_LOGIN_LOG, exportLoginLogVOList, filePath);
        return JsonResult.successResult(url);
    }

    /**
     * 分页查询操作日志
     */
    @PostMapping("/operationlog/pagelist")
    @PreAuthorize("hasAuthority('sys:operationlog:list')")
    public JsonResult<PageVO<SysOperationLog>> operationlogList(@RequestBody PageParam<SysOperationLog, SysOperationLogCondition> pageParam) {
        if (pageParam.getSearchParam() == null) {
            pageParam.setSearchParam(new SysOperationLogCondition());
        }
        IPage<SysOperationLog> iPage = sysOperationLogService.page(pageParam.getPage(), pageParam.getSearchParam().buildQueryParam());
        return JsonResult.successResult(new PageVO<>(iPage));
    }

    /**
     * 导出操作日志
     *
     * @return 文件二进制流
     */
    @PostMapping("/operationlog/export")
    @PreAuthorize("hasAuthority('sys:operationlog:export')")
    public JsonResult exportOperationlog(@RequestBody SysOperationLogCondition sysOperationLogCondition) {
        PageParam<SysOperationLog, SysOperationLogCondition> pageParam = new PageParam<>();
        pageParam.setPageSize(ExcelUtil.LIMITT);
        pageParam.setSearchParam(sysOperationLogCondition);

        IPage<SysOperationLog> iPage = sysOperationLogService.page(pageParam.getPage(), pageParam.getSearchParam().buildQueryParam());
        List<SysOperationLog> sysOperationLogList = iPage.getRecords();
        List<ExportOperationLogVO> exportOperationLogVOList = new ArrayList<>(sysOperationLogList.size());
        for (SysOperationLog sysOperationLog : sysOperationLogList) {
            exportOperationLogVOList.add(new ExportOperationLogVO(sysOperationLog));
        }

        DataSavePathEnum.EXCEL_TMP.mkdirs();
        String filePath = String.format("%s%s_%s%s", DataSavePathEnum.EXCEL_TMP.getPath(), OperationModel.SYS_OPERATION_LOG, DateUtil.getDetailTimeIgnoreUnit(), ExcelTypeEnum.XLSX.getValue());
        Object url = GlobalProperties.getVirtualPathURL() + StringUtil.replace(filePath, GlobalProperties.getRootPath(), "");
        ExcelUtil.exportData(ExportOperationLogVO.class, OperationModel.SYS_OPERATION_LOG, exportOperationLogVOList, filePath);
        return JsonResult.successResult(url);
    }

    /**
     * 分页查询错误日志
     */
    @PostMapping("/errorlog/pagelist")
    @PreAuthorize("hasAuthority('sys:errorlog:list')")
    public JsonResult<PageVO<SysErrorLog>> errorlogList(@RequestBody PageParam<SysErrorLog, SysErrorLogCondition> pageParam) {
        if (pageParam.getSearchParam() == null) {
            pageParam.setSearchParam(new SysErrorLogCondition());
        }
        IPage<SysErrorLog> iPage = sysErrorLogService.page(pageParam.getPage(), pageParam.getSearchParam().buildQueryParam());
        return JsonResult.successResult(new PageVO<>(iPage));
    }

    /**
     * 批量删除错误日志
     *
     * @param idList id数组
     */
    @OperationLog(module = OperationModel.SYS_ERROR_LOG, description = OperationAction.REMOVE)
    @PostMapping("/errorlog/delete")
    @PreAuthorize("hasAuthority('sys:errorlog:remove')")
    public JsonResult deleteErrorLogsByIds(@RequestBody IntIdListDTO intIdListDTO) {
        return JsonResult.build(sysErrorLogService.removeByIds(intIdListDTO.getIdList()));
    }

    /**
     * 批量导出错误日志
     *
     * @return 文件二进制流
     */
    @PostMapping("/errorlog/export")
    @PreAuthorize("hasAuthority('sys:errorlog:export')")
    public JsonResult exportErrorlog(@RequestBody SysErrorLogCondition sysErrorLogCondition) {
        PageParam<SysErrorLog, SysErrorLogCondition> pageParam = new PageParam<>();
        pageParam.setPageSize(ExcelUtil.LIMITT);
        pageParam.setSearchParam(sysErrorLogCondition);

        IPage<SysErrorLog> iPage = sysErrorLogService.page(pageParam.getPage(), pageParam.getSearchParam().buildQueryParam());
        List<SysErrorLog> sysErrorLogList = iPage.getRecords();
        List<ExportErrorLogVO> errorLogVOList = new ArrayList<>(sysErrorLogList.size());
        for (SysErrorLog sysErrorLog : sysErrorLogList) {
            errorLogVOList.add(new ExportErrorLogVO(sysErrorLog));
        }

        DataSavePathEnum.EXCEL_TMP.mkdirs();
        String filePath = String.format("%s%s_%s%s", DataSavePathEnum.EXCEL_TMP.getPath(), OperationModel.SYS_ERROR_LOG, DateUtil.getDetailTimeIgnoreUnit(), ExcelTypeEnum.XLSX.getValue());
        Object url = GlobalProperties.getVirtualPathURL() + StringUtil.replace(filePath, GlobalProperties.getRootPath(), "");
        ExcelUtil.exportData(ExportErrorLogVO.class, OperationModel.SYS_ERROR_LOG, errorLogVOList, filePath);
        return JsonResult.successResult(url);
    }

}
