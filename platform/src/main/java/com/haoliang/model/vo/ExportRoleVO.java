package com.haoliang.model.vo;

import com.alibaba.excel.annotation.ExcelProperty;
import com.haoliang.common.enums.BooleanEnum;
import com.haoliang.common.util.DateUtil;
import com.haoliang.model.SysRole;
import lombok.Data;
import org.springframework.beans.BeanUtils;

@Data
public class ExportRoleVO {

    public ExportRoleVO(SysRole sysRole) {
        BeanUtils.copyProperties(sysRole, this);
        this.setEnabled(sysRole.getEnabled().equals(BooleanEnum.TRUE.intValue()) ? "启用" : "禁用");
        this.createTime = DateUtil.format(sysRole.getCreateTime(), DateUtil.EXCEL_DATETIME_FORMAT);
    }

    @ExcelProperty("创建日期")
    private String createTime;

    @ExcelProperty("唯一标识")
    private Integer id;

    @ExcelProperty("角色名称")
    private String roleName;

    @ExcelProperty("权限字符")
    private String roleCode;

    @ExcelProperty("使用状态")
    private String enabled;

}
