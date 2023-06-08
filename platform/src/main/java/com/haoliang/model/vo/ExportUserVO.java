package com.haoliang.model.vo;

import com.alibaba.excel.annotation.ExcelProperty;
import com.haoliang.common.enums.BooleanEnum;
import com.haoliang.common.util.DateUtil;
import lombok.Data;
import org.springframework.beans.BeanUtils;

/**
 * @Description 导出用户的模板
 * @Author Dominick Li
 * @CreateTime 2022/10/24 15:24
 **/
@Data
public class ExportUserVO {

    public ExportUserVO(UserVO sysUser) {
        BeanUtils.copyProperties(sysUser, this);
        this.enabled = sysUser.getEnabled().equals(BooleanEnum.TRUE.intValue()) ? "启用" : "禁用";
        this.createTime = DateUtil.format(sysUser.getCreateTime(), DateUtil.EXCEL_DATETIME_FORMAT);
    }

    @ExcelProperty("注册日期")
    private String createTime;

    @ExcelProperty("唯一标识")
    private Integer id;

    @ExcelProperty("账号")
    private String username;

    @ExcelProperty("用户姓名")
    private String name;

    @ExcelProperty("邮箱号")
    private String email;

    @ExcelProperty("手机号")
    private String mobile;

    @ExcelProperty("使用状态")
    private String enabled;

    @ExcelProperty("角色名称")
    private String roleName;


//    @ExcelProperty("渠道名称")
//    private String channelName;

}
