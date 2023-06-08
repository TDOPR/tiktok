package com.haoliang.model.condition;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.haoliang.common.base.BaseCondition;
import com.haoliang.common.util.StringUtil;
import com.haoliang.common.enums.RoleTypeEnum;
import lombok.Data;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/11/4 16:10
 **/
@Data
public class SysRoleCondition extends BaseCondition {

    /**
     * 角色名称 等值
     */
    private String roleName;

    /**
     * 角色编码 等值
     */
    private String roleCode;

    /**
     * 启用状态  1=启用 0=禁用 ''等于查询所有
     */
    private Integer enabled;

    @Override
    public QueryWrapper buildQueryParam() {
        this.buildBaseQueryWrapper();
        this.getQueryWrapper().ne("id", RoleTypeEnum.ADMIN.getId());
        if (enabled != null) {
            this.getQueryWrapper().eq("enabled", enabled);
        }
        if (StringUtil.isNotBlank(roleName)) {
            this.getQueryWrapper().eq("roleName", roleName);
        }
        if (StringUtil.isNotBlank(roleCode)) {
            this.getQueryWrapper().eq("roleCode", roleCode);
        }
        return this.getQueryWrapper();
    }
}
