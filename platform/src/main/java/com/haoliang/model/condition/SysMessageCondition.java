package com.haoliang.model.condition;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.haoliang.common.base.BaseCondition;
import com.haoliang.common.util.StringUtil;
import com.haoliang.model.SysMessage;
import lombok.Data;

/**
 * @author Dominick Li
 * @Description 国际化查询条件
 * @CreateTime 2022/11/7 9:56
 **/
@Data
public class SysMessageCondition extends BaseCondition<SysMessage> {

    /**
     * key名称
     */
    private String keyName;

    /**
     * 中文
     */
    private String zhCn;

    /**
     * 繁体中文
     */
    private String zhTw;

    /**
     * 英文
     */
    private String enUs;

    /**
     * 类型 0=管理端 1=客户端
     */
    private Integer type;

    @Override
    public QueryWrapper buildQueryParam() {
        this.buildBaseQueryWrapper();
        if (type != null) {
            this.getQueryWrapper().eq("type", type);
        }
        if (StringUtil.isNotBlank(keyName)) {
            keyName = keyName.replaceAll("%", "////%").replaceAll("_", "////_");
            this.getQueryWrapper().like("keyName", keyName);
        }
        if (StringUtil.isNotBlank(zhCn)) {
            zhCn = zhCn.replaceAll("%", "////%").replaceAll("_", "////_");
            this.getQueryWrapper().like("zhCn", zhCn);
        }
        if (StringUtil.isNotBlank(zhTw)) {
            zhTw = zhTw.replaceAll("%", "////%").replaceAll("_", "////_");
            this.getQueryWrapper().like("zhTw", zhTw);
        }
        if (StringUtil.isNotBlank(enUs)) {
            enUs = enUs.replaceAll("%", "////%").replaceAll("_", "////_");
            this.getQueryWrapper().like("errorMsg", enUs);
        }
        return this.getQueryWrapper();
    }
}
