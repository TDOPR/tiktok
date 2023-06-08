package com.haoliang.model.condition;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.haoliang.common.base.BaseCondition;
import com.haoliang.controller.monitor.server.SysFile;
import lombok.Data;
import org.springframework.util.StringUtils;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/11/22 16:44
 **/
@Data
public class SysFileCondition extends BaseCondition<SysFile> {

    /**
     * 文件名称
     */
    private String fileName;

    /**
     * 文件类型
     */
    private String fileType;

    @Override
    public QueryWrapper buildQueryParam() {
        this.buildBaseQueryWrapper();
        if (StringUtils.hasText(fileType)) {
            this.getQueryWrapper().eq("fileType", fileType);
        }
        if (StringUtils.hasText(fileName)) {
            fileName = fileName.replaceAll("%", "////%").replaceAll("_", "////_");
            this.getQueryWrapper().like("fileName", fileName);
        }
        return this.getQueryWrapper();
    }

}
