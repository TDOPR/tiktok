package com.haoliang.model.vo;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.haoliang.common.config.GlobalProperties;
import com.haoliang.common.util.StringUtil;
import com.haoliang.model.Banner;
import lombok.Data;
import org.springframework.beans.BeanUtils;

import java.time.LocalDateTime;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/4/11 18:10
 **/
@Data
public class BannerVO {

    @JsonFormat(shape = JsonFormat.Shape.STRING)
    private Long id;

    private String name;

    private Integer sortIndex;

    private Integer enabled;

    /**
     * 中文路径
     */
    private String zhUrl;

    /**
     * 英文路径
     */
    private String enUrl;

    /**
     * 印尼语
     */
    private String inUrl;

    /**
     * 泰语
     */
    private String thUrl;

    /**
     * 越南语
     */
    private String viUrl;


    private LocalDateTime createTime;

    private LocalDateTime lastmodifiedTime;

    public BannerVO(Banner banner) {
        BeanUtils.copyProperties(banner, this);
        String pre = GlobalProperties.getVirtualPathURL();
        String rootPath = GlobalProperties.getRootPath();
        this.zhUrl = pre + StringUtil.replace(banner.getZhPath(), rootPath, "");
        this.enUrl = pre + StringUtil.replace(banner.getEnPath(), rootPath, "");
        this.inUrl = pre + StringUtil.replace(banner.getInPath(), rootPath, "");
        this.thUrl = pre + StringUtil.replace(banner.getThPath(), rootPath, "");
        this.viUrl = pre + StringUtil.replace(banner.getViPath(), rootPath, "");
    }

}
