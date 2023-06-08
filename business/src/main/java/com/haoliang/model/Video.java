package com.haoliang.model;

import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.haoliang.common.base.BaseModelCID;
import lombok.Data;

/**
 * @author Dominick Li
 * @Description 公会相关视频
 * @CreateTime 2023/5/10 10:23
 **/
@Data
@TableName("video")
public class Video extends BaseModelCID {

    /**
     * 标题
     */
    private String title;

    /**
     * 使用启用 1=启用 0=禁用
     */
    private Integer enabled;

    /**
     * 语种
     */
    @JsonIgnore
    private Integer language;

    /**
     * 文件夹路径
     */
    @JsonIgnore
    private String folderPath;

    /**
     * 封面图路径
     */
    private String bannerUrl;

    /**
     * 播放地址
     */
    private String playUrl;

}
