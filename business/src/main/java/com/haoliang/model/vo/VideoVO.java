package com.haoliang.model.vo;

import lombok.Data;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/5/10 15:13
 **/
@Data
public class VideoVO {

    private String title;

    /**
     * 封面图路径
     */
    private String bannerUrl;

    /**
     * 播放地址
     */
    private String playUrl;

}
