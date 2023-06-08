package com.haoliang.model;

import com.baomidou.mybatisplus.annotation.TableName;
import com.haoliang.common.base.BaseModel;
import lombok.Data;

/**
 * @author Dominick Li
 * @Description app首页轮播图
 * @CreateTime 2023/1/9 10:11
 **/
@Data
@TableName("banner")
public class Banner extends BaseModel {

    /**
     * 图片名称
     */
    private String name;

    /**
     * 中文路径
     */
    private String zhPath;

    /**
     * 英文路径
     */
    private String enPath;

    /**
     * 印尼语
     */
    private String inPath;

    /**
     * 泰语
     */
    private String thPath;

    /**
     * 越南语
     */
    private String viPath;

    /**
     * 排序
     */
    private Integer sortIndex;

    /**
     * 使用启用 1=启用 0=禁用
     */
    private Integer enabled;

}
