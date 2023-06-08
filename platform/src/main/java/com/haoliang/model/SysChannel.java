package com.haoliang.model;

import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import com.haoliang.common.base.BaseModelCID;
import lombok.Data;

import java.util.List;


/**
 * @author Dominick Li
 * @description 系统机构表
 **/
@Data
@TableName("sys_channel")
public class SysChannel extends BaseModelCID {

    /**
     * 机构名称
     *
     * @required
     */
    private String channelName;
    /**
     * 机构编码
     *
     * @required
     */
    private String channelCode;
    /**
     * 排序下标
     *
     * @required
     */
    private Integer sortIndex;
    /**
     * 父机构编号
     */
    private Integer parentId = 0;

    @TableField(exist = false)
    private List<SysChannel> children;

}
