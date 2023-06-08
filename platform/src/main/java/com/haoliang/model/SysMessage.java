package com.haoliang.model;

import com.baomidou.mybatisplus.annotation.TableName;
import com.haoliang.common.base.BaseModelCID;
import lombok.Data;

import javax.validation.constraints.NotEmpty;

/**
 * @author Dominick Li
 * @Description 国际化信息
 * @CreateTime 2022/11/4 15:37
 **/
@Data
@TableName("sys_message")
public class SysMessage extends BaseModelCID {

    /**
     * key名称
     *
     * @required
     */
    @NotEmpty
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
     * 类型 0=后台 1=客户端
     */
    private Integer type;

}
