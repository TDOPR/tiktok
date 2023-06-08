package com.haoliang.model.dto;

import lombok.Data;

import java.util.List;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/5/6 10:05
 **/
@Data
public class ForceDeleteDTO {

    /**
     * 是否强制  trur=强制删除(会删除用户已接取的tiktok任务)
     */
    private boolean force;

    /**
     * 删除的任务id
     */
    private List<Long> idList;

}
