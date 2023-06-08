package com.haoliang.common.model.dto;

import lombok.Data;

/**
 * @author Dominick Li
 * @description 存储文件所在硬盘的详细信息
 **/
@Data
public class WorkspaceHarDiskInfo {

    public WorkspaceHarDiskInfo(String filesystem, String size, String used, String use) {
        this.filesystem = filesystem;
        this.size = size;
        this.used = used;
        this.use = use;
    }

    /**
     * 所属的文件系统
     */
    private String filesystem;

    /**
     * 硬盘大小
     */
    private String size;

    /**
     * 已使用的大小
     */
    private String used;

    /**
     * 使用率
     */
    private String use;

}
