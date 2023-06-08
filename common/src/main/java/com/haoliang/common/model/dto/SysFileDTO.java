package com.haoliang.common.model.dto;

import org.springframework.web.multipart.MultipartFile;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/11/22 17:05
 **/
public class SysFileDTO {

    /**
     * 上传的文件
     */
    private MultipartFile file;

    /**
     * 文件名称
     */
    private String fileName;

    /**
     * 文件描述
     */
    private String fileDesc;

}
