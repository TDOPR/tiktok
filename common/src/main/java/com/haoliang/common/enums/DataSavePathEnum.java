package com.haoliang.common.enums;

import com.haoliang.common.config.GlobalProperties;

import java.io.File;

public enum DataSavePathEnum {

    TMP("tmp/", "临时文件存储"),
    USER_HEAD_IMAGE("userImage/", "用户头像存储路径"),
    APP("app/", "客户端发布目录"),
    BANNER("banner/", "banner图存储目录"),
    ARTICLE("article/", "文章封面图存储"),
    SYS_FILE("sysfile/", "系统文件存储路径"),
    TASK_IMAGE("taskImage/", "Tiktok任务截图文件"),
    EXCEL_TMP(TMP.key + "excel/", "导出的excel临时存储目录"),
    VIDEO( "video/", "请客吃饭视频路径"),
    GUILD_VIDEO("guildvideo/","公会相关视频")
    ;

    DataSavePathEnum(String key, String value) {
        this.key = key;
        this.value = value;
        this.path = GlobalProperties.getRootPath() + key;
    }


    private String key;

    private String value;

    /**
     * 文件存储路径
     */
    private String path;

    public String getPath() {
        return path;
    }

    public File mkdirs() {
        File file = new File(path);
        if (!file.getParentFile().exists()) {
            file.mkdirs();
        }
        return file;
    }

    public File getFile() {
        return mkdirs();
    }
}
