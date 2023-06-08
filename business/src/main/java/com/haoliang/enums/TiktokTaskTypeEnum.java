package com.haoliang.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum TiktokTaskTypeEnum {

    CONCERN_TASK(1, "关注任务"),
    COMMENTS(2, "评论"),
    LIKE_TASK(3, "点赞任务");

    private int code;

    private String name;

    public static String getNameByCode(int code) {
        for (TiktokTaskTypeEnum tiktokTaskTypeEnum : values()) {
            if (code == tiktokTaskTypeEnum.getCode()) {
                return tiktokTaskTypeEnum.getName();
            }
        }
        return "";
    }

    public static   Integer getTttLogType(Integer code){
        if(code==1){
            return TttLogTypeEnum.CONCERN.getValue();
        }else if(code==2){
            return TttLogTypeEnum.COMMENTS.getValue();
        }else{
            return TttLogTypeEnum.LIKE.getValue();
        }
    }
}
