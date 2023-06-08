package com.haoliang.common.util;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/11/23 18:31
 **/
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class OsUtil {

    public  static boolean isLinuxOs(){
        String os = System.getProperty("os.name").toLowerCase();
        return os != null && os.indexOf("linux") != -1;
    }

    public  static  boolean isWindowOs(){
        String os = System.getProperty("os.name").toLowerCase();
        return  os != null && os.indexOf("window") != -1;
    }

    public  static  String getOsName(){
        return System.getProperty("os.name").toLowerCase();
    }

}
