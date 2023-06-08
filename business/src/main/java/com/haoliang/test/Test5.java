package com.haoliang.test;

import com.haoliang.common.util.HttpUtil;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/4/24 17:08
 **/
public class Test5 {

    public static void main(String[] args) {
        System.out.println(HttpUtil.get("http://api.tiktok-guild.com:8999/home/getPdfUrl"));
    }

}
