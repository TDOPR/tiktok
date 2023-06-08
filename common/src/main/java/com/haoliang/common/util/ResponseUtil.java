package com.haoliang.common.util;

import com.haoliang.common.enums.ContentTypeEnum;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServletResponse;
import java.io.*;
import java.net.URL;

/**
 * @author Dominick Li
 * @Description HttpResponse 工具类
 * @CreateTime 2022/11/7 10:47
 **/
@Slf4j
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class ResponseUtil {

    /**
     * 导出json文件
     * @param response 输出流
     * @param text 文本内容
     * @param fileName 文件名称
     */
    public static void exportJson(HttpServletResponse response, String text,String fileName) {
        response.reset();
        response.setContentType("text/plain;charset=UTF-8");
        response.addHeader("Content-Transfer-Encoding", "base64");
        response.setCharacterEncoding("utf-8");

        //设置文件的名称和格式
        response.setHeader("Content-disposition", "attachment;filename*=utf-8''" + fileName + ".json");
        BufferedOutputStream buff = null;
        ServletOutputStream outStr = null;
        try {
            outStr = response.getOutputStream();
            buff = new BufferedOutputStream(outStr);
            buff.write(text.getBytes("UTF-8"));
            buff.flush();
            buff.close();
        } catch (Exception e) {
            log.error("导出文件文件出错:{}",e);
        } finally {
            try {
                buff.close();
            } catch (Exception e) {
                log.error("关闭流对象出错 e:{}",e);
            }
            try {
                outStr.close();
            } catch (Exception e) {
                log.error("关闭流对象出错 e:{}",e);
            }
        }
    }


    /**
     * 根据图片的绝对路径构建图片输出流
     * 单台机器部署可以使用,分布式的情况不能这样
     */
    public static void downloadFileByLocal(HttpServletResponse response, File file, ContentTypeEnum type) throws Exception {
        downloadFileByInputStream(response, new FileInputStream(file), type);
    }

    /**
     * 根据输入流返回
     * zip
     */
    public static void downloadFileByInputStream(HttpServletResponse response, InputStream fileinput, ContentTypeEnum type) {
        BufferedInputStream bis = null;
        OutputStream out = null;
        try {
            bis = new BufferedInputStream(fileinput);
            if (type != ContentTypeEnum.OCTET_STREAM) {
                response.reset();
                response.setContentType(type.getValue() + ";charset=UTF-8");
            }
            response.addHeader("Content-Transfer-Encoding", "base64");
            out = response.getOutputStream();
            byte[] buffer = new byte[4096];
            int size = 0;
            while ((size = bis.read(buffer, 0, buffer.length)) != -1) {
                out.write(buffer, 0, size);
            }
        }  catch (IOException e) {
            log.error("下载文件出错：读写出错:{}", e.getMessage());
        } finally {
            try {
                if (out != null) {
                    out.close();
                }
            } catch (IOException e) {
            }

            try {
                if (bis != null) {
                    bis.close();
                }
            } catch (IOException e) {
            }

            try {
                if (fileinput != null) {
                    fileinput.close();
                }
            } catch (IOException e) {
            }
        }
    }


    /*
     *根据图片的url资源返回图片的输出流
     */
    public static void showPicByUrl(HttpServletResponse response, String url) {
        try (InputStream is = new URL(url).openStream()) {
            downloadFileByInputStream(response, is, ContentTypeEnum.JPG);
        } catch (Exception e) {
            log.error("图片链接资源不存在: ,url:{},error:{}", url, e);
        }
    }
}
