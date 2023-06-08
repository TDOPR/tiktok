package com.haoliang.common.util;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import java.io.*;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

/**
 * @author Dominick Li
 * @CreateTime 2019/08-/3 22:09
 * @description zip压缩操作工具类
 **/
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class ZipUtil {

    private static boolean compress(File f, String baseDir, ZipOutputStream zos) {
        if (!f.exists()) {
            System.out.println("待压缩的文件目录或文件" + f.getName() + "不存在");
            return false;
        }
        File[] fs = f.listFiles();
        BufferedInputStream bis = null;
        byte[] bufs = new byte[1024 * 10];
        FileInputStream fis = null;
        if(baseDir.length() == 1){
            baseDir = "";
        }
        try {
            for (int i = 0; i < fs.length; i++) {
                String fName = fs[i].getName();
                System.out.println("压缩：" + baseDir + fName);
                if (fs[i].isFile()) {
                    ZipEntry zipEntry = new ZipEntry(baseDir + fName);//
                    zos.putNextEntry(zipEntry);
                    //读取待压缩的文件并写进压缩包里
                    fis = new FileInputStream(fs[i]);
                    bis = new BufferedInputStream(fis, 1024 * 10);
                    int read = 0;
                    while ((read = bis.read(bufs, 0, 1024 * 10)) != -1) {
                        zos.write(bufs, 0, read);
                    }
                } else if (fs[i].isDirectory()) {
                    compress(fs[i], baseDir + fName + "/", zos);
                }
                if (null != bis)
                    bis.close();
                if (null != fis)
                    fis.close();
            }
            return true;
        } catch (IOException e) {
            e.printStackTrace();
            return false;
        }

    }

    /**
     * 压缩目录成zip包
     * @param sourcePath 目录路径
     * @param zipName   zip包名称
     */
    public static String floderPathToZip(String sourcePath,String zipName) {
        File soucrceFile = new File(sourcePath);
        String zipPath = soucrceFile.getParentFile().getAbsolutePath() + File.separator + zipName+".zip";
        File zipFile = new File(zipPath);
        try (ZipOutputStream zos = new ZipOutputStream(new FileOutputStream(zipFile))) {
            System.out.println(File.separator);
            String baseDir = sourcePath.substring(sourcePath.lastIndexOf("/") + 1) + "/";
            System.out.println(baseDir);
            if (compress(soucrceFile, baseDir, zos)) {
                return zipPath;
            }
            return null;
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }


    public static void main(String[] args) throws Exception {
       String zipPath= floderPathToZip("D:\\solist\\docso\\data\\temp\\test","9ce55a26a595488c8923cbb7e73dcc80");
        System.out.println(zipPath);
        Thread.sleep(100099900L);
    }


    private static boolean FilesToCompress(List<String> files, String baseDir, ZipOutputStream zos) {

        BufferedInputStream bis = null;
        byte[] bufs = new byte[1024 * 10];
        FileInputStream fis = null;
        try {
            for (int i = 0; i < files.size(); i++) {
                File fs = new File(files.get(i));
                if(!fs.exists()){
                    continue;
                }
                try {
                    String fName = fs.getName();
                    System.out.println("压缩：" + baseDir + fName);
                    ZipEntry zipEntry = new ZipEntry(baseDir + fName);//
                    zos.putNextEntry(zipEntry);
                    //读取待压缩的文件并写进压缩包里
                    fis = new FileInputStream(fs);
                    bis = new BufferedInputStream(fis, 1024 * 10);
                    int read = 0;
                    while ((read = bis.read(bufs, 0, 1024 * 10)) != -1) {
                        zos.write(bufs, 0, read);
                    }
                }finally {
                    try {
                        if (null != bis)
                            bis.close();
                        if (null != fis)
                            fis.close();
                    } catch (IOException e) {
                        e.printStackTrace();
                    }
                }
            }
            return true;
        } catch (IOException e) {
            e.printStackTrace();
            return false;
        }

    }


}

