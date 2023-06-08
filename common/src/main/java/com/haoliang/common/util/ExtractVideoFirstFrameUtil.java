package com.haoliang.common.util;


import org.bytedeco.javacpp.opencv_core;
import org.bytedeco.javacv.FFmpegFrameGrabber;
import org.bytedeco.javacv.Frame;
import org.bytedeco.javacv.Java2DFrameConverter;
import org.bytedeco.javacv.OpenCVFrameConverter;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/5/10 14:04
 **/
public class ExtractVideoFirstFrameUtil {

    private final static String TYPE="jpg";

    /**
     * 提取视频第一帧图片
     *
     * @param mp4Path 视频地址
     * @param picPath 图片地址
     * @return 提取的图片地址
     */
    public static boolean ffmpegExtractImage(String mp4Path, String picPath) {
        try {
            FFmpegFrameGrabber ff = FFmpegFrameGrabber.createDefault(mp4Path);
            ff.start();
            String rotate = ff.getVideoMetadata("rotate");
            Frame f;
            int i = 0;
            f = ff.grabImage();
            opencv_core.IplImage src = null;
            if (null != rotate && rotate.length() > 1) {
                OpenCVFrameConverter.ToIplImage converter = new OpenCVFrameConverter.ToIplImage();
                src = converter.convert(f);
                f = converter.convert(rotate(src, Integer.valueOf(rotate)));
            }
            if (null == f || null == f.image) {
                return false;
            }
            Java2DFrameConverter converter = new Java2DFrameConverter();
            BufferedImage bi = converter.getBufferedImage(f);
            File output = new File(picPath);
            try {
                ImageIO.write(bi, TYPE, output);
            } catch (IOException e) {
                e.printStackTrace();
            }
            ff.stop();
            return true;
        } catch (Exception e) {
            return false;
        }
    }

    /*
     * 旋转角度的。这个是为了保证截取到的图和视频中的旋转信息一致
     */
    public static opencv_core.IplImage rotate(opencv_core.IplImage src, int angle) {
        opencv_core.IplImage img = opencv_core.IplImage.create(src.height(), src.width(), src.depth(), src.nChannels());
        opencv_core.cvTranspose(src, img);
        opencv_core.cvFlip(img, img, angle);
        return img;
    }

    public static void main(String[] args) {
        boolean flag = ffmpegExtractImage("C:\\Users\\Administrator\\Desktop\\公司文档\\Tiktok\\视频\\TTG机制MG动画_英文版.mp4", "C:\\Users\\Administrator\\Desktop\\公司文档\\Tiktok\\视频\\TTG机制MG动画_英文版.jpg");
    }
}
