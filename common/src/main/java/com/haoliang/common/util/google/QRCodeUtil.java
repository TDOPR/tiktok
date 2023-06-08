package com.haoliang.common.util.google;

import com.google.zxing.BarcodeFormat;
import com.google.zxing.EncodeHintType;
import com.google.zxing.MultiFormatWriter;
import com.google.zxing.common.BitMatrix;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

/**
 * @author Dominick Li
 * @Description 根据谷歌认证器秘钥信息生成二维码图片
 * @CreateTime 2022/11/21 14:32
 **/
public class QRCodeUtil {
    /**
     * 二维码颜色
     */
    private static final int BLACK = 0xFF000000;
    private static final int WHITE = 0xFFFFFFFF;

    /**
     * 图片的宽度
     */
    private static int WIDTH = 200;
    /**
     * 图片的高度
     */
    private static int HEIGHT = 200;
    /**
     * 图片的格式
     */
    private static String FORMAT = "png";


    /**
     * 生成二维码
     *
     * @param basePath 配置文件定义的生成二维码存放文件夹
     * @param content 二维码内容
     * @return 文件路径
     */
    public static String generateQRCodeImg(String basePath, String content){
        try {
            Map<EncodeHintType, String> encodeMap = new HashMap<EncodeHintType, String>();
            // 内容编码，生成二维码矩阵
            encodeMap.put(EncodeHintType.CHARACTER_SET, "utf-8");
            BitMatrix bitMatrix = new MultiFormatWriter().encode(content, BarcodeFormat.QR_CODE, WIDTH, HEIGHT, encodeMap);

            File file = new File(basePath);
            if (!file.exists() && !file.isDirectory()){
                file.mkdirs();
            }

            //文件名，默认为时间为名
            String filePath = basePath + System.currentTimeMillis() + "." + FORMAT;

            File outputFile = new File(filePath);
            if (!outputFile.exists()){
                // 生成二维码文件
                writeToFile(bitMatrix, FORMAT, outputFile);
            }
            return filePath;
        } catch (Exception e) {
            e.printStackTrace();
        }
        return "";
    }

    /**
     * 把二维码矩阵保存为文件
     *
     * @param matrix 二维码矩阵
     * @param format 文件类型，这里为png
     * @param file  文件句柄
     * @throws IOException
     */
    public static void writeToFile(BitMatrix matrix, String format, File file) throws IOException {
        BufferedImage image = toBufferedImage(matrix);
        if (!ImageIO.write(image, format, file)) {
            throw new IOException("Could not write an image of format " + format + " to " + file);
        }
    }

    /**
     * 生成二维码矩阵（内存）
     *
     * @param matrix 二维码矩阵
     * @return
     */
    public static BufferedImage toBufferedImage(BitMatrix matrix) {
        int width = matrix.getWidth();
        int height = matrix.getHeight();
        BufferedImage image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
        for (int x = 0; x < width; x++) {
            for (int y = 0; y < height; y++) {
                image.setRGB(x, y, matrix.get(x, y) ? BLACK : WHITE);
            }
        }
        return image;
    }
}
