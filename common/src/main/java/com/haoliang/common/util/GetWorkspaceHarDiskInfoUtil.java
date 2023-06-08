package com.haoliang.common.util;


import com.haoliang.common.model.dto.WorkspaceHarDiskInfo;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import java.io.File;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

@Slf4j
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class GetWorkspaceHarDiskInfoUtil {

    /**
     * 获取shell执行脚本
     * @param command
     * @return
     */
    public static List<String> runShell(String command) {
        try {
            //容器
            List<String> list = new ArrayList();
            Process process;
            process = Runtime.getRuntime().exec(new String[]{"/bin/sh", "-c", command}, null, null);
            InputStreamReader ir = new InputStreamReader(process
                    .getInputStream());
            LineNumberReader input = new LineNumberReader(ir);
            String line;
            process.waitFor();
            System.out.println("********************linux 执行shell 开始*****************************");
            int rowIndex = 0;
            while ((line = input.readLine()) != null) {
                System.out.println(line);
                if (rowIndex > 0) {
                    //把连续的空格转成1个空格
                    line = line.replaceAll(" + ", " ");
                    list.add(line);
                }
                rowIndex++;
            }
            System.out.println("********************linux 执行shell 结束*****************************");
            return list;
        } catch (Exception e) {
            log.error("runShell  ,error={}", e.getMessage());
        }
        return Collections.EMPTY_LIST;
    }

    /**
     * 获取应用文件存储磁盘的信息
     *
     * @return
     */
    public static WorkspaceHarDiskInfo getWorkspaceHarDiskInfo(String savePath) {
        if (!new File(savePath).exists()) {
            System.err.println(String.format("%s not exists",savePath));
            return null;
        }
        if (OsUtil.isLinuxOs()) {
            String command = String.format("df -h  %s", savePath);
            List<String> list = runShell(command);
            if (list == Collections.EMPTY_LIST) {
                System.err.println(String.format("runShell command=%s result is null", command));
                return null;
            }
            String[] array = list.get(0).split(" ");
            // Filesystem               Size  Used Avail Use% Mounted on
            // dev/mapper/centos-root  453G  176G  255G  41% /
            return new WorkspaceHarDiskInfo(array[0], array[1], array[2], array[4]);
        } else {
            String firstChar = savePath.substring(0, 1).toUpperCase();
            //总空间
            long size = 0;
            //可用空间
            long free = 0;
            firstChar += ":/";
            File win = new File(firstChar);
            if (win.exists()) {
                size = win.getTotalSpace();
                free = win.getFreeSpace();
            }
            String usedStr = String.format("%.2fG", (size - free) * 1.00 / 1024 / 1024 / 1024);
            String sizeStr = String.format("%.2fG", (size * 1.00 / 1024 / 1024 / 1024));
            return new WorkspaceHarDiskInfo(firstChar, sizeStr, usedStr, (int) ((1 - free * 1.0 / size) * 100) + "%");
        }
    }


    public static void main(String[] args) {
        String savePath = "/home/etop/comparison-dev/data";
        savePath = "d:/solist";
        //设置硬盘使用率超过多少发送邮件通知
        Integer cordon = 60;
        WorkspaceHarDiskInfo workspaceHarDiskInfo = getWorkspaceHarDiskInfo(savePath);
        if (workspaceHarDiskInfo == null) {
            System.err.println(String.format("未获取 {} 相关的磁盘信息", savePath));
            return;
        }
        System.out.println(String.format("目录所属文件系统: %s  ,硬盘使用详情: %s/%s ,使用率达到：%s", workspaceHarDiskInfo.getFilesystem(), workspaceHarDiskInfo.getUsed(), workspaceHarDiskInfo.getSize(), workspaceHarDiskInfo.getUse()));
        if (Integer.parseInt(workspaceHarDiskInfo.getUse().substring(0, workspaceHarDiskInfo.getUse().length() - 1)) > cordon) {
            System.err.println("已超过警戒线" + cordon + "%,请及时处理，避免服务因存储不足导致异常的情况");
        } else {
            System.out.println("硬盘资源充足");
        }
    }
}
