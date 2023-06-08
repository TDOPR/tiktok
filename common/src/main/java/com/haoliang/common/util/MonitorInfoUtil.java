package com.haoliang.common.util;

import com.sun.management.OperatingSystemMXBean;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.StringUtils;

import java.io.*;
import java.lang.management.ManagementFactory;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * @Description 获取服务器的性能指标数据
 * @Author lijunming
 * @Date 2019/8/13 19:01
 **/
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class MonitorInfoUtil {

    private  static  String moninorCmd;

    static {
        String userDir = System.getProperty("user.dir");
        moninorCmd = userDir +File.separator+ "bin" + File.separator + "server_monitor.sh";
    }


    private final static Logger logger = LoggerFactory.getLogger(MonitorInfoUtil.class);

    public static void main(String[] args) {
//        String a="user=21.89  totald=230.28 diskUsage=9.51";
//        String arr[]=a.split(" ");
//        List items=new ArrayList();
//        items.add(arr[0].split("=")[1]+"G/"+arr[1].split("=")[1]+"G");
//        System.out.println(items.size());
        //getMontiorList();
        System.out.println(getDiskUsage());
    }

    public static List getMontiorList() {
        long startTime=System.currentTimeMillis();
        if (OsUtil.isLinuxOs()) {
            return runShell(moninorCmd);
        } else if (OsUtil.isWindowOs()) {
            List list = new ArrayList<>();
            list.add(getCpuUsage());
            list.add(getDiskUsage());
            list.add(getMemoryUsage());
            list.add(getGpuUsageRate());
            long stopTime=System.currentTimeMillis();
            logger.info("获取服务器性能数据耗时："+(stopTime-startTime));
            System.out.println(stopTime-startTime);
            return list;
        }
        return null;
    }

    //执行脚步获取服务器性能信息
    public static List runShell(String shStr) {
        try {
            List list = new ArrayList();
            Process process;
            process = Runtime.getRuntime().exec(new String[]{"/bin/sh", "-c", shStr}, null, null);
            InputStreamReader ir = new InputStreamReader(process
                    .getInputStream());
            LineNumberReader input = new LineNumberReader(ir);
            String line;
            process.waitFor();
            int i=0;
            List items=null;
            StringBuilder gpuInfoTextBuilder=new StringBuilder();
            logger.info("********************linux 收集服务器监控信息 开始*****************************");
            while ((line = input.readLine()) != null) {
                logger.info(line);
                if(i==0){
                    list.add(line);
                }else if(i==1){
                    String[] arr=line.split(" ");
                    items=new ArrayList();
                    items.add(arr[0].split("=")[1]+"G/"+arr[1].split("=")[1]+"G");
                    items.add(arr[2].split("=")[1]);
                    list.add(items);
                }else{
                    gpuInfoTextBuilder.append(line + "\n");
                }
                i++;
            }
            String gpus=gpuInfoTextBuilder.toString();
            items=readGpu(gpus);
            list.add(items);
            logger.info("********************linux 收集服务器监控信息 结束*****************************");
            return list;
        } catch (Exception e) {
            logger.error("runShell cmd={} ,error={}", shStr, e);
            return null;
        }
    }


    /**
     * 获取内存使用率
     */
    public static List<String> getMemoryUsage() {
        List list=new ArrayList();
        OperatingSystemMXBean osmxb = (OperatingSystemMXBean) ManagementFactory.getOperatingSystemMXBean();
        //总的物理内存
        long totalvirtualMemory = osmxb.getTotalPhysicalMemorySize();
        //剩余物理内存
        long freePhysicalMemorySize = osmxb.getFreePhysicalMemorySize();
        String first=String.format("%.2f",(totalvirtualMemory-freePhysicalMemorySize)*1.00/1024/1024/1024)+"G";
        String second=String.format("%.2f",totalvirtualMemory*1.00/1024/1024/1024)+"G";
        list.add(first+"/"+second);
        list.add(String.format("%.2f",(1 - freePhysicalMemorySize * 1.0 / totalvirtualMemory) * 100));
        return list;
    }

    /**
     * 获取磁盘使用率
     */
    public static List<String> getDiskUsage() {
        //总空间
        long allTotal = 0;
        //剩余空间
        long allFree = 0;
        for (char c = 'A'; c <= 'Z'; c++) {
            String dirName = c + ":/";
            File win = new File(dirName);
            if (win.exists()) {
                long total = (long) win.getTotalSpace();
                long free = (long) win.getFreeSpace();
                allTotal = allTotal + total;
                allFree = allFree + free;
            }
        }
        List list=new ArrayList();
        String first=String.format("%.2fG",(allTotal-allFree)*1.00/1024/1024/1024);
        String second=String.format("%.2fG",(allTotal*1.00/1024/1024/1024));
        list.add(first+"/"+second);
        list.add(String.format("%.2f",(1 - allFree * 1.0 / allTotal) * 100));
        return list;
    }


    private static final int PERCENT = 100;
    private static final int FAULTLENGTH = 10;

    /**
     * 计算cpu占用率
     */
    public static String getCpuUsage() {
        try {
            String procCmd = System.getenv("windir") + "//system32//wbem//wmic.exe process get Caption,CommandLine,KernelModeTime,ReadOperationCount,ThreadCount,UserModeTime,WriteOperationCount";
            //第一次读取CPU信息
            long[] c0 = readCpu(Runtime.getRuntime().exec(procCmd));
            //第二次次读取CPU信息
            long[] c1 = readCpu(Runtime.getRuntime().exec(procCmd));
            if (c0 != null && c1 != null) {
                long idletime = c1[0] - c0[0];
                long busytime = c1[1] - c0[1];
                return String.format("%.2f",(PERCENT * (busytime) * 1.0 / (busytime + idletime)));
            } else {
                return "0";
            }
        } catch (Exception e) {
            e.printStackTrace();
            return "0";
        }
    }


    /**
     * 获取gpu信息
     */
    public static List<String> getGpuUsageRate() {
        Process process = null;
        try {
            process = Runtime.getRuntime().exec("nvidia-smi.exe");
            process.getOutputStream().close();
            BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream()));
            StringBuffer stringBuffer = new StringBuffer();
            String line = "";
            while (null != (line = reader.readLine())) {
                stringBuffer.append(line + "\n");
            }
            String gpus=stringBuffer.toString();
            return readGpu(gpus);
        } catch (IOException e) {
            e.printStackTrace();
            return Arrays.asList("0Mib","0");
        }
    }




    //读取gpu信息
    private static List<String> readGpu(String text){
        try {
            String[] split = text.split("\\|===============================\\+======================\\+======================\\|");
            String[] gpusInfo = split[1].split("                                                                               ");
            // 分割多个gpu
            String[] gpuInfo = gpusInfo[0].split("\\+-------------------------------\\+----------------------\\+----------------------\\+");
            List<String> gpuInfoList = new ArrayList<>();
            Integer usedMemory = 0;
            Integer totalMenory = 0;
            for (int i = 0; i < gpuInfo.length - 1; i++) {
                String[] nameAndInfo = gpuInfo[i].split("\n");
                String[] split1 = nameAndInfo[1].split("\\|")[1]
                        .split("\\s+");//去空格
                StringBuffer name = new StringBuffer();
                for (int j = 0; j < split1.length - 1; j++) {
                    if (j > 1 && j != split1.length) {
                        name.append(split1[j] + " ");
                    }
                }
                String[] info = nameAndInfo[2].split("\\|")[2].split("\\s+");
                usedMemory += Integer.parseInt(info[1].split("MiB")[0]);
                totalMenory += Integer.parseInt(info[3].split("MiB")[0]);
            }
            Double usageRate = usedMemory * 100.00 / totalMenory;
            String first = usedMemory.toString() + "Mib";
            String second = totalMenory.toString() + "Mib";
            gpuInfoList.add(first + "/" + second);
            gpuInfoList.add(String.format("%.2f", usageRate));
            return gpuInfoList;
        }catch (Exception e){
            return Arrays.asList("0Mib","0");
        }
    }


    //读取cpu信息
    private static long[] readCpu(final Process proc) {
        long[] retn = new long[2];
        try {
            proc.getOutputStream().close();
            InputStreamReader ir = new InputStreamReader(proc.getInputStream());
            LineNumberReader input = new LineNumberReader(ir);
            String line = input.readLine();
            if (line == null || line.length() < FAULTLENGTH) {
                return null;
            }
            int capidx = line.indexOf("Caption");
            int cmdidx = line.indexOf("CommandLine");
            int rocidx = line.indexOf("ReadOperationCount");
            int umtidx = line.indexOf("UserModeTime");
            int kmtidx = line.indexOf("KernelModeTime");
            int wocidx = line.indexOf("WriteOperationCount");
            long idletime = 0;
            //读取物理设备时间
            long kneltime = 0;
            //执行代码占用时间
            long usertime = 0;
            while ((line = input.readLine()) != null) {
                if (line.length() < wocidx) {
                    continue;
                }
                String caption = substring(line, capidx, cmdidx - 1).trim();
                String cmd = substring(line, cmdidx, kmtidx - 1).trim();

                if (cmd.indexOf("wmic.exe") >= 0) {
                    continue;
                }
                String s1 = substring(line, kmtidx, rocidx - 1).trim();
                String s2 = substring(line, umtidx, wocidx - 1).trim();
                List<String> digitS1 = getDigit(s1);
                List<String> digitS2 = getDigit(s2);
                if ("System Idle Process".equals(caption) || "System".equals(caption)) {
                    if (s1.length() > 0) {
                        if (StringUtils.hasText(digitS1.get(0))) {
                            idletime += Long.valueOf(digitS1.get(0)).longValue();
                        }
                    }
                    if (s2.length() > 0) {
                        if (StringUtils.hasText(digitS2.get(0))) {
                            idletime += Long.valueOf(digitS2.get(0)).longValue();
                        }
                    }
                    continue;
                }
                if (s1.length() > 0) {
                    if (StringUtils.hasText(digitS1.get(0))) {
                        kneltime += Long.valueOf(digitS1.get(0)).longValue();
                    }
                }

                if (s2.length() > 0) {
                    if (StringUtils.hasText(digitS2.get(0))) {
                        kneltime += Long.valueOf(digitS2.get(0)).longValue();
                    }
                }
            }
            retn[0] = idletime;
            retn[1] = kneltime + usertime;
            return retn;
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            try {
                proc.getInputStream().close();
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        return null;
    }

    //从字符串文本中获得数字
    private static List<String> getDigit(String text) {
        List<String> digitList = new ArrayList<String>();
        digitList.add(text.replaceAll("\\D", ""));
        return digitList;
    }

    //截取字符串
    private static String substring(String src, int startIdx, int endIdx) {
        byte[] b = src.getBytes();
        String tgt = "";
        for (int i = startIdx; i <= endIdx; i++) {
            tgt += (char) b[i];
        }
        return tgt;
    }


}
