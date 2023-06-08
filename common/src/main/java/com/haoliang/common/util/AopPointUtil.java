package com.haoliang.common.util;

import com.haoliang.common.constant.SystemConstants;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.reflect.MethodSignature;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.multipart.support.StandardMultipartHttpServletRequest;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.HashMap;
import java.util.Map;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/10/26 11:37
 **/
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class AopPointUtil {

    /**
     * 获取切面中的参数
     */
    public static Map<String, Object> getRequestParamsByJoinPoint(JoinPoint joinPoint) {
        //参数名
        String[] paramNames = ((MethodSignature) joinPoint.getSignature()).getParameterNames();
        //参数值
        Object[] paramValues = joinPoint.getArgs();

        Map<String, Object> requestParams = new HashMap<>(paramNames.length);
        for (int i = 0; i < paramNames.length; i++) {
            if (paramNames[i].equals(SystemConstants.TOKEN_NAME)) {
                //过滤token参数
                continue;
            }
            Object value = paramValues[i];
            //如果是文件对象
            if (value instanceof MultipartFile) {
                MultipartFile file = (MultipartFile) value;
                value = file.getOriginalFilename();  //获取文件名
            } else if (value instanceof StandardMultipartHttpServletRequest) {
                StandardMultipartHttpServletRequest req = (StandardMultipartHttpServletRequest) value;
                value = req.getParameterMap().get("filename");
                // MultipartFile file=
            } else if (value instanceof HttpServletRequest || value instanceof HttpServletResponse) {
                continue;
            }
            requestParams.put(paramNames[i], value);
        }
        return requestParams;
    }

}
