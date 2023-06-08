package com.haoliang.security;

import com.alibaba.fastjson.JSONObject;
import com.haoliang.common.constant.SystemConstants;
import com.haoliang.common.enums.ReturnMessageEnum;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.util.MessageUtil;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.web.AuthenticationEntryPoint;
import org.springframework.stereotype.Component;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

/**
 * @author Dominick Li
 * @description 自定义认证失败异常
 **/
@Component
public class MyAuthenticationEntryPoint implements AuthenticationEntryPoint, java.io.Serializable {

    private static final long serialVersionUID = -8970718410437077606L;

    @Override
    public void commence(HttpServletRequest request,
                         HttpServletResponse response,
                         AuthenticationException authException) throws IOException {
        // This is invoked when sysUser tries to access a secured REST resource without supplying any credentials
        // We should just send a 401 Unauthorized response because there is no 'login page' to redirect to
        String language = request.getHeader(SystemConstants.LANGUAGE);
        JsonResult jsonResult = JsonResult.failureResult(401, MessageUtil.get(ReturnMessageEnum.UNAUTHORIZED.getKey(), language));
        response.setCharacterEncoding("UTF-8");
        response.getWriter().println(JSONObject.toJSONString(jsonResult));
    }


}
