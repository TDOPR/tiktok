package com.haoliang.server;

import com.haoliang.common.enums.ReturnMessageEnum;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.util.ErrorLogUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseBody;

/**
 * @author Dominick Li
 * @description 全局异常处理类
 **/
@ControllerAdvice
@Slf4j
public class GlobalExceptionAdvice {

    @ExceptionHandler(value = Exception.class)
    @ResponseBody
    public JsonResult exceptionHndler(Exception e) {
        if (e instanceof AccessDeniedException) {
            return JsonResult.failureResult(HttpStatus.FORBIDDEN.value(), "No permission to perform this operation!");
        }
        log.error("globale exception  msg={}", e);
        ErrorLogUtil.save(e);
        return JsonResult.failureResult(ReturnMessageEnum.ERROR);
    }

    @ResponseBody
    @ExceptionHandler(MethodArgumentNotValidException.class)
    public JsonResult handleValidException(MethodArgumentNotValidException e) {
        //将错误信息返回给前台
        String field, msg;
        StringBuilder sb = new StringBuilder();
        for (FieldError fieldError : e.getBindingResult().getFieldErrors()) {
            // 获取错误验证字段名
            field = fieldError.getField();
            msg = fieldError.getDefaultMessage();
            sb.append("param[").append(field).append("]").append(msg).append(",");
        }
        sb.deleteCharAt(sb.length() - 1);
        return JsonResult.failureResult(sb.toString());
    }

}



