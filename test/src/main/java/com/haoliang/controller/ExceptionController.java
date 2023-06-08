package com.haoliang.controller;

import com.haoliang.common.model.JsonResult;
import lombok.extern.slf4j.Slf4j;
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
public class ExceptionController {


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
            sb.append("参数名[").append(field).append("]").append(msg).append(",");
        }
        sb.deleteCharAt(sb.length() - 1);
        return JsonResult.failureResult(sb.toString());
    }

}



