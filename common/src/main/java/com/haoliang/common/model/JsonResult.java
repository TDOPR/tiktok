package com.haoliang.common.model;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.haoliang.common.enums.ReturnMessageEnum;
import com.haoliang.common.util.StringUtil;
import org.springframework.http.HttpStatus;

import java.io.Serializable;

/**
 * @author Dominick Li
 * @CreateTime 2021/10/9 14:05
 * @description 返回数据封装
 **/
public class JsonResult<T> implements Serializable {

    /**
     * 成功标识 200成功，其它异常
     */
    private int code = 200;

    /**
     * 提示信息
     */
    private String msg;

    /**
     * 数据
     */
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private T data;

    private static final long serialVersionUID = -7268040542410707954L;


    public JsonResult() {
    }

    public JsonResult(int code) {
        this.setCode(code);
    }

    public JsonResult(int code, String msg) {
        this(code);
        this.setMsg(msg);
    }

    public JsonResult(int code, String msg, T data) {
        this(code, msg);
        this.setData(data);
    }

    public static JsonResult build(boolean flag) {
        return new JsonResult(flag ? HttpStatus.OK.value() : HttpStatus.INTERNAL_SERVER_ERROR.value(), flag ? ReturnMessageEnum.OK.toString() : ReturnMessageEnum.FAILED.toString());
    }

    public static JsonResult successResult() {
        return new JsonResult(HttpStatus.OK.value(), ReturnMessageEnum.OK.toString());
    }

    public static JsonResult successResult(String msg) {
        return new JsonResult(HttpStatus.OK.value(), defaultSuccessMsg(msg));
    }


    public static <T> JsonResult<T> successResult(T obj) {
        return new JsonResult(HttpStatus.OK.value(), ReturnMessageEnum.OK.toString(), obj);
    }

    public static <T> JsonResult<T> successTestResult(T obj) {
        return new JsonResult(HttpStatus.OK.value(), "ok", obj);
    }


    public static <T> JsonResult<T> successResult(String msg, T obj) {
        return new JsonResult(HttpStatus.OK.value(), defaultSuccessMsg(msg), obj);
    }


    public static JsonResult failureResult() {
        return new JsonResult(HttpStatus.INTERNAL_SERVER_ERROR.value(), ReturnMessageEnum.FAILED.toString());
    }

    public static JsonResult failureResult(Integer code, String msg) {
        return new JsonResult(code, defautlErrorMsg(msg));
    }

    public static JsonResult failureResult(Integer code, String msg, Object data) {
        return new JsonResult(code, defautlErrorMsg(msg), data);
    }

    public static JsonResult failureResult(String msg) {
        return new JsonResult(HttpStatus.INTERNAL_SERVER_ERROR.value(), defautlErrorMsg(msg));
    }

    public static JsonResult failureResult(ReturnMessageEnum returnMessageEnum) {
        return new JsonResult(HttpStatus.INTERNAL_SERVER_ERROR.value(), returnMessageEnum.toString());
    }
    public static JsonResult failureResult(Integer code,ReturnMessageEnum returnMessageEnum) {
        return new JsonResult(code, returnMessageEnum.toString());
    }

    public static JsonResult failureResult(Object data) {
        return new JsonResult(HttpStatus.INTERNAL_SERVER_ERROR.value(), ReturnMessageEnum.FAILED.toString(), data);
    }

    protected static String defautlErrorMsg(String msg) {
        if (StringUtil.isNotEmpty(msg)) {
            return msg;
        } else {
            return ReturnMessageEnum.FAILED.toString();
        }
    }

    protected static String defaultSuccessMsg(String msg) {
        if (StringUtil.isNotEmpty(msg)) {
            return msg;
        } else {
            return ReturnMessageEnum.OK.toString();
        }
    }

    public int getCode() {
        return code;
    }

    public void setCode(int code) {
        this.code = code;
    }

    public String getMsg() {
        return msg;
    }

    public void setMsg(String msg) {
        this.msg = msg;
    }

    public T getData() {
        return data;
    }

    public void setData(T data) {
        this.data = data;
    }


}
