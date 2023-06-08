package com.haoliang.common.model;

import lombok.Getter;

import java.io.Serializable;

/**
 * @author Dominick Li
 * @CreateTime 2021/10/9 14:05
 * @description 返回http请求数据封装
 **/
@Getter
public class HttpResult<T> implements Serializable {

    /**
     * 成功标识 200成功，其它异常
     */
    private boolean success;

    /**
     * 提示信息
     */
    private String msg;

    /**
     * 数据
     */
    private T data;

    private static final long serialVersionUID = -7268040542410707954L;


    public HttpResult() {
    }

    public HttpResult(boolean success, T data) {
        this.success=success;
        this.data=data;
    }

    public HttpResult(boolean success,String msg) {
        this.success=success;
        this.msg=msg;
    }


    public static <T> HttpResult<T> successResult(T obj) {
        return new HttpResult(true, obj);
    }

    public static HttpResult failureResult(String msg) {
        return new HttpResult(false,msg);
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


    public void setSuccess(boolean success) {
        this.success = success;
    }
}
