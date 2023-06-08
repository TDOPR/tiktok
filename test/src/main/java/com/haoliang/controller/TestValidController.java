package com.haoliang.controller;

import com.alibaba.fastjson.JSONObject;
import com.haoliang.common.annotation.PrintLog;
import com.haoliang.common.model.JsonResult;
import com.haoliang.model.ValidSave;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.client.RestTemplate;

import javax.validation.Valid;

/**
 * @Description
 * @Author Dominick Li
 * @CreateTime 2022/10/21 17:21
 **/
@RestController
@RequestMapping("/")
public class TestValidController {

    @PostMapping("/testValid")
    @PrintLog
    public JsonResult login(@Valid @RequestBody ValidSave validSave) {
        return JsonResult.successResult();
    }


    public static void main(String[] args)  {
        HttpHeaders httpHeaders = new HttpHeaders();
        httpHeaders.setContentType(MediaType.APPLICATION_JSON);
        JSONObject jsonData=new JSONObject();
        jsonData.put("username","test");
        jsonData.put("password","12345");
        jsonData.put("age",17);
        HttpEntity<Object> httpEntity = new HttpEntity<>(jsonData, httpHeaders);
        RestTemplate restTemplate = new RestTemplate();
        ResponseEntity<String> entity = restTemplate.postForEntity("http://localhost:9999/testValid", httpEntity, String.class);
        System.out.println(entity.getBody());
    }

}
