package com.haoliang.service;


import com.haoliang.common.model.JsonResult;

public interface SysDictionaryService {

    JsonResult modifyBaseDictionary(java.util.Map<String, String> data);

    JsonResult reloadSetting();
}
