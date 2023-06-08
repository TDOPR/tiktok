package com.haoliang.controller;

import com.alibaba.fastjson.JSONObject;
import com.haoliang.common.annotation.IgnoreWebSecurity;
import com.haoliang.common.annotation.PrintLog;
import com.haoliang.common.annotation.RepeatSubmit;
import com.haoliang.common.config.AppParamProperties;
import com.haoliang.common.config.GlobalProperties;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.model.ThreadLocalManager;
import com.haoliang.common.model.dto.PageDTO;
import com.haoliang.common.util.IpAddrUtil;
import com.haoliang.enums.TiktokSettingEnum;
import com.haoliang.model.dto.*;
import com.haoliang.model.vo.*;
import com.haoliang.service.*;
import com.haoliang.service.impl.WalletsServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;

/**
 * @author Dominick Li
 * @Description 首页
 * @CreateTime 2023/4/16 10:30
 **/
@RestController
@RequestMapping("/home")
public class HomeController {

    @Autowired
    private AppUserService appUserService;

    @Resource
    private AppVersionService appVersionService;

    @Resource
    private AppParamProperties appParamProperties;

    @Autowired
    private WalletsServiceImpl walletsService;

    /**
     * 主页信息
     */
    @PostMapping
    @IgnoreWebSecurity
    public JsonResult<HomeVO> home(@RequestBody PageDTO pageDTO) {
        return appUserService.home(pageDTO);
    }

    /**
     * 获取客服联系账号
     */
    @IgnoreWebSecurity
    @GetMapping("/getCustomer")
    public JsonResult getCustomer() {
        JSONObject object = new JSONObject();
        object.put("customer", walletsService.getCustomerMap().get(ThreadLocalManager.getLanguage()));
        return JsonResult.successResult(object);
    }

    /**
     * 注册 奖励
     */
    @PrintLog
    @RepeatSubmit
    @PostMapping("/register")
    public JsonResult register(@Valid @RequestBody AppUserRegisterDTO appUserRegisterDTO, HttpServletRequest request) {
        return appUserService.register(appUserRegisterDTO, IpAddrUtil.getLocalIp(request));
    }

    /**
     * 登录
     */
    @PostMapping("/login")
    public JsonResult login(@Valid @RequestBody AppUserLoginDTO appUserLogin, HttpServletRequest request) {
        return appUserService.login(appUserLogin, IpAddrUtil.getLocalIp(request));
    }

    /**
     * 找回密码
     */
    @PostMapping("/findPassword")
    public JsonResult findPassword(@Valid @RequestBody FindPasswordDTO findPasswordDTO) {
        return appUserService.findPassword(findPasswordDTO);
    }

    /**
     * 获取隐私政策和用户协议pdf下载链接
     */
    @GetMapping("/getPdfUrl")
    public JsonResult getPdfUrl() {
        String language = ThreadLocalManager.getLanguage();
        //GlobalProperties.
        String prevUrl = GlobalProperties.getCallBackUrl() + appParamProperties.getVirtualPathPrefix() + "/";
        String userAgreementUrl = String.format("%spdf/%s/user.pdf", prevUrl, language);
        String privacyPolicyUrl = String.format("%spdf/%s/privacy.pdf", prevUrl, language);
        return JsonResult.successResult(new PdfVO(userAgreementUrl, privacyPolicyUrl));
    }

    /**
     * 检测版本更新信息
     */
    @PostMapping("/checkVersion")
    public JsonResult<CheckVersionVO> checkVersion(@RequestBody CheckVersionDTO checkVersionDTO) {
        return appVersionService.checkVersion(checkVersionDTO);
    }

    /**
     * 获取官网需要的app下载地址
     */
    @GetMapping("/getAppDownloadAddress")
    public JsonResult<AppDownloadVO> getAppDownloadAddress() {
        return JsonResult.successResult(AppDownloadVO.builder()
                .ios(TiktokSettingEnum.IOS.stringValue())
                .android(TiktokSettingEnum.ANDROID.stringValue())
                .build());
    }


}
