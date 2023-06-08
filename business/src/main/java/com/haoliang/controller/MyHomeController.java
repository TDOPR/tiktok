package com.haoliang.controller;

import com.haoliang.common.annotation.PrintLog;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.model.dto.UpdatePasswordDTO;
import com.haoliang.model.TikTokAccount;
import com.haoliang.model.dto.AppUserDTO;
import com.haoliang.model.dto.MobileDTO;
import com.haoliang.model.vo.ItemInfoVO;
import com.haoliang.model.vo.MyDetailVO;
import com.haoliang.model.vo.TiktokInfoVO;
import com.haoliang.service.AppUserService;
import com.haoliang.sms.enums.CountryTelephoneCode;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

/**
 * 我的主页
 * @author Dominick Li
 * @CreateTime 2022/11/18 11:04
 **/
@RestController
@RequestMapping("/myhome")
public class MyHomeController {

    @Autowired
    private AppUserService appUserService;

    /**
     * 获取用户和平台详细信息
     */
    @GetMapping
    public JsonResult<MyDetailVO> getMyDetail() {
        return appUserService.getMyDetail();
    }

    /**
     * 修改用户名
     */
    @PrintLog
    @PostMapping("/modifyUserName")
    public JsonResult modifyUserDetail(@RequestBody AppUserDTO appUserDTO) {
        return appUserService.modifyUserName(appUserDTO);
    }

    /**
     * 获取发送短信验证码需要填写的国家地区编码
     */
    @GetMapping("/getCountryCode")
    public JsonResult getCountryCode() {
        return JsonResult.successResult(CountryTelephoneCode.getCountryTelephoneCode());
    }

    /**
     * 发送短信验证码
     */
    @GetMapping("/sendSms/{mobile}/{type}")
    public JsonResult sendSms(@PathVariable String mobile,@PathVariable Integer type) {
        return appUserService.sendSms(mobile,type);
    }

    /**
     * 绑定手机号
     */
    @PostMapping("/bindMobile")
    public JsonResult bindMobile(@RequestBody MobileDTO mobileDTO) {
        return appUserService.bindMobile(mobileDTO);
    }

    /**
     * 获取已关联的tiktok账号信息
     *
     * @return
     */
    @GetMapping("/getTiktokInfo")
    public JsonResult<TiktokInfoVO> tiktok() {
        return appUserService.getTiktokInfo();
    }

    /**
     * 绑定Tiktok账号
     */
    @PostMapping("/bindTiktok")
    public JsonResult bindTiktok(@RequestBody TikTokAccount tikTokAccount) {
        return appUserService.bindTiktok(tikTokAccount);
    }

    /**
     * 更换Tiktok账号
     */
    @PostMapping("/editTiktok")
    public JsonResult editTiktok(@RequestBody TikTokAccount tikTokAccount) {
        return appUserService.editTiktok(tikTokAccount);
    }

    /**
     * 上传头像
     */
    @PostMapping("/uploadHeadImage")
    public JsonResult uploadHeadImage(@RequestParam("file") MultipartFile file) throws Exception {
        return appUserService.uploadHeadImage(file);
    }

    /**
     * 修改密码
     */
    @PostMapping("/updatePassword")
    public JsonResult updatePassword(@RequestBody UpdatePasswordDTO updatePasswordDTO) {
        return appUserService.updatePassword(updatePasswordDTO);
    }

    /**
     * 跳过新手教程
     */
    @GetMapping("/skip")
    public JsonResult skip(){
        return appUserService.skip();
    }

    /**
     * 查看团队信息
     */
    @GetMapping("/itemInfo")
    public JsonResult<ItemInfoVO> itemInfo(){
        return  appUserService.getItemInfo();
    }

    /**
     * 退出登录
     */
    @GetMapping("/loginOut")
    public JsonResult loginOut() {
        return appUserService.loginOut();
    }


}
