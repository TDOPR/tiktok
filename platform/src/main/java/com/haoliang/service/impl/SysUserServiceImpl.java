package com.haoliang.service.impl;

import com.alibaba.excel.support.ExcelTypeEnum;
import com.alibaba.fastjson.JSONObject;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.haoliang.common.config.GlobalProperties;
import com.haoliang.common.config.SysSettingParam;
import com.haoliang.common.constant.CacheKeyPrefixConstants;
import com.haoliang.common.constant.OperationModel;
import com.haoliang.common.enums.*;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.model.PageParam;
import com.haoliang.common.model.SysLoginLog;
import com.haoliang.common.model.ThreadLocalManager;
import com.haoliang.common.model.vo.PageVO;
import com.haoliang.common.service.SysLoginLogService;
import com.haoliang.common.util.*;
import com.haoliang.common.util.encrypt.AESUtil;
import com.haoliang.common.util.excel.ExcelUtil;
import com.haoliang.common.util.google.GoogleAuthenticatorUtil;
import com.haoliang.common.util.redis.RedisUtil;
import com.haoliang.common.config.LoginConfig;
import com.haoliang.common.enums.RoleTypeEnum;
import com.haoliang.mapper.AppUserMapper;
import com.haoliang.mapper.SysRoleMapper;
import com.haoliang.mapper.SysUserMapper;
import com.haoliang.model.AppUsers;
import com.haoliang.model.SysRole;
import com.haoliang.model.SysUser;
import com.haoliang.model.condition.SysUserCondition;
import com.haoliang.model.dto.LoginDTO;
import com.haoliang.common.model.dto.UpdateStatusDTO;
import com.haoliang.model.dto.UpdateUserInfoDTO;
import com.haoliang.model.vo.ExportUserVO;
import com.haoliang.model.vo.TokenVO;
import com.haoliang.model.vo.UserVO;
import com.haoliang.service.SysUserService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletResponse;
import java.io.File;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;

import static com.haoliang.common.util.google.QRCodeUtil.generateQRCodeImg;


/**
 * @author Dominick Li
 * @CreateTime 2020/3/17 10:25
 **/
@Service
@Slf4j
public class SysUserServiceImpl extends ServiceImpl<SysUserMapper, SysUser> implements SysUserService {

    @Autowired
    private SysLoginLogService sysLoginLogService;

    @Autowired
    private LoginConfig loginConfig;

    @Resource
    private SysRoleMapper sysRoleMapper;

    @Resource
    private AppUserMapper appUserMapper;

    @Override
    public JsonResult getGoogleQRCode(LoginDTO loginDTO) {
        //校验用户是否被锁定
        if (isLock(loginDTO.getUsername())) {
            return JsonResult.failureResult(ReturnMessageEnum.ACCOUNT_LOCK.setAndToString(loginConfig.getLockTime()));
        }
        String password = loginDTO.getPassword();
        SysUser sysUser = this.getOne(new LambdaQueryWrapper<SysUser>().eq(SysUser::getUsername, loginDTO.getUsername()).eq(SysUser::getDeleted, 0));
        //登录状态标识
        boolean loginFlag = true;
        if (sysUser == null) {
            return JsonResult.failureResult(ReturnMessageEnum.ACCOUNT_NOT_EXISTS);
        } else if (!sysUser.getPassword().equals(AESUtil.encrypt(password, sysUser.getSalt()))) {
            loginFlag = false;
        } else if (sysUser.getEnabled() == 0) {
            return JsonResult.failureResult(ReturnMessageEnum.ACCOUNT_DISABLED);
        }
        //验证错误次数,当在指定时间内达到指定次数则锁定用户
        setFailCount(sysUser.getUsername(), loginFlag);
        if (!loginFlag) {
            //错误标识
            return JsonResult.failureResult(ReturnMessageEnum.PASSWORD_ERROR);
        }
        //判断是否是第一次登录
        Long count = sysLoginLogService.count(new LambdaQueryWrapper<SysLoginLog>()
                .eq(SysLoginLog::getUsername, loginDTO.getUsername())
                .eq(SysLoginLog::getUserType, 1)
        );
        if (count > 0) {
            return JsonResult.failureResult(201, "error");
        }
        String url = GoogleAuthenticatorUtil.getQRBarcodeURL(sysUser.getUsername(), loginConfig.getCaptcha().getGoogleHost(), sysUser.getGoogleSecret());
        String filePath = generateQRCodeImg(DataSavePathEnum.TMP.getPath(), url);
        String googleQRCodeUrl = GlobalProperties.getVirtualPathURL() + StringUtil.replace(filePath, GlobalProperties.getRootPath(), "");
        JSONObject object = new JSONObject();
        object.put("googleQRCodeUrl", googleQRCodeUrl);
        return JsonResult.successResult(object);
    }


    @Override
    public JsonResult login(LoginDTO loginDTO, String clientIp) {

        //校验用户是否被锁定
        if (isLock(loginDTO.getUsername())) {
            return JsonResult.failureResult(ReturnMessageEnum.ACCOUNT_LOCK.setAndToString(loginConfig.getLockTime()));
        }

        //校验图片验证码
        JsonResult jsonResult = checkImageCaptcha(loginDTO);
        if (jsonResult.getCode() != HttpStatus.OK.value()) {
            return jsonResult;
        }
        //对前端加密过的数据进行解密
        //String password = RSAUtil.decryptString(loginDTO.getPassword());
        String password = loginDTO.getPassword();
        SysUser sysUser = this.getOne(new LambdaQueryWrapper<SysUser>().eq(SysUser::getUsername, loginDTO.getUsername()).eq(SysUser::getDeleted, 0));
        //登录状态标识
        boolean loginFlag = true;
        if (sysUser == null) {
            return JsonResult.failureResult(ReturnMessageEnum.ACCOUNT_NOT_EXISTS);
        } else if (!sysUser.getPassword().equals(AESUtil.encrypt(password, sysUser.getSalt()))) {
            loginFlag = false;
        } else if (sysUser.getEnabled() == 0) {
            return JsonResult.failureResult(ReturnMessageEnum.ACCOUNT_DISABLED);
        }

        //校验google验证码
        if (!checkGoogleCaptcha(loginDTO.getCode(), sysUser.getGoogleSecret())) {
            return JsonResult.failureResult(ReturnMessageEnum.VERIFICATION_CODE_ERROR);
        }

        //验证错误次数,当在指定时间内达到指定次数则锁定用户
        setFailCount(sysUser.getUsername(), loginFlag);
        if (!loginFlag) {
            //错误标识
            return JsonResult.failureResult(ReturnMessageEnum.PASSWORD_ERROR);
        }

        String tokenKey = CacheKeyPrefixConstants.TOKEN + sysUser.getId() + ":" + IdUtil.simpleUUID();
        SysRole sysRole = sysRoleMapper.selectById(sysUser.getRoleId());
        String token = JwtTokenUtil.getToken(sysUser.getId(), sysUser.getUsername(), sysRole.getRoleCode(), sysRole.getId());

        //单点登录需要删除用户在其它地方登录的Token
        if (SysSettingParam.isEnableSso()) {
            RedisUtil.deleteObjects(CacheKeyPrefixConstants.TOKEN + sysUser.getId() + ":*");
        }
        RedisUtil.setCacheObject(tokenKey, token, Duration.ofSeconds(GlobalProperties.getTokenExpire()));
        sysLoginLogService.save(new SysLoginLog(sysUser.getUsername(), clientIp, UserTypeEnum.SYSTEM.getValue()));
        return JsonResult.successResult(new TokenVO(tokenKey, sysUser));
    }

    @Override
    public JsonResult saveUser(SysUser sysUser) {
        SysUser exists = this.getOne(new LambdaQueryWrapper<SysUser>().eq(SysUser::getUsername, sysUser.getUsername()));
        if (exists != null && !exists.getId().equals(sysUser.getId())) {
            return JsonResult.failureResult(ReturnMessageEnum.ACCOUNT_EXISTS);
        }
        if (sysUser.getId() == null) {
            //设置密码加密使用的盐
            sysUser.setSalt(IdUtil.simpleUUID());
            sysUser.setGoogleSecret(GoogleAuthenticatorUtil.genSecret(sysUser.getUsername(), loginConfig.getCaptcha().getGoogleHost()));
            sysUser.setPassword(AESUtil.encrypt(sysUser.getPassword(), sysUser.getSalt()));
            this.save(sysUser);
        } else {
            UpdateWrapper<SysUser> wrapper = Wrappers.update();
            wrapper.lambda()
                    .set(SysUser::getRoleId, sysUser.getRoleId())
                    .set(SysUser::getName, sysUser.getName())
                    .set(SysUser::getMobile, sysUser.getMobile())
                    .set(SysUser::getEmail, sysUser.getEmail())
                    .set(SysUser::getEnabled, sysUser.getEnabled())
                    //.set(SysUser::getChannelId, sysUser.getChannelId())
                    .eq(SysUser::getId, sysUser.getId());
            update(wrapper);
        }
        return JsonResult.successResult();
    }

    @Override
    public JsonResult<PageVO<UserVO>> queryByCondition(PageParam<SysUser, SysUserCondition> pageParam) {
        IPage<UserVO> page = this.baseMapper.selectPageVo(pageParam.getPage(), pageParam.getSearchParam());
        return JsonResult.successResult(new PageVO(page));
    }

    @Override
    public JsonResult userEnabled(UpdateStatusDTO updateStatusDTO) {
        UpdateWrapper<SysUser> wrapper = Wrappers.update();
        wrapper.lambda()
                .set(SysUser::getEnabled, updateStatusDTO.getEnabled())
                .eq(SysUser::getId, updateStatusDTO.getId());
        update(wrapper);
        return JsonResult.successResult();
    }

    @Override
    public JsonResult deleteByIdList(List<Integer> idList) {
        UpdateWrapper<SysUser> wrapper = Wrappers.update();
        wrapper.lambda()
                .set(SysUser::getDeleted, 1)
                .in(SysUser::getId, idList);
        update(wrapper);
        return JsonResult.successResult();
    }

    @Override
    public JsonResult updatePassword(UpdateUserInfoDTO updateUserInfoDTO) {
        Integer userId = JwtTokenUtil.getUserIdFromToken(ThreadLocalManager.getToken());
        Integer roleId = JwtTokenUtil.getRoleIdFromToken(ThreadLocalManager.getToken());
        if (!RoleTypeEnum.PROXY.getId().equals(roleId)) {
            SysUser sysUser = this.getOne(new LambdaQueryWrapper<SysUser>().select(SysUser::getSalt, SysUser::getPassword).eq(SysUser::getId, userId));
            String oldPwd = updateUserInfoDTO.getOldPassword();
            oldPwd = AESUtil.encrypt(oldPwd, sysUser.getSalt());
            if (sysUser.getPassword().equals(oldPwd)) {
                UpdateWrapper<SysUser> wrapper = Wrappers.update();
                wrapper.lambda()
                        .set(SysUser::getMobile, updateUserInfoDTO.getMobile())
                        .set(SysUser::getEmail, updateUserInfoDTO.getEmail())
                        .set(SysUser::getName, updateUserInfoDTO.getNickName())
                        .set(SysUser::getPassword, AESUtil.encrypt(updateUserInfoDTO.getPassword(), sysUser.getSalt()))
                        .eq(SysUser::getId, userId);
                this.update(wrapper);
                return JsonResult.successResult();
            } else {
                return JsonResult.failureResult(ReturnMessageEnum.ORIGINAL_PASSWORD_ERROR);
            }
        } else {
            AppUsers appUsers = appUserMapper.selectOne(new LambdaQueryWrapper<AppUsers>().select(AppUsers::getSalt, AppUsers::getPassword).eq(AppUsers::getId, userId));
            String oldPwd = updateUserInfoDTO.getOldPassword();
            oldPwd = AESUtil.encrypt(oldPwd, appUsers.getSalt());
            if (appUsers.getPassword().equals(oldPwd)) {
                UpdateWrapper<AppUsers> wrapper = Wrappers.update();
                wrapper.lambda()
                        .set(AppUsers::getMobile, updateUserInfoDTO.getMobile())
                        .set(AppUsers::getNickName, updateUserInfoDTO.getNickName())
                        .set(AppUsers::getPassword, AESUtil.encrypt(updateUserInfoDTO.getPassword(), appUsers.getSalt()))
                        .eq(AppUsers::getId, userId);
                appUserMapper.update(null, wrapper);
                return JsonResult.successResult();
            } else {
                return JsonResult.failureResult(ReturnMessageEnum.ORIGINAL_PASSWORD_ERROR);
            }
        }
    }


    @Override
    public JsonResult restPassword(Integer id) {
        SysUser sysUser = this.getOne(new LambdaQueryWrapper<SysUser>().select(SysUser::getSalt).eq(SysUser::getId, id));
        UpdateWrapper<SysUser> wrapper = Wrappers.update();
        wrapper.lambda()
                .set(SysUser::getPassword, AESUtil.encrypt(SysSettingParam.getInitPassword(), sysUser.getSalt()))
                .in(SysUser::getId, id);
        update(wrapper);
        return JsonResult.successResult();
    }


    /**
     * 校验图片验证码
     */
    private JsonResult checkImageCaptcha(LoginDTO loginDTO) {
        if (loginConfig.getCaptcha().isEnable() && !loginConfig.getCaptcha().isGoogle()) {
            //后台图片验证码
            String cacheKey = CacheKeyPrefixConstants.CAPTCHA_CODE + loginDTO.getUuid();
            String code = RedisUtil.getCacheObject(cacheKey);
            if (code == null) {
                return JsonResult.failureResult(ReturnMessageEnum.VERIFICATION_CODE_EXPIRE);
            } else if (!code.equals(loginDTO.getCode())) {
                return JsonResult.failureResult(ReturnMessageEnum.VERIFICATION_CODE_ERROR);
            }
        }
        return JsonResult.successResult();
    }

    /**
     * 校验google身份验证器
     */
    private boolean checkGoogleCaptcha(String code, String googleSecret) {
        if (loginConfig.getCaptcha().isEnable() && loginConfig.getCaptcha().isGoogle()) {
            //验证谷歌认证器 验证码
            if (StringUtil.isAnyBlank(code, googleSecret)) {
                return false;
            }
            return GoogleAuthenticatorUtil.authcode(code, googleSecret);
        }
        return true;
    }

    /**
     * 校验用户账号是否被锁定指定分钟
     */
    private boolean isLock(String username) {
        if (loginConfig.isEnableErrorLock()) {
            String cacheKey = CacheKeyPrefixConstants.PWD_ERROR_COUNT + username;
            Integer errorNumber = RedisUtil.getCacheObject(cacheKey);
            if (errorNumber != null && errorNumber.equals(loginConfig.getMaxErrorNumber())) {
                return true;
            }
        }
        return false;
    }


    /**
     * 密码锁定校验
     */
    private void setFailCount(String username, boolean loginFlag) {
        if (loginConfig.isEnableErrorLock()) {
            String cacheKey = CacheKeyPrefixConstants.PWD_ERROR_COUNT + username;
            if (loginFlag) {
                // 如果登录成功,清除之前的缓存
                RedisUtil.deleteObject(cacheKey);
            } else {
                // 获取用户登录错误次数,然后累加1
                Integer errorNumber = RedisUtil.getCacheObject(cacheKey);
                errorNumber = errorNumber == null ? 1 : errorNumber + 1;
                RedisUtil.setCacheObject(cacheKey, errorNumber, Duration.ofMillis(loginConfig.getLockTime()));
            }
        }
    }

    @Override
    public JsonResult export(SysUserCondition sysUserCondition) {
        PageParam<SysUser, SysUserCondition> pageParam = new PageParam<>();
        pageParam.setPageSize(ExcelUtil.LIMITT);
        pageParam.setSearchParam(sysUserCondition);

        JsonResult<PageVO<UserVO>> jsonResult = this.queryByCondition(pageParam);
        PageVO<UserVO> data = jsonResult.getData();
        List<ExportUserVO> exportUserVOList = new ArrayList<>(data.getContent().size());
        for (UserVO userVO : data.getContent()) {
            exportUserVOList.add(new ExportUserVO(userVO));
        }

        DataSavePathEnum.EXCEL_TMP.mkdirs();
        String filePath = String.format("%s%s_%s%s", DataSavePathEnum.EXCEL_TMP.getPath(), OperationModel.SYS_USER, DateUtil.getDetailTimeIgnoreUnit(), ExcelTypeEnum.XLSX.getValue());
        Object url = GlobalProperties.getVirtualPathURL() + StringUtil.replace(filePath, GlobalProperties.getRootPath(), "");
        ExcelUtil.exportData(ExportUserVO.class, OperationModel.SYS_USER, exportUserVOList, filePath);
        return JsonResult.successResult(url);
    }

    @Override
    public void generateGoogleQRCode(Integer userId, HttpServletResponse response) throws Exception {
        SysUser sysUser = this.getOne(new LambdaQueryWrapper<SysUser>().select(SysUser::getUsername, SysUser::getGoogleSecret).eq(SysUser::getId, userId));
        String url = GoogleAuthenticatorUtil.getQRBarcodeURL(sysUser.getUsername(), loginConfig.getCaptcha().getGoogleHost(), sysUser.getGoogleSecret());
        String filePath = generateQRCodeImg(DataSavePathEnum.TMP.getPath(), url);
        ResponseUtil.downloadFileByLocal(response, new File(filePath), ContentTypeEnum.PNG);
    }

    @Override
    public JsonResult proxylogin(LoginDTO loginDTO, String localIp) {
        AppUsers appUsers = appUserMapper.selectOne(new LambdaQueryWrapper<AppUsers>()
                .eq(AppUsers::getEnabled, BooleanEnum.TRUE.intValue())
                .eq(AppUsers::getEmail, loginDTO.getUsername())
        );

        if (appUsers == null) {
            return JsonResult.failureResult(ReturnMessageEnum.EMAIL_NOT_EXISTS);
        } else if (appUsers.getEnabled().equals(BooleanEnum.FALSE.intValue())) {
            return JsonResult.failureResult(ReturnMessageEnum.ACCOUNT_DISABLED);
        } else if (!appUsers.getPassword().equals(AESUtil.encrypt(loginDTO.getPassword(), appUsers.getSalt()))) {
            return JsonResult.failureResult(ReturnMessageEnum.PASSWORD_ERROR);
        }else if(appUsers.getProxyRole().equals(BooleanEnum.FALSE.intValue())){
            return JsonResult.failureResult("该账号未开通代理商角色！");
        }

        String token = JwtTokenUtil.getProxyToken(appUsers.getId(),appUsers.getEmail());

        //单点登录需要删除用户在其它地方登录的Token
        if (SysSettingParam.isEnableSso()) {
            RedisUtil.deleteObjects(CacheKeyPrefixConstants.APP_TOKEN + appUsers.getId() + ":*");
        }

        //把token存储到缓存中
        String tokenKey = CacheKeyPrefixConstants.APP_TOKEN + appUsers.getId() + ":" + IdUtil.simpleUUID();
        RedisUtil.setCacheObject(tokenKey, token, Duration.ofSeconds(GlobalProperties.getTokenExpire()));
        sysLoginLogService.save(new SysLoginLog(appUsers.getEmail(), localIp, UserTypeEnum.CLIENT.getValue()));

        return JsonResult.successResult(new TokenVO(tokenKey, appUsers));
    }
}
