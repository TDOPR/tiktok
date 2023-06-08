package com.haoliang.service.impl;

import cn.hutool.core.io.FileUtil;
import cn.hutool.extra.emoji.EmojiUtil;
import com.alibaba.fastjson.JSONObject;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.core.toolkit.support.SFunction;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.haoliang.common.config.AppParamProperties;
import com.haoliang.common.config.GlobalProperties;
import com.haoliang.common.config.LoginConfig;
import com.haoliang.common.config.SysSettingParam;
import com.haoliang.common.constant.CacheKeyPrefixConstants;
import com.haoliang.common.constant.SystemConstants;
import com.haoliang.common.enums.*;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.model.PageParam;
import com.haoliang.common.model.SysLoginLog;
import com.haoliang.common.model.ThreadLocalManager;
import com.haoliang.common.model.dto.PageDTO;
import com.haoliang.common.model.dto.UpdatePasswordDTO;
import com.haoliang.common.model.dto.UpdateStatusDTO;
import com.haoliang.common.model.vo.PageVO;
import com.haoliang.common.service.SysLoginLogService;
import com.haoliang.common.util.*;
import com.haoliang.common.util.encrypt.AESUtil;
import com.haoliang.common.util.redis.RedisUtil;
import com.haoliang.constant.TiktokConfig;
import com.haoliang.enums.*;
import com.haoliang.mapper.*;
import com.haoliang.model.*;
import com.haoliang.model.condition.AppUsersCondition;
import com.haoliang.model.dto.*;
import com.haoliang.model.vo.*;
import com.haoliang.service.*;
import com.haoliang.sms.util.SmsUtils;
import org.apache.commons.io.FileUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import javax.annotation.Resource;
import java.io.File;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.Duration;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/11/4 11:21
 **/
@Service
public class AppUserServiceImpl extends ServiceImpl<AppUserMapper, AppUsers> implements AppUserService {

    @Autowired
    private SysLoginLogService sysLoginLogService;

    @Autowired
    private WalletsService walletsService;

    @Autowired
    private WalletTttLogsService walletTttLogsService;

    @Autowired
    private TreePathService treePathService;

    @Autowired
    private KLineDataService kLineDataService;

    @Autowired
    private UpdatePwdLogService updatePwdLogService;

    @Resource
    private BannerMapper bannerMapper;

    @Resource
    private LoginConfig loginConfig;

    @Resource
    private NewsMapper newsMapper;

    @Resource
    private ArticleMapper articleMapper;

    @Resource
    private VipOrdersMapper vipOrdersMapper;

    @Resource
    private TikTokAccountMapper tikTokAccountMapper;

    @Resource
    private AppParamProperties appParamProperties;

    @Override
    public JsonResult isRegisterCheck(String email, Integer type) {
        AppUsers appUsers = this.getOne(new LambdaQueryWrapper<AppUsers>().select(AppUsers::getId).eq(AppUsers::getEmail, email));
        if (EmailTypeEnum.REGISTER.getValue().equals(type)) {
            //注册的时候判断邮箱号是否被注册
            if (appUsers != null) {
                return JsonResult.failureResult(ReturnMessageEnum.EMAIL_EXISTS);
            }
        } else if (appUsers == null) {
            //找回密码的时候和支付验证的时候判断邮箱号是否存在
            return JsonResult.failureResult(ReturnMessageEnum.EMAIL_NOT_EXISTS);
        }
        return JsonResult.successResult();
    }

    @Override
    public JsonResult<MyDetailVO> getMyDetail() {
        Integer userId = JwtTokenUtil.getUserIdFromToken(ThreadLocalManager.getToken());
        AppUsers appUsers = this.selectColumnsByUserId(userId, AppUsers::getLevel, AppUsers::getMobile, AppUsers::getValid, AppUsers::getVipLevel, AppUsers::getNickName,
                AppUsers::getInviteCode, AppUsers::getHeadImage, AppUsers::getCreateTime);
        MyDetailVO myDetailVO = new MyDetailVO();
        myDetailVO.setOfficialWebsite(appParamProperties.getRegisterUrl());
        BeanUtils.copyProperties(appUsers, myDetailVO);
        if (StringUtil.isNoneBlank(appUsers.getMobile())) {
            myDetailVO.setCountryCode(appUsers.getMobile().substring(0, 2));
            myDetailVO.setMobile(appUsers.getMobile().substring(2));
        }
        myDetailVO.setValid(appUsers.getValid().equals(BooleanEnum.TRUE.intValue()));
        myDetailVO.setUserId(userId);
        return JsonResult.successResult(myDetailVO);
    }

    @Override
    public JsonResult<TiktokInfoVO> getTiktokInfo() {
        Integer userId = JwtTokenUtil.getUserIdFromToken(ThreadLocalManager.getToken());
        TiktokInfoVO tiktokInfoVO = new TiktokInfoVO();
        List<TikTokAccount> tikTokAccountList = tikTokAccountMapper.selectList(new LambdaQueryWrapper<TikTokAccount>()
                .eq(TikTokAccount::getUserId, userId)
                .orderByDesc(TikTokAccount::getCreateTime)
        );
        Optional<TikTokAccount> find = tikTokAccountList.stream().filter(tikTokAccount -> tikTokAccount.getActive().equals(BooleanEnum.TRUE.intValue())).findFirst();
        if (find.isPresent()) {
            tiktokInfoVO.setTiktokUserName(find.get().getUsername());
        }
        tiktokInfoVO.setTikTokAccountList(tikTokAccountList);
        return JsonResult.successResult(tiktokInfoVO);
    }

    @Override
    public JsonResult home(PageDTO pageDTO) {
        List<Banner> bannerList = bannerMapper.selectList(
                new LambdaQueryWrapper<Banner>()
                        .eq(Banner::getEnabled, BooleanEnum.TRUE.intValue())
                        .orderByAsc(Banner::getSortIndex)
        );
        List<String> bannerPathList;
        String language = ThreadLocalManager.getLanguage();
        LanguageEnum languageEnum = LanguageEnum.nameOf(language);
        switch (languageEnum) {
            case ZH_CN:
                bannerPathList = bannerList.stream().map(Banner::getZhPath).collect(Collectors.toList());
                break;
            case EN_US:
                bannerPathList = bannerList.stream().map(Banner::getEnPath).collect(Collectors.toList());
                break;
            case VI_VN:
                bannerPathList = bannerList.stream().map(Banner::getViPath).collect(Collectors.toList());
                break;
            case TH_TH:
                bannerPathList = bannerList.stream().map(Banner::getThPath).collect(Collectors.toList());
                break;
            case IN_ID:
                bannerPathList = bannerList.stream().map(Banner::getInPath).collect(Collectors.toList());
                break;
            default:
                bannerPathList = new ArrayList<>();
                break;
        }
        List<String> bannerUrlList = new ArrayList<>();
        for (String str : bannerPathList) {
            bannerUrlList.add(GlobalProperties.getVirtualPathURL() + StringUtil.replace(str, GlobalProperties.getRootPath(), ""));
        }

        Page<Article> page = articleMapper.selectPage(pageDTO.getPage(), new LambdaQueryWrapper<Article>()
                .eq(Article::getLanguage, languageEnum.getType())
                .eq(Article::getEnabled, BooleanEnum.TRUE.intValue())
        );
        List<ArticleHomeVO> articleHomeVOList = new ArrayList<>();
        for (Article article : page.getRecords()) {
            articleHomeVOList.add(new ArticleHomeVO(article));
        }
        String prevUrl = GlobalProperties.getCallBackUrl() + appParamProperties.getVirtualPathPrefix() + "/";
        //查询是否有强制通知的公告
        HomeNoticeVO noticeVO = newsMapper.selectForceNotice(language.toLowerCase());
        HomeVO homeVO = new HomeVO();
        homeVO.setNotice(noticeVO);
        homeVO.setTotalPage((int) page.getPages());
        homeVO.setNewsList(articleHomeVOList);
        homeVO.setPlatformDesc(String.format("%shtml/index.html?imgName=%s&language=%s", prevUrl, "platformdesc", language));
        homeVO.setBusinessPlan(String.format("%spdf/%s/%s.pdf", prevUrl, language, "businessplan"));
        homeVO.setProductDesc(String.format("%shtml/index.html?imgName=%s&language=%s", prevUrl, "productdesc", language));
        homeVO.setJobModel(String.format("%shtml/index.html?imgName=%s&language=%s", prevUrl, "jobmodel", language));
        homeVO.setWhitePaper(String.format("%spdf/en_US/whitepaper.pdf", prevUrl));
        homeVO.setBannerList(bannerUrlList);
        return JsonResult.successResult(homeVO);
    }

    @Override
    public JsonResult login(AppUserLoginDTO appUserLoginDTO, String localIp) {
        AppUsers appUsers = this.getOne(
                new LambdaQueryWrapper<AppUsers>()
                        .eq(AppUsers::getEmail, appUserLoginDTO.getEmail())
        );

        if (appUsers == null) {
            return JsonResult.failureResult(ReturnMessageEnum.EMAIL_NOT_EXISTS);
        } else if (appUsers.getEnabled().equals(BooleanEnum.FALSE.intValue())) {
            return JsonResult.failureResult(ReturnMessageEnum.ACCOUNT_DISABLED);
        } else if (!appUsers.getPassword().equals(AESUtil.encrypt(appUserLoginDTO.getPassword(), appUsers.getSalt()))) {
            return JsonResult.failureResult(ReturnMessageEnum.PASSWORD_ERROR);
        }

        //修改登录次数
        UpdateWrapper<AppUsers> updateWrapper = Wrappers.update();
        updateWrapper.lambda()
                .set(AppUsers::getLoginCount, appUsers.getLoginCount() + 1)
                .eq(AppUsers::getId, appUsers.getId());
        this.update(updateWrapper);

        String token = JwtTokenUtil.getToken(appUsers.getId());

        //单点登录需要删除用户在其它地方登录的Token
        if (SysSettingParam.isEnableSso()) {
            RedisUtil.deleteObjects(CacheKeyPrefixConstants.APP_TOKEN + appUsers.getId() + ":*");
        }

        //把token存储到缓存中
        String tokenKey = CacheKeyPrefixConstants.APP_TOKEN + appUsers.getId() + ":" + IdUtil.simpleUUID();
        RedisUtil.setCacheObject(tokenKey, token, Duration.ofSeconds(GlobalProperties.getTokenExpire()));
        sysLoginLogService.save(new SysLoginLog(appUsers.getEmail(), localIp, UserTypeEnum.CLIENT.getValue()));

        //返回token给客户端
        JSONObject json = new JSONObject();
        json.put(SystemConstants.TOKEN_NAME, tokenKey);
        json.put("greenhorn", appUsers.getGreenhorn() == 1);
        return JsonResult.successResult(json);
    }

    @Override
    @Transactional
    public JsonResult register(AppUserRegisterDTO appUserRegisterDTO, String localIp) {
        if (GlobalProperties.isProdEnv()) {
            String cacheKey = CacheKeyPrefixConstants.CAPTCHA_CODE + appUserRegisterDTO.getUuid();
            String code = RedisUtil.getCacheObject(cacheKey);
            if (code == null) {
                return JsonResult.failureResult(ReturnMessageEnum.VERIFICATION_CODE_EXPIRE);
            }
            if (!code.equals(appUserRegisterDTO.getCode())) {
                return JsonResult.failureResult(ReturnMessageEnum.VERIFICATION_CODE_ERROR);
            }
        }
        AppUsers appUsers;
        appUsers = this.getOne(new LambdaQueryWrapper<AppUsers>().eq(AppUsers::getEmail, appUserRegisterDTO.getEmail()));
        if (appUsers == null) {
            Integer inviteUserId = null;
            if (StringUtil.isNotEmpty(appUserRegisterDTO.getInviteCode())) {
                //根据邀请码找到对应的用户
                AppUsers inviteUser = this.getOne(new LambdaQueryWrapper<AppUsers>().eq(AppUsers::getInviteCode, appUserRegisterDTO.getInviteCode()));
                if (inviteUser == null) {
                    return JsonResult.failureResult(ReturnMessageEnum.INVITE_CODE_ERROR);
                }
                inviteUserId = inviteUser.getId();
            }
            //注册
            appUsers = new AppUsers();
            appUsers.setEmail(appUserRegisterDTO.getEmail());
            appUsers.setNickName(appUsers.getEmail());
            //生成邀请码
            appUsers.setInviteCode(getInviteCode());
            //设置用户的邀请人ID
            appUsers.setInviteId(inviteUserId);
            //设置密码加密用的盐
            appUsers.setSalt(IdUtil.simpleUUID());
            appUsers.setPassword(AESUtil.encrypt(appUserRegisterDTO.getPassword(), appUsers.getSalt()));
            this.save(appUsers);

            //创建一条钱包记录
            BigDecimal exchangeRate = kLineDataService.getNowExchangeRate();
            Wallets wallets = new Wallets();
            //注册即送指定$美元对等的TTT币
            wallets.setWalletAmount(new BigDecimal(TiktokConfig.REGISTER_REWARDS).divide(exchangeRate, TiktokConfig.NUMBER_OF_DIGITS, RoundingMode.FLOOR));
            wallets.setUserId(appUsers.getId());
            walletsService.save(wallets);
            //添加一笔ttt账户流水
            walletTttLogsService.insertWalletLogs(appUsers.getId(), wallets.getWalletAmount(), FlowingActionEnum.INCOME, TttLogTypeEnum.ENROLL_IN_BENEFITS);
            //添加一条零撸用户收益上限记录
            vipOrdersMapper.insert(VipOrders.builder()
                    .userId(appUsers.getId())
                    .level(VipLevelEnum.ZERO.getLevel())
                    .total(VipLevelEnum.ZERO.getOutOfSaleAmount())
                    .allowance(VipLevelEnum.ZERO.getOutOfSaleAmount())
                    .build());
            //添加
            this.baseMapper.insertBatchNoticeByUserId(appUsers.getId());

            //添加一条默认的treepath记录
            TreePath treePath = TreePath.builder()
                    .ancestor(appUsers.getId())
                    .descendant(appUsers.getId())
                    .level(0)
                    .build();
            treePathService.save(treePath);
            if (inviteUserId != null) {
                //保存上下级关系
                treePathService.insertTreePath(appUsers.getId(), inviteUserId);
            }

            String token = JwtTokenUtil.getToken(appUsers.getId());
            //把token存储到缓存中
            String tokenKey = CacheKeyPrefixConstants.APP_TOKEN + appUsers.getId() + ":" + IdUtil.simpleUUID();
            RedisUtil.setCacheObject(tokenKey, token, Duration.ofSeconds(GlobalProperties.getTokenExpire()));
            sysLoginLogService.save(new SysLoginLog(appUsers.getEmail(), localIp, 2));
            //返回token给客户端
            JSONObject json = new JSONObject();
            json.put(SystemConstants.TOKEN_NAME, tokenKey);
            json.put("greenhorn", true);
            json.put("usdNumber", TiktokConfig.REGISTER_REWARDS);
            return JsonResult.successResult(json);
        } else {
            return JsonResult.failureResult(ReturnMessageEnum.EMAIL_EXISTS);
        }
    }

    @Override
    public JsonResult addUser(AppUsers appUsers) {
        if (!StringUtil.checkEmail(appUsers.getEmail())) {
            return JsonResult.failureResult("用户名必须是邮箱格式！");
        }
        AppUsers exists = this.getOne(new LambdaQueryWrapper<AppUsers>().eq(AppUsers::getEmail, appUsers.getEmail()));
        if (exists == null) {
            //设置密码加密用的盐
            appUsers.setSalt(IdUtil.simpleUUID());
            appUsers.setPassword(AESUtil.encrypt(appUsers.getPassword(), appUsers.getSalt()));
            appUsers.setInviteCode(getInviteCode());
            this.save(appUsers);
            //创建一条钱包记录
            BigDecimal exchangeRate = kLineDataService.getNowExchangeRate();
            Wallets wallets = new Wallets();
            //注册即送指定$美元对等的TTT币
            wallets.setWalletAmount(new BigDecimal(TiktokConfig.REGISTER_REWARDS).divide(exchangeRate, TiktokConfig.NUMBER_OF_DIGITS, RoundingMode.FLOOR));
            wallets.setUserId(appUsers.getId());
            walletsService.save(wallets);
            //添加一笔ttt账户流水
            walletTttLogsService.insertWalletLogs(appUsers.getId(), wallets.getWalletAmount(), FlowingActionEnum.INCOME, TttLogTypeEnum.ENROLL_IN_BENEFITS);
            //添加一条零撸用户收益上限记录
            vipOrdersMapper.insert(VipOrders.builder()
                    .userId(appUsers.getId())
                    .level(VipLevelEnum.ZERO.getLevel())
                    .total(VipLevelEnum.ZERO.getOutOfSaleAmount())
                    .allowance(VipLevelEnum.ZERO.getOutOfSaleAmount())
                    .build());
            //添加
            this.baseMapper.insertBatchNoticeByUserId(appUsers.getId());
            //添加一条默认的treepath记录
            TreePath treePath = TreePath.builder()
                    .ancestor(appUsers.getId())
                    .descendant(appUsers.getId())
                    .level(0)
                    .build();
            treePathService.save(treePath);
            return JsonResult.successResult();
        } else {
            return JsonResult.failureResult(ReturnMessageEnum.EMAIL_EXISTS);
        }
    }


    @Override
    @Transactional
    public JsonResult findPassword(FindPasswordDTO findPasswordDTO) {
        if (GlobalProperties.isProdEnv()) {
            String cacheKey = CacheKeyPrefixConstants.CAPTCHA_CODE + findPasswordDTO.getUuid();
            String code = RedisUtil.getCacheObject(cacheKey);
            if (code == null) {
                return JsonResult.failureResult(ReturnMessageEnum.VERIFICATION_CODE_EXPIRE);
            }
            if (!code.equals(findPasswordDTO.getCode())) {
                return JsonResult.failureResult(ReturnMessageEnum.VERIFICATION_CODE_ERROR);
            }
        }
        AppUsers appUsers;
        appUsers = this.getOne(new LambdaQueryWrapper<AppUsers>().eq(AppUsers::getEmail, findPasswordDTO.getEmail()));
        if (appUsers == null) {
            return JsonResult.failureResult(ReturnMessageEnum.EMAIL_NOT_EXISTS);
        }
        appUsers.setPassword(AESUtil.encrypt(findPasswordDTO.getPassword(), appUsers.getSalt()));
        this.saveOrUpdate(appUsers);
        UpdatePwdLog updatePwdLog = new UpdatePwdLog(appUsers.getId(), LocalDateTime.now());
        updatePwdLogService.saveOrUpdate(updatePwdLog);
        return JsonResult.successResult();
    }

    /**
     * 生成唯一邀请码
     */
    private String getInviteCode() {
        int start = 100000;
        int end = 999999;
        int code = RandomUtil.randomInt(start, end);
        String inviteCode = String.valueOf(code);
        AppUsers exists;
        while (true) {
            exists = this.getOne(new LambdaQueryWrapper<AppUsers>().eq(AppUsers::getInviteCode, inviteCode));
            if (exists != null) {
                inviteCode = IdUtil.generateShortUUID(8);
            } else {
                break;
            }
        }
        return inviteCode;
    }

//    private String getInviteCode() {
//        String inviteCode = IdUtil.generateShortUUID(8);
//        AppUsers exists;
//        while (true) {
//            exists = this.getOne(new LambdaQueryWrapper<AppUsers>().eq(AppUsers::getInviteCode, inviteCode));
//            if (exists != null) {
//                inviteCode = IdUtil.generateShortUUID(8);
//            } else {
//                break;
//            }
//        }
//        return inviteCode;
//    }

    @Override
    public JsonResult<PageVO<AppUsersVO>> pageList(PageParam<AppUsers, AppUsersCondition> pageParam) {
        Integer roleId = JwtTokenUtil.getRoleIdFromToken(ThreadLocalManager.getToken());

        if (pageParam.getSearchParam() == null) {
            pageParam.setSearchParam(new AppUsersCondition());
        }

        if (RoleTypeEnum.PROXY.getId().equals(roleId)) {
            pageParam.getSearchParam().setProxyUserId(JwtTokenUtil.getUserIdFromToken(ThreadLocalManager.getToken()));
            pageParam.getSearchParam().setProxy(true);
        }

        Integer vipLevel = pageParam.getSearchParam().getVipLevel();
        if (vipLevel != null) {
            if (vipLevel == -1) {
                pageParam.getSearchParam().setVipLevel(null);
                pageParam.getSearchParam().setValid(0);
            } else if (vipLevel == -2) {
                pageParam.getSearchParam().setVipLevel(null);
                pageParam.getSearchParam().setValid(1);
            }
        }

        IPage<AppUsersVO> iPage = this.baseMapper.page(pageParam.getPage(), pageParam.getSearchParam());
        //查询下级数量
        for (AppUsersVO appUsersVO : iPage.getRecords()) {
            appUsersVO.setWallet(AppUserWalletVO.builder()
                    .usd(NumberUtil.toPlainString(appUsersVO.getUsd()))
                    .ttt(NumberUtil.toPlainString(appUsersVO.getTtt()))
                    .recharge(NumberUtil.toPlainString(appUsersVO.getRecharge()))
                    .withdraw(NumberUtil.toPlainString(appUsersVO.getWithdraw()))
                    .consume(NumberUtil.toPlainString(appUsersVO.getConsume()))
                    .build());
            appUsersVO.setReward(walletTttLogsService.getUserReward(appUsersVO.getId()));
            appUsersVO.setSubordinateNumber(treePathService.countByAncestor(appUsersVO.getId()));
            appUsersVO.setCommunity(treePathService.getAdminItemInfoByUserId(appUsersVO.getId()));
        }
        return JsonResult.successResult(new PageVO<>(iPage.getTotal(), iPage.getPages(), iPage.getRecords()));
    }

    @Override
    public JsonResult<List<TreeUserIdDTO>> treeDiagram(Integer userId) {
        return treePathService.findTreeById(userId);
    }

    @Override
    public JsonResult modifyUserName(AppUserDTO appUserDTO) {
        if (EmojiUtil.containsEmoji(appUserDTO.getNickName())) {
            return JsonResult.failureResult(ReturnMessageEnum.NO_SUPPORT_EMPJI);
        }

        UpdateWrapper<AppUsers> updateWrapper = Wrappers.update();
        updateWrapper.lambda()
                .set(AppUsers::getNickName, appUserDTO.getNickName())
                .eq(AppUsers::getId, JwtTokenUtil.getUserIdFromToken(ThreadLocalManager.getToken()));
        boolean flag = this.update(updateWrapper);
        return JsonResult.build(flag);
    }

    @Override
    public JsonResult uploadHeadImage(MultipartFile file) throws Exception {
        Integer userId = JwtTokenUtil.getUserIdFromToken(ThreadLocalManager.getToken());

        AppUsers appUsers = this.selectColumnsByUserId(userId, AppUsers::getHeadImage);
        String headName = appUsers.getHeadImage().substring(appUsers.getHeadImage().lastIndexOf("/") + 1);

        String suffix = FileUtil.getSuffix(file.getOriginalFilename());
        String fileName = userId + "_" + IdUtil.getSnowflakeNextId() + "." + suffix;
        String savePath = DataSavePathEnum.USER_HEAD_IMAGE.getPath();
        FileUtil.del(new File(savePath, headName));

        String url = GlobalProperties.getVirtualPathURL() + StringUtil.replace(savePath, GlobalProperties.getRootPath(), "") + fileName;
        //复制文件流到本地文件
        FileUtils.copyInputStreamToFile(file.getInputStream(), new File(DataSavePathEnum.USER_HEAD_IMAGE.getFile(), fileName));
        UpdateWrapper<AppUsers> updateWrapper = Wrappers.update();
        updateWrapper.lambda()
                .set(AppUsers::getHeadImage, url)
                .eq(AppUsers::getId, userId);
        boolean flag = this.update(updateWrapper);

        if (flag) {
            JSONObject object = new JSONObject();
            object.put("url", url);
            return JsonResult.successResult(object);
        }
        return JsonResult.failureResult();
    }


    @Override
    @Transactional
    public JsonResult updatePassword(UpdatePasswordDTO updatePasswordDTO) {
        Integer userId = JwtTokenUtil.getUserIdFromToken(ThreadLocalManager.getToken());
        AppUsers sysUser = this.selectColumnsByUserId(userId, AppUsers::getSalt, AppUsers::getPassword);

        String oldPwd = AESUtil.encrypt(updatePasswordDTO.getOldPassword(), sysUser.getSalt());
        if (sysUser.getPassword().equals(oldPwd)) {
            UpdateWrapper<AppUsers> wrapper = Wrappers.update();
            wrapper.lambda()
                    .set(AppUsers::getPassword, AESUtil.encrypt(updatePasswordDTO.getPassword(), sysUser.getSalt()))
                    .eq(AppUsers::getId, userId);
            update(wrapper);
            UpdatePwdLog updatePwdLog = new UpdatePwdLog(userId, LocalDateTime.now());
            updatePwdLogService.saveOrUpdate(updatePwdLog);
            return JsonResult.successResult();
        }
        return JsonResult.failureResult(ReturnMessageEnum.ORIGINAL_PASSWORD_ERROR);
    }

    @Override
    public AppUsers selectColumnsByUserId(Integer userId, SFunction<AppUsers, ?>... columns) {
        return this.getOne(
                new LambdaQueryWrapper<AppUsers>()
                        .select(columns)
                        .eq(AppUsers::getId, userId)
        );
    }

    @Override
    public JsonResult bindTiktok(TikTokAccount tikTokAccount) {
        Integer userId = JwtTokenUtil.getUserIdFromToken(ThreadLocalManager.getToken());
        TikTokAccount tokAccount = tikTokAccountMapper.selectById(tikTokAccount.getId());
        if (tokAccount != null) {
            return JsonResult.failureResult(ReturnMessageEnum.TIKTOK_USERNAME_EXISTS);
        }
        tikTokAccount.setUserId(userId);
        Long count = tikTokAccountMapper.selectCount(new LambdaQueryWrapper<TikTokAccount>().eq(TikTokAccount::getUserId, userId));
        tikTokAccount.setActive(count == 0 ? BooleanEnum.TRUE.intValue() : BooleanEnum.FALSE.intValue());
        tikTokAccountMapper.insert(tikTokAccount);
        return JsonResult.successResult();
    }

    @Override
    @Transactional
    public JsonResult editTiktok(TikTokAccount tikTokAccount) {
        Integer userId = JwtTokenUtil.getUserIdFromToken(ThreadLocalManager.getToken());
        UpdateWrapper<TikTokAccount> updateWrapper = Wrappers.update();
        updateWrapper.lambda()
                .set(TikTokAccount::getActive, BooleanEnum.FALSE.intValue())
                .eq(TikTokAccount::getUserId, userId);
        tikTokAccountMapper.update(null, updateWrapper);
        updateWrapper = Wrappers.update();
        updateWrapper.lambda()
                .set(TikTokAccount::getActive, BooleanEnum.TRUE.intValue())
                .eq(TikTokAccount::getId, tikTokAccount.getId());
        tikTokAccountMapper.update(null, updateWrapper);
        return JsonResult.successResult();
    }

    @Override
    public JsonResult sendSms(String mobile, Integer type) {
        String uuid = IdUtil.simpleUUID();
        String verifyKey = CacheKeyPrefixConstants.CAPTCHA_CODE + uuid;
        int start = NumberUtil.getTenNResult(loginConfig.getCaptcha().getNumberLength());
        int end = start * 10 - 1;
        int intCode = RandomUtil.randomInt(start, end);
        String code = String.valueOf(intCode);
        //boolean flag = true;
        String countryTelephoneCode = mobile.substring(0, 2);
        mobile = mobile.substring(2);
        boolean flag = SmsUtils.send(countryTelephoneCode, mobile, code, loginConfig.getCaptcha().getExpirationTime().toString());
        if (flag) {
            //短信验证码有效期3分钟
            RedisUtil.setCacheObject(verifyKey, code, Duration.ofMinutes(3));
            JSONObject object = new JSONObject();
            object.put("uuid", uuid);
            return JsonResult.successResult(object);
        }
        return JsonResult.failureResult(ReturnMessageEnum.SEND_SMS_FAIL);
    }

    @Override
    public JsonResult portrait() {
        List<AppUsers> appUsersList = this.list(new LambdaQueryWrapper<AppUsers>()
                .select(AppUsers::getLevel, AppUsers::getCreateTime)
        );
        //三周内为老用户
        LocalDate isNew = LocalDate.now().minusDays(21);
        //新用户
        Integer newUserNumber = 0;
        //总用户
        Integer totalUserNumber = appUsersList.size();
        for (AppUsers appUsers : appUsersList) {
            if (appUsers.getCreateTime().toLocalDate().isAfter(isNew)) {
                newUserNumber++;
            }
        }
        Map<Integer, List<AppUsers>> map = appUsersList.stream().collect(Collectors.groupingBy(AppUsers::getLevel));
        List<ChartDataVO> levelList = new ArrayList<>();
        levelList.add(new ChartDataVO("非星级用户", map.containsKey(0) ? map.get(0).size() : 0));
        Integer size;
        for (ProxyLevelEnum proxyLevelEnum : ProxyLevelEnum.values()) {
            size = map.containsKey(proxyLevelEnum.getLevel()) ? map.get(proxyLevelEnum.getLevel()).size() : 0;
            levelList.add(new ChartDataVO(proxyLevelEnum.getLabelName(), size));
        }
        List<Wallets> walletsList = walletsService.list(new LambdaQueryWrapper<Wallets>()
                .select(Wallets::getWalletAmount, Wallets::getUsdWalletAmount)
        );
        Map<AmountRegionEunm, Integer> tttRegionMap = new HashMap<>();
        Map<AmountRegionEunm, Integer> usdRegionMap = new HashMap<>();
        AmountRegionEunm amountRegionEunm;
        for (Wallets wallets : walletsList) {
            amountRegionEunm = AmountRegionEunm.getByAmountGe(wallets.getUsdWalletAmount());
            usdRegionMap.put(amountRegionEunm, usdRegionMap.containsKey(amountRegionEunm) ? usdRegionMap.get(amountRegionEunm) + 1 : 1);
            amountRegionEunm = AmountRegionEunm.getByAmountGe(wallets.getWalletAmount());
            tttRegionMap.put(amountRegionEunm, tttRegionMap.containsKey(amountRegionEunm) ? tttRegionMap.get(amountRegionEunm) + 1 : 1);
        }
        List<ChartDataVO> tttList = new ArrayList<>();
        List<ChartDataVO> usdList = new ArrayList<>();
        for (AmountRegionEunm amountRegion : AmountRegionEunm.values()) {
            tttList.add(new ChartDataVO(amountRegion.getName(), tttRegionMap.containsKey(amountRegion) ? tttRegionMap.get(amountRegion) : 0));
            usdList.add(new ChartDataVO(amountRegion.getName(), usdRegionMap.containsKey(amountRegion) ? usdRegionMap.get(amountRegion) : 0));
        }
        return JsonResult.successResult(AppUserPortraitVO.builder()
                .newUserNumber(newUserNumber)
                .totalUserNumber(totalUserNumber)
                .levelUserList(levelList)
                .tttList(tttList)
                .usdList(usdList)
                .build());
    }


    @Override
    public JsonResult bindMobile(MobileDTO mobileDTO) {
        //验证手机号已被其它用户绑定
        Integer userId = JwtTokenUtil.getUserIdFromToken(ThreadLocalManager.getToken());
        AppUsers appUsers = this.getOne(new LambdaQueryWrapper<AppUsers>().eq(AppUsers::getMobile, mobileDTO.getMobile()));
        if (appUsers != null && !appUsers.getId().equals(userId)) {
            return JsonResult.failureResult(ReturnMessageEnum.MOBILE_EXISTS);
        }

        if (GlobalProperties.isProdEnv()) {
            String cacheKey = CacheKeyPrefixConstants.CAPTCHA_CODE + mobileDTO.getUuid();
            String code = RedisUtil.getCacheObject(cacheKey);
            if (code == null) {
                return JsonResult.failureResult(ReturnMessageEnum.VERIFICATION_CODE_EXPIRE);
            }
            if (!code.equals(mobileDTO.getCode())) {
                return JsonResult.failureResult(ReturnMessageEnum.VERIFICATION_CODE_ERROR);
            }
        }

        UpdateWrapper<AppUsers> updateWrapper = Wrappers.update();
        updateWrapper.lambda().
                set(AppUsers::getMobile, mobileDTO.getMobile())
                .eq(AppUsers::getId, JwtTokenUtil.getUserIdFromToken(ThreadLocalManager.getToken()));
        this.update(updateWrapper);
        return JsonResult.successResult();
    }

    @Override
    public JsonResult skip() {
        Integer userId = JwtTokenUtil.getUserIdFromToken(ThreadLocalManager.getToken());
        UpdateWrapper<AppUsers> updateWrapper = Wrappers.update();
        updateWrapper.lambda()
                .set(AppUsers::getGreenhorn, BooleanEnum.FALSE.intValue())
                .eq(AppUsers::getId, userId);
        this.update(updateWrapper);
        return JsonResult.successResult();
    }

    @Override
    @Transactional
    public JsonResult batchAddUser() {

        UserTestDTO test100 = new UserTestDTO(100);
        //test100的直推一代
        UserTestDTO test101 = new UserTestDTO(101);
        UserTestDTO test102 = new UserTestDTO(102);
        UserTestDTO test103 = new UserTestDTO(103);
        UserTestDTO test104 = new UserTestDTO(104);
        UserTestDTO test105 = new UserTestDTO(105);
        UserTestDTO test106 = new UserTestDTO(106);
        UserTestDTO test107 = new UserTestDTO(107);
        UserTestDTO test108 = new UserTestDTO(108);

        //一代
        test100.setChildList(Arrays.asList(test101, test102, test103, test104, test105, test106, test107, test108));
        //设置二代
        UserTestDTO test109 = new UserTestDTO(109);
        test101.setChildList(Arrays.asList(test109));
        UserTestDTO test110 = new UserTestDTO(110);
        test102.setChildList(Arrays.asList(test110));
        UserTestDTO test111 = new UserTestDTO(111);
        test103.setChildList(Arrays.asList(test111));
        //三代
        UserTestDTO test112 = new UserTestDTO(112);
        test109.setChildList(Arrays.asList(test112));
        UserTestDTO test113 = new UserTestDTO(113);
        test110.setChildList(Arrays.asList(test113));
        UserTestDTO test114 = new UserTestDTO(114);
        test111.setChildList(Arrays.asList(test114));
        //四代
        UserTestDTO test115 = new UserTestDTO(115);
        test112.setChildList(Arrays.asList(test115));
        UserTestDTO test116 = new UserTestDTO(116);
        test113.setChildList(Arrays.asList(test116));
        UserTestDTO test117 = new UserTestDTO(117);
        test114.setChildList(Arrays.asList(test117));
        //五代
        UserTestDTO test118 = new UserTestDTO(118);
        test115.setChildList(Arrays.asList(test118));

        UserTestDTO test119 = new UserTestDTO(119);
        UserTestDTO test120 = new UserTestDTO(120);
        UserTestDTO test121 = new UserTestDTO(121);
        test116.setChildList(Arrays.asList(test119, test120, test121));


        List<UserTestDTO> userTestDTOS = new ArrayList<>();
        //设置三代
        for (int i = 122; i <= 180; i++) {
            userTestDTOS.add(new UserTestDTO(i));
        }
        test117.setChildList(userTestDTOS);
        dg(test100, null);
        return JsonResult.successResult();
    }

    @Override
    @Transactional
    public JsonResult batchAddSubUser(BatchAddSubUserDTO batchAddSubUserDTO) {
        String password = "9666635A7F04602BDF204FDC989FF63C";
        String salt = "a6de3873cc50416aae0a32722dae6aec";
        if (batchAddSubUserDTO.getNumber() > 12000) {
            return JsonResult.failureResult("设置的用户数超出12000");
        }
        Integer userId = null;
        VipLevelEnum vipLevelEnum=VipLevelEnum.ONE;
        for (int i = 0; i < batchAddSubUserDTO.getNumber(); i++) {
            AppUsers appUsers = new AppUsers();
            //生成邀请码
            appUsers.setInviteCode(getInviteCode());
            appUsers.setEmail(appUsers.getInviteCode() + "@test.com");
            //设置用户的邀请人ID
            appUsers.setInviteId(batchAddSubUserDTO.getUserId());
            //设置密码加密用的盐
            appUsers.setSalt(salt);
            appUsers.setValid(BooleanEnum.TRUE.intValue());
            appUsers.setVipLevel(VipLevelEnum.ONE.getLevel());
            appUsers.setPassword(password);
            this.save(appUsers);
            if (i == 0) {
                userId = appUsers.getId();
            }
            //创建一条钱包记录
            Wallets wallets = new Wallets();
            wallets.setUsdWalletAmount(new BigDecimal("10000"));
            wallets.setUserId(appUsers.getId());
            walletsService.save(wallets);
            //添加一条默认的treepath记录
            TreePath treePath = TreePath.builder()
                    .ancestor(appUsers.getId())
                    .descendant(appUsers.getId())
                    .level(0)
                    .build();
            treePathService.save(treePath);
            //添加套餐购买记录表中
            vipOrdersMapper.insert(VipOrders.builder()
                    .userId(appUsers.getId())
                    .total(vipLevelEnum.getOutOfSaleAmount())
                    .allowance(vipLevelEnum.getOutOfSaleAmount())
                    .level(vipLevelEnum.getLevel())
                    .build());
            //保存上下级关系
            treePathService.insertTreePath(appUsers.getId(), batchAddSubUserDTO.getUserId());
        }
        for (int i = 0; i < batchAddSubUserDTO.getSecondNumber(); i++) {
            AppUsers appUsers = new AppUsers();
            //生成邀请码
            appUsers.setInviteCode(getInviteCode());
            appUsers.setEmail(appUsers.getInviteCode() + "@test.com");
            //设置用户的邀请人ID
            appUsers.setInviteId(userId);
            //设置密码加密用的盐
            appUsers.setSalt(salt);
            appUsers.setValid(BooleanEnum.TRUE.intValue());
            appUsers.setVipLevel(VipLevelEnum.ONE.getLevel());
            appUsers.setPassword(password);
            this.save(appUsers);

            //创建一条钱包记录
            Wallets wallets = new Wallets();
            wallets.setUsdWalletAmount(new BigDecimal("10000"));
            wallets.setUserId(appUsers.getId());
            walletsService.save(wallets);
            //添加一条默认的treepath记录
            TreePath treePath = TreePath.builder()
                    .ancestor(appUsers.getId())
                    .descendant(appUsers.getId())
                    .level(0)
                    .build();
            treePathService.save(treePath);
            vipOrdersMapper.insert(VipOrders.builder()
                    .userId(appUsers.getId())
                    .total(vipLevelEnum.getOutOfSaleAmount())
                    .allowance(vipLevelEnum.getOutOfSaleAmount())
                    .level(vipLevelEnum.getLevel())
                    .build());
            //保存上下级关系
            treePathService.insertTreePath(appUsers.getId(), userId);
        }
        return JsonResult.successResult();
    }

    /**
     * 递归添加数据
     */
    public void dg(UserTestDTO userTestDTO, Integer inviteUserId) {
        String password = "9666635A7F04602BDF204FDC989FF63C";
        String salt = "a6de3873cc50416aae0a32722dae6aec";

        AppUsers appUsers = new AppUsers();
        appUsers.setId(userTestDTO.getUserId() + 10000);
        appUsers.setEmail(appUsers.getId() + "@test.com");
        //生成邀请码
        appUsers.setInviteCode(getInviteCode());
        //设置用户的邀请人ID
        appUsers.setInviteId(inviteUserId);
        //设置密码加密用的盐
        appUsers.setSalt(salt);
        appUsers.setValid(BooleanEnum.TRUE.intValue());
        appUsers.setVipLevel(VipLevelEnum.ONE.getLevel());
        appUsers.setPassword(password);
        this.save(appUsers);

        //创建一条钱包记录
        Wallets wallets = new Wallets();
        wallets.setUsdWalletAmount(new BigDecimal("10000"));
        wallets.setUserId(appUsers.getId());
        walletsService.save(wallets);
        //添加一条默认的treepath记录
        TreePath treePath = TreePath.builder()
                .ancestor(appUsers.getId())
                .descendant(appUsers.getId())
                .level(0)
                .build();
        treePathService.save(treePath);
        vipOrdersMapper.insert(VipOrders.builder()
                .userId(appUsers.getId())
                .level(VipLevelEnum.ONE.getLevel())
                .total(VipLevelEnum.ONE.getOutOfSaleAmount())
                .allowance(VipLevelEnum.ONE.getOutOfSaleAmount())
                .build());
        if (inviteUserId != null) {
            //保存上下级关系
            treePathService.insertTreePath(appUsers.getId(), inviteUserId);
        }

        if (userTestDTO.getChildList() != null) {
            for (UserTestDTO ut : userTestDTO.getChildList()) {
                dg(ut, appUsers.getId());
            }
        }
    }

    @Override
    public JsonResult loginOut() {
        RedisUtil.deleteObject(ThreadLocalManager.getToken());
        return JsonResult.successResult();
    }

    @Override
    public JsonResult<ItemInfoVO> getItemInfo() {
        Integer userId = JwtTokenUtil.getUserIdFromToken(ThreadLocalManager.getToken());
        String parent = this.baseMapper.getParentEmail(userId);
        if (StringUtil.isNotBlank(parent)) {
            String arr[] = parent.split("@");
            if (arr.length > 0) {
                String pre = arr[0];
                int len = pre.length();
                if (len > 4) {
                    int charLen = len - 4;
                    StringBuilder sb = new StringBuilder();
                    for (int i = 0; i < charLen; i++) {
                        sb.append("*");
                    }
                    String data = pre.substring(len - 4);
                    sb.append(data).append(arr[1]);
                    parent = sb.toString();
                }
            }
        }
        List<String> childList = this.baseMapper.getChildList(userId);
        ItemInfoVO itemInfoVO = new ItemInfoVO();
        itemInfoVO.setParent(parent);
        itemInfoVO.setChildList(childList);
        return JsonResult.successResult(itemInfoVO);
    }

    @Override
    public JsonResult editProxyStatus(UpdateStatusDTO updateStatusDTO) {
        UpdateWrapper<AppUsers> updateWrapper = Wrappers.update();
        updateWrapper.lambda()
                .set(AppUsers::getProxyRole, updateStatusDTO.getEnabled())
                .eq(AppUsers::getId, updateStatusDTO.getId());
        this.update(updateWrapper);
        return JsonResult.successResult();
    }
}
