package com.haoliang.service;


import com.baomidou.mybatisplus.extension.service.IService;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.model.PageParam;
import com.haoliang.common.model.dto.UpdatePasswordDTO;
import com.haoliang.common.model.vo.PageVO;
import com.haoliang.model.SysUser;
import com.haoliang.model.condition.SysUserCondition;
import com.haoliang.model.dto.LoginDTO;
import com.haoliang.common.model.dto.UpdateStatusDTO;
import com.haoliang.model.dto.UpdateUserInfoDTO;
import com.haoliang.model.vo.UserVO;

import javax.servlet.http.HttpServletResponse;
import java.util.List;


public interface SysUserService extends IService<SysUser> {

    /**
     * 登录认证
     */
    JsonResult login(LoginDTO loginDTO, String clientIp);


    /**
     * 启用或者禁用用户
     */
    JsonResult userEnabled(UpdateStatusDTO updateStatusDTO);

    /**
     * 修改密码
     */
    JsonResult updatePassword(UpdateUserInfoDTO updateUserInfoDTO);


    /**
     * 充值密码
     */
    JsonResult restPassword(Integer id);

    /**
     * 导出
     */
    JsonResult export(SysUserCondition sysUserCondition);

    /**
     * 添加用户
     */
    JsonResult saveUser(SysUser sysUser);

    JsonResult deleteByIdList(List<Integer> idList);


    JsonResult<PageVO<UserVO>> queryByCondition(PageParam<SysUser, SysUserCondition> pageParam);

    /**
     * 生成谷歌认证器图片流
     * @param userId
     * @param response
     * @throws Exception
     */
    void generateGoogleQRCode(Integer userId, HttpServletResponse response) throws Exception;

    /**
     * 判断用户是否首次登录 是则返回谷歌认证器二维码
     */
    JsonResult getGoogleQRCode(LoginDTO loginDTO);

    /**
     * 代理商登录
     */
    JsonResult proxylogin(LoginDTO loginDTO, String localIp);
}
