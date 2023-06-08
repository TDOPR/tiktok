package com.haoliang.service;

import com.baomidou.mybatisplus.core.toolkit.support.SFunction;
import com.baomidou.mybatisplus.extension.service.IService;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.model.PageParam;
import com.haoliang.common.model.dto.PageDTO;
import com.haoliang.common.model.dto.UpdatePasswordDTO;
import com.haoliang.common.model.dto.UpdateStatusDTO;
import com.haoliang.common.model.vo.PageVO;
import com.haoliang.model.AppUsers;
import com.haoliang.model.TikTokAccount;
import com.haoliang.model.condition.AppUsersCondition;
import com.haoliang.model.dto.*;
import com.haoliang.model.vo.*;
import org.springframework.web.multipart.MultipartFile;
import java.util.List;

public interface AppUserService extends IService<AppUsers> {

    JsonResult login(AppUserLoginDTO appUserLoginDTO, String localIp);

    JsonResult register(AppUserRegisterDTO appUserRegisterDTO,String localIp);

    JsonResult findPassword(FindPasswordDTO findPasswordDTO);

    JsonResult home(PageDTO pageDTO);

    JsonResult<PageVO<AppUsersVO>> pageList(PageParam<AppUsers, AppUsersCondition> pageParam);

    JsonResult<List<TreeUserIdDTO>> treeDiagram(Integer userId);

    JsonResult modifyUserName(AppUserDTO appUserDTO);

    JsonResult uploadHeadImage(MultipartFile file) throws Exception;

    JsonResult updatePassword(UpdatePasswordDTO updatePasswordDTO);

    JsonResult<MyDetailVO> getMyDetail();

    AppUsers selectColumnsByUserId(Integer userId, SFunction<AppUsers, ?>... columns);

    JsonResult batchAddUser();

    JsonResult loginOut();

    JsonResult isRegisterCheck(String email, Integer type);

    JsonResult bindTiktok(TikTokAccount tikTokAccount);

    JsonResult bindMobile(MobileDTO mobileDTO);

    JsonResult sendSms(String mobile,Integer type);


    JsonResult editTiktok(TikTokAccount tikTokAccount);

    JsonResult<TiktokInfoVO> getTiktokInfo();

    JsonResult portrait();

    JsonResult batchAddSubUser(BatchAddSubUserDTO batchAddSubUserDTO);

    JsonResult addUser(AppUsers appUsers);

    JsonResult skip();

    JsonResult<ItemInfoVO> getItemInfo();

    JsonResult editProxyStatus(UpdateStatusDTO updateStatusDTO);
}
