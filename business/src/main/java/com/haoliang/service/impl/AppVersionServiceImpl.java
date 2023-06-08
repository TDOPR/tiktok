package com.haoliang.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.haoliang.common.enums.BooleanEnum;
import com.haoliang.common.enums.LanguageEnum;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.model.ThreadLocalManager;
import com.haoliang.common.model.dto.IntIdListDTO;
import com.haoliang.enums.TiktokSettingEnum;
import com.haoliang.enums.SystemEnum;
import com.haoliang.mapper.AppVersionsMapper;
import com.haoliang.model.AppVersions;
import com.haoliang.model.dto.CheckVersionDTO;
import com.haoliang.model.vo.CheckVersionVO;
import com.haoliang.service.AppVersionService;
import org.springframework.stereotype.Service;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/1/7 11:43
 **/
@Service
public class AppVersionServiceImpl extends ServiceImpl<AppVersionsMapper, AppVersions> implements AppVersionService {

    @Override
    public JsonResult active(Integer id) {
        AppVersions appVersions = this.getById(id);

        //取消之前的版本
        UpdateWrapper<AppVersions> appVersionsUpdateWrapper = Wrappers.update();
        appVersionsUpdateWrapper.lambda()
                .set(AppVersions::getActive, BooleanEnum.FALSE.intValue())
                .eq(AppVersions::getActive, BooleanEnum.TRUE.intValue())
                .eq(AppVersions::getSystemName, appVersions.getSystemName());
        this.update(appVersionsUpdateWrapper);

        //使用当前版本作为最新版本
        appVersionsUpdateWrapper = Wrappers.update();
        appVersionsUpdateWrapper.lambda()
                .set(AppVersions::getActive, BooleanEnum.TRUE.intValue())
                .eq(AppVersions::getId, id);
        this.update(appVersionsUpdateWrapper);
        return JsonResult.successResult();
    }

    @Override
    public JsonResult deleteByIdList(IntIdListDTO intIdListDTO) {
        this.removeByIds(intIdListDTO.getIdList());
        return JsonResult.successResult();
    }

    @Override
    public JsonResult<CheckVersionVO> checkVersion(CheckVersionDTO checkVersionDTO) {
        AppVersions appVersions = this.getOne(
                new LambdaQueryWrapper<AppVersions>()
                        .eq(AppVersions::getSystemName, checkVersionDTO.getSystemName())
                        .eq(AppVersions::getActive, BooleanEnum.TRUE.intValue())
        );

        if (appVersions == null || appVersions.getVersion().equals(checkVersionDTO.getVersion())) {
            return JsonResult.successResult(
                    CheckVersionVO.builder()
                            .flag(false)
                            .build()
            );
        }

        //ios返回的下载链接使用官网地址
        String downloadAddress = SystemEnum.ANDROID.getName().equals(checkVersionDTO.getSystemName()) ? TiktokSettingEnum.ANDROID.stringValue() : TiktokSettingEnum.RECOMMENDED_ADDRESS.stringValue();

        //更新说明需要根据国际化语言判断
        String language = ThreadLocalManager.getLanguage();
        LanguageEnum languageEnum = LanguageEnum.nameOf(language);
        String updateDesc;
        switch (languageEnum) {
            case ZH_CN:
                updateDesc = appVersions.getZnUpdateDesc();
                break;
            case EN_US:
                updateDesc = appVersions.getEnUpdateDesc();
                break;
            case IN_ID:
                updateDesc = appVersions.getInUpdateDesc();
                break;
            case TH_TH:
                updateDesc = appVersions.getThUpdateDesc();
                break;
            case VI_VN:
                updateDesc = appVersions.getViUpdateDesc();
                break;
            default:
                updateDesc = "";
                break;
        }

        return JsonResult.successResult(
                CheckVersionVO.builder()
                        .flag(true)
                        .version(appVersions.getVersion())
                        .updateDesc(updateDesc)
                        .downloadAddress(downloadAddress)
                        .force(BooleanEnum.TRUE.intValue().equals(appVersions.getForceUpdate()))
                        .build()
        );
    }
}
