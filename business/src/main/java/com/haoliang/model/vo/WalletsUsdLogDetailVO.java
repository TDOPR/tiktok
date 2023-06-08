package com.haoliang.model.vo;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.haoliang.common.constant.LanguageKeyConstants;
import com.haoliang.common.model.ThreadLocalManager;
import com.haoliang.common.util.MessageUtil;
import com.haoliang.constant.TiktokConfig;
import com.haoliang.enums.TttLogTypeEnum;
import com.haoliang.enums.UsdLogTypeEnum;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/11/17 18:11
 **/
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class WalletsUsdLogDetailVO {

    /**
     * 日期下拉列表
     */
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private List<ViewSelectVO> dateSectionList;

    /**
     * 流水明细
     */
    private List<WalletUsdLogVO> tableList;

    /**
     * 存入金额
     */
    private String deposit;

    /**
     * 取出金额
     */
    private String takeOut;

    /**
     * 流水类型
     */
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private List<ViewSelectVO> typeList;

    /**
     * 总页数
     */
    private Integer totalPage;

    /**
     * usd钱包流水页面显示的类型下拉
     */
    public static List<ViewSelectVO> buildUsdTypeList() {
        List<ViewSelectVO> list = new ArrayList<>();
        String language = ThreadLocalManager.getLanguage();
        list.add(new ViewSelectVO(MessageUtil.get(LanguageKeyConstants.All, language), "-1"));
        for (UsdLogTypeEnum usdLogTypeEnum : UsdLogTypeEnum.values()) {
            list.add(new ViewSelectVO(usdLogTypeEnum.toString(), usdLogTypeEnum.getValue().toString()));
        }
        return list;
    }

}
