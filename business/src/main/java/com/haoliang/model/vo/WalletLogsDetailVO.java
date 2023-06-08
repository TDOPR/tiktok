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
public class WalletLogsDetailVO {

    /**
     * 日期下拉列表
     */
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private List<ViewSelectVO> dateSectionList;

    /**
     * 流水明细
     */
    private List<WalletLogVO> tableList;

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
     * ttt钱包流水页面显示的类型下拉
     */
    public static List<ViewSelectVO> buildTttTypeList() {
        List<ViewSelectVO> list = new ArrayList<>();
        String language = ThreadLocalManager.getLanguage();
        list.add(new ViewSelectVO(MessageUtil.get(LanguageKeyConstants.All, language), "-1"));
        list.add(new ViewSelectVO(MessageUtil.get(TiktokConfig.AGENCY_KEY, language), "0"));
        list.add(new ViewSelectVO(MessageUtil.get(TiktokConfig.TASK_KEY, language), "1"));
        list.add(new ViewSelectVO(MessageUtil.get(TttLogTypeEnum.TO_USD.getKey(), language), "2"));
        return list;
    }
}
