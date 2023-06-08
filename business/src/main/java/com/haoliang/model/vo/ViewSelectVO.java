package com.haoliang.model.vo;

import com.haoliang.common.constant.LanguageKeyConstants;
import com.haoliang.common.model.ThreadLocalManager;
import com.haoliang.common.util.DateUtil;
import com.haoliang.common.util.MessageUtil;
import com.haoliang.enums.TttLogTypeEnum;
import com.haoliang.model.dto.DateSection;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/11/17 16:39
 **/
@Data
@NoArgsConstructor
@AllArgsConstructor
public class ViewSelectVO {

    //标签
    private String lable;

    private String value;

    public ViewSelectVO(TttLogTypeEnum tttLogTypeEnum) {
        this.lable = tttLogTypeEnum.toString();
        this.value = tttLogTypeEnum.getValue().toString();
    }

    /**
     * 获取钱包流水月份title
     */
    public static List<ViewSelectVO> getSelectListByDateSection(DateSection dateSection) {
        List<ViewSelectVO> viewSelectVOList = new ArrayList<>();
        viewSelectVOList.add(new ViewSelectVO(MessageUtil.get(LanguageKeyConstants.All, ThreadLocalManager.getLanguage()), "-1"));
        if (dateSection != null && dateSection.getMaxDate() != null) {
            viewSelectVOList.add(new ViewSelectVO(MessageUtil.getMonthStrByLanguage(dateSection.getMaxDate().getMonthValue()), dateSection.getMaxDate().getYear() + "-" + dateSection.getMaxDate().getMonthValue()));
            int monthNumber = DateUtil.betweenMonths(dateSection.getMinDate(), dateSection.getMaxDate());
            if (monthNumber > 11) {
                monthNumber = 11;
            }
            LocalDate localDate = dateSection.getMaxDate();
            for (int i = 1; i <= monthNumber; i++) {
                //生成最近12个月的 年-月数据
                localDate = localDate.minusMonths(1);
                viewSelectVOList.add(new ViewSelectVO(MessageUtil.getMonthStrByLanguage(localDate.getMonthValue()), localDate.getYear() + "-" + localDate.getMonthValue()));
            }
        }
        return viewSelectVOList;
    }
}
