package com.haoliang.model.vo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;

import java.util.List;

/**
 * @Description
 * @Author Dominick Li
 * @CreateTime 2023/3/26 16:41
 **/
@Data
@Builder
@AllArgsConstructor
public class AppUserPortraitVO {

    /**
     * 新用户数量
     */
    private Integer newUserNumber;

    /**
     * 总用户数量
     */
    private Integer totalUserNumber;


    /**
     * 星级用户
     */
    private List<ChartDataVO> levelUserList;

    /**
     * ttt持有分布
     */
    private List<ChartDataVO> tttList;

    /**
     * usd持有分布
     */
    private List<ChartDataVO> usdList;
}
