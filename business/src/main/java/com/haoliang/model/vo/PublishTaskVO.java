package com.haoliang.model.vo;

import com.haoliang.common.model.vo.PageVO;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.util.List;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/3/1 14:57
 **/
@Data
@NoArgsConstructor
public class PublishTaskVO extends PageVO<TiktokTaskVO> {

    /**
     * 剩余可完成收益
     */
    private BigDecimal remainde;

    public PublishTaskVO(BigDecimal remainde, long totalElements, long totalPages, List<TiktokTaskVO> content) {
        super(totalElements, totalPages, content);
        this.remainde = remainde;
    }

}
