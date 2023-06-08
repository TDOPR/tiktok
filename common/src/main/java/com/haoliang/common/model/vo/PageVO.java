package com.haoliang.common.model.vo;

import com.baomidou.mybatisplus.core.metadata.IPage;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@NoArgsConstructor
public class PageVO<T> implements java.io.Serializable {

    private static final long serialVersionUID = 2763256922817929825L;

    /**
     * 总记录数
     */
    private long totalElements;

    /**
     * 总页数
     */
    private long totalPages;

    /**
     * 列表数据
     */
    private List<T> content;

    public PageVO(long totalElements, long totalPages, List<T> content) {
        this.totalElements = totalElements;
        this.totalPages = totalPages;
        this.content = content;
    }

    public PageVO(IPage iPage) {
        this.totalElements = iPage.getTotal();
        this.totalPages = iPage.getPages();
        this.content = iPage.getRecords();
    }
}

