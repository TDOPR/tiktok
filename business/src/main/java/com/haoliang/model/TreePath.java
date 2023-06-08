package com.haoliang.model;

import com.baomidou.mybatisplus.annotation.TableName;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.time.LocalDateTime;

/**
 * @author Dominick Li
 * @Description 客户组织关系
 * @CreateTime 2023/2/27 14:39
 **/
@Data
@Builder
@TableName("tree_paths")
@NoArgsConstructor
@AllArgsConstructor
public class TreePath implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 父
     */
    private Integer ancestor;
    /**
     * 子
     */
    private Integer descendant;
    /**
     * 子是父的几级
     */
    private Integer level;
}
