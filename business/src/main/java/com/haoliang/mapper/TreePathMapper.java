package com.haoliang.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.haoliang.model.TreePath;
import com.haoliang.model.dto.TreePathAmountDTO;
import com.haoliang.model.dto.TreePathLevelDTO;
import com.haoliang.model.dto.TreeUserIdDTO;
import org.apache.ibatis.annotations.Param;

import java.math.BigDecimal;
import java.util.List;

/**
 * @author Administrator
 */
public interface TreePathMapper extends BaseMapper<TreePath> {

    int insertTreePath(int uid,@Param("pid") int pid);

    List<TreePathAmountDTO> getTaskEarningsByUserIdAndLevelList(@Param("uid")Integer userId, @Param("levelList") List<Integer> levelList,@Param("typeList") List<Integer> typeList);

    BigDecimal getMyItemAmountByUserId(@Param("uid")Integer userId);

    List<Integer> getAllAncestorIdByUserId(@Param("uid")Integer userId);

    List<TreeUserIdDTO> findTreeById(Integer userId);

    Integer teamSumValid(Integer userId);

    Integer teamStarSum(Integer userId);

    List<TreePathLevelDTO> getTreePathLevelOrderByLevel(@Param("uid")Integer userId);

    Integer getGenerationUserNum(Integer userId);

    Integer getItemUserNum(Integer userId);

    Integer teamSum(Integer userId);

    Integer teamMeshUser(Integer userId);
}
