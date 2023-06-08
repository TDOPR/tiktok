package com.haoliang.service;

import com.baomidou.mybatisplus.extension.service.IService;
import com.haoliang.common.model.JsonResult;
import com.haoliang.model.TreePath;
import com.haoliang.model.dto.TreePathAmountDTO;
import com.haoliang.model.dto.TreePathLevelDTO;
import com.haoliang.model.dto.TreeUserIdDTO;
import com.haoliang.model.vo.MyCommunityAdminVO;
import com.haoliang.model.vo.MyCommunityVO;

import java.util.List;

public interface TreePathService extends IService<TreePath> {

    /**
     * 获取社区最近几代的任务收益
     * @param userId 供应商Id
     * @param levelList 包含的代数
     * @return
     */
    List<TreePathAmountDTO> getTaskEarningsByUserIdAndLevelList(Integer userId,List<Integer> levelList);

    /**
     * 获取代理商的团队数据
     * @param userId
     * @return
     */
    @Deprecated
    List<TreePath> getTreePathByUserId(Integer userId);

    /**
     * 获取三代的用户关系数据
     * @param userId
     * @return
     */
    @Deprecated
    List<TreePath> getThreeAlgebraTreePathByUserId(Integer userId);

    /**
     * 查询下级数量
     * @param userId 供应商Id
     */
    Integer countByAncestor(Integer userId);

    /**
     * 插入供应商 团队数据
     * @param userId 用户Id
     * @param inviteUserId 邀请人的用户Id
     */
    void insertTreePath(Integer userId, Integer inviteUserId);

    JsonResult<List<TreeUserIdDTO>> findTreeById(Integer userId);

    List<TreePathLevelDTO> getTreePathLevelOrderByLevel(Integer userId);

    MyCommunityVO getItemInfoByUserId(Integer userId);

    MyCommunityAdminVO getAdminItemInfoByUserId(Integer userId);
}
