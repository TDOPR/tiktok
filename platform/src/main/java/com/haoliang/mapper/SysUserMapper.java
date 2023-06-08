package com.haoliang.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.haoliang.model.SysUser;
import com.haoliang.model.condition.SysUserCondition;
import com.haoliang.model.vo.UserVO;
import org.apache.ibatis.annotations.Param;

import java.util.List;

public interface SysUserMapper extends BaseMapper<SysUser> {

    List<String> findAllByUsernameByRoleId(@Param("roleId") Integer roleId);

    IPage<UserVO> selectPageVo(IPage<SysUser> page, @Param("user") SysUserCondition sysUserCondition);

    List<Integer> findExistsByRoleId(@Param("idList") List<Integer> idList);
}
