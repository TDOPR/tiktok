package com.haoliang.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.haoliang.model.WalletsUsdWithdraw;
import com.haoliang.model.condition.WalletsUsdWithdrawCondition;
import com.haoliang.model.usd.EvmWithdraw;
import com.haoliang.model.vo.WalletsUsdWithdrawVO;
import org.apache.ibatis.annotations.Param;

public interface WalletsUsdWithdrawMapper extends BaseMapper<WalletsUsdWithdraw> {
    IPage<WalletsUsdWithdrawVO> page(Page<EvmWithdraw> page,@Param("param") WalletsUsdWithdrawCondition searchParam);
}
