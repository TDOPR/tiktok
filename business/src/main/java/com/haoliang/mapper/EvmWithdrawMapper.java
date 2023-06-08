
package com.haoliang.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.haoliang.model.usd.EvmWithdraw;
import com.haoliang.model.condition.WalletsUsdWithdrawCondition;
import com.haoliang.model.vo.WalletsUsdWithdrawVO;
import org.apache.ibatis.annotations.Param;

import java.math.BigDecimal;

public interface EvmWithdrawMapper extends BaseMapper<EvmWithdraw> {
}
