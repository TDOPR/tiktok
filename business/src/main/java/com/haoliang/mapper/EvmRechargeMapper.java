package com.haoliang.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.haoliang.model.usd.EvmRecharge;
import com.haoliang.model.condition.EvmTokenRechargeCondition;
import com.haoliang.model.vo.EvmRechargeVO;
import org.apache.ibatis.annotations.Param;

public interface EvmRechargeMapper extends BaseMapper<EvmRecharge> {
    IPage<EvmRechargeVO> page(IPage<EvmRecharge> page, @Param("param") EvmTokenRechargeCondition searchParam);
}
