
package com.haoliang.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.haoliang.model.usd.TrxAddressPool;

public interface TrxAddressPoolMapper extends BaseMapper<TrxAddressPool> {

    TrxAddressPool randomGetAddress(String networdName);

    int deleteByAddress(String address);
}
