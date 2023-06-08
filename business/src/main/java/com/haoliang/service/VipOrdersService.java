package com.haoliang.service;

import com.baomidou.mybatisplus.extension.service.IService;
import com.haoliang.enums.TttLogTypeEnum;
import com.haoliang.model.VipOrders;

import java.math.BigDecimal;
import java.util.List;

public interface VipOrdersService  extends IService<VipOrders> {

    /**
     * 查询vip套餐余额大于0的记录
     * @param userId
     * @return
     */
    List<VipOrders> findByUserIdOrderByLevelAes(Integer userId,Integer level);

    /**
     * 社区收益从套餐包里扣费
     */
    BigDecimal  chargebacks(Integer userId,Integer level, BigDecimal amount, TttLogTypeEnum tttLogTypeEnum);

    List<VipOrders> getListByUserIdAndLevel(Integer userId,Integer level);

    void clearFrozenAmount();

}
