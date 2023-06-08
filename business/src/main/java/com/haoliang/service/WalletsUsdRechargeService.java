package com.haoliang.service;

import com.baomidou.mybatisplus.extension.service.IService;
import com.haoliang.model.WalletsUsdRecharge;

public interface WalletsUsdRechargeService extends IService<WalletsUsdRecharge> {

    void scanRechargeList();

}
