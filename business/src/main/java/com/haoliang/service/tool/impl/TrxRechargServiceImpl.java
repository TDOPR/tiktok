package com.haoliang.service.tool.impl;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.haoliang.mapper.TrxRechargeMapper;
import com.haoliang.model.usd.TrxRecharge;
import com.haoliang.service.tool.TrxRechargService;
import org.springframework.stereotype.Service;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/4/4 12:10
 **/
@Service
public class TrxRechargServiceImpl extends ServiceImpl<TrxRechargeMapper, TrxRecharge>  implements TrxRechargService {
}
