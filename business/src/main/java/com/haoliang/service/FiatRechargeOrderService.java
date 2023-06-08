package com.haoliang.service;

import com.baomidou.mybatisplus.extension.service.IService;
import com.haoliang.common.model.JsonResult;
import com.haoliang.model.FiatRechargeOrder;
import com.haoliang.model.dto.PayAmountDTO;
import com.haoliang.pay.id.model.IdPayCallBack;
import com.haoliang.pay.th.model.ThPayCallBack;
import com.haoliang.pay.vn.model.VnCallBack;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/4/15 14:21
 **/
public interface FiatRechargeOrderService extends IService<FiatRechargeOrder> {

    JsonResult info();

    JsonResult pay(PayAmountDTO amountDTO);

    JsonResult queryStatus(Long orderNo);

    JsonResult cancel(Long orderNo);

    String idCallBack(IdPayCallBack idPayCallBack);

    String vnCallBack(VnCallBack vnCallBack);

    String thCallBack(ThPayCallBack thPayCallBack);

    void editOrders(FiatRechargeOrder fiatRechargeOrder, Integer resStatus);
}
