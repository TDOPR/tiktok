package com.haoliang.service;

import com.baomidou.mybatisplus.extension.service.IService;
import com.haoliang.common.model.JsonResult;
import com.haoliang.model.FiatWithdrawOrder;
import com.haoliang.model.dto.ProxyPayAmountDTO;
import com.haoliang.model.vo.FiatWithdrawInfoVO;
import com.haoliang.pay.id.model.IdProxyPayCallBack;
import com.haoliang.pay.th.model.ThPayCallBack;
import com.haoliang.pay.vn.model.VnCallBack;

import java.util.List;

public interface FiatWithdrawOrderService  extends IService<FiatWithdrawOrder> {

    JsonResult<List<FiatWithdrawInfoVO>> info();

    JsonResult pay(ProxyPayAmountDTO amountDTO);

    String idCallBack(IdProxyPayCallBack idProxyPayCallBack);

    boolean callProxyPay(FiatWithdrawOrder fiatWithdrawOrder);

    String vnCallBack(VnCallBack vnCallBack);

    String thCallBack(ThPayCallBack thPayCallBack);

    void scanDelayProxyPayData();

    void editOrders(FiatWithdrawOrder fiatWithdrawOrder, Integer resStatus);
}
