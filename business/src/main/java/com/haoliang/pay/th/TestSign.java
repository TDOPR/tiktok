package com.haoliang.pay.th;

import com.haoliang.common.util.BeanMapUtil;
import com.haoliang.pay.th.model.ThPayCallBack;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/4/20 16:13
 **/
public class TestSign {
    public static void main(String[] args) {

        if(true) {
            //代付验签
            //mch_id=20, order_id=9546, order_no=1648954085581635584, state=2, trade_amount=100, message=, sign=248285602f2d145eaff5fd7510cec0c0
            ThPayCallBack thPayCallBack = new ThPayCallBack();
            thPayCallBack.setMch_id("20");
            thPayCallBack.setOrder_id("9546");
            thPayCallBack.setOrder_no("1648954085581635584");
            thPayCallBack.setState(2);
            thPayCallBack.setTrade_amount(100);
            thPayCallBack.setMessage("");
            thPayCallBack.setSign("248285602f2d145eaff5fd7510cec0c0");
            boolean flag = ThPayUtils.verifySign(BeanMapUtil.beanToStrMap(thPayCallBack));
            System.out.println("代付验签:"+flag);
        }

        if(true){
            //mch_id=20, order_id=9546, order_no=1648954085581635584, state=1, trade_amount=100, message=, sign=2f2d7d63e7b75b1bbe3c2bf1f9f2d1df
            ThPayCallBack thPayCallBack = new ThPayCallBack();
            thPayCallBack.setMch_id("20");
            thPayCallBack.setOrder_id("9546");
            thPayCallBack.setOrder_no("1648954085581635584");
            thPayCallBack.setState(1);
            thPayCallBack.setTrade_amount(100);
            thPayCallBack.setMessage("");
            thPayCallBack.setSign("2f2d7d63e7b75b1bbe3c2bf1f9f2d1df");
            boolean flag = ThPayUtils.verifySign(BeanMapUtil.beanToStrMap(thPayCallBack));
            System.out.println("代收验签:"+flag);
        }
    }
}
