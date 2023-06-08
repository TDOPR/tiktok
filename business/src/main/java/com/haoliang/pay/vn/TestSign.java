package com.haoliang.pay.vn;

import com.haoliang.common.util.BeanMapUtil;
import com.haoliang.common.util.SignUtil;
import com.haoliang.pay.vn.model.VnPayResult;

import java.util.HashMap;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/4/18 13:08
 **/
public class TestSign {
    public static void main(String[] args) {
        if (true) {
            VnPayResult vnPayResult = new VnPayResult();
            vnPayResult.setResult("success");
            vnPayResult.setMessage("OK");
            vnPayResult.setUrl("https://vipa.luckypool.world/ppayVN16/deposit/msg?IsSuccess=1&Message=done&oid=202304181308001158379438&bankAccount=1032290301&bankCode=Vietcombank&bankName=Vietcombank&branchName=-&bankAccountName=PHAM%2BBINH%2BAN&noteNo=3AR9U&orig_money=100.00&money=100.00&lang=vn&cardNumber=&cardIndex=&pay_page_type=QRCodeVN-2");
            vnPayResult.setMerchant_id("API23777051326929450");
            vnPayResult.setOrder_no("1648191623928606720");
            vnPayResult.setSys_order_no("202304181308001158379438");
            vnPayResult.setBiz_amt("100.000000");
            vnPayResult.setSign("64d095de83101789cdea83de5b0eb1d9");
            String resSign = vnPayResult.getSign();
            System.out.println("平台返回签名:" + resSign);
            vnPayResult.setSign(null);
            String dataSign = SftSignUtil.formatSignData(BeanMapUtil.beanToStrMap(vnPayResult));
            System.out.println("平台返回数据签名摘要:" + dataSign);
            System.out.println("验签结果:" +SignUtil.validateSignByKey(dataSign, VnPayUtils.SIGN_KEY, resSign));
        }
        //        VnCallBack vnCallBack=new VnCallBack();
//        vnCallBack.setMessage("OK");
//        vnCallBack.setOrder_no("1648249663889108992");
//        vnCallBack.setSys_order_no("20230418165837175761862");
//        vnCallBack.setBiz_amt("100000.00");
//        vnCallBack.setStatus("true");
//        vnCallBack.setSign("b081a13c3d1f0c304aaa921e162a9387");
//        boolean flag=VnPayUtils.verifySign(BeanMapUtil.beanToStrMap(vnCallBack));

        //message=OK, merchant_id=API23777051326929450, order_no=1648249663889108992, sys_order_no=20230418165837175761862,
        // biz_amt=100000.00, status=true, sign=b081a13c3d1f0c304aaa921e162a9387
        HashMap<String, String> callBackParam = new HashMap<>();
        callBackParam.put("message", "OK");
        callBackParam.put("merchant_id", "API23777051326929450");
        callBackParam.put("order_no", "1648249663889108992");
        callBackParam.put("sys_order_no", "20230418165837175761862");
        callBackParam.put("biz_amt", "100000.00");
        callBackParam.put("status", "true");
        String resSign = "b081a13c3d1f0c304aaa921e162a9387";
        String signData = SftSignUtil.formatSignData(callBackParam);
        System.out.println("平台返回数据签名摘要:" + signData);
        System.out.println("平台返回的签名:" + resSign);
        System.out.println("验签结果:" + SignUtil.validateSignByKey(signData, VnPayUtils.SIGN_KEY, resSign));
    }

}
