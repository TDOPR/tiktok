package com.haoliang.pay.id;

import com.haoliang.common.constant.RSAKeyConstants;
import com.haoliang.common.util.BeanMapUtil;
import com.haoliang.common.util.encrypt.RsaUtil;
import com.haoliang.pay.id.model.IdPayCallBack;
import com.haoliang.pay.id.model.IdProxyPayCallBack;

import java.util.*;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/4/13 15:37
 **/
public class TestSign {
    public static void main(String[] args) throws Exception {
        if (false) {
            //私钥加密公钥解密
            String password = "123456";
            String encrypt = RsaUtil.encryptByPrivate(password, RSAKeyConstants.PRIVATE_KEY);
            String decrypt = RsaUtil.decryptByPublic(encrypt, RSAKeyConstants.PUBLIC_KEY);
            System.out.println("原文:" + password);
            System.out.println("用私钥加密后的数据:" + encrypt);
            System.out.println("用公钥解密的数据:" + decrypt);
            System.out.println("验签是否成功:" + RsaUtil.verifySignByPub(password, encrypt, RSAKeyConstants.PUBLIC_KEY));
        }
        if (false) {
            IdPayCallBack idPayCallBack = new IdPayCallBack();
            idPayCallBack.setCode("00");
            idPayCallBack.setEmail("test@email.com");
            idPayCallBack.setMethod("BNI");
            idPayCallBack.setMsg("SUCCESS");
            idPayCallBack.setName("JackMa");
            idPayCallBack.setOrderNum("1646394908200206336");
            idPayCallBack.setPayFee("7000");
            idPayCallBack.setPayMoney("10000");
            idPayCallBack.setPlatOrderNum("PRE1646394916110082122");
            idPayCallBack.setPlatSign("b1cM1G/j1yITub0IoHfL0sKkx07CXgJHfVDgbIoMOckOcSP5sW9QUSx0xhO1dGYXWV3uTmNsYgn7h9mgENZUfktCEv2mHHuNq+hcAHQbC1t70Fc/KIs4vlnJT6f+Ypuy6g1SXnWd/TGO3wH6EGBedcclZl7tVQc4FYZepSWrWL0=");
            idPayCallBack.setStatus("SUCCESS");
            boolean pass = IdPayUtils.verifySign(BeanMapUtil.beanToTreeMap(idPayCallBack));
            System.out.println("代收结果验签:" + pass);
        }

        if (true) {
            IdProxyPayCallBack idProxyPayCallBack=new IdProxyPayCallBack();
            idProxyPayCallBack.setBankCode("014");
            idProxyPayCallBack.setDescription("test cash");
            idProxyPayCallBack.setFee("6836");
            idProxyPayCallBack.setFeeType("0");
            idProxyPayCallBack.setMoney("16837");
            idProxyPayCallBack.setName("Elvira");
            idProxyPayCallBack.setNumber("4850412265");
            idProxyPayCallBack.setOrderNum("1647813372286693376");
            idProxyPayCallBack.setPlatOrderNum("W0620230417110459000871");
            idProxyPayCallBack.setPlatSign("HIunpmnH/XN839jkPxPAta4cD/TFbtv2N6CuBk24FqCDcBgWhpP0kVJb0kkIzOxLHSOq4V4gNEYVpVrripVPiTWbZ/7b6NtKQ3u7SD29a0U2Y5mt0mTgjnXqZUYvnB63EploJZFDAvC7PrB9iwx7OoGo/kA5jMlNsTJoYbSaMlw=");
            idProxyPayCallBack.setStatus("2");
            idProxyPayCallBack.setStatusMsg("SUCCESS");
            boolean pass=IdPayUtils.verifySign(BeanMapUtil.beanToTreeMap(idProxyPayCallBack));
            System.out.println("代付结果验签:" + pass);
        }
    }



}
