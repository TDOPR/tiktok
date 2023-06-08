package com.haoliang.model.vo;

import com.haoliang.common.config.GlobalProperties;
import lombok.Data;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/12/26 10:48
 **/
@Data
public class PdfVO {

    public PdfVO(String userAgreementUrl, String privacyPolicyUrl) {
        this.userAgreementUrl = userAgreementUrl;
        this.privacyPolicyUrl = privacyPolicyUrl;
        this.systemEmail = GlobalProperties.getEmail();
    }

    /**
     * 用户协议
     */
    private String userAgreementUrl;

    /**
     * 隐私政策
     */
    private String privacyPolicyUrl;

    /**
     * toPlainString邮箱号
     */
    private String systemEmail;

}
