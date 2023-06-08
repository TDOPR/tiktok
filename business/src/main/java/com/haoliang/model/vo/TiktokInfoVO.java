package com.haoliang.model.vo;

import com.haoliang.model.TikTokAccount;
import lombok.Data;

import java.util.List;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/3/13 16:26
 **/
@Data
public class TiktokInfoVO {

    /**
     * tiktok账号
     */
    private String tiktokUserName;

    /**
     * 已关联的账号列表
     */
    private List<TikTokAccount> tikTokAccountList;

}
