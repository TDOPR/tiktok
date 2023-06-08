package com.haoliang.common.constant;

/**
 * 缓存key的前缀常量
 */
public interface CacheKeyPrefixConstants {

    /**
     * 分布式锁
     */
    String DISTRIBUTED_LOCK = "distributed_lock:";

    /**
     * 验证码
     */
    String CAPTCHA_CODE = "captcha_codes:";

    /**
     * 防重提交
     */
    String REPEAT_SUBMIT = "repeat_submit:";

    /**
     * 限流
     */
    String RATE_LIMIT = "rate_limit:";

    /**
     * 登录账户密码错误次数
     */
    String PWD_ERROR_COUNT = "pwd_error_count:";

    /**
     * 用户Token
     */
    String TOKEN = "token:";

    /**
     * 用户Token
     */
    String APP_TOKEN = "app_token:";

    /**
     * 菜单缓存
     */
    String SYS_MENU = "sys_menu";

    /**
     * 法币充值订单操作
     */
    String RECHARGE_ORDERS = "recharge_orders:";

    /**
     * 法币提现订单操作
     */
    String WITHDRAW_ORDERS = "withdraw_orders:";

}
